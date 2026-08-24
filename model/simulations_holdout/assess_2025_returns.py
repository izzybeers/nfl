#%%
import numpy as np
import pandas as pd
import nflreadpy as nfl
from datetime import datetime
import sys
sys.path.insert(0, '..')
from model_functions import assess_probability_ranges, calculate_probability_ranges, calculate_ev_ranges, calculate_odds_ranges, get_prediction_by_model_type, prepare_new_data, read_from_supabase, read_bettinglines
from simulations_holdout.portfolio_optimization_functions import get_optimal_portfolio, calculate_ev_and_risk, remove_large_monotonicity_violations
from simulations_holdout.portfolio_assessment_helper_functions import *

team_lookup = read_from_supabase('MainData', 'TeamLookup')

df_schedules = nfl.load_schedules(2025)[['game_id','gameday','week', 'home_team','away_team', 'home_score','away_score', 'home_moneyline', 'away_moneyline']]
home_df = pd.DataFrame(df_schedules[['week', 'home_team','home_score', 'away_team', 'away_score', 'home_moneyline']], columns = ['Week', 'team', 'team_score', 'opponent_team', 'opp_score', 'moneyline'])
away_df = pd.DataFrame(df_schedules[['week', 'away_team','away_score', 'home_team', 'home_score', 'away_moneyline']], columns = ['Week', 'team', 'team_score', 'opponent_team', 'opp_score', 'moneyline'])
game_results = pd.concat([home_df, away_df], axis = 0)
game_results['win'] = game_results['team_score'] > game_results['opp_score']
game_results['diff'] = game_results['team_score'] - game_results['opp_score']
game_results = game_results[['Week','team', 'opponent_team', 'win','diff','moneyline']]
game_results['team'] = np.where(game_results['team'] == 'LA', 'LAR', game_results['team'])
game_results['opponent_team'] = np.where(game_results['opponent_team'] == 'LA', 'LAR', game_results['opponent_team'])

def clean_name(x):
  return x.str.replace(r"\b(jr|sr|i{1,3}|iv|v|vi{1,3}|ix|x|xi{1,3})\b", "", regex=True, case=False)\
    .str.replace(r"[^\w\s]", "", regex=True)\
      .str.replace(r"\s+", " ", regex=True)\
        .str.strip().str.lower()

url = f"https://github.com/nflverse/nflverse-data/releases/download/stats_player/stats_player_week_2025.parquet"
df = pd.read_parquet(url)
player_columns_to_keep = ['player_id', 'week', 'team', 'passing_yards', 'rushing_yards', 'receiving_yards', 'rushing_tds', 'receiving_tds', 'receptions']
playergl = df[player_columns_to_keep]
player_id_name_mapping = pd.DataFrame(nfl.load_players()[['gsis_id', 'display_name', 'position_group']], columns = ['gsis_id','display_name', 'Position']).drop_duplicates()\
  .assign(cleaned_name = lambda x: clean_name(x['display_name']))

bet_recs_raw = read_from_supabase(schema = 'betting', table_name = 'BetRecommendations', eq_col_name = 'Season', eq_value = '2025', chunk_size = 1000)

#player props
model_lookup_table = read_from_supabase('predictions', 'ModelSelections')
model_mapping = model_lookup_table[['response_var', 'model_type', 'training_root_folder', 'extra_calibration']].drop_duplicates()

# model_lookup_table = pd.DataFrame([
#     ['passing_yards_150',              'neural_net',    True],
#     ['passing_yards_180',              'neural_net',    True],
#     ['passing_yards_210',              'gbm',           False],
#     ['passing_yards_240',              'rf',            True],
#     ['passing_yards_270',              'tree ensemble', False],
#     ['passing_yards_300',              'neural_net',    True],
#     ['passing_yards_330',              'xgb',           False],
#     ['passing_yards_360',              'rf',            False],
#     ['rushing_yards_25',               'rf',            False],
#     ['rushing_yards_40',               'neural_net',    False],
#     ['rushing_yards_60',               'xgb',           False],
#     ['rushing_yards_80',               'full ensemble', False],
#     ['rushing_yards_100',              'gbm',           False],
#     ['rushing_yards_120',              'tree ensemble', False],
#     ['rushing_yards_140',              'rf',            False],
#     ['receiving_yards_25',             'xgb',           False],
#     ['receiving_yards_40',             'neural_net',    False],
#     ['receiving_yards_60',             'neural_net',    False],
#     ['receiving_yards_80',             'tree ensemble', False],
#     ['receiving_yards_100',            'full ensemble', False],
#     ['receiving_yards_120',            'neural_net',    True],
#     ['receiving_yards_140',            'neural_net',    True],
#     ['rushing_receiving_yards_40',     'xgb',           False],
#     ['rushing_receiving_yards_70',     'gbm',           True],
#     ['rushing_receiving_yards_100',    'gbm',           True],
#     ['rushing_receiving_yards_130',    'full ensemble', False],
#     ['receptions_4',                   'gbm',           False],
#     ['receptions_6',                   'rf',            False],
#     ['receptions_8',                   'rf',            False],
#     ['receptions_10',                  'full ensemble', False],
#     ['anytime_td_scorer',              'full ensemble', False],
#     ['team_win',                       'xgb',           True],
#     ['team_differential_2.5',          'tree ensemble', False],
#     ['team_differential_3.5',          'xgb',           False],
#     ['team_differential_6.5',          'xgb',           True],
#     ['team_differential_7.5',          'xgb',           False],
#     ['team_differential_minus2.5',     'neural_net',    True],
#     ['team_differential_minus3.5',     'gbm',           True],
#     ['team_differential_minus6.5',     'gbm',           False],
#     ['team_differential_minus7.5',     'tree ensemble', False],
# ], columns=[
#     'response_var',
#     'model_type',
#     'extra_calibration'
# ])
# model_lookup_table['training_root_folder'] = (
#     './models/training/'
#     + model_lookup_table['response_var']
#     + '/'
# )
# model_lookup_table['fullfit_root_folder'] = (
#     './models/full_fit/'
#     + model_lookup_table['response_var']
#     + '/'
# )
# old_model_assessments = {
#     # Passing
#     'passing_yards_150': 'Weakest',
#     'passing_yards_180': 'Medium',
#     'passing_yards_210': 'Weakest',
#     'passing_yards_240': 'Weakest',
#     'passing_yards_270': 'Medium',
#     'passing_yards_300': 'Medium',
#     'passing_yards_330': 'Medium',
#     'passing_yards_360': 'Strong',

#     # Rushing
#     'rushing_yards_25': 'Medium',
#     'rushing_yards_40': 'Medium',
#     'rushing_yards_60': 'Weakest',
#     'rushing_yards_80': 'Weakest',
#     'rushing_yards_100': 'Weakest',
#     'rushing_yards_120': 'Medium',
#     'rushing_yards_140': 'Medium',

#     # Receiving
#     'receiving_yards_25': 'Medium',
#     'receiving_yards_40': 'Medium',
#     'receiving_yards_60': 'Medium',
#     'receiving_yards_80': 'Strong',
#     'receiving_yards_100': 'Weakest',
#     'receiving_yards_120': 'Strong',
#     'receiving_yards_140': 'Extra Strong',

#     # Rushing + receiving
#     'rushing_receiving_yards_40': 'Strong',
#     'rushing_receiving_yards_70': 'Medium',
#     'rushing_receiving_yards_100': 'Weakest',
#     'rushing_receiving_yards_130': 'Extra Strong',

#     # Receptions
#     'receptions_4': 'Medium',
#     'receptions_6': 'Medium',
#     'receptions_8': 'Weakest',
#     'receptions_10': 'Extra Strong',

#     # TD
#     'anytime_td_scorer': 'Medium',

#     # Team
#     'team_win': 'Weakest',
#     'team_differential_2.5': 'Medium',
#     'team_differential_3.5': 'Weakest',
#     'team_differential_6.5': 'Weakest',
#     'team_differential_7.5': 'Weakest',
#     'team_differential_minus2.5': 'Weakest',
#     'team_differential_minus3.5': 'Weakest',
#     'team_differential_minus6.5': 'Weakest',
# }

# model_lookup_table['model_assessment'] = (
#     model_lookup_table['response_var']
#     .map(old_model_assessments)
# )
# model_mapping = model_lookup_table[['response_var', 'model_type', 'training_root_folder', 'extra_calibration']].drop_duplicates()



latest_player_betting_lines =  bet_recs_raw.sort_values('run_time', ascending = False).drop_duplicates(subset = ['Week', 'Player', 'Bet_Type', 'Label'], keep = 'first')\
  .assign(cleaned_name = lambda x: clean_name(x['Player']))\
              .merge(player_id_name_mapping.drop(columns = 'Position').drop_duplicates(), on = 'cleaned_name', how = 'left')\
                .merge(playergl, left_on = ['gsis_id', 'Week'], right_on = ['player_id','week'], how = 'left')\
                  .assign(BetHit = lambda x: np.select(
                    condlist = [(x['Bet_Type'] == 'Anytime TD Scorer') & (x['rushing_tds'] + x['receiving_tds'] > 0),
                                (x['Bet_Type'] == 'Receiving') & (x['receiving_yards'] >= pd.to_numeric(x['Label'].str.replace('+','', regex = False), errors = 'coerce')),
                                (x['Bet_Type'] == 'Rushing') & (x['rushing_yards'] >= pd.to_numeric(x['Label'].str.replace('+','', regex = False), errors = 'coerce')),
                                (x['Bet_Type'] == 'Passing') & (x['passing_yards'] >= pd.to_numeric(x['Label'].str.replace('+','', regex = False), errors = 'coerce'))],
                    choicelist = [True, True, True, True], default = False),
                    Total_Touchdowns = lambda x: x['rushing_tds'] + x['receiving_tds'],
                    BetTypePositionCombo = lambda x: x['Bet_Type'] + ' - ' + x['Position'],
                    response_var = lambda x: np.where(x['Bet_Type'] == 'Anytime TD Scorer', 'anytime_td_scorer', x['Bet_Type'].str.lower() + '_yards_' + x['Label'].str.replace('+','', regex = False)))\
                      [['Week', 'gsis_id', 'team', 'Position', 'response_var', 'BetHit', 'Odds']]

#ResultPayoutPer100 = lambda x: np.where(x['BetHit'], x['ToPayPer100'], 0)

#team_bets:
moneyline_betting_lines = game_results.assign(BetHit = lambda x: x['win'],
                                              response_var = 'team_win').\
                                                rename(columns = {'moneyline': 'Odds', 'week': 'Week'})\
                                                  [['response_var','team','opponent_team','Week','Odds','BetHit']]
  
#pull betting lines data to get betting lines for categories we didn't do last year (Receptions, Rushing+Receiving, Spread)
#bettinglines = read_from_supabase('betting', 'BettingLines', chunk_size = 1000, order_cols = ['selectionId', 'runTimeUTC'], keyset_pagination=True)
bettinglines = read_bettinglines()
bettinglines = bettinglines.assign(
    run_dt=lambda x: pd.to_datetime(x["runTimeUTC"], utc=True),
    start_dt=lambda x: pd.to_datetime(x["startEventDateUTC"], utc=True),
    points_num=lambda x: pd.to_numeric(x["points"], errors="coerce")
)

bettinglines_pregame = bettinglines[
    bettinglines["run_dt"] < bettinglines["start_dt"] - pd.Timedelta(minutes=10)
]

bettinglines_mostrecent = bettinglines_pregame.sort_values('run_dt', ascending = False)\
  .drop_duplicates(subset = ['EventName', 'name', 'subcategoryId', 'label', 'points_num'], keep = 'first')

string_of_all_player_names = '(' + '|'.join(player_id_name_mapping['cleaned_name']) + ')'
bettinglines_mostrecent['player_name'] = clean_name(bettinglines_mostrecent['name']).str.extract(string_of_all_player_names)
bettinglines_mostrecent = bettinglines_mostrecent[(bettinglines_mostrecent['name'].str.contains('Receptions')) |\
                                                  (bettinglines_mostrecent['name'].str.contains('Rushing') & bettinglines_mostrecent['name'].str.contains('Receiving')) |\
                                                  (bettinglines_mostrecent['name'] == 'Spread Alternate')]
bettinglines_mostrecent = bettinglines_mostrecent[bettinglines_mostrecent['points_num'].isna() | bettinglines_mostrecent['points_num'].isin([-7.5, -6.5, -3.5, -2.5, 2.5, 3.5, 6.5, 7.5])]
bettinglines_mostrecent = bettinglines_mostrecent.merge(player_id_name_mapping, left_on = 'player_name', right_on = 'cleaned_name', how='left')\
  .assign(Bet_Type = lambda x: np.select(
  [x['name'] == 'Spread Alternate', x['name'].str.contains('Receptions')],
  ['Spread Alternate', 'Receptions'],
  default = 'Rushing Receiving'
),
response_var=lambda x: np.where(
    x["Bet_Type"] == "Spread Alternate",
    "team_differential_"
    + (-x["points_num"])
        .map(lambda v: f"{v:g}" if pd.notna(v) else np.nan)
        .astype(str)
        .str.replace("-", "minus", regex=False),
    np.where(
        x["Bet_Type"] == "Receptions",
        "receptions_" + x["label"].str.replace("+", "", regex=False),
        "rushing_receiving_yards_" + x["label"].str.replace("+", "", regex=False)
    )
))

bettinglines_mostrecent['gameday'] = pd.to_datetime(bettinglines_mostrecent['startEventDateUTC']).dt.tz_convert('America/New_York').dt.date.astype(str)
bettinglines_mostrecent = bettinglines_mostrecent.merge(pd.DataFrame(df_schedules[['gameday','week']], columns = ['gameday','week']).drop_duplicates(), on = 'gameday', how='left')

receptions_and_rushrec_bets = bettinglines_mostrecent.query('Bet_Type.isin(["Receptions","Rushing Receiving"])')\
    .rename(columns = {'americanOdds': 'Odds'})\
      .merge(playergl, left_on = ['gsis_id','week'], right_on = ['player_id','week'], how='left')\
        .assign(BetHit = lambda x: np.select(
                    condlist = [(x['Bet_Type'] == 'Receptions') & (x['receptions'] >= pd.to_numeric(x['label'].str.replace('+','', regex = False), errors = 'coerce')),
                                (x['Bet_Type'] == 'Rushing Receiving') & (x['rushing_yards'] + x['receiving_yards'] >= pd.to_numeric(x['label'].str.replace('+','', regex = False), errors = 'coerce'))],
                    choicelist = [True, True], default = False))\
  .rename(columns = {'americanOdds': 'Odds',
                     'week': 'Week'})[[col for col in latest_player_betting_lines.columns]]
                    
player_prop_bets = pd.concat([latest_player_betting_lines, receptions_and_rushrec_bets], axis = 0)

spread_bets = bettinglines_mostrecent.query("(Bet_Type == 'Spread Alternate')")\
  .assign(team_shortname = lambda x: x['label'].str[3:].str.strip())\
  .merge(team_lookup[['TV_abbr','ShortName']], left_on = 'team_shortname', right_on = 'ShortName')\
    .merge(game_results[['team','opponent_team','Week','diff']], left_on = ['TV_abbr', 'week'], right_on = ['team','Week'], how = 'left')\
  [['Week', 'team', 'opponent_team', 'response_var', 'americanOdds', 'diff', 'points_num']]\
  .assign(BetHit=lambda x: x["diff"] > -x["points_num"])\
  .rename(columns = {'americanOdds': 'Odds'})[[col for col in moneyline_betting_lines.columns]]

team_bets = pd.concat([moneyline_betting_lines, spread_bets], axis = 0).assign(label = lambda x: x['team'])

player_preds_df = pd.DataFrame()
for r in np.unique(model_mapping[~model_mapping['response_var'].str.contains('team')]['response_var']):
  print(r)
  new_data = pd.read_parquet(f"../ml_ready_data/final_test/{r}.parquet")
  model_type = model_mapping[model_mapping['response_var'] == r]['model_type'].iloc[0]
  extra_calibration = model_mapping[model_mapping['response_var'] == r]['extra_calibration'].iloc[0]
  root_folder = model_mapping[model_mapping['response_var'] == r]['training_root_folder'].iloc[0]
  preds = get_prediction_by_model_type(r, model_type, root_folder.replace('.','..'), new_data, extra_calibration)
  player_preds_df = pd.concat([player_preds_df, pd.DataFrame({
    'response_var': r,
    'Week': new_data['week'].astype(int),
    'gsis_id': new_data['gsis_id'],
    'Model_Probability': preds
  })])

team_preds_df = pd.DataFrame()
for r in np.unique(model_mapping[model_mapping['response_var'].str.contains('team')]['response_var']):
  new_data = pd.read_parquet(f"../ml_ready_data/final_test/{r}.parquet")
  model_type = model_mapping[model_mapping['response_var'] == r]['model_type'].iloc[0]
  extra_calibration = model_mapping[model_mapping['response_var'] == r]['extra_calibration'].iloc[0]
  root_folder = model_mapping[model_mapping['response_var'] == r]['training_root_folder'].iloc[0]
  preds = get_prediction_by_model_type(r, model_type, root_folder.replace('./', '../'), new_data, extra_calibration)
  team_preds_df = pd.concat([team_preds_df, pd.DataFrame({
    'response_var': r,
    'Week': new_data['week'].astype(int),
    'team': new_data['team'],
    'Model_Probability': preds
  })], axis = 0)

player_preds_df = player_preds_df.merge(player_prop_bets, on = ['response_var','gsis_id','Week']).merge(game_results[['Week','team','opponent_team']], on = ['Week','team'])
team_preds_df = team_preds_df.merge(
    team_bets,
    on=["response_var", "team", "Week"],
    validate="one_to_one"
)
player_prop_key_counts = (
    player_prop_bets
    .groupby(["response_var", "gsis_id", "Week"])
    .size()
)
assert player_prop_key_counts.max() == 1, player_prop_key_counts.sort_values(ascending=False).head(20)
team_bet_key_counts = (
    team_bets
    .groupby(["response_var", "team", "Week"])
    .size()
)
assert team_bet_key_counts.max() == 1, team_bet_key_counts.sort_values(ascending=False).head(20)


all_bets_df = pd.concat([player_preds_df.rename(columns = {'gsis_id': 'label'}), team_preds_df.assign(Position = 'Team')], axis = 0)

correlations_up_to_2024 = read_from_supabase('MainData', 'Correlations').query('max_year == 2024')
latest_timestamp = correlations_up_to_2024['created_at'].max()
correlations_up_to_2024 = correlations_up_to_2024[correlations_up_to_2024['created_at'] == latest_timestamp].copy()

#quick checks before running optimizer:
print(all_bets_df.columns)
print(all_bets_df[["response_var", "Odds", "BetHit", "Model_Probability"]].head())
assert "Odds" in all_bets_df.columns
assert "BetHit" in all_bets_df.columns
assert "Model_Probability" in all_bets_df.columns
assert prepare_new_data.__module__ == "model_functions"
assert get_prediction_by_model_type.__module__ == "model_functions"

assert all_bets_df["Odds"].notna().all()
assert all_bets_df["Model_Probability"].between(0, 1).all()
assert all_bets_df["BetHit"].notna().all()

team_diff_live_check = bettinglines_mostrecent.query("name == 'Spread Alternate'")[
    ["EventName", "label", "points_num", "americanOdds", "run_dt", "start_dt"]
].assign(
    minutes_before_kickoff=lambda x: (x["start_dt"] - x["run_dt"]).dt.total_seconds() / 60
)

assert (team_diff_live_check["minutes_before_kickoff"] >= 10).all()

screened = remove_large_monotonicity_violations(all_bets_df, tolerance=0.05)

player_bets_only = screened[~screened['response_var'].str.contains('team_')]



#player + team bets, no screened
t1 = datetime.now()
portfolio_20_bets_with_no_penalties = get_optimal_portfolio(all_bets_df, model_lookup_table, correlations_up_to_2024, max_bets = 20, penalize_by_model = False, penalize_by_bin = False, apply_ev_caps = True, return_all_portfolios = True)
print(datetime.now() - t1)

t1 = datetime.now()
portfolio_20_bets_with_model_penalties = get_optimal_portfolio(all_bets_df, model_lookup_table, correlations_up_to_2024, max_bets = 20, penalize_by_model = True, penalize_by_bin = False, apply_ev_caps = True, return_all_portfolios = True)
print(datetime.now() - t1)

# t1 = datetime.now()
# portfolio_20_bets_with_model_and_bin_penalties = get_optimal_portfolio(all_bets_df, model_lookup_table, correlations_up_to_2024, max_bets = 20, penalize_by_model = True, penalize_by_bin = True, apply_ev_caps = True, return_all_portfolios = True)
# print(datetime.now() - t1)

t1 = datetime.now()
uncapped_portfolio_20_bets_with_model_penalties = get_optimal_portfolio(all_bets_df, model_lookup_table, correlations_up_to_2024, max_bets = 20, penalize_by_model = True, penalize_by_bin = False, apply_ev_caps = False, return_all_portfolios = True)
print(datetime.now() - t1)

#player + team bets, screened (removed monotonicity violations)
t1 = datetime.now()
portfolio_20_bets_screened_with_no_penalties = get_optimal_portfolio(screened, model_lookup_table, correlations_up_to_2024, max_bets = 20, penalize_by_model = False, penalize_by_bin = False, apply_ev_caps = True, return_all_portfolios = True)
print(datetime.now() - t1)

t1 = datetime.now()
portfolio_20_bets_screened_with_model_penalties = get_optimal_portfolio(screened, model_lookup_table, correlations_up_to_2024, max_bets = 20, penalize_by_model = True, penalize_by_bin = False, apply_ev_caps = True, return_all_portfolios = True)
print(datetime.now() - t1)

# t1 = datetime.now()
# portfolio_20_bets_screened_with_model_and_bin_penalties = get_optimal_portfolio(screened, model_lookup_table, correlations_up_to_2024, max_bets = 20, penalize_by_model = True, penalize_by_bin = True, apply_ev_caps = True, return_all_portfolios = True)
# print(datetime.now() - t1)

t1 = datetime.now()
uncapped_portfolio_20_bets_screened_with_model_penalties = get_optimal_portfolio(screened, model_lookup_table, correlations_up_to_2024, max_bets = 20, penalize_by_model = True, penalize_by_bin = False, apply_ev_caps = False, return_all_portfolios = True)
print(datetime.now() - t1)

#player bets only (removed monotonicity violations)
t1 = datetime.now()
portfolio_20_bets_screened_player_only_with_no_penalties = get_optimal_portfolio(player_bets_only, model_lookup_table, correlations_up_to_2024, max_bets = 20, penalize_by_model = False, penalize_by_bin = False, apply_ev_caps = True, return_all_portfolios = True)
print(datetime.now() - t1)

t1 = datetime.now()
portfolio_20_bets_screened_player_only_with_model_penalties = get_optimal_portfolio(player_bets_only, model_lookup_table, correlations_up_to_2024, max_bets = 20, penalize_by_model = True, penalize_by_bin = False, apply_ev_caps = True, return_all_portfolios = True)
print(datetime.now() - t1)

t1 = datetime.now()
portfolio_10_bets_screened_player_only_with_model_penalties = get_optimal_portfolio(player_bets_only, model_lookup_table, correlations_up_to_2024, max_bets = 10, penalize_by_model = True, penalize_by_bin = False, apply_ev_caps = True, return_all_portfolios = True)
print(datetime.now() - t1)

# t1 = datetime.now()
# portfolio_20_bets_screened_player_only_with_model_and_bin_penalties = get_optimal_portfolio(player_bets_only, model_lookup_table, correlations_up_to_2024, max_bets = 20, penalize_by_model = True, penalize_by_bin = True, apply_ev_caps = True, return_all_portfolios = True)
# print(datetime.now() - t1)

t1 = datetime.now()
uncapped_portfolio_screened_player_only_20_bets_with_model_penalties = get_optimal_portfolio(player_bets_only, model_lookup_table, correlations_up_to_2024, max_bets = 20, penalize_by_model = True, penalize_by_bin = False, apply_ev_caps = False, return_all_portfolios = True)
print(datetime.now() - t1)

#player + team bets, no screened
t1 = datetime.now()
portfolio_10_bets_with_no_penalties = get_optimal_portfolio(all_bets_df, model_lookup_table, correlations_up_to_2024, max_bets = 10, penalize_by_model = False, penalize_by_bin = False, apply_ev_caps = True, return_all_portfolios = True)
print(datetime.now() - t1)

t1 = datetime.now()
portfolio_10_bets_with_model_penalties = get_optimal_portfolio(all_bets_df, model_lookup_table, correlations_up_to_2024, max_bets = 10, penalize_by_model = True, penalize_by_bin = False, apply_ev_caps = True, return_all_portfolios = True)
print(datetime.now() - t1)

# t1 = datetime.now()
# portfolio_10_bets_with_model_and_bin_penalties = get_optimal_portfolio(all_bets_df, model_lookup_table, correlations_up_to_2024, max_bets = 10, penalize_by_model = True, penalize_by_bin = True, apply_ev_caps = True, return_all_portfolios = True)
# print(datetime.now() - t1)

t1 = datetime.now()
uncapped_portfolio_10_bets_with_model_penalties = get_optimal_portfolio(all_bets_df, model_lookup_table, correlations_up_to_2024, max_bets = 10, penalize_by_model = True, penalize_by_bin = False, apply_ev_caps = False, return_all_portfolios = True)
print(datetime.now() - t1)

#player + team bets, screened (removed monotonicity violations)
t1 = datetime.now()
portfolio_10_bets_screened_with_no_penalties = get_optimal_portfolio(screened, model_lookup_table, correlations_up_to_2024, max_bets = 10, penalize_by_model = False, penalize_by_bin = False, apply_ev_caps = True, return_all_portfolios = True)
print(datetime.now() - t1)

t1 = datetime.now()
portfolio_10_bets_screened_with_model_penalties = get_optimal_portfolio(screened, model_lookup_table, correlations_up_to_2024, max_bets = 10, penalize_by_model = True, penalize_by_bin = False, apply_ev_caps = True, return_all_portfolios = True)
print(datetime.now() - t1)

# t1 = datetime.now()
# portfolio_10_bets_screened_with_model_and_bin_penalties = get_optimal_portfolio(screened, model_lookup_table, correlations_up_to_2024, max_bets = 10, penalize_by_model = True, penalize_by_bin = True, apply_ev_caps = True, return_all_portfolios = True)
# print(datetime.now() - t1)

t1 = datetime.now()
uncapped_portfolio_10_bets_screened_with_model_penalties = get_optimal_portfolio(screened, model_lookup_table, correlations_up_to_2024, max_bets = 10, penalize_by_model = True, penalize_by_bin = False, apply_ev_caps = False, return_all_portfolios = True)
print(datetime.now() - t1)

#player bets only (removed monotonicity violations)
t1 = datetime.now()
portfolio_10_bets_screened_player_only_with_no_penalties = get_optimal_portfolio(player_bets_only, model_lookup_table, correlations_up_to_2024, max_bets = 10, penalize_by_model = False, penalize_by_bin = False, apply_ev_caps = True, return_all_portfolios = True)
print(datetime.now() - t1)

t1 = datetime.now()
portfolio_10_bets_screened_player_only_with_model_penalties = get_optimal_portfolio(player_bets_only, model_lookup_table, correlations_up_to_2024, max_bets = 10, penalize_by_model = True, penalize_by_bin = False, apply_ev_caps = True, return_all_portfolios = True)
print(datetime.now() - t1)

# t1 = datetime.now()
# portfolio_10_bets_screened_player_only_with_model_and_bin_penalties = get_optimal_portfolio(player_bets_only, model_lookup_table, correlations_up_to_2024, max_bets = 10, penalize_by_model = True, penalize_by_bin = True, apply_ev_caps = True, return_all_portfolios = True)
# print(datetime.now() - t1)

t1 = datetime.now()
uncapped_portfolio_screened_player_only_10_bets_with_model_penalties = get_optimal_portfolio(player_bets_only, model_lookup_table, correlations_up_to_2024, max_bets = 10, penalize_by_model = True, penalize_by_bin = False, apply_ev_caps = False, return_all_portfolios = True)
print(datetime.now() - t1)


game_days_of_week_home = pd.DataFrame(nfl.load_schedules(2025)[['season','week','weekday','home_team']], columns = ['season','week','weekday','home_team']).rename(columns = {'home_team':'team'})
game_days_of_week_away = pd.DataFrame(nfl.load_schedules(2025)[['season','week','weekday','away_team']], columns = ['season','week','weekday','away_team']).rename(columns = {'away_team':'team'})
game_days_of_week = pd.concat([game_days_of_week_home, game_days_of_week_away],axis=0)
thursday_games = game_days_of_week.query('weekday.isin(["Thursday","Wednesday","Friday"])').drop(columns = 'weekday')
sunday_games = game_days_of_week.query('weekday.isin(["Saturday","Sunday"])').drop(columns = 'weekday')
monday_games = game_days_of_week.query('weekday == "Monday"').drop(columns = 'weekday')

thursday_portfolio_10_bets_with_model_penalties_only_all_portfolios = get_optimal_portfolio(screened.merge(thursday_games, left_on = ['Week','team'], right_on = ['week','team']), model_lookup_table, correlations_up_to_2024, max_bets = 10, penalize_by_model = True, penalize_by_bin = False, apply_ev_caps = True, return_all_portfolios = True)
sunday_portfolio_10_bets_with_model_penalties_only_all_portfolios = get_optimal_portfolio(screened.merge(sunday_games, left_on = ['Week','team'], right_on =  ['week','team']), model_lookup_table, correlations_up_to_2024, max_bets = 10, penalize_by_model = True, penalize_by_bin = False, apply_ev_caps = True, return_all_portfolios = True)
monday_portfolio_10_bets_with_model_penalties_only_all_portfolios = get_optimal_portfolio(screened.merge(monday_games, left_on = ['Week','team'], right_on = ['week','team']), model_lookup_table, correlations_up_to_2024, max_bets = 10, penalize_by_model = True, penalize_by_bin = False, apply_ev_caps = True, return_all_portfolios = True)
thursday_portfolio_10_bets_no_penalties_all_portfolios = get_optimal_portfolio(screened.merge(thursday_games, left_on = ['Week','team'], right_on = ['week','team']), model_lookup_table, correlations_up_to_2024, max_bets = 10, penalize_by_model = False, penalize_by_bin = False, apply_ev_caps = True, return_all_portfolios = True)
sunday_portfolio_10_bets_no_penalties_all_portfolios = get_optimal_portfolio(screened.merge(sunday_games, left_on = ['Week','team'], right_on =  ['week','team']), model_lookup_table, correlations_up_to_2024, max_bets = 10, penalize_by_model = False, penalize_by_bin = False, apply_ev_caps = True, return_all_portfolios = True)
monday_portfolio_10_bets_no_penalties_all_portfolios = get_optimal_portfolio(screened.merge(monday_games, left_on = ['Week','team'], right_on = ['week','team']), model_lookup_table, correlations_up_to_2024, max_bets = 10, penalize_by_model = False, penalize_by_bin = False, apply_ev_caps = True, return_all_portfolios = True)

list_of_portfolios_to_run = {
    "Player + team, 20 bets, no penalties, unscreened": portfolio_20_bets_with_no_penalties,
    "Player + team, 20 bets, model penalties, unscreened": portfolio_20_bets_with_model_penalties,
    #"Player + team, 20 bets, model + bin penalties, unscreened": portfolio_20_bets_with_model_and_bin_penalties,
    "Player + team, 20 bets, model penalties, unscreened, no EV caps": uncapped_portfolio_20_bets_with_model_penalties,

    "Player + team, 20 bets, no penalties, screened": portfolio_20_bets_screened_with_no_penalties,
    "Player + team, 20 bets, model penalties, screened": portfolio_20_bets_screened_with_model_penalties,
    #"Player + team, 20 bets, model + bin penalties, screened": portfolio_20_bets_screened_with_model_and_bin_penalties,
    "Player + team, 20 bets, model penalties, screened, no EV caps": uncapped_portfolio_20_bets_screened_with_model_penalties,

    "Player only, 20 bets, no penalties, screened": portfolio_20_bets_screened_player_only_with_no_penalties,
    "Player only, 20 bets, model penalties, screened": portfolio_20_bets_screened_player_only_with_model_penalties,
    #"Player only, 20 bets, model + bin penalties, screened": portfolio_20_bets_screened_player_only_with_model_and_bin_penalties,
    "Player only, 20 bets, model penalties, screened, no EV caps": uncapped_portfolio_screened_player_only_20_bets_with_model_penalties,

    "Player + team, 10 bets, no penalties, unscreened": portfolio_10_bets_with_no_penalties,
    "Player + team, 10 bets, model penalties, unscreened": portfolio_10_bets_with_model_penalties,
    #"Player + team, 10 bets, model + bin penalties, unscreened": portfolio_10_bets_with_model_and_bin_penalties,
    "Player + team, 10 bets, model penalties, unscreened, no EV caps": uncapped_portfolio_10_bets_with_model_penalties,

    "Player + team, 10 bets, no penalties, screened": portfolio_10_bets_screened_with_no_penalties,
    "Player + team, 10 bets, model penalties, screened": portfolio_10_bets_screened_with_model_penalties,
    #"Player + team, 10 bets, model + bin penalties, screened": portfolio_10_bets_screened_with_model_and_bin_penalties,
    "Player + team, 10 bets, model penalties, screened, no EV caps": uncapped_portfolio_10_bets_screened_with_model_penalties,

    "Player only, 10 bets, no penalties, screened": portfolio_10_bets_screened_player_only_with_no_penalties,
    "Player only, 10 bets, model penalties, screened": portfolio_10_bets_screened_player_only_with_model_penalties,
    #"Player only, 10 bets, model + bin penalties, screened": portfolio_10_bets_screened_player_only_with_model_and_bin_penalties,
    "Player only, 10 bets, model penalties, screened, no EV caps": uncapped_portfolio_screened_player_only_10_bets_with_model_penalties,
}


list_of_portfolios_to_run = {
    "Player + team, 20 bets, no penalties, screened": portfolio_20_bets_screened_with_no_penalties,
    "Player only, 10 bets, no penalties, screened": portfolio_10_bets_screened_player_only_with_no_penalties
}

bet_amount = 50
for sd_cap in [1000, 10, 7, 5, 3, 2, 1.5, 1, 0.5]:
    print(f"\n\n========== SD CAP: {sd_cap} ==========")
    for portfolio_name, portfolio in list_of_portfolios_to_run.items():
        print(f"\n{portfolio_name}")
        amounts = get_amounts_from_portfolio(
            find_best_portfolio_by_mu_with_sd_cap(portfolio, sd_cap),
            bet_amount,
            0.50
        )
        print_return_summary(amounts, bet_amount)
        print(aggregate_portfolios(amounts, 'Type', bet_amount))
        print(aggregate_portfolios(amounts, 'Week', bet_amount))

bet_amount = 50
print('gameday portfolios')
for sd_cap in [1000, 10, 7, 5, 3, 2, 1.5, 1, 0.5]:
    print(f"\n\n========== SD CAP: {sd_cap} ==========")
    print('With model penalty')
    thursday_amts = get_amounts_from_portfolio(find_best_portfolio_by_mu_with_sd_cap(thursday_portfolio_10_bets_with_model_penalties_only_all_portfolios,sd_cap), 0.2*bet_amount, 0.50)
    sunday_amts = get_amounts_from_portfolio(find_best_portfolio_by_mu_with_sd_cap(sunday_portfolio_10_bets_with_model_penalties_only_all_portfolios,sd_cap), 0.6*bet_amount, 0.50)
    monday_amts = get_amounts_from_portfolio(find_best_portfolio_by_mu_with_sd_cap(monday_portfolio_10_bets_with_model_penalties_only_all_portfolios,sd_cap), 0.2*bet_amount, 0.50)
    thursday_amts['Portfolio_Weight']  *= 0.2
    sunday_amts['Portfolio_Weight']  *= 0.6
    monday_amts['Portfolio_Weight']  *= 0.2
    print('Thursday bet counts:')
    print(
        thursday_amts
        .groupby('Week', as_index=False)
        .agg(
            num_bets=('Portfolio_Weight', 'size'),
            med_odds=('Odds', 'median')
        )
    )
    portfolios_by_day = pd.concat([thursday_amts, sunday_amts, monday_amts], axis = 0)
    amounts =  portfolios_by_day
    print_return_summary(amounts, bet_amount)
    print(aggregate_portfolios(amounts, 'Type', bet_amount))
    print(aggregate_portfolios(amounts, 'Week', bet_amount))
    print('Without model penalty:')
    thursday_amts = get_amounts_from_portfolio(find_best_portfolio_by_mu_with_sd_cap(thursday_portfolio_10_bets_no_penalties_all_portfolios,sd_cap), 0.2*bet_amount, 0.50)
    sunday_amts = get_amounts_from_portfolio(find_best_portfolio_by_mu_with_sd_cap(sunday_portfolio_10_bets_no_penalties_all_portfolios,sd_cap), 0.6*bet_amount, 0.50)
    monday_amts = get_amounts_from_portfolio(find_best_portfolio_by_mu_with_sd_cap(monday_portfolio_10_bets_no_penalties_all_portfolios,sd_cap), 0.2*bet_amount, 0.50)
    thursday_amts['Portfolio_Weight']  *= 0.2
    sunday_amts['Portfolio_Weight']  *= 0.6
    monday_amts['Portfolio_Weight']  *= 0.2
    print(
        thursday_amts
        .groupby('Week', as_index=False)
        .agg(
            num_bets=('Portfolio_Weight', 'size'),
            med_odds=('Odds', 'median')
        )
    )
    portfolios_by_day = pd.concat([thursday_amts, sunday_amts, monday_amts], axis = 0)
    amounts =  portfolios_by_day
    print_return_summary(amounts, bet_amount)
    print(aggregate_portfolios(amounts, 'Type', bet_amount))
    print(aggregate_portfolios(amounts, 'Week', bet_amount))

  
#top N bets by ev only:
number_of_straight_bets = range(1, 21)
print('Top straight bets with no odds cap.')
for n in number_of_straight_bets:
  top_bets_by_ev_only = calculate_ev_and_risk(screened, model_lookup_table, penalize_by_model = False, penalize_by_bin = False, positive_only = True, apply_ev_caps = True)\
  .sort_values(['Week', 'EVProfitPer100'], ascending = [True, False]).groupby('Week',as_index=False).head(n).reset_index(drop=True)\
    .assign(Portfolio_Weight = n/100)
  weekly_returns = aggregate_portfolios(top_bets_by_ev_only, 'Week')
  weekly_returns = aggregate_portfolios(top_bets_by_ev_only, 'Type')
  print(f"Top {n} bets per week by ev only")
  print_return_summary(top_bets_by_ev_only, 100)
  print(weekly_returns)

#straight bets with odds cap
number_of_straight_bets = range(1, 21)
odds_cap = 1000
print('Top straight bets with odds cap 1000.')
for n in number_of_straight_bets:
  top_bets_by_ev_only = calculate_ev_and_risk(screened[screened['Odds'] < odds_cap], model_lookup_table, penalize_by_model = False, penalize_by_bin = False, positive_only = True, apply_ev_caps = True)\
  .sort_values(['Week', 'EVProfitPer100'], ascending = [True, False]).groupby('Week',as_index=False).head(n).reset_index(drop=True)\
    .assign(Portfolio_Weight = n/100)
  weekly_returns = aggregate_portfolios(top_bets_by_ev_only, 'Week')
  weekly_returns = aggregate_portfolios(top_bets_by_ev_only, 'Type')
  print(f"Top {n} bets per week by ev only")
  print_return_summary(top_bets_by_ev_only, 100)
  print(weekly_returns)



  betting_lines_with_probability = calculate_odds_ranges(calculate_ev_ranges(calculate_probability_ranges(calculate_ev_and_risk(all_bets_df, model_lookup_table, penalize_by_model = False, penalize_by_bin = False, positive_only = False))))\
    .assign(ResultPayoutPer100 = lambda x: np.where(x['BetHit'], x['ProfitPer100']+100, 0))

  betting_lines_with_probability.groupby(['EV_Range_group','Odds_Range'],as_index=False)\
    .agg(ResultPayoutPer100 = ('ResultPayoutPer100','sum'),
         NumBets = ('ResultPayoutPer100', 'size')).\
          assign(EstimatedReturn100Each = lambda x: (x['ResultPayoutPer100'] - 100*x['NumBets'])/(100*x['NumBets']))\
    .sort_values(['EV_Range_group','Odds_Range'])\
    .pivot(columns = 'EV_Range_group', index = 'Odds_Range', values = 'EstimatedReturn100Each')

# betting_lines_with_probability.\
#   query('EVProfitPer100 > 0').\
#     assign(bets_return = lambda x: (x['ResultPayoutPer100']-100)/100).\
#       groupby('response_var',as_index=False).\
#         agg(avg_return = ('bets_return','mean'),
#         num_bets = ('bets_return','size'),
#         bet_win_pct = ('BetHit','mean'))\
#           .sort_values('avg_return', ascending = False)

# betting_lines_with_probability.\
#   query('EVProfitPer100 > 25').\
#     assign(bets_return = lambda x: (x['ResultPayoutPer100']-100)/100).\
#       groupby('response_var',as_index=False).\
#         agg(avg_return = ('bets_return','mean'),
#         num_bets = ('bets_return','size'))\
#           .sort_values('avg_return', ascending = False)

# betting_lines_with_probability.\
#   query('EVProfitPer100 > 50').\
#     assign(bets_return = lambda x: (x['ResultPayoutPer100']-100)/100).\
#       groupby('response_var',as_index=False).\
#         agg(avg_return = ('bets_return','mean'),
#         num_bets = ('bets_return','size'),
#         bet_win_pct = ('BetHit','mean'))\
#           .sort_values('avg_return', ascending = False)

  
#check draftkings margin by each odds range -- add this in.



  #return if place every bet with positive EV:
  spent = 100*len(betting_lines_with_probability)
  payout = sum(betting_lines_with_probability['ResultPayoutPer100'])
  print(f"Money spent if bet on everything $100 each: ${spent:,.0f}")
  print(f"Total payout: ${payout:,.0f}")
  print(f"Return: {100*(payout-spent)/spent:,.1f}%")
  positive_ev_bets = betting_lines_with_probability[betting_lines_with_probability['EVProfitPer100'] > 0]
  spent = 100*len(positive_ev_bets)
  payout = sum(positive_ev_bets['ResultPayoutPer100'])
  print(f"Money spent if bet on all positive ev bets $100 each: ${spent:,.0f}")
  print(f"Total payout: ${payout:,.0f}")
  print(f"Return: {100*(payout-spent)/spent:,.1f}%")
  positive_ev_bets_above_25 = betting_lines_with_probability[betting_lines_with_probability['EVProfitPer100'] > 25]
  spent = 100*len(positive_ev_bets_above_25)
  payout = sum(positive_ev_bets_above_25['ResultPayoutPer100'])
  print(f"Money spent if bet on all >25 ev bets $100 each: ${spent:,.0f}")
  print(f"Total payout: ${payout:,.0f}")
  print(f"Return: {100*(payout-spent)/spent:,.1f}%")
  positive_ev_bets_above_50 = betting_lines_with_probability[betting_lines_with_probability['EVProfitPer100'] > 50]
  spent = 100*len(positive_ev_bets_above_50)
  payout = sum(positive_ev_bets_above_50['ResultPayoutPer100'])
  print(f"Money spent if bet on all >50 ev bets $100 each: ${spent:,.0f}")
  print(f"Total payout: ${payout:,.0f}")
  print(f"Return: {100*(payout-spent)/spent:,.1f}%")


#how well is model calibrated at different EV ranges, compared to implied probability?

legacy_ev_check = all_bets_df.copy()\
    .assign(ProfitPer100=lambda x: np.where(x['Odds'] > 0, x['Odds'], 10000 / -x['Odds']),
        BettingLine_Probability=lambda x: x['Odds'].apply(lambda o: 100 / (o + 100) if o > 0 else -o / (-o + 100) if o < 0 else np.nan),
        EV=lambda x: x['Model_Probability'] * (x['ProfitPer100'] + 100) - 100)

ev_bins = [-np.inf, -50, -25, 0, 25, 50, 75, 100, 150, 200, 300, np.inf]
ev_labels = ['< -50', '-50 to -25', '-25 to 0', '0 to 25', '25 to 50', '50 to 75', '75 to 100',
             '100 to 150', '150 to 200', '200 to 300', '300+']
odds_bins = [-np.inf, -300, -200, -150, -1, 150, 200, 300, 500, 1000, 2000, np.inf]

odds_labels = ['<= -300', '-299 to -200', '-199 to -150', '-149 to -1', '0 to +149',
               '+150 to +199', '+200 to +299', '+300 to +499', '+500 to +999', '+1000 to +1999', '2000+']

legacy_ev_check['EV_Range'] = pd.cut(legacy_ev_check['EV'], bins=ev_bins, labels=ev_labels, right=False)

legacy_ev_calibration = legacy_ev_check.groupby('EV_Range', observed=False)\
  .agg(N=('BetHit', 'size'),
       Actual_Pct=('BetHit', 'mean'),
       Model_Probability=('Model_Probability', 'mean'),
       BettingLine_Probability=('BettingLine_Probability', 'mean'),
       Avg_EV=('EV', 'mean'),
       Median_EV=('EV', 'median')
    ).reset_index()

legacy_ev_calibration.assign(
    Model_Error = lambda x: x['Model_Probability'] - x['Actual_Pct'],
    BettingLine_Error = lambda x: x['BettingLine_Probability'] - x['Actual_Pct'],
    Avg_EV=lambda x: x['Avg_EV'].round(1),
    Median_EV=lambda x: x['Median_EV'].round(1)
)
import matplotlib.pyplot as plt

plot_df = legacy_ev_calibration.copy()

plt.figure(figsize=(12, 6))

plt.plot(plot_df['EV_Range'].astype(str), 100 * plot_df['Actual_Pct'], marker='o', label='Actual %')
plt.plot(plot_df['EV_Range'].astype(str), 100 * plot_df['BettingLine_Probability'], marker='o', label='Betting Line Probability')
plt.plot(plot_df['EV_Range'].astype(str), 100 * plot_df['Model_Probability'], marker='o', label='Model Probability')

plt.xlabel('EV Range')
plt.ylabel('Probability (%)')
plt.title('Actual vs Model vs Betting Line Probability by EV Range')
plt.xticks(rotation=45)
plt.legend()
plt.grid(alpha=.3)
plt.tight_layout()
plt.show()



legacy_ev_check['Odds_Range'] = pd.cut(legacy_ev_check['Odds'], bins=odds_bins, labels=odds_labels, right=False)
odds_calibration = legacy_ev_check.groupby('Odds_Range', observed=False).agg(
    Actual_Pct=('BetHit', 'mean'),
    BettingLine_Probability=('BettingLine_Probability', 'mean'),
    Model_Probability=('Model_Probability', 'mean')
).reset_index()

plt.figure(figsize=(12, 6))
plt.plot(odds_calibration['Odds_Range'].astype(str), 100 * odds_calibration['Actual_Pct'], marker='o', label='Actual %')
plt.plot(odds_calibration['Odds_Range'].astype(str), 100 * odds_calibration['BettingLine_Probability'], marker='o', label='Betting Line Probability')
plt.plot(odds_calibration['Odds_Range'].astype(str), 100 * odds_calibration['Model_Probability'], marker='o', label='Model Probability')

plt.xlabel('Odds Range')
plt.ylabel('Probability (%)')
plt.title('Actual vs Model vs Betting Line Probability by Odds Range')
plt.xticks(rotation=45)
plt.legend()
plt.grid(alpha=.3)
plt.tight_layout()
plt.show()


#how much does the sportsbook take by odds range (if we were to bet on every bet):

odds_return_check = betting_lines_with_probability.copy()
odds_return_check['Odds_Range'] = pd.cut(odds_return_check['Odds'], bins=odds_bins, labels=odds_labels, right=False)
odds_return_check['Implied_Prob'] = odds_return_check['Odds'].apply(lambda o: 100 / (o + 100) if o > 0 else -o / (-o + 100))

odds_return_summary = odds_return_check.groupby('Odds_Range', observed=False)\
    .agg(N=('ResultPayoutPer100', 'size'), Payout=('ResultPayoutPer100', 'sum'), Hit_Rate=('BetHit', 'mean'),
         Avg_Implied_Prob=('Implied_Prob', 'mean'))\
    .reset_index()\
    .assign(Spent=lambda x: 100 * x['N'], Return_Pct=lambda x: (x['Payout'] - x['Spent']) / x['Spent'],
            ImpliedProbPctError = lambda x: (x['Avg_Implied_Prob'] - x['Hit_Rate'])/x['Hit_Rate'])

odds_return_summary[['Odds_Range', 'N', 'Hit_Rate', 'Avg_Implied_Prob', 'ImpliedProbPctError', 'Spent', 'Payout', 'Return_Pct']]

odds_return_check['Category'] = np.select(
    [
        odds_return_check['response_var'].str.contains('rushing_receiving'),
        odds_return_check['response_var'].str.contains('receiving|receptions'),
        odds_return_check['response_var'].str.contains('rushing'),
        odds_return_check['response_var'].str.contains('passing'),
        odds_return_check['response_var'].str.contains('anytime_td'),
        odds_return_check['response_var'].str.contains('team')
    ],
    ['Rush + Receiving', 'Receiving', 'Rushing', 'Passing', 'ATD', 'Team'],
    default='Other'
)

#By betting category:

plot_odds_bins = [-np.inf, -200, -100, 199, 399, 799, 1499, np.inf]
plot_odds_labels = ['<= -200', '-199 to -100', '+100 to +199', '+200 to +399',
                    '+400 to +799', '+800 to +1499', '+1500+']

odds_return_check['Plot_Odds_Range'] = pd.cut(
    odds_return_check['Odds'], bins=plot_odds_bins, labels=plot_odds_labels, right=True
)

odds_return_check['Category'] = np.select(
    [
        odds_return_check['response_var'].str.contains('rushing_receiving'),
        odds_return_check['response_var'].str.contains('receiving'),
        odds_return_check['response_var'].str.contains('rushing'),
        odds_return_check['response_var'].str.contains('passing'),
        odds_return_check['response_var'].str.contains('receptions'),
        odds_return_check['response_var'].str.contains('anytime_td'),
        odds_return_check['response_var'].str.contains('team')
    ],
    ['Rush + Receiving', 'Receiving', 'Rushing', 'Passing', 'Receptions', 'ATD', 'Team'],
    default='Other'
)

category_odds = odds_return_check.groupby(['Category', 'Plot_Odds_Range'], observed=False)\
    .agg(N=('BetHit', 'size'), Payout=('ResultPayoutPer100', 'sum'),
    Avg_Implied_Prob = ('Implied_Prob', 'mean'),
    Hit_Rate = ('BetHit','mean'))\
    .reset_index()\
    .assign(Return_Pct=lambda x: 100 * (x['Payout'] - 100 * x['N']) / (100 * x['N']),
            ImpliedProbPctError = lambda x: (x['Avg_Implied_Prob'] - x['Hit_Rate'])/x['Hit_Rate'])

plt.figure(figsize=(14, 14))

for category, df in category_odds.groupby('Category'):
    df = df[df['N'] > 100]
    df = df.set_index('Plot_Odds_Range').reindex(plot_odds_labels).reset_index()
    plt.plot(df['Plot_Odds_Range'].astype(str), df['Return_Pct'], marker='o', label=category)

    for _, row in df.iterrows():
        plt.annotate(f"N={row['N']}", (str(row['Plot_Odds_Range']), row['Return_Pct']),
                     textcoords='offset points', xytext=(0, 6), ha='center', fontsize=8)

plt.axhline(0, linestyle='--', alpha=.5)
plt.xlabel('Odds Range')
plt.ylabel('Return (%)')
plt.title('Return by Odds Range and Category (N > 100)')
plt.xticks(rotation=45)
plt.legend()
plt.tight_layout()
plt.show()

for category, df in category_odds.groupby('Category'):
    df = df[df['N'] > 100]
    df = df.set_index('Plot_Odds_Range').reindex(plot_odds_labels).reset_index()
    plt.plot(df['Plot_Odds_Range'].astype(str), df['ImpliedProbPctError'], marker='o', label=category)

    for _, row in df.iterrows():
        plt.annotate(f"N={row['N']}", (str(row['Plot_Odds_Range']), row['Return_Pct']),
                     textcoords='offset points', xytext=(0, 6), ha='center', fontsize=8)

plt.axhline(0, linestyle='--', alpha=.5)
plt.xlabel('Odds Range')
plt.ylabel('Return (%)')
plt.title('Return by Odds Range and Category (N > 100)')
plt.xticks(rotation=45)
plt.legend()
plt.tight_layout()
plt.show()
#%%