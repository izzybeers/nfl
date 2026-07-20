#%%

from model_functions import read_from_supabase
import nfl_data_py as nfl
import pandas as pd
import nflreadpy as nfl
from model_functions import assess_probability_ranges, calculate_probability_ranges, calculate_ev_ranges, calculate_odds_ranges


clean_names = function(name)
{
  return(tolower(name) %>% str_remove_all("[[:punct:]]+") %>% str_remove("\\b(jr|sr|i{1,3}|iv|v|vi{1,3}|ix|x|xi{1,3})\\b") %>% str_squish() %>% trimws())
}
  
team_lookup = read_from_supabase('MainData', 'TeamLookup')


get_random_bet_iterations = function(num_iterations)
{
  random_bets_df = data.frame()
  for(j in 1:num_iterations)
  {
    for (w in 1:22)
    {
      bets_to_choose = bet_recommendations %>% filter(Week == w) %>% group_by(Player, Bet_Type, Label) %>% arrange(desc(run_time)) %>% slice(1) %>% ungroup()
      
      bet_weights = c()
      for(i in 1:10)
      {
        if(i < 10)
        {
          bet_weights = c(bet_weights, runif(1,0, 1 - sum(bet_weights)))
        } else {
          bet_weights = c(bet_weights, 1 - sum(bet_weights))
        }
      }
      bet_weights = bet_weights[bet_weights*50 > 0.1]
      bet_weights = bet_weights/sum(bet_weights)
      num_bets = length(bet_weights)
      bets_indices_chosen = sample(1:nrow(bets_to_choose), num_bets)
      bets_chosen = bets_to_choose[bets_indices_chosen,]
      bets_chosen$Amount = 50*bet_weights
      random_bets_df = data.frame(rbind(random_bets_df, bets_chosen %>% mutate(SampleIteration = j)))
    }
  }
  
  all_bets = random_bets_df %>% mutate(ToPay = ifelse(Odds > 0, Amount + Odds*Amount/100, Amount + (100/(-1*Odds))*Amount),
                                       Result = ifelse(BetHit == TRUE, 'Win', 'Loss'),
                                       ResultPayout = ifelse(BetHit == TRUE, ToPay, 0))
  
  bets_summary = all_bets  %>%
    group_by(SampleIteration) %>% summarise(TotalSpent = sum(Amount), Payout = sum(ResultPayout), Return = (sum(Payout)-sum(Amount))/sum(Amount))
  median_index = random_bets_summary %>% arrange(Return) %>% slice(floor(num_iterations/2))  %>% pull(SampleIteration)                                     
  median_bet = random_bets_df %>% filter(SampleIteration == median_index)  %>%
    mutate(Bettor = 'Random Selection (Median)')
  return(list(all_bets, bets_summary, median_bet))
}

#pull 2025 stats:

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
model_mapping = model_lookup_table[['response_var', 'model_type', 'root_folder', 'extra_calibration']].drop_duplicates()
  
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
bettinglines = read_from_supabase('betting', 'BettingLines', chunk_size = 1000)

bettinglines_mostrecent = bettinglines.sort_values('runTimeUTC', ascending = False)\
  .drop_duplicates(subset = ['EventName', 'name', 'subcategoryId', 'label', 'points'], keep = 'first')

string_of_all_player_names = '(' + '|'.join(player_id_name_mapping['cleaned_name']) + ')'
bettinglines_mostrecent['player_name'] = clean_name(bettinglines_mostrecent['name']).str.extract(string_of_all_player_names)
bettinglines_mostrecent = bettinglines_mostrecent[(bettinglines_mostrecent['name'].str.contains('Receptions')) |\
                                                  (bettinglines_mostrecent['name'].str.contains('Rushing') & bettinglines_mostrecent['name'].str.contains('Receiving')) |\
                                                  (bettinglines_mostrecent['name'] == 'Spread Alternate')]
bettinglines_mostrecent = bettinglines_mostrecent[bettinglines_mostrecent['points'].isna() | bettinglines_mostrecent['points'].astype(str).isin(["-7.5","-6.5","-3.5","-2.5","2.5","3.5","6.5","7.5"])]
bettinglines_mostrecent = bettinglines_mostrecent.merge(player_id_name_mapping, left_on = 'player_name', right_on = 'cleaned_name', how='left')\
  .assign( Bet_Type = lambda x: np.select(
  [x['name'] == 'Spread Alternate', x['name'].str.contains('Receptions')],
  ['Spread Alternate', 'Receptions'],
  default = 'Rushing Receiving'
),
response_var = lambda x: np.where(x['Bet_Type'] == 'Spread Alternate', 'team_differential_' + x['points'].astype(str).str.replace('-','minus'),
np.where(x['Bet_Type'] == 'Receptions', 'receptions_' + x['label'].str.replace('+',''),
'rushing_receiving_yards_' + x['label'].str.replace('+','')))
).query('response_var.isin(@model_mapping["response_var"])')

bettinglines_mostrecent['gameday'] = pd.to_datetime(bettinglines_mostrecent['startEventDateUTC']).dt.tz_convert('America/New_York').dt.date.astype(str)
bettinglines_mostrecent = bettinglines_mostrecent.merge(pd.DataFrame(df_schedules[['gameday','week']], columns = ['gameday','week']).drop_duplicates(), on = 'gameday', how='left')

receptions_and_rushrec_bets = bettinglines_mostrecent.query('Bet_Type.isin(["Receptions","Rushing Receiving"])')\
    .rename(columns = {'americanOdds': 'Odds'})\
      .merge(playergl, left_on = ['gsis_id','week'], right_on = ['player_id','week'], how='left')\
        .merge(player_id_name_mapping, on = 'gsis_id', how = 'left')\
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
  [['Week', 'team', 'opponent_team', 'response_var', 'americanOdds', 'diff', 'points']]\
  .assign(BetHit = lambda x: np.where(x['diff'] > (-1)*x['points'].astype(float), True, False))\
  .rename(columns = {'americanOdds': 'Odds'})[[col for col in moneyline_betting_lines.columns]]

team_bets = pd.concat([moneyline_betting_lines, spread_bets], axis = 0).assign(label = lambda x: x['team'])

player_preds_df = pd.DataFrame()
for r in np.unique(model_mapping[~model_mapping['response_var'].str.contains('team')]['response_var']):
  print(r)
  new_data = pd.read_parquet(f"./ml_ready_data/final_test/{r}.parquet")
  model_type = model_mapping[model_mapping['response_var'] == r]['model_type'].iloc[0]
  extra_calibration = model_mapping[model_mapping['response_var'] == r]['extra_calibration'].iloc[0]
  root_folder = model_mapping[model_mapping['response_var'] == r]['root_folder'].iloc[0]
  preds = get_prediction_by_model_type(r, model_type, extra_calibration, root_folder, new_data)
  player_preds_df = pd.concat([player_preds_df, pd.DataFrame({
    'response_var': r,
    'Week': new_data['week'].astype(int),
    'gsis_id': new_data['gsis_id'],
    'Model_Probability': preds
  })])

team_preds_df = pd.DataFrame()
for r in np.unique(model_mapping[model_mapping['response_var'].str.contains('team')]['response_var']):
  new_data = pd.read_parquet(f"./ml_ready_data/final_test/{r}.parquet")
  model_type = model_mapping[model_mapping['response_var'] == r]['model_type'].iloc[0]
  extra_calibration = model_mapping[model_mapping['response_var'] == r]['extra_calibration'].iloc[0]
  root_folder = model_mapping[model_mapping['response_var'] == r]['root_folder'].iloc[0]
  preds = get_prediction_by_model_type(r, model_type, extra_calibration, root_folder, new_data)
  team_preds_df = pd.concat([team_preds_df, pd.DataFrame({
    'response_var': r,
    'Week': new_data['week'].astype(int),
    'team': new_data['team'],
    'Model_Probability': preds
  })], axis = 0)

player_preds_df = player_preds_df.merge(player_prop_bets, on = ['response_var','gsis_id','Week']).merge(game_results[['Week','team','opponent_team']], on = ['Week','team'])
team_preds_df = team_preds_df.merge(team_bets, on = ['response_var','team','Week'])

all_bets_df = pd.concat([player_preds_df.rename(columns = {'gsis_id': 'label'}), team_preds_df.assign(Position = 'Team')], axis = 0)

correlations_up_to_2024 = pd.read_csv('../correlations_up_to_2024.csv')
portfolios = get_optimal_portfolio(all_bets_df, correlations_up_to_2024, max_bets = 20)


#update the below:
  betting_lines_with_probability = calculate_odds_ranges(calculate_ev_ranges(calculate_probability_ranges(\
    pd.concat([preds_df.merge(player_prop_bets, on = ['response_var', 'gsis_id', 'week'], how = 'inner')\
        .assign(EV_per_100 = lambda x: x['Model_Probability']*(x['ToPayPer100']-100) - (1-x['Model_Probability'])*100)\
          .rename(columns = {'gsis_id': 'label'}),
      team_preds_df.merge(moneyline_betting_lines, on = ['response_var','team','week'], how = 'inner')\
        .assign(EV_per_100 = lambda x: x['Model_Probability']*(x['ToPayPer100']-100) - (1-x['Model_Probability'])*100)\
          .rename(columns = {'team': 'label'})
          ], axis = 0))))

  betting_lines_with_probability.groupby(['EV_Range_group','Odds_Range'],as_index=False)\
    .agg(ResultPayoutPer100 = ('ResultPayoutPer100','sum'),
         NumBets = ('ResultPayoutPer100', 'size')).\
          assign(EstimatedReturn100Each = lambda x: (x['ResultPayoutPer100'] - 100*x['NumBets'])/(100*x['NumBets']))\
    .sort_values(['EV_Range_group','Odds_Range'])\
    .pivot(columns = 'EV_Range_group', index = 'Odds_Range', values = 'EstimatedReturn100Each')

betting_lines_with_probability.\
  query('EV_per_100 > 0').\
    assign(bets_return = lambda x: (x['ResultPayoutPer100']-100)/100).\
      groupby('response_var',as_index=False).\
        agg(avg_return = ('bets_return','mean'),
        num_bets = ('bets_return','size'),
        bet_win_pct = ('BetHit','mean'))\
          .sort_values('avg_return', ascending = False)

betting_lines_with_probability.\
  query('EV_per_100 > 25').\
    assign(bets_return = lambda x: (x['ResultPayoutPer100']-100)/100).\
      groupby('response_var',as_index=False).\
        agg(avg_return = ('bets_return','mean'),
        num_bets = ('bets_return','size'))\
          .sort_values('avg_return', ascending = False)

betting_lines_with_probability.\
  query('EV_per_100 > 50').\
    assign(bets_return = lambda x: (x['ResultPayoutPer100']-100)/100).\
      groupby('response_var',as_index=False).\
        agg(avg_return = ('bets_return','mean'),
        num_bets = ('bets_return','size'),
        bet_win_pct = ('BetHit','mean'))\
          .sort_values('avg_return', ascending = False)

  


  #return if place every bet with positive EV:
  spent = 100*len(betting_lines_with_probability)
  payout = sum(betting_lines_with_probability['ResultPayoutPer100'])
  print(f"Money spent if bet on everything $100 each: ${spent:,.0f}")
  print(f"Total payout: ${payout:,.0f}")
  print(f"Return: {100*(payout-spent)/spent:,.1f}%")
  positive_ev_bets = betting_lines_with_probability[betting_lines_with_probability['EV_per_100'] > 0]
  spent = 100*len(positive_ev_bets)
  payout = sum(positive_ev_bets['ResultPayoutPer100'])
  print(f"Money spent if bet on all positive ev bets $100 each: ${spent:,.0f}")
  print(f"Total payout: ${payout:,.0f}")
  print(f"Return: {100*(payout-spent)/spent:,.1f}%")
  positive_ev_bets_above_25 = betting_lines_with_probability[betting_lines_with_probability['EV_per_100'] > 25]
  spent = 100*len(positive_ev_bets_above_25)
  payout = sum(positive_ev_bets_above_25['ResultPayoutPer100'])
  print(f"Money spent if bet on all >25 ev bets $100 each: ${spent:,.0f}")
  print(f"Total payout: ${payout:,.0f}")
  print(f"Return: {100*(payout-spent)/spent:,.1f}%")
  positive_ev_bets_above_50 = betting_lines_with_probability[betting_lines_with_probability['EV_per_100'] > 50]
  spent = 100*len(positive_ev_bets_above_50)
  payout = sum(positive_ev_bets_above_50['ResultPayoutPer100'])
  print(f"Money spent if bet on all >50 ev bets $100 each: ${spent:,.0f}")
  print(f"Total payout: ${payout:,.0f}")
  print(f"Return: {100*(payout-spent)/spent:,.1f}%")

    #next steps:
    #incorporate portfolio optimization
    #use this opportunity to develop workflow for predictions
    #categories that it performs better at? 
#%%