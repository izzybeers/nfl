#%%

from model_functions import read_from_supabase
import nfl_data_py as nfl
import pandas as pd
import nflreadpy as nfl
from model_functions import assess_probability_ranges, calculate_probability_ranges


clean_names = function(name)
{
  return(tolower(name) %>% str_remove_all("[[:punct:]]+") %>% str_remove("\\b(jr|sr|i{1,3}|iv|v|vi{1,3}|ix|x|xi{1,3})\\b") %>% str_squish() %>% trimws())
}

#df must have column: profit_per_100 (expected value profit on a $100 bet)
calculate_ev_ranges = function(df) {
  level_ev_range_groups <- c(
    "Very High",
    "High",
    "A little high",
    "Slightly above break-even",
    "Slightly below break-even",
    "A little low",
    "Low",
    "Very low"
  )
  
  return(df %>% mutate(
    EV_Range_group = factor(case_when(
      profit_per_100 > 100 ~ 'Very High',
      profit_per_100 > 50 ~ 'High',
      profit_per_100 > 20 ~ 'A little high',
      profit_per_100 > 0 ~ 'Slightly above break-even',
      profit_per_100 > -5 ~ 'Slightly below break-even',
      profit_per_100 > -20 ~ 'A little low',
      profit_per_100 > -30 ~ 'Low',
      profit_per_100 > -50 ~ 'Very low',
      .default = 'Extremely low'
    ), levels = level_ev_range_groups, ordered = TRUE)))
}

#df must have column Odds for the bet (american moneyline)
calculate_odds_ranges = function(df) {
  levels_odds = c(
    'Negative Odds',
    'Positive Odds Up To +250',
    'Odds +251 to +450',
    'Odds +451 to +650',
    'Odds +651 to +1000',
    'Odds +1000 to +2000',
    'Odds above +2000'
  )
  return(df %>% mutate(
    Odds_Range = factor(case_when(
      Odds < 0 ~ 'Negative Odds',
      Odds < 250 ~ 'Positive Odds Up To +250',
      Odds < 450 ~ 'Odds +251 to +450',
      Odds < 650 ~ 'Odds +451 to +650',
      Odds < 1000 ~ 'Odds +651 to +1000',
      Odds < 2000 ~ 'Odds +1000 to +2000',
      .default = 'Odds above +2000'), levels = levels_odds, ordered = TRUE)
  )
  )
}


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

df_schedules = nfl.load_schedules(2025)[['week', 'home_team','away_team', 'home_score','away_score', 'home_moneyline', 'away_moneyline']]
home_df = pd.DataFrame(df_schedules[['week', 'home_team','home_score','away_score', 'home_moneyline']], columns = ['week', 'team', 'team_score', 'opp_score', 'moneyline'])
away_df = pd.DataFrame(df_schedules[['week', 'away_team','away_score','home_score', 'away_moneyline']], columns = ['week', 'team', 'team_score', 'opp_score', 'moneyline'])
game_results = pd.concat([home_df, away_df], axis = 0)
game_results['win'] = game_results['team_score'] > game_results['opp_score']
game_results['diff'] = game_results['team_score'] - game_results['opp_score']
game_results = game_results[['week','team','win','diff','moneyline']]

url = f"https://github.com/nflverse/nflverse-data/releases/download/stats_player/stats_player_week_2025.parquet"
df = pd.read_parquet(url)
player_columns_to_keep = ['player_id', 'week', 'team', 'passing_yards', 'rushing_yards', 'receiving_yards', 'rushing_tds', 'receiving_tds', 'receptions']
playergl = df[player_columns_to_keep]
player_id_name_mapping = pd.DataFrame(nfl.load_players()[['gsis_id', 'display_name']], columns = ['gsis_id','display_name']).drop_duplicates()\
  .assign(cleaned_name = lambda x: x['display_name']
            .str.replace(r"\b(jr|sr|i{1,3}|iv|v|vi{1,3}|ix|x|xi{1,3})\b", "", regex=True, case=False)
            .str.replace(r"[^\w\s]", "", regex=True)
            .str.replace(r"\s+", " ", regex=True)
            .str.strip().str.lower())

bet_recs_raw = read_from_supabase(schema = 'betting', table_name = 'BetRecommendations', eq_col_name = 'Season', eq_value = '2025', chunk_size = 1000)

#player props
model_lookup_table = read_from_supabase('predictions', 'ModelSelections')
model_mapping = model_lookup_table[['response_var', 'model_type', 'root_folder', 'extra_calibration']].drop_duplicates()
  
latest_player_betting_lines =  bet_recs_raw.sort_values('run_time', ascending = False).drop_duplicates(subset = ['Week', 'Player', 'Bet_Type', 'Label'], keep = 'first')\
  .assign(cleaned_name = lambda x: x['Player']
            .str.replace(r"\b(jr|sr|i{1,3}|iv|v|vi{1,3}|ix|x|xi{1,3})\b", "", regex=True, case=False)
            .str.replace(r"[^\w\s]", "", regex=True)
            .str.replace(r"\s+", " ", regex=True)
            .str.strip().str.lower())\
              .merge(player_id_name_mapping, on = 'cleaned_name', how = 'left')\
                .merge(playergl, left_on = ['gsis_id', 'Week'], right_on = ['player_id','week'], how = 'left')\
                  .assign(BetHit = lambda x: np.select(
                    condlist = [(x['Bet_Type'] == 'Anytime TD Scorer') & (x['rushing_tds'] + x['receiving_tds'] > 0),
                                (x['Bet_Type'] == 'Receiving') & (x['receiving_yards'] > pd.to_numeric(x['Label'].str.replace('+','', regex = False), errors = 'coerce')),
                                (x['Bet_Type'] == 'Rushing') & (x['rushing_yards'] > pd.to_numeric(x['Label'].str.replace('+','', regex = False), errors = 'coerce')),
                                (x['Bet_Type'] == 'Passing') & (x['passing_yards'] > pd.to_numeric(x['Label'].str.replace('+','', regex = False), errors = 'coerce'))],
                    choicelist = [True, True, True, True], default = False),
                    Total_Touchdowns = lambda x: x['rushing_tds'] + x['receiving_tds'],
                    BetTypePositionCombo = lambda x: x['Bet_Type'] + ' - ' + x['Position'],
                    ToPayPer100 = lambda x: np.where(x['Odds'] > 0, 100 + x['Odds'], 100 + 100**2/(-1*x['Odds'])),
                    ResultPayoutPer100 = lambda x: np.where(x['BetHit'], x['ToPayPer100'], 0),
                    response_var = lambda x: np.where(x['Bet_Type'] == 'Anytime TD Scorer', 'anytime_td_scorer', x['Bet_Type'].str.lower() + '_yards_' + x['Label'].str.replace('+','', regex = False)))\
                      [['week', 'gsis_id', 'response_var', 'BetHit', 'Odds', 'ToPayPer100', 'ResultPayoutPer100']]

#team_bets:
moneyline_betting_lines = game_results.assign(ToPayPer100 = lambda x: np.where(x['moneyline'] > 0, 100 + x['moneyline'], 100 + 100**2/(-1*x['moneyline'])),
                                              ResultPayoutPer100 = lambda x: x['ToPayPer100']*x['win'],
                                              response_var = 'team_win').\
                                                rename(columns = {'win': 'BetHit',
                                                                  'moneyline': 'Odds'})\
                                                                    [['response_var','team','week','Odds','ToPayPer100','ResultPayoutPer100','BetHit']]
  
player_preds_df = pd.DataFrame()
for r in np.unique(latest_player_betting_lines['response_var']):
  print(r)
  new_data = pd.read_parquet(f"./ml_ready_data/final_test/{r}.parquet")
  model_type = model_mapping[model_mapping['response_var'] == r]['model_type'].iloc[0]
  extra_calibration = model_mapping[model_mapping['response_var'] == r]['extra_calibration'].iloc[0]
  root_folder = model_mapping[model_mapping['response_var'] == r]['root_folder'].iloc[0]
  preds = get_prediction_by_model_type(r, model_type, extra_calibration, root_folder, new_data)
  player_preds_df = pd.concat([player_preds_df, pd.DataFrame({
    'response_var': r,
    'week': new_data['week'].astype(int),
    'gsis_id': new_data['gsis_id'],
    'Model_Probability': preds
  })])

  r = 'team_win'
  new_data = pd.read_parquet(f"./ml_ready_data/final_test/{r}.parquet")
  model_type = model_mapping[model_mapping['response_var'] == r]['model_type'].iloc[0]
  extra_calibration = model_mapping[model_mapping['response_var'] == r]['extra_calibration'].iloc[0]
  root_folder = model_mapping[model_mapping['response_var'] == r]['root_folder'].iloc[0]
  preds = get_prediction_by_model_type(r, model_type, extra_calibration, root_folder, new_data)
  moneyline_preds_df = pd.DataFrame({
    'response_var': r,
    'week': new_data['week'].astype(int),
    'team': new_data['team'],
    'Model_Probability': preds
  })

  betting_lines_with_probability = calculate_probability_ranges(\
    pd.concat([preds_df.merge(latest_betting_lines, on = ['response_var', 'gsis_id', 'week'], how = 'inner')\
        .assign(EV_per_100 = lambda x: x['Model_Probability']*(x['ToPayPer100']-100) - (1-x['Model_Probability'])*100)\
          .rename(columns = {'gsis_id': 'label'}),
      moneyline_preds_df.merge(moneyline_betting_lines, on = ['response_var','team','week'], how = 'inner')\
        .assign(EV_per_100 = lambda x: x['Model_Probability']*(x['ToPayPer100']-100) - (1-x['Model_Probability'])*100)\
          .rename(columns = {'team': 'label'})
          ], axis = 0))



    #return if place every bet with positive EV:
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
    positive_ev_bets_above_100 = betting_lines_with_probability[betting_lines_with_probability['EV_per_100'] > 100]
    spent = 100*len(positive_ev_bets_above_100)
    payout = sum(positive_ev_bets_above_100['ResultPayoutPer100'])
    print(f"Money spent if bet on all >100 ev bets $100 each: ${spent:,.0f}")
    print(f"Total payout: ${payout:,.0f}")
    print(f"Return: {100*(payout-spent)/spent:,.1f}%")

    #next steps:
    #add in calculate_ev_ranges and calculate_odds_ranges
    #pull betting lines for the models we didn't do last year
    #incorporate portfolio optimization
    #use this opportunity to develop workflow for predictions
#%%