library(dplyr)
library(googlesheets4)
library(gbm3)

#this is run at the end of each week to add to the main dataset so the model can be rerun, and take the prediction df from last week and add to training data.
#First, we get the results from last week (the response variable: passing yds, rushing yds, receiving yds, and touchdowns)
#Then, we pull the data for the upcoming week 
setwd("~/nfl")
source('data_collection/scripts/get_player_gamelogs_script.R')
source('data_collection/scripts/calculate_player_seasonal_stats_script.R')
source('data_collection/scripts/get_team_gamelogs_script.R')
source('data_collection/scripts/get_target_rankings_script.R')
source('data_collection/scripts/get_weather_and_stadium_data_script.R')
source('data_collection/scripts/get_injuries_data_script.R')
source('data_collection/scripts/get_playoff_clinching_data_script.R')
source('data_collection/scripts/join_all_tables.R')

source('model/scripts/model_prep_script.R')
source('data_collection/scripts/global.R')

this_season = 2025
this_week = 1
data_collection_min_year = this_season - 2 #need 2 years of historical data for feature engineering


qb1_starting = read.csv('https://docs.google.com/spreadsheets/d/e/2PACX-1vT9_LcNO2d8L5kzbJQZZti9kxfAZRFRAl2oJz5WlpusfvL1txbkc8OU6BSlB54TA9HCBHRlIxi9MpuT/pub?gid=2014202336&single=true&output=csv') %>%
  filter(Season == this_season) %>% select(Team, !!sym(paste0('Week', this_week)))
colnames(qb1_starting) = c('Team', 'Qb1_starting')


gs4_auth(cache = ".secrets", email = "izzyb961@gmail.com")

team_abbreviations = team_lookup_table %>% select(Team, FullName, TV_abbr)
missing_cutoff = 0.95 #remove columns with more than this % of missing values
qb1_by_year = read.csv('https://docs.google.com/spreadsheets/d/e/2PACX-1vT9_LcNO2d8L5kzbJQZZti9kxfAZRFRAl2oJz5WlpusfvL1txbkc8OU6BSlB54TA9HCBHRlIxi9MpuT/pub?gid=1914165552&single=true&output=csv') %>% mutate(Temp = 1)
basic_cols = c('player_id', 'Name', 'names', 'Position', 'Month', 'Gtm', 'Week', 'Day', 'Date', 'Time','Time_of_Day', 'Team', 'Team_FullName', 'Game_Location', 'Opp', 'Opp_FullName', 'Result', 'Season')

#html tags for gamelogs:
gamelog_html_table_tag = 'stats'
gamelog_html_playoff_table_tag = 'stats_playoffs'
gamelog_advanced_html_rushing_table_tag = 'adv_rushing_and_receiving'
gamelog_advanced_html_passing_table_tag = 'passing_advanced'
gamelog_advanced_playoffs_html_passing_table_tag = 'passing_advanced_post'
gamelog_advanced_playoffs_html_rushing_table_tag = 'adv_rushing_and_receiving_post'


#collect upcoming data for this week:
player_bios = readRDS('data_collection/saved_data_files/player_bios.rds') #this likely won't change mid-season, so we can use the existing one



new_week_player_gamelogs = get_player_gamelogs(year_cutoff = this_season, max_year_cutoff = this_season, player_bios = player_bios, basic_cols = basic_cols, missing_threshold = missing_cutoff,
                                               gamelog_html_table_tag, gamelog_html_playoff_table_tag, gamelog_advanced_html_rushing_table_tag, gamelog_advanced_html_passing_table_tag, gamelog_advanced_playoffs_html_passing_table_tag, gamelog_advanced_playoffs_html_rushing_table_tag,
                                               wk = this_week, response_only = FALSE, predict_mode = TRUE)

#game start:
page = get_html_content(url = 'https://www.rotowire.com/football/lineups.php')
games <- html_elements(page, ".lineups .lineup.is-nfl")
starter_table = data.frame()
positions = c('QB', 'RB', 'WR', 'WR', 'WR', 'TE', 'K')
for (g in 1:length(games))
{
  this_game = games[[g]]
  away_team = this_game %>% html_node('.lineup__team.is-visit') %>% html_text(trim = TRUE)
  away_team = team_abbreviations$Team[team_abbreviations$TV_abbr == away_team]
  home_team = this_game %>% html_node('.lineup__team.is-home') %>% html_text(trim = TRUE)
  home_team = team_abbreviations$Team[team_abbreviations$TV_abbr == home_team]
  starters_away = (this_game %>% html_nodes('.lineup__list.is-visit .lineup__player a')  %>%  html_attr("title"))[1:7]
  starters_away = sapply(strsplit(starters_away, '   '), function(x) trimws(x[[length(x)]]))
  # starter_positions_away = this_game %>% html_nodes('.lineup__list.is-visit .lineup__player  .lineup__pos')  %>% html_text(trim = TRUE)
  starters_home = (this_game %>% html_nodes('.lineup__list.is-home .lineup__player a')  %>%  html_attr("title"))[1:7]
  # starter_positions_home = this_game %>% html_nodes('.lineup__list.is-home .lineup__player  .lineup__pos')  %>% html_text(trim = TRUE)
  this_game_table = data.frame(bind_rows(
    data.frame(positions, team = away_team, player = starters_away),
    data.frame(positions, team = home_team, player = starters_home)))
  starter_table = rbind(starter_table,
                        this_game_table)
}
starter_table$match = 1

new_week_player_gamelogs = new_week_player_gamelogs %>% left_join(starter_table, join_by('Position' == 'positions', 'Name' == 'player', 'Team' == 'team')) %>%
  mutate(GS = ifelse(is.na(match), 0, 1))
previous_gamelogs = readRDS('data_collection/saved_data_files/player_gamelogs.rds') %>% filter(!(Season == this_season & Week >= this_week))
player_gamelogs = bind_rows(previous_gamelogs, new_week_player_gamelogs)
# saveRDS(player_gamelogs, 'data_collection/saved_data_files/player_gamelogs.rds')

#we can use the existing table, season-end stats won't be calculated in the middle of the week:
player_seasonal_stats = readRDS('data_collection/saved_data_files/player_end_of_season_summary_stats.rds')
grouped_table_with_team = readRDS('data_collection/saved_data_files/player_end_of_season_summary_stats_with_team.rds')

team_res = get_team_gamelogs(start_year = this_season, end_year = this_season, basic_cols = basic_cols, missing_threshold = missing_cutoff, predict_mode = TRUE, calculate_season_end_stats = FALSE, wk = this_week)
team_gamelogs_this_week = team_res[[1]]
# previous_team_gamelogs = readRDS('data_collection/saved_data_files/team_gamelogs.rds') %>% filter(!(Season == this_season & Week >= this_week))
# new_team_gamelogs = bind_rows(previous_team_gamelogs, team_gamelogs_this_week)
# saveRDS(team_gamelogs, 'saved_data_files/team_gamelogs.rds')
team_seasonal_stats = readRDS('data_collection/saved_data_files/team_end_of_season_summary_stats.rds')

#target rankings:
#try with just one gamelog vs all gamelogs:
#remember to fix the manual qb starting thing:
player_rankings_this_week = get_players_target_rankings(min_year = this_season, max_year = this_season, player_gamelogs = player_gamelogs, player_seasonal = grouped_table_with_team,
                                                        team_gamelogs = team_gamelogs_this_week, qb1_by_year = qb1_by_year, wk = this_week, predict_mode = TRUE, manual_qb_starters = qb1_starting)
# , manual_qb_starters = manual_qb_starters)
# player_rankings = saveRDS(player_rankings, 'saved_data_files/player_rankings_within_team.rds')
previous_player_rankings = readRDS('data_collection/saved_data_files/player_rankings_within_team.rds')

new_player_rankings = list(player_rankings_this_week[[1]],
                           player_rankings_this_week[[2]],
                           previous_player_rankings[[3]],
                           previous_player_rankings[[4]],
                           bind_rows(player_rankings_this_week[[5]]))



#fields related to weather, stadium, location, date:
#try with just one gamelog vs all gamelogs:
weather_and_stadium_data_this_week =  get_weather_and_stadium_data(games = team_gamelogs_this_week, forecast = TRUE)
# partial_weather_data = readRDS('data_collection/saved_data_files/weather_and_stadium_data.rds') %>% filter(!(Season == this_season & Week >= this_week))
# weather_and_stadium_data = bind_rows(partial_weather_data, weather_and_stadium_data_this_week)
# saveRDS(weather_and_stadium_data, 'data_collection/saved_data_files/weather_and_stadium_data.rds')

#fields related to whether team has clinched, has been eliminated, or has control over playoff fate in the next game (clinch or eliminate)
playoff_clinching_data_this_week = get_playoff_clinching_data(min_year = this_season, max_year = this_season, wk = this_week, predict_mode = TRUE)
# partial_playoff_clinching_data = readRDS('data_collection/saved_data_files/playoff_clinching_table.rds') %>% filter(!(Season == this_season & Week >= this_week))
# full_playoff_clinching_table = bind_rows(partial_playoff_clinching_data, playoff_clinching_data_this_week)

# saveRDS(full_playoff_clinching_table, 'data_collection/saved_data_files/playoff_clinching_table.rds')

#fields related to player injury status:

injuries_data_this_week = get_injuries_data(min_year = this_season, max_year = this_season, wk = this_week)
# partial_injuries_data = readRDS('data_collection/saved_data_files/injuries_data.rds') %>% filter(!(Season == this_season & Week >= this_week))
# injuries_data = bind_rows(partial_injuries_data, injuries_data_this_week)
# saveRDS(injuries_data, 'data_collection/saved_data_files/injuries_data.rds')

join_res = join_all_tables(player_bios = player_bios,
                           player_gamelogs = new_week_player_gamelogs,
                           player_seasonal_stats = player_seasonal_stats,
                           team_gamelogs = team_gamelogs_this_week, team_seasonal_stats = team_seasonal_stats,
                           player_rankings = new_player_rankings,
                           weather_and_stadium_data = weather_and_stadium_data_this_week,
                           playoff_clinching_data = playoff_clinching_data_this_week,
                           injuries_data = injuries_data_this_week,
                           missing_cutoff = missing_cutoff,
                           season_data_cutoff = this_season,
                           predict_mode = TRUE)


passing_data_column_categories = readRDS('model/data/passing_data_column_categories.rds')
rushing_data_column_categories = readRDS('model/data/rushing_data_column_categories.rds')
receiving_data_column_categories = readRDS('model/data/receiving_data_column_categories.rds')
touchdown_data_column_categories = readRDS('model/data/touchdown_data_column_categories.rds')

prediction_data_model_prepped  = model_prep(join_res[[1]], join_res[[2]], join_res[[3]], join_res[[4]],
                                            passing_data_column_categories, rushing_data_column_categories, receiving_data_column_categories, touchdown_data_column_categories,
                                            train_test_split = FALSE, train_mode = FALSE) 

 
type = 'super_reduced'

new_passing_data = prediction_data_model_prepped[[1]]
new_rushing_data = prediction_data_model_prepped[[2]]
new_receiving_data = prediction_data_model_prepped[[3]]
new_touchdown_data = prediction_data_model_prepped[[4]]

saveRDS(new_passing_data, paste0('model/prediction_results/passing/',this_season,'_wk', this_week, '_prediction_df'))
saveRDS(new_rushing_data, paste0('model/prediction_results/rushing/',this_season,'_wk', this_week, '_prediction_df'))
saveRDS(new_receiving_data, paste0('model/prediction_results/receiving/',this_season,'_wk', this_week, '_prediction_df'))
saveRDS(new_touchdown_data, paste0('model/prediction_results/touchdown/',this_season,'_wk', this_week, '_prediction_df'))

new_passing_data = readRDS(paste0('model/prediction_results/passing/',this_season,'_wk', this_week, '_prediction_df'))
new_rushing_data = readRDS(paste0('model/prediction_results/rushing/',this_season,'_wk', this_week, '_prediction_df'))
new_receiving_data = readRDS(paste0('model/prediction_results/receiving/',this_season,'_wk', this_week, '_prediction_df'))
new_touchdown_data = readRDS(paste0('model/prediction_results/touchdown/',this_season,'_wk', this_week, '_prediction_df'))

results_by_bet_type = function(data, response_vector, model_name, model_type, raw_data)
{
  all_results = rbind()
  for(response in response_vector)
  {
    print(response)
    load_model = readRDS(paste0('model/tunings_and_models/', model_name, '/',type,'/model_', tolower(response),'.rds'))
    tree = gbm.perf(load_model, method = "cv", plot.it = FALSE)
    df = data[[response]]
    df_prepped = data_prep(df, response, pre_model = FALSE)
    preds = predict(load_model, df_prepped, n.trees = tree, type = "response")
    results = data.frame(this_season, this_week, response, df$player_id, preds) %>% unique()
    colnames(results) = c('Season', 'Week', 'response', 'player_id', 'probability')
    model_confidence = readRDS(paste0('model/tunings_and_models/', model_name, '/', type, '/confidence.rds'))[,response]
    
    #fix this since preds isn't the right column name:
    results = results %>% mutate(Probability_Bin = case_when(
      probability < 0.01 ~ 'Predicted_Less_1Pct',
      probability < 0.1 ~ 'Predicted_0.01_to_0.1',
      probability < 0.2 ~ 'Predicted_0.1_to_0.2',
      probability < 0.3 ~ 'Predicted_0.2_to_0.3',
      probability < 0.4 ~ 'Predicted_0.3_to_0.4',
      probability < 0.5 ~ 'Predicted_0.4_to_0.5',
      probability < 0.6 ~ 'Predicted_0.5_to_0.6',
      probability < 0.7 ~ 'Predicted_0.6_to_0.7',
      probability < 0.8 ~ 'Predicted_0.7_to_0.8',
      probability < 0.9 ~ 'Predicted_0.8_to_0.9',
      TRUE ~ 'Predicted_0.9_to_0.99'
    ),
    Confidence = model_confidence[Probability_Bin]) %>% select(-Probability_Bin)
    
    rm(load_model)
    rm(df)
    rm(df_prepped)
    gc()
    
    all_results = all_results%>%
      bind_rows(results)
  }
  return(all_results %>% left_join(raw_data %>% select('player_id', 'names', 'Day', 'Date', 'Time', 'Time_of_Day', 'Team', 'Opp', 'positions', 'Home', 'GS'),
                                   join_by(player_id)) %>% 
           rename('Starting' = 'GS') %>%
           mutate(update_time = format(
             force_tz(Sys.time(), "America/New_York"),
             "%Y-%m-%d %I:%M %p"
           )))
}




all_results_passing = results_by_bet_type(data = new_passing_data,
                                          response_vector = passing_response, #taken from global.R
                                          model_name = 'passing',
                                          model_type = 'super_reduced',
                                          raw_data = join_res[[1]])
all_results_rushing = results_by_bet_type(data = new_rushing_data,
                                          response_vector = rushing_response, #taken from global.R
                                          model_name = 'rushing',
                                          model_type = 'super_reduced',
                                          raw_data = join_res[[2]])
all_results_receiving = results_by_bet_type(data = new_receiving_data,
                                          response_vector = receiving_response, #taken from global.R -- TEMPORARY SINCE 160 WASNT WORKING
                                          model_name = 'receiving',
                                          model_type = 'super_reduced',
                                          raw_data = join_res[[3]])
all_results_touchdown = results_by_bet_type(data = new_touchdown_data,
                                            response_vector = touchdown_response, #taken from global.R
                                            model_name = 'touchdown',
                                            model_type = 'super_reduced',
                                            raw_data = join_res[[4]])




sheet_id = '19sWOOPFI37WaR5lmlYS6UUrV-0dmTn0iTFqUp26sfGI'

sheet_append(ss = sheet_id, data = all_results_passing, sheet = 'passing_predictions')
sheet_append(ss = sheet_id, data = all_results_rushing, sheet = 'rushing_predictions')
sheet_append(ss = sheet_id, data = all_results_receiving, sheet = 'receiving_predictions')
sheet_append(ss = sheet_id, data = all_results_touchdown, sheet = 'touchdown_predictions')

find_differences= function(orig, new)
{
  cols_orig_not_new = setdiff(colnames(orig), colnames(new))
  print(paste('Columns in original df and not new:', ifelse(length(cols_orig_not_new) == 0, 'None', paste(setdiff(colnames(orig), colnames(new)), collapse = ","))))
  
  cols_new_not_orig = setdiff(colnames(new), colnames(orig))
  print(paste('Columns in new df and not original:', ifelse(length(cols_new_not_orig) == 0, 'None', paste(setdiff(colnames(new), colnames(orig)), collapse = ","))))
  
  player_ids_orig_not_new = setdiff(unique(orig$player_id), unique(new$player_id))
  print(paste('Playerids in original df and not new:', ifelse(length(player_ids_orig_not_new) == 0, 'None',paste(player_ids_orig_not_new, collapse = ","))))
  
  player_ids_new_not_orig = setdiff(unique(new$player_id), unique(orig$player_id))
  print(paste('Playerids in new df and not original:', ifelse(length(player_ids_new_not_orig) == 0, 'None', paste(player_ids_new_not_orig, collapse = ","))))
  
  print('Values not in common:')
  common_cols = intersect(colnames(orig), colnames(new))
  combined_df = cbind(row1 = orig[,common_cols], row2 = new[,common_cols])
  print(combined_df %>% t() %>% filter(row1 != row2))
}




the_real_week_10_pass = readRDS('model/data/passing_preliminary_data.rds') %>% filter(Season == 2024 & Week == 10)
the_real_week_10_rush = readRDS('model/data/rushing_preliminary_data.rds') %>% filter(Season == 2024 & Week == 10)
the_real_week_10_rec = readRDS('model/data/receiving_preliminary_data.rds') %>% filter(Season == 2024 & Week == 10)
the_real_week_10_td = readRDS('model/data/touchdown_preliminary_data.rds') %>% filter(Season == 2024 & Week == 10)

new_week_10_pass = join_res[[1]] %>% filter(Season == 2024 & Week == 10)
new_week_10_rush = join_res[[2]] %>% filter(Season == 2024 & Week == 10)
new_week_10_rec = join_res[[3]] %>% filter(Season == 2024 & Week == 10)
new_week_10_td = join_res[[4]] %>% filter(Season == 2024 & Week == 10)

find_differences(the_real_week_10_pass, new_week_10_pass)

dim(new_week_10_pass)
dim(the_real_week_10_pass)

dim(new_week_10_rush)
dim(the_real_week_10_rush)

dim(new_week_10_rec)
dim(the_real_week_10_rec)

dim(new_week_10_td)
dim(the_real_week_10_td)



#steps:
#make sure functions are working and run without error
#turn the response var thing into a function and apply that everywhere√
#connect to draftkings for all prop bets√
#get the qb starter thing under control
#build front end
#connect the probability results to a csv√
#make the top20 unique to the model
#why does df_prepped have the wrong player_id