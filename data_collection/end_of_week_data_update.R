#This is run at the end of each week to add to the main dataset so the model can be rerun.
#First we take the prediction data compiled at the beginning of the week
#Then we get the results for the week (the response variable: passing yds, rushing yds, receiving yds, and touchdowns)
#Add this to the main training data
setwd("~/nfl")
source('data_collection/scripts/get_player_bios_script.R')
source('data_collection/scripts/get_player_gamelogs_script.R')
source('data_collection/scripts/join_all_tables.R')
source('data_collection/scripts/get_team_gamelogs_script.R')
source('model/scripts/model_prep_script.R')
source('model/scripts/run_models.R')
source('data_collection/scripts/global.R')
source('model/scripts/model_results.R')

#html tags for gamelogs:
gamelog_html_table_tag = 'stats'
gamelog_html_playoff_table_tag = 'stats_playoffs'
gamelog_advanced_html_rushing_table_tag = 'adv_rushing_and_receiving'
gamelog_advanced_html_passing_table_tag = 'passing_advanced'
gamelog_advanced_playoffs_html_passing_table_tag = 'passing_advanced_post'
gamelog_advanced_playoffs_html_rushing_table_tag = 'adv_rushing_and_receiving_post'
basic_cols = c('player_id', 'Name', 'names', 'Position', 'Month', 'Gtm', 'Week', 'Day', 'Time_of_Day', 'Time', 'Date', 'Team', 'Team_FullName', 'Game_Location', 'Opp', 'Opp_FullName', 'Result', 'Season')
missing_cutoff = 0.95

this_season = 2025
this_week = 4
data_collection_min_year = this_season - 2 #need 2 years of historical data for feature engineering
qb1_by_year = read.csv('https://docs.google.com/spreadsheets/d/e/2PACX-1vT9_LcNO2d8L5kzbJQZZti9kxfAZRFRAl2oJz5WlpusfvL1txbkc8OU6BSlB54TA9HCBHRlIxi9MpuT/pub?gid=1914165552&single=true&output=csv') %>%
  filter(Season == this_season)


# player_bios = readRDS('data_collection/saved_data_files/player_bios.rds')
player_bios = get_player_bios(year_cutoff = 2022, max_year_cutoff = this_season) #update this occasionally due to players changing teams

#get results from_last_week: passing_yds, rushing_yds, receiving_yds, etc:
player_gamelogs_results = get_player_gamelogs(year_cutoff = this_season, max_year_cutoff = this_season, player_bios = player_bios[which(player_bios$max_year == this_season),], basic_cols = basic_cols, missing_threshold = missing_cutoff,
                                              gamelog_html_table_tag, gamelog_html_playoff_table_tag, gamelog_advanced_html_rushing_table_tag, gamelog_advanced_html_passing_table_tag, gamelog_advanced_playoffs_html_passing_table_tag, gamelog_advanced_playoffs_html_rushing_table_tag,
                                              wk = this_week - 1, response_only = TRUE)

missing_players = setdiff(player_gamelogs_results$player_id, past_week_gamelogs$player_id)
missing_player_gamelogs_results = get_player_gamelogs(year_cutoff = this_season, max_year_cutoff = this_season, player_bios = player_bios[which(player_bios$max_year == this_season),] %>% filter(player_id %in% missing_players), basic_cols = basic_cols, missing_threshold = missing_cutoff,
                                              gamelog_html_table_tag, gamelog_html_playoff_table_tag, gamelog_advanced_html_rushing_table_tag, gamelog_advanced_html_passing_table_tag, gamelog_advanced_playoffs_html_passing_table_tag, gamelog_advanced_playoffs_html_rushing_table_tag,
                                              wk = this_week - 1, response_only = FALSE)

passing_results = player_gamelogs_results %>% select(player_id, Season, Week, Passing_Yds)
rushing_results = player_gamelogs_results %>% select(player_id, Season, Week, Rushing_Yds)
receiving_results = player_gamelogs_results %>% select(player_id, Season, Week, Receiving_Yds)
touchdown_results = player_gamelogs_results %>% select(player_id, Season, Week, Total_Touchdowns)

#update gamelogs and all other raw files by combining it with the prediction raw data files from last week

full_gamelogs = readRDS('data_collection/saved_data_files/player_gamelogs.rds')
past_week_gamelogs = readRDS(paste0('model/prediction_results/raw_data/', this_season, '_wk', (this_week-1), '_player_gamelogs_df')) %>% select(-any_of(c('Passing_Yds', 'Rushing_Yds', 'Receiving_Yds', 'Total_Touchdowns', 'GS'))) %>% left_join(player_gamelogs_results %>% select(player_id, Passing_Yds, Rushing_Yds, Receiving_Yds, Total_Touchdowns,GS), join_by('player_id'))
combined_gamelogs = rbind(full_gamelogs, past_week_gamelogs %>% select(any_of(colnames(full_gamelogs))))
saveRDS(combined_gamelogs, 'data_collection/saved_data_files/player_gamelogs.rds')

team_res = get_team_gamelogs(start_year = this_season, end_year = this_season, wk = this_week- 1, basic_cols = basic_cols, missing_threshold = missing_cutoff, calculate_season_end_stats = FALSE)
team_gamelogs = team_res[[1]]
present_stats_columns = c('Win','Differential','Playoffs', colnames(team_gamelogs)[str_detect(colnames(team_gamelogs),'Offense|Defense') & !str_detect(colnames(team_gamelogs), c('Cumulative|Last3|Avg|Median|Max|Min|SD|Rank'))])
team_gamelogs = team_gamelogs %>% select(Team, all_of(present_stats_columns))
full_team_gamelogs = readRDS('data_collection/saved_data_files/team_gamelogs.rds')
past_week_team_gamelogs = readRDS(paste0('model/prediction_results/raw_data/', this_season, '_wk', (this_week-1), '_team_gamelogs_df.rds')) %>% select(-any_of(present_stats_columns)) %>% left_join(team_gamelogs, join_by('Team'))
combined_team_gamelogs = rbind(full_team_gamelogs, past_week_team_gamelogs %>% select(any_of(colnames(full_team_gamelogs))))
saveRDS(combined_team_gamelogs, 'data_collection/saved_data_files/team_gamelogs.rds')

player_rankings_within_team = readRDS('data_collection/saved_data_files/player_rankings_within_team.rds')
past_week_player_rankings = readRDS(paste0('model/prediction_results/raw_data/', this_season, '_wk', (this_week-1), '_target_rankings_df.rds'))
combined_player_rankings = list()
for(i in 1:length(past_week_player_rankings))
{
  if(i %in% c(1,2))
  {
    combined_player_rankings[[i]] = rbind(player_rankings_within_team[[i]], past_week_player_rankings[[i]])
  } else if (i %in% c(3,4)) {
    combined_player_rankings[[i]] = player_rankings_within_team[[i]] #this doesn't change mid season
  } else {
    qb_starters_last_week = combined_gamelogs %>%
      filter(Season == this_season & Week == (this_week-1) & Position == 'QB' & !is.na(Passing_Yds) & GS == 1) %>%
      select(Season, Week, Team, Name) %>%
      left_join(qb1_by_year %>% select(Team, Qb1) %>% mutate(match = 1), join_by('Name'== 'Qb1', 'Team' == 'Team')) %>% 
      mutate(match = ifelse(is.na(match), 0, 1)) %>% rename('Qb1_starting' = 'match') %>% select(-Name)
    combined_player_rankings[[i]] = rbind(player_rankings_within_team[[i]], qb_starters_last_week)
  }
}
saveRDS(combined_player_rankings, 'data_collection/saved_data_files/player_rankings_within_team.rds')

full_weather_stadium_data = readRDS('data_collection/saved_data_files/weather_and_stadium_data.rds')
past_week_weather_stadium_data = readRDS(paste0('model/prediction_results/raw_data/', this_season, '_wk', (this_week-1), '_weather_stadium_df.rds')) %>% select(any_of(colnames(full_weather_stadium_data)))
combined_weather_stadium_data = rbind(full_weather_stadium_data, past_week_weather_stadium_data) 
saveRDS(combined_weather_stadium_data, 'data_collection/saved_data_files/weather_and_stadium_data.rds')

full_playoff_clinching_data = readRDS('data_collection/saved_data_files/playoff_clinching_table.rds')
past_week_playoff_data = readRDS(paste0('model/prediction_results/raw_data/', this_season, '_wk', (this_week-1), '_playoff_clinching_df.rds'))  %>% select(any_of(colnames(full_playoff_clinching_data)))
#if the playoff data is missing all the teams:
past_week_playoff_data = cbind(past_week_playoff_data %>% select(-Team), Team = unique(team_gamelogs$Team)) %>% select(Season, Week, Team, everything())
combined_playoff_data = rbind(full_playoff_clinching_data, past_week_playoff_data)
saveRDS(combined_playoff_data, 'data_collection/saved_data_files/playoff_clinching_table.rds')

full_injuries_data = readRDS('data_collection/saved_data_files/injuries_data.rds')
past_week_injuries_data = readRDS(paste0('model/prediction_results/raw_data/', this_season, '_wk', (this_week-1), '_injuries_df.rds'))  %>% select(any_of(colnames(full_injuries_data)))
combined_injuries_data = rbind(full_injuries_data, past_week_injuries_data)
saveRDS(combined_injuries_data, 'data_collection/saved_data_files/injuries_data.rds')


#decide if you want to do all data here or just the new week:
join_res = join_all_tables(player_bios,
                           player_gamelogs = combined_gamelogs,
                           player_seasonal_stats = readRDS('data_collection/saved_data_files/player_end_of_season_summary_stats.rds'),
                           team_gamelogs = combined_team_gamelogs,
                           team_seasonal_stats = readRDS('data_collection/saved_data_files/team_end_of_season_summary_stats.rds'),
                           player_rankings = combined_player_rankings,
                           weather_and_stadium_data  = combined_weather_stadium_data,
                           playoff_clinching_data = combined_playoff_data,
                           injuries_data = combined_injuries_data,
                           missing_cutoff,
                           season_data_cutoff = 2022)

old_passing_data = readRDS('model/data/passing_preliminary_data.rds')
old_rushing_data = readRDS('model/data/rushing_preliminary_data.rds')
old_receiving_data = readRDS('model/data/receiving_preliminary_data.rds')
old_touchdown_data = readRDS('model/data/touchdown_preliminary_data.rds')

#CHANGE THIS IF JOIN RES IS ONLY RECENT WEEK:
saveRDS(join_res[[1]], 'model/data/passing_preliminary_data.rds')
saveRDS(join_res[[2]], 'model/data/rushing_preliminary_data.rds')
saveRDS(join_res[[3]], 'model/data/receiving_preliminary_data.rds')
saveRDS(join_res[[4]], 'model/data/touchdown_preliminary_data.rds')


saveRDS(join_res[[5]],
        'model/data/passing_data_column_categories.rds')

saveRDS(join_res[[6]],
        'model/data/rushing_data_column_categories.rds')

saveRDS(join_res[[7]],
        'model/data/receiving_data_column_categories.rds')

saveRDS(join_res[[8]],
        'model/data/touchdown_data_column_categories.rds')


current_passing_data = readRDS('model/data/passing_preliminary_data.rds')
last_week_passing_data = readRDS(paste0('model/prediction_results/passing/wk_',(this_week-1), '_prediction_df')) %>%
  left_join(passing_results, join_by(player_id == player_id, Season == Season, Week == Week))
current_passing_data = bind_rows(current_passing_data, last_week_passing_data)
saveRDS(current_passing_data, '..model/data/passing_preliminary_data.rds')

previous_preliminary_rushing_data = readRDS('model/data/rushing_preliminary_data.rds')
last_week_preliminary_rushing_data = readRDS('model/prediction_results/prediction_rushing_preliminary_data.rds')

last_week_rushing_predictions = readRDS(paste0('model/prediction_results/rushing/',this_season,'_wk',(this_week-1), '_prediction_df'))

last_week_complete_rushing_data_model = list()
for(p in 1:length(last_week_rushing_predictions))
{
  #make sure last week's prediction data only includes last week, not all weeks of season.
  last_week_complete_rushing_data_model[[p]] = last_week_rushing_predictions[[p]] %>% left_join(rushing_results %>% select(-Season, -Week), join_by(player_id == player_id))
}
  
current_rushing_data = bind_rows(current_rushing_data, last_week_rushing_data)
saveRDS(current_rushing_data, 'model/data/rushing_preliminary_data.rds')

current_receiving_data = readRDS('model/data/receiving_preliminary_data.rds')
last_week_receiving_data = readRDS(paste0('model/prediction_results/receiving/wk_',(this_week-1), '_prediction_df')) %>%
  left_join(receiving_results, join_by(player_id == player_id, Season == Season, Week == Week))
current_receiving_data = bind_rows(current_receiving_data, last_week_receiving_data)
saveRDS(current_receiving_data, 'model/data/receiving_preliminary_data.rds')

current_touchdown_data = readRDS('model/data/touchdown_preliminary_data.rds')
last_week_touchdown_data = readRDS(paste0('model/prediction_results/touchdown/wk_',(this_week-1), '_prediction_df')) %>%
  left_join(touchdown_results, join_by(player_id == player_id, Season == Season, Week == Week))
current_touchdown_data = bind_rows(current_touchdown_data, last_week_touchdown_data)
saveRDS(current_touchdown_data, 'model/data/touchdown_preliminary_data.rds')



passing_data_column_categories = readRDS('model/data/passing_data_column_categories.rds')
rushing_data_column_categories = readRDS('model/data/rushing_data_column_categories.rds')
receiving_data_column_categories = readRDS('model/data/receiving_data_column_categories.rds')
touchdown_data_column_categories = readRDS('model/data/touchdown_data_column_categories.rds')

current_passing_data = readRDS('model/data/passing_preliminary_data.rds')
current_rushing_data = readRDS('model/data/rushing_preliminary_data.rds')
current_receiving_data = readRDS('model/data/receiving_preliminary_data.rds')
current_touchdown_data = readRDS('model/data/touchdown_preliminary_data.rds')

model_prep_results = model_prep(current_passing_data, current_rushing_data, current_receiving_data, current_touchdown_data,
                                passing_data_column_categories, rushing_data_column_categories, receiving_data_column_categories, touchdown_data_column_categories,
                                train_test_split = FALSE, train_mode = TRUE) 


new_passing_data = model_prep_results[[1]]
new_rushing_data = model_prep_results[[2]]
new_receiving_data = model_prep_results[[3]]
new_touchdown_data = model_prep_results[[4]]
saveRDS(new_passing_data, 'model/data/model_ready_passing_train_df.rds')
saveRDS(new_rushing_data, 'model/data/model_ready_rushing_train_df.rds')
saveRDS(new_receiving_data, 'model/data/model_ready_receiving_train_df.rds')
saveRDS(new_touchdown_data, 'model/data/model_ready_touchdown_train_df.rds')

new_passing_data =  readRDS('model/data/model_ready_passing_train_df.rds')
new_rushing_data = readRDS('model/data/model_ready_rushing_train_df.rds')
new_receiving_data = readRDS('model/data/model_ready_receiving_train_df.rds')
new_touchdown_data = readRDS('model/data/model_ready_touchdown_train_df.rds')


#train model
type = 'super_reduced'
model_manual_remove = c('min_year', 'max_year')
for(response in passing_response)
{
  model_name = 'passing'
  response_col_to_remove = 'Passing_Yds'
  # params = readRDS(paste0('model/tunings_and_models/',model_name,'/',type,'/all_tunings_', tolower(response),'.rds'))
  model = readRDS(paste0('model/tunings_and_models/',model_name,'/',type,'/model_', tolower(response),'.rds'))
  params = c(tree = gbm.perf(model, method = "cv", plot.it = FALSE), i = model$params$interaction_depth, n = model$params$min_num_obs_in_node, s = model$params$shrinkage, b = model$params$bag_fraction)
  run_one_model(response = response, model_name = model_name, column_categories = passing_data_column_categories, data_file_path = paste0('model/data/model_ready_',model_name,'_train_df.rds'),
             manual_remove = model_manual_remove, response_col_to_remove = response_col_to_remove, path = type,
               t_per_s = params['tree'], i_range = params['i'], s_range = params['s'], n_range = params['n'], b_range = params['b'])
}
for(response in rushing_response)
{
  model_name = 'rushing'
  response_col_to_remove = 'Rushing_Yds'
  model = readRDS(paste0('model/tunings_and_models/',model_name,'/',type,'/model_', tolower(response),'.rds'))
  params = c(tree = gbm.perf(model, method = "cv", plot.it = FALSE), i = model$params$interaction_depth, n = model$params$min_num_obs_in_node, s = model$params$shrinkage, b = model$params$bag_fraction)
  run_one_model(response = response, model_name = model_name, column_categories = rushing_data_column_categories, data_file_path =  paste0('model/data/model_ready_',model_name,'_train_df.rds'),
             manual_remove = model_manual_remove, response_col_to_remove = response_col_to_remove, path = type,
             t_per_s = params['tree'], i_range = params['i'], s_range = params['s'], n_range = params['n'], b_range = params['b'])
}
for(response in receiving_response)
{
  model_name = 'receiving'
  response_col_to_remove = 'Receiving_Yds'
  model = readRDS(paste0('model/tunings_and_models/',model_name,'/',type,'/model_', tolower(response),'.rds'))
  params = c(tree = gbm.perf(model, method = "cv", plot.it = FALSE), i = model$params$interaction_depth, n = model$params$min_num_obs_in_node, s = model$params$shrinkage, b = model$params$bag_fraction)
  run_one_model(response = response, model_name = model_name, column_categories = receiving_data_column_categories, data_file_path =  paste0('model/data/model_ready_',model_name,'_train_df.rds'),
             manual_remove = model_manual_remove, response_col_to_remove = response_col_to_remove, path = type,
             t_per_s = params['tree'], i_range = params['i'], s_range = params['s'], n_range = params['n'], b_range = params['b'])
}
for(response in touchdown_response)
{
  model_name = 'touchdown'
  response_col_to_remove = 'Total_Touchdowns'
  model = readRDS(paste0('model/tunings_and_models/',model_name,'/',type,'/model_', tolower(response),'.rds'))
  params = c(tree = gbm.perf(model, method = "cv", plot.it = FALSE), i = model$params$interaction_depth, n = model$params$min_num_obs_in_node, s = model$params$shrinkage, b = model$params$bag_fraction)
  run_one_model(response = response, model_name = model_name, column_categories = touchdown_data_column_categories, data_file_path =  paste0('model/data/model_ready_',model_name,'_train_df.rds'),
                manual_remove = model_manual_remove, response_col_to_remove = response_col_to_remove, path = type,
                t_per_s = params['tree'], i_range = params['i'], s_range = params['s'], n_range = params['n'], b_range = params['b'])
}



