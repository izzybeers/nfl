#This is run at the end of each week to add to the main dataset so the model can be rerun.
#First we take the prediction data compiled at the beginning of the week
#Then we get the results for the week (the response variable: passing yds, rushing yds, receiving yds, and touchdowns)
#Add this to the main training data
setwd("~/nfl")
source('data_collection/scripts/get_player_gamelogs_script.R')
source('data_collection/scripts/join_all_tables.R')
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

this_season = 2025
this_week = 1
data_collection_min_year = this_season - 2 #need 2 years of historical data for feature engineering


#get results from_last_week: passing_yds, rushing_yds, receiving_yds, etc:
player_gamelogs_results = get_player_gamelogs(year_cutoff = this_season, max_year_cutoff = this_season, player_bios = player_bios, basic_cols = basic_cols, missing_threshold = missing_cutoff,
                                              gamelog_html_table_tag, gamelog_html_playoff_table_tag, gamelog_advanced_html_rushing_table_tag, gamelog_advanced_html_passing_table_tag, gamelog_advanced_playoffs_html_passing_table_tag, gamelog_advanced_playoffs_html_rushing_table_tag,
                                              wk = this_week - 1, response_only = TRUE)

passing_results = player_gamelog_results %>% select(player_id, Season, Week, Passing_Yds)
rushing_results = player_gamelog_results %>% select(player_id, Season, Week, Rushing_Yds)
receiving_results = player_gamelog_results %>% select(player_id, Season, Week, Receiving_Yds)
touchdown_results = player_gamelog_results %>% select(player_id, Season, Week, Total_Touchdowns)

#Combine it with the prediction data collected last week:

current_passing_data = readRDS('model/data/passing_preliminary_data.rds')
last_week_passing_data = readRDS(paste0('model/prediction_results/passing/wk_',(this_week-1), '_prediction_df')) %>%
  left_join(passing_results, join_by(player_id == player_id, Season == Season, Week == Week))
current_passing_data = bind_rows(current_passing_data, last_week_passing_data)
saveRDS(current_passing_data, '..model/data/passing_preliminary_data.rds')

current_rushing_data = readRDS('model/data/rushing_preliminary_data.rds')
last_week_rushing_data = readRDS(paste0('model/prediction_results/passing/wk_',(this_week-1), '_prediction_df')) %>%
  left_join(rushing_results, join_by(player_id == player_id, Season == Season, Week == Week))
current_rushing_data = bind_rows(current_rushing_data, last_week_rushing_data)
saveRDS(current_rushing_data, 'model/data/rushing_preliminary_data.rds')

current_receiving_data = readRDS('model/data/receiving_preliminary_data.rds')
last_week_receiving_data = readRDS(paste0('model/prediction_results/passing/wk_',(this_week-1), '_prediction_df')) %>%
  left_join(receiving_results, join_by(player_id == player_id, Season == Season, Week == Week))
current_receiving_data = bind_rows(current_receiving_data, last_week_receiving_data)
saveRDS(current_receiving_data, 'model/data/receiving_preliminary_data.rds')

current_touchdown_data = readRDS('model/data/touchdown_preliminary_data.rds')
last_week_touchdown_data = readRDS(paste0('model/prediction_results/passing/wk_',(this_week-1), '_prediction_df')) %>%
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


model_names = c('passing', 'rushing', 'receiving', 'touchdown')
response_list = list(passing_response, rushing_response,receiving_response, touchdown_response)


