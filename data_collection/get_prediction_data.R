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
this_week = 6
data_collection_min_year = this_season - 2 #need 2 years of historical data for feature engineering

qb1_by_year = read.csv('https://docs.google.com/spreadsheets/d/e/2PACX-1vT9_LcNO2d8L5kzbJQZZti9kxfAZRFRAl2oJz5WlpusfvL1txbkc8OU6BSlB54TA9HCBHRlIxi9MpuT/pub?gid=1914165552&single=true&output=csv') %>%
  filter(Season == this_season)



gs4_auth(cache = ".secrets", email = "izzyb961@gmail.com")

team_abbreviations = team_lookup_table %>% select(Team, FullName, TV_abbr, Depth_abbr)
missing_cutoff = 0.95 #remove columns with more than this % of missing values
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
new_week_player_gamelogs_raw = get_player_gamelogs(year_cutoff = this_season, max_year_cutoff = this_season, player_bios = player_bios, basic_cols = basic_cols, missing_threshold = missing_cutoff,
                                                   gamelog_html_table_tag, gamelog_html_playoff_table_tag, gamelog_advanced_html_rushing_table_tag, gamelog_advanced_html_passing_table_tag, gamelog_advanced_playoffs_html_passing_table_tag, gamelog_advanced_playoffs_html_rushing_table_tag,
                                                   wk = this_week, response_only = FALSE, predict_mode = TRUE)

#game start:
page = get_html_content(url = 'https://www.rotowire.com/football/lineups.php')
games = html_elements(page, ".lineups .lineup.is-nfl")
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
  extra_info_starters_away = (this_game %>% html_nodes('.lineup__list.is-visit .lineup__player span') %>% html_text(trim = TRUE))[1:7]
  starters_home = (this_game %>% html_nodes('.lineup__list.is-home .lineup__player a')  %>%  html_attr("title"))[1:7]
  starters_home = sapply(strsplit(starters_home, '   '), function(x) trimws(x[[length(x)]]))
  extra_info_starters_home = (this_game %>% html_nodes('.lineup__list.is-home .lineup__player span') %>% html_text(trim = TRUE))[1:7]
  this_game_table = data.frame(bind_rows(
    data.frame(positions, team = away_team, player = starters_away, descriptor = extra_info_starters_away),
    data.frame(positions, team = home_team, player = starters_home, descriptor = extra_info_starters_home)))
  starter_table = rbind(starter_table,
                        this_game_table)
}
starter_table$match = 1

bye_teams = setdiff(team_lookup_table$Team, starter_table$team)

qb1_starting = qb1_by_year %>% left_join(starter_table %>% filter(positions == 'QB' & !(descriptor %in% c('I', 'D'))) %>% select(-positions), join_by('Team' == 'team', 'Qb1' == 'player')) %>% select(Team, match) %>%
   rename('Qb1_starting' = 'match') %>% mutate(Qb1_starting = ifelse(is.na(Qb1_starting), 0, Qb1_starting))

if(length(bye_teams) > 0)
{
  qb1_starting = qb1_starting %>% filter(!(Team %in% bye_teams))
}

new_week_player_gamelogs = new_week_player_gamelogs_raw %>%
  left_join(starter_table %>% filter(!(descriptor %in% c('I', 'D'))), join_by('Position' == 'positions', 'Name' == 'player', 'Team' == 'team')) %>%
  mutate(GS = ifelse(is.na(match), 0, 1)) %>% select(-match, -descriptor)
# new_week_player_gamelogs = new_week_player_gamelogs_raw %>% left_join(depth_charts_all_teams, join_by('Position' == 'Position', 'Name' == 'Player', 'Team' == 'Team')) %>%
#   mutate(GS = ifelse(is.na(match), 0, 1))
previous_gamelogs = readRDS('data_collection/saved_data_files/player_gamelogs.rds')
old_cols = colnames(previous_gamelogs)
missing_cols = setdiff(colnames(previous_gamelogs), colnames(new_week_player_gamelogs))
new_week_player_gamelogs[,missing_cols] = NA
player_gamelogs = bind_rows(previous_gamelogs, new_week_player_gamelogs)



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


#mid week run:
new_player_rankings[[5]] = qb1_starting %>% mutate(Season = this_season, Week = this_week) %>% select(Season, Week, Team, Qb1_starting)



#fields related to weather, stadium, location, date:

#check that weather came through;
weather_and_stadium_data_this_week =  get_weather_and_stadium_data(games = team_gamelogs_this_week, forecast = TRUE)
# partial_weather_data = readRDS('data_collection/saved_data_files/weather_and_stadium_data.rds') %>% filter(!(Season == this_season & Week >= this_week))
# weather_and_stadium_data = bind_rows(partial_weather_data, weather_and_stadium_data_this_week)
# saveRDS(weather_and_stadium_data, 'data_collection/saved_data_files/weather_and_stadium_data.rds')

#fields related to whether team has clinched, has been eliminated, or has control over playoff fate in the next game (clinch or eliminate)
playoff_clinching_data_this_week = get_playoff_clinching_data(min_year = this_season, max_year = this_season, wk = this_week, predict_mode = TRUE)
if(nrow(playoff_clinching_data_this_week) == 1) #just one NA row with no team
{
  playoff_clinching_data_this_week = cbind(playoff_clinching_data_this_week %>% select(-Team), Team = unique(team_gamelogs$Team)) %>% select(Season, Week, Team, everything())
}
# partial_playoff_clinching_data = readRDS('data_collection/saved_data_files/playoff_clinching_table.rds') %>% filter(!(Season == this_season & Week >= this_week))
# full_playoff_clinching_table = bind_rows(partial_playoff_clinching_data, playoff_clinching_data_this_week)

# saveRDS(full_playoff_clinching_table, 'data_collection/saved_data_files/playoff_clinching_table.rds')

#fields related to player injury status:

injuries_data_this_week = tryCatch({
  get_injuries_data(min_year = this_season, max_year = this_season, wk = this_week)
  },
    error = function(e) {
      get_injuries_data(min_year = this_season, max_year = this_season, wk = this_week)
    }
)
# partial_injuries_data = readRDS('data_collection/saved_data_files/injuries_data.rds') %>% filter(!(Season == this_season & Week >= this_week))
# injuries_data = bind_rows(partial_injuries_data, injuries_data_this_week)
# saveRDS(injuries_data, 'data_collection/saved_data_files/injuries_data.rds')

#ONE TIME SAVES:
saveRDS(new_week_player_gamelogs, paste0('model/prediction_results/raw_data/',this_season,'_wk', this_week, '_player_gamelogs_df.rds'))
saveRDS(team_gamelogs_this_week, paste0('model/prediction_results/raw_data/',this_season,'_wk', this_week, '_team_gamelogs_df.rds'))
saveRDS(weather_and_stadium_data_this_week, paste0('model/prediction_results/raw_data/',this_season,'_wk', this_week, '_weather_stadium_df.rds'))
saveRDS(injuries_data_this_week, paste0('model/prediction_results/raw_data/',this_season,'_wk', this_week, '_injuries_df.rds'))
saveRDS(new_player_rankings, paste0('model/prediction_results/raw_data/', this_season,'_wk',this_week, '_target_rankings_df.rds'))
saveRDS(playoff_clinching_data_this_week, paste0('model/prediction_results/raw_data/', this_season,'_wk',this_week, '_playoff_clinching_df'))

#MID WEEK RERUN:
#Run game start section, weather and stadium data, and injuries data, and leave the rest in tact
#rerun the others if variables were lost
player_bios = readRDS('data_collection/saved_data_files/player_bios.rds') 
team_gamelogs_this_week = readRDS(paste0('model/prediction_results/raw_data/',this_season,'_wk', this_week, '_team_gamelogs_df.rds'))
new_player_rankings = readRDS(paste0('model/prediction_results/raw_data/', this_season,'_wk',this_week, '_target_rankings_df.rds'))
playoff_clinching_data_this_week = readRDS(paste0('model/prediction_results/raw_data/', this_season,'_wk',this_week, '_playoff_clinching_df.rds'))
player_seasonal_stats = readRDS('data_collection/saved_data_files/player_end_of_season_summary_stats.rds')
team_seasonal_stats = readRDS('data_collection/saved_data_files/team_end_of_season_summary_stats.rds')
new_week_player_gamelogs_raw = readRDS(paste0('model/prediction_results/raw_data/',this_season,'_wk', this_week, '_player_gamelogs_df'))


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

#ONE TIME SAVES:
saveRDS(join_res[[1]], paste0('model/prediction_results/passing/',this_season,'_wk', this_week, '_prediction_passing_preliminary_data.rds'))
saveRDS(join_res[[2]], paste0('model/prediction_results/rushing/',this_season,'_wk', this_week, '_prediction_rushing_preliminary_data.rds'))
saveRDS(join_res[[3]], paste0('model/prediction_results/receiving/',this_season,'_wk', this_week, '_prediction_receiving_preliminary_data.rds'))
saveRDS(join_res[[4]], paste0('model/prediction_results/touchdown/',this_season,'_wk', this_week, '_prediction_touchdown_preliminary_data.rds'))



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


#write predictions to excel file:

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
                                            response_vector = receiving_response, #taken from global.R
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

passing_extra_info = join_res[[1]] %>% filter(Season == this_season & Week == this_week) %>%
  select(Week, player_id, names, min_year, draft_round, draft_pick, original_draft_team,
         Home, International,
         Pct_Active, Pct_GS, Avg_Passing_1D, Avg_Passing_Cmp, Last_Season_Passing_TD_mean, Opp_Avg_Defense_PassY_Allowed, Passing_Yds_Lag1, Avg_Passing_Yds, Avg_Passing_Att, Avg_Passing_TD, Is_Qb1,
         Last_Season_Pct_Active, Last_Season_Passing_TD_mean, Last_Season_Passing_Yds_median, Last_Season_Passing_Comp_Pct, Opp_Last_Season_Avg_Defense_PassY_mean_Allowed)
passing_extra_info = passing_extra_info |>
  dplyr::mutate(dplyr::across(where(is.numeric), ~replace(., !is.finite(.), NA_real_)))

rushing_extra_info = join_res[[2]] %>% filter(Season == this_season & Week == this_week) %>%
  select(Week, player_id, names, min_year, draft_round, draft_pick, original_draft_team,
         Home, International,
         Pct_Active, Pct_GS, Avg_Rushing_Att, Avg_Rushing_Yds, Rushing_Yds_Lag1, Avg_Rushing_YBC, Last_Season_Rushing_YBC_median, Avg_Rushing_1D, Avg_Rushing_YAC, Last_Season_Rushing_YBC_mean, Opp_Avg_Defense_RushY_Allowed,
         Last_Season_Pct_Active, Last_Season_Rushing_Yds_max, Last_Season_Rushing_Att_mean, Last_Season_Rushing_TD_mean, Last_Season_Rushing_Yds_mean)
rushing_extra_info = rushing_extra_info |>
  dplyr::mutate(dplyr::across(where(is.numeric), ~replace(., !is.finite(.), NA_real_)))

receiving_extra_info = join_res[[3]] %>% filter(Season == this_season & Week == this_week) %>%
  select(Week, player_id, names, min_year, draft_round, draft_pick, original_draft_team,
         Home, International,
         Pct_Active, Pct_GS, Avg_Receiving_1D, Avg_Receiving_Tgt, Avg_Receiving_YBC, Avg_Receiving_Yds, Last_Season_Receiving_YBC_mean, Last_Season_Receiving_1D_mean, Last_Season_Receiving_Yds_mean, Last_Season_Receiving_YBC_median, Last_Season_Receiving_Yds_median, Avg_Receiving_Rec,
         Last_Season_Pct_Active, Last_Season_Receiving_Tgt_mean, Last_Season_Receiving_Yds_mean, Last_Season_Receiving_1D_mean, Last_Season_Receiving_1D_sd, Last_Season_Receiving_YBC_mean, Last_Season_Receiving_YBC_max)
receiving_extra_info = receiving_extra_info |>
  dplyr::mutate(dplyr::across(where(is.numeric), ~replace(., !is.finite(.), NA_real_)))

touchdown_extra_info = join_res[[4]] %>% filter(Season == this_season & Week == this_week) %>%
  select(Week, player_id, names, min_year, draft_round, draft_pick, original_draft_team,
         Home, International,
         Pct_Active, Pct_GS, Last_Season_Total_Touchdowns_sd, Last_Season_Receiving_YAC_mean, Avg_Receiving_YAC, Avg_Receiving_Rec, Avg_Rushing_YAC, Avg_Total_Touchdowns, Avg_Receiving_Tgt, Last_Season_Total_Touchdowns_mean, Two_Seasons_Ago_Receiving_YAC_mean,
         Last_Season_Pct_Active, Last_Season_Receiving_1D_Per_Tgt)
touchdown_extra_info = touchdown_extra_info |>
  dplyr::mutate(dplyr::across(where(is.numeric), ~replace(., !is.finite(.), NA_real_)))


sheet_append(ss = sheet_id, data = passing_extra_info, sheet = 'passing_extra_info')
sheet_append(ss = sheet_id, data = rushing_extra_info, sheet = 'rushing_extra_info')
sheet_append(ss = sheet_id, data = receiving_extra_info, sheet = 'receiving_extra_info')
sheet_append(ss = sheet_id, data = touchdown_extra_info, sheet = 'touchdown_extra_info')


#update the prediction df and raw data files:


#if predictions have already been run at some point this week:
model_names = c('passing','rushing','receiving','touchdown')
data_list = list(new_passing_data, new_rushing_data, new_receiving_data, new_touchdown_data)

for(m in 1:length(model_names))
{
  previous_data = tryCatch({readRDS(paste0('model/prediction_results/',model_names[m],'/',this_season,'_wk', this_week, '_prediction_df.rds'))}, error = function(e) { return(NULL)})
  new_list = list()
  for(i in 1:length(data_list[[m]]))
  {
    print(i)
    this_df = tryCatch({previous_data[[i]] %>% filter(!(player_id %in% data_list[[m]][[i]]$player_id) | as.POSIXct(paste(Date, as.integer(Season) + ifelse(grepl("^(January|February)\\b", Date), 1L, 0L), Time), format = "%B %e %Y %I:%M%p", tz = "America/New_York") < Sys.time())}, error = function(e) { return(NULL)})
    new_df = rbind(this_df, data_list[[m]][[i]] %>% filter(as.POSIXct(paste(Date, as.integer(Season) + ifelse(grepl("^(January|February)\\b", Date), 1L, 0L), Time), format = "%B %e %Y %I:%M%p", tz = "America/New_York") >= Sys.time()))
    new_list[[i]] = new_df
  }
  saveRDS(new_list, paste0('model/prediction_results/', model_names[m], '/', this_season,'_wk', this_week, '_prediction_df.rds'))
}





#updates throughout the week for player gamelogs (starters), weather, and injuries:
existing_current_week_gamelogs = readRDS(paste0('model/prediction_results/raw_data/',this_season,'_wk', this_week, '_player_gamelogs_df.rds')) %>% filter(!(player_id %in% new_week_player_gamelogs$player_id) | Date < Sys.time())
saveRDS(rbind(existing_current_week_gamelogs, new_week_player_gamelogs %>% filter(Date >= Sys.time())), paste0('model/prediction_results/raw_data/',this_season,'_wk', this_week, '_player_gamelogs_df.rds'))

existing_current_weather_data = readRDS(paste0('model/prediction_results/raw_data/',this_season,'_wk', this_week, '_weather_stadium_df.rds')) %>% filter(!(Team %in% weather_and_stadium_data_this_week$Team))
saveRDS(rbind(existing_current_weather_data, weather_and_stadium_data_this_week), paste0('model/prediction_results/raw_data/',this_season,'_wk', this_week, '_weather_stadium_df.rds'))

target_rankings_data_to_write = readRDS(paste0('model/prediction_results/raw_data/',this_season,'_wk', this_week, '_target_rankings_df.rds'))
target_rankings_data_to_write[[5]] = rbind(target_rankings_data_to_write[[5]] %>% filter(!(Team %in% new_player_rankings[[5]]$Team)),
                                           new_player_rankings[[5]])
saveRDS(target_rankings_data_to_write, paste0('model/prediction_results/raw_data/',this_season,'_wk', this_week, '_target_rankings_df.rds'))

existing_injuries_data = readRDS(paste0('model/prediction_results/raw_data/',this_season,'_wk', this_week, '_injuries_df.rds')) %>% filter(!(Player %in% injuries_data_this_week$Player))
saveRDS(rbind(existing_injuries_data, injuries_data_this_week), paste0('model/prediction_results/raw_data/',this_season,'_wk', this_week, '_injuries_df.rds'))

existing_passing_preliminary_data = readRDS(paste0('model/prediction_results/passing/',this_season,'_wk', this_week, '_prediction_passing_preliminary_data.rds')) %>% filter(!(player_id %in% join_res[[1]]$player_id) | as.POSIXct(paste(Date, as.integer(Season) + ifelse(grepl("^(January|February)\\b", Date), 1L, 0L), Time), format = "%B %e %Y %I:%M%p", tz = "America/New_York") < Sys.time())
saveRDS(rbind(existing_passing_preliminary_data, join_res[[1]] %>% filter(as.POSIXct(paste(Date, as.integer(Season) + ifelse(grepl("^(January|February)\\b", Date), 1L, 0L), Time), format = "%B %e %Y %I:%M%p", tz = "America/New_York") >= Sys.time())), paste0('model/prediction_results/passing/',this_season,'_wk', this_week, '_prediction_passing_preliminary_data.rds'))

existing_rushing_preliminary_data = readRDS(paste0('model/prediction_results/rushing/',this_season,'_wk', this_week, '_prediction_rushing_preliminary_data.rds')) %>% filter(!(player_id %in% join_res[[2]]$player_id) | as.POSIXct(paste(Date, as.integer(Season) + ifelse(grepl("^(January|February)\\b", Date), 1L, 0L), Time), format = "%B %e %Y %I:%M%p", tz = "America/New_York") < Sys.time())
saveRDS(rbind(existing_rushing_preliminary_data, join_res[[2]] %>% filter(as.POSIXct(paste(Date, as.integer(Season) + ifelse(grepl("^(January|February)\\b", Date), 1L, 0L), Time), format = "%B %e %Y %I:%M%p", tz = "America/New_York") >= Sys.time())), paste0('model/prediction_results/rushing/',this_season,'_wk', this_week, '_prediction_rushing_preliminary_data.rds'))

existing_receiving_preliminary_data = readRDS(paste0('model/prediction_results/receiving/',this_season,'_wk', this_week, '_prediction_receiving_preliminary_data.rds')) %>% filter(!(player_id %in% join_res[[3]]$player_id) | as.POSIXct(paste(Date, as.integer(Season) + ifelse(grepl("^(January|February)\\b", Date), 1L, 0L), Time), format = "%B %e %Y %I:%M%p", tz = "America/New_York") < Sys.time())
saveRDS(rbind(existing_receiving_preliminary_data, join_res[[3]] %>% filter(as.POSIXct(paste(Date, as.integer(Season) + ifelse(grepl("^(January|February)\\b", Date), 1L, 0L), Time), format = "%B %e %Y %I:%M%p", tz = "America/New_York") >= Sys.time())), paste0('model/prediction_results/receiving/',this_season,'_wk', this_week, '_prediction_receiving_preliminary_data.rds'))

existing_touchdown_preliminary_data = readRDS(paste0('model/prediction_results/touchdown/',this_season,'_wk', this_week, '_prediction_touchdown_preliminary_data.rds')) %>% filter(!(player_id %in% join_res[[4]]$player_id) | as.POSIXct(paste(Date, as.integer(Season) + ifelse(grepl("^(January|February)\\b", Date), 1L, 0L), Time), format = "%B %e %Y %I:%M%p", tz = "America/New_York") < Sys.time())
saveRDS(rbind(existing_touchdown_preliminary_data, join_res[[4]] %>% filter(as.POSIXct(paste(Date, as.integer(Season) + ifelse(grepl("^(January|February)\\b", Date), 1L, 0L), Time), format = "%B %e %Y %I:%M%p", tz = "America/New_York") >= Sys.time())), paste0('model/prediction_results/touchdown/',this_season,'_wk', this_week, '_prediction_touchdown_preliminary_data.rds'))

