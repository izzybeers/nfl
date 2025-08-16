#This is generally a one-time script run to pull all historical data. It would only ever be rerun if a bug was found or if the website made a lot of updates. It takes a long time.
setwd("~/nfl")
source('data_collection/scripts/get_player_bios_script.R')
source('data_collection/scripts/get_player_gamelogs_script.R')
source('data_collection/scripts/calculate_player_seasonal_stats_script.R')
source('data_collection/scripts/get_team_gamelogs_script.R')
source('data_collection/scripts/get_target_rankings_script.R')
source('data_collection/scripts/get_weather_and_stadium_data_script.R')
source('data_collection/scripts/get_injuries_data_script.R')
source('data_collection/scripts/get_playoff_clinching_data_script.R')
source('data_collection/scripts/join_all_tables.R')

source('model/model_prep_script.R')
source('data_collection/scripts/global.R')
team_abbreviations = team_lookup_table %>% select(Team, FullName, TV_abbr)
model_min_year = 2022
data_collection_min_year = model_min_year - 2 #need 2 years of historical data for feature engineering
missing_cutoff = 0.95 #remove columns with more than this % of missing values
qb1_by_year = read.csv('https://docs.google.com/spreadsheets/d/e/2PACX-1vT9_LcNO2d8L5kzbJQZZti9kxfAZRFRAl2oJz5WlpusfvL1txbkc8OU6BSlB54TA9HCBHRlIxi9MpuT/pub?gid=1914165552&single=true&output=csv') %>% mutate(Temp = 1)
max_year = 2025
basic_cols = c('player_id', 'Name', 'names', 'Position', 'Month', 'Gtm', 'Week', 'Day', 'Time_of_Day', 'Time', 'Date', 'Team', 'Team_FullName', 'Game_Location', 'Opp', 'Opp_FullName', 'Result', 'Season')


#html tags for gamelogs:
gamelog_html_table_tag = 'stats'
gamelog_html_playoff_table_tag = 'stats_playoffs'
gamelog_advanced_html_rushing_table_tag = 'adv_rushing_and_receiving'
gamelog_advanced_html_passing_table_tag = 'passing_advanced'
gamelog_advanced_playoffs_html_passing_table_tag = 'passing_advanced_post'
gamelog_advanced_playoffs_html_rushing_table_tag = 'adv_rushing_and_receiving_post'



player_bios = get_player_bios(year_cutoff = data_collection_min_year, max_year_cutoff = max_year)

saveRDS(player_bios, 'data_collection/saved_data_files/player_bios.rds')



player_gamelogs = get_player_gamelogs(year_cutoff = data_collection_min_year, max_year_cutoff = max_year, player_bios = player_bios, basic_cols = basic_cols, missing_threshold = missing_cutoff,
                    gamelog_html_table_tag, gamelog_html_playoff_table_tag, gamelog_advanced_html_rushing_table_tag, gamelog_advanced_html_passing_table_tag, gamelog_advanced_playoffs_html_passing_table_tag, gamelog_advanced_playoffs_html_rushing_table_tag)
# saveRDS(player_gamelogs, 'data_collection/saved_data_files/player_gamelogs.rds')
# player_gamelogs = readRDS('data_collection/saved_data_files/player_gamelogs.rds')

seasonal_stats = calculate_player_seasonal_stats(player_gamelogs = player_gamelogs, basic_cols = basic_cols, missing_threshold = missing_cutoff)
player_seasonal_stats = seasonal_stats[[1]]
grouped_table_with_team = seasonal_stats[[2]]
saveRDS(player_seasonal_stats, 'data_collection/saved_data_files/player_end_of_season_summary_stats.rds')
saveRDS(grouped_table_with_team, 'data_collection/saved_data_files/player_end_of_season_summary_stats_with_team.rds')
# seasonal_stats = readRDS('data_collection/saved_data_files/player_end_of_season_summary_stats.rds')
# grouped_table_with_team = readRDS('data_collection/saved_data_files/player_end_of_season_summary_stats_with_team.rds')

team_res = get_team_gamelogs(start_year = data_collection_min_year, end_year = max_year, basic_cols = basic_cols, missing_threshold = missing_cutoff, calculate_season_end_stats = TRUE)
team_gamelogs = team_res[[1]]
team_seasonal_stats = team_res[[2]]
saveRDS(team_gamelogs, 'data_collection/saved_data_files/team_gamelogs.rds')
saveRDS(team_seasonal_stats, 'data_collection/saved_data_files/team_end_of_season_summary_stats.rds')
# team_gamelogs = readRDS('data_collection/saved_data_files/team_gamelogs.rds')
# team_seasonal_stats = readRDS('data_collection/saved_data_files/team_end_of_season_summary_stats.rds')

#target rankings:
#try with just one gamelog vs all gamelogs:
player_rankings = get_players_target_rankings(min_year = model_min_year, max_year = max_year, player_gamelogs = player_gamelogs, player_seasonal = grouped_table_with_team,
                    team_gamelogs = team_gamelogs, qb1_by_year = qb1_by_year)
player_rankings = saveRDS(player_rankings, 'data_collection/saved_data_files/player_rankings_within_team.rds')
# player_rankings = readRDS('data_collection/saved_data_files/player_rankings_within_team.rds')

#fields related to weather, stadium, location, date:
#try with just one gamelog vs all gamelogs:
weather_and_stadium_data =  get_weather_and_stadium_data(games = team_gamelogs)
saveRDS(weather_and_stadium_data, 'data_collection/saved_data_files/weather_and_stadium_data.rds')
# weather_and_stadium_data = readRDS('data_collection/saved_data_files/weather_and_stadium_data.rds')

#fields related to whether team has clinched, has been eliminated, or has control over playoff fate in the next game (clinch or eliminate)
playoff_clinching_data = get_playoff_clinching_data(min_year = model_min_year, max_year = max_year)
saveRDS(playoff_clinching_data, 'data_collection/saved_data_files/playoff_clinching_table.rds')
# playoff_clinching_data = readRDS('data_collection/saved_data_files/playoff_clinching_table.rds')

#fields related to player injury status:

injuries_data = get_injuries_data(min_year = model_min_year, max_year = max_year)
saveRDS(injuries_data, 'data_collection/saved_data_files/injuries_data.rds')
# injuries_data = readRDS('data_collection/saved_data_files/injuries_data.rds')

join_res = join_all_tables(player_bios, player_gamelogs, player_seasonal_stats,
            team_gamelogs, team_seasonal_stats,
            player_rankings,
            weather_and_stadium_data,
            playoff_clinching_data,
            injuries_data,
            missing_cutoff,
            season_data_cutoff = model_min_year)

saveRDS(join_res[[1]], 'model/data/passing_preliminary_data.rds')
saveRDS(join_res[[2]], 'model/data/rushing_preliminary_data.rds')
saveRDS(join_res[[3]], 'model/data/receiving_preliminary_data.rds')
saveRDS(join_res[[4]], 'model/data/touchdown_preliminary_data.rds')


saveRDS(join_res[[5]],
        '../model/data/passing_data_column_categories.rds')

saveRDS(join_res[[6]],
        '../model/data/rushing_data_column_categories.rds')

saveRDS(join_res[[7]],
        '../model/data/receiving_data_column_categories.rds')

saveRDS(join_res[[8]],
        '../model/data/touchdown_data_column_categories.rds')



