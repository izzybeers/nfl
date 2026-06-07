library(nflreadr)
library(nflseedR)
library(dplyr)
library(jsonlite)
library(lubridate)
library(gemini.R)
library(stringr)
library(httr)
library(purrr)
library(gbm)
source('data_collection/scripts/global.R')
source('data_collection/scripts/player_data.R')
source('data_collection/scripts/team_game_data.R')
source('data_collection/scripts/get_playoff_clinching_data_script.R')
#source('data_collection/scripts/nlp.R')
source('data_collection/scripts/blue_chip_analysis.R')

t1 = Sys.time()

team_data_all =  pull_all_team_stats(min_year, max_year, recalculate_seasonal = TRUE)
team_data_combined = team_data_all[[1]]
opp_data_combined= team_data_all[[2]]
team_column_categories = team_data_all[[3]]
team_redzone_drives = team_data_all[[4]]

player_data_all = pull_all_player_stats(min_year, max_year, team_redzone_drives = team_redzone_drives, recalculate_seasonal = TRUE)
player_data_combined = player_data_all[[1]]
defense_data_combined = player_data_all[[2]]
player_column_categories = player_data_all[[3]]

column_categories = c(team_column_categories, player_column_categories)

#Blue chip analysis looks at a player's stats in the current year (up until the week of the dataset's row) and two previous seasons. Only players who have been playing that long are considered.
#3 metrics are used to assess whether a player is a blue chip:
#1) Avg yards per game must be above a certain threshold
#2) If they missed any games, the team's performance in that area (passing or rushing) must have been a statistically significant difference from when they played vs not played.
#3) If they played on more than one team during this timeframe, there was no statistically significant difference in their performance on different teams.

#quarterbacks
#offense_pct > 0.5 means we are only considering games where the QB played more than half the snaps in the game. Any less will be considered a game they did not play for purposes of the analysis.
blue_chip_analysis_passing = get_blue_chip_analysis(player_df = player_data_combined %>% filter(position_group == 'QB' & offense_pct > 0.5) %>% 
                                                      select(gsis_id, display_name, season, team, week, passing_yards, avg_passing_yards, Last_Season_avg_passing_yards, Two_Seasons_Ago_avg_passing_yards, Last_Season_weeks_active, Two_Seasons_Ago_weeks_active) %>%
                                                      rename('player_metric' = 'passing_yards',
                                                             'avg_player_metric' = 'avg_passing_yards',
                                                             'last_season_avg_player_metric' = 'Last_Season_avg_passing_yards',
                                                             'two_seasons_ago_avg_player_metric' = 'Two_Seasons_Ago_avg_passing_yards'),
                                                    team_df = team_data_combined %>% select(team, season, week, team_passing_yards) %>%
                                                                                          rename('team_metric' = 'team_passing_yards'),
                                                    direction_play_vs_not_play = 'greater')

#Rushing
#offense_pct > 0.2 means we are only considering games where the RB played more than 20% of snaps. This will make sure we are not looking at a player's performance in a game if
#they aren't one of the main RBs and only do one or two rushes. Also, we only consider running backs here; quarterbacks and receivers don't rush enough to be considered a rushing blue chip.
blue_chip_analysis_rushing = get_blue_chip_analysis(player_df = player_data_combined %>% filter(position_group == 'RB' & offense_pct > 0.2) %>% 
                                                      select(gsis_id, display_name, season, team, week, rushing_yards, avg_rushing_yards, Last_Season_avg_rushing_yards, Two_Seasons_Ago_avg_rushing_yards, Last_Season_weeks_active, Two_Seasons_Ago_weeks_active,
                                                             pct_share_of_carries, pct_share_of_rushing_yards) %>%
                                                      rename('player_metric' = 'rushing_yards',
                                                             'avg_player_metric' = 'avg_rushing_yards',
                                                             'last_season_avg_player_metric' = 'Last_Season_avg_rushing_yards',
                                                             'two_seasons_ago_avg_player_metric' = 'Two_Seasons_Ago_avg_rushing_yards',
                                                             'pct_share' = 'pct_share_of_rushing_yards'),
                                                    team_df = team_data_combined %>% select(team, season, week, team_rushing_yards) %>%
                                                      rename('team_metric' = 'team_rushing_yards'),
                                                    direction_play_vs_not_play = 'greater')

#receiving
#offense_pct > 0.2 means we are only considering games where the player played more than 20% of snaps. We are also only considering wide receivers and tight ends.
blue_chip_analysis_receiving = get_blue_chip_analysis(player_df = player_data_combined %>% filter(position_group %in% c('WR', 'TE') & offense_pct > 0.2) %>% 
                                                      select(gsis_id, display_name, season, team, week, receiving_yards, avg_receiving_yards, Last_Season_avg_receiving_yards, Two_Seasons_Ago_avg_receiving_yards, Last_Season_weeks_active, Two_Seasons_Ago_weeks_active,
                                                             pct_share_of_targets, pct_share_of_intended_air_yards) %>%
                                                      rename('player_metric' = 'receiving_yards',
                                                             'avg_player_metric' = 'avg_receiving_yards',
                                                             'last_season_avg_player_metric' = 'Last_Season_avg_receiving_yards',
                                                             'two_seasons_ago_avg_player_metric' = 'Two_Seasons_Ago_avg_receiving_yards',
                                                             'pct_share' = 'pct_share_of_intended_air_yards'),
                                                    team_df = team_data_combined %>% select(team, season, week, team_passing_yards) %>%
                                                      rename('team_metric' = 'team_passing_yards'),
                                                    direction_play_vs_not_play = 'greater')



#add present stats to opp table:
#opp_defense_sacks_suffered_forced
#opp_defense_attempts_allowed
#include opp_defense_sacks_suffered_forced/opp_attempts in the efficiency metrics but keep the raw present stats too
#opp_defense_passing_yards_allowed
#add opp_defense_passing_yards
#add opp_defense_passing_yards_allowed/opp_attempts to efficiency metrics
#opp rushing yards
#opp carries
#add opp_rushing_yards/opp_carries to efficiency metrics

#finished player defensive stats
#now go through the team data and get that working

blue_chip_analysis_defense_pass_rushers = get_blue_chip_analysis(player_df = defense_data_combined %>% filter(position %in% c('DE', 'DT', 'DL', 'LB', 'OLB') & defense_pct > 0.2) %>%
                                                        rename('player_metric' = 'def_pressure_score',
                                                               'avg_player_metric' = 'avg_def_pressure_score',
                                                               'last_season_avg_player_metric' = 'Last_Season_avg_def_pressure_score',
                                                               'two_seasons_ago_avg_player_metric' = 'Two_Seasons_Ago_avg_def_pressure_score',
                                                               'pct_share' = 'pct_share_of_pressures'),
                                                      team_df = opp_data_combined %>% rename('team' = 'opponent_team') %>% mutate(team_metric = opp_sacks_forced_per_attempt_allowed_current_game),
                                                      direction_play_vs_not_play = 'greater')

blue_chip_analysis_defense_rush_tackles = get_blue_chip_analysis(player_df = defense_data_combined %>% filter(position %in% c('DT', 'NT', 'MLB', 'ILB', 'DL', 'LB', 'DE', 'OLB') & defense_pct > 0.2)%>%
                                                                   rename('player_metric' = 'def_tackles_score',
                                                                          'avg_player_metric' = 'avg_def_tackles_score',
                                                                          'last_season_avg_player_metric' = 'Last_Season_avg_def_tackles_score',
                                                                          'two_seasons_ago_avg_player_metric' = 'Two_Seasons_Ago_avg_def_tackles_score',
                                                                          'pct_share' = 'pct_share_of_tackles'),
                                                                 team_df = opp_data_combined %>% rename('team' = 'opponent_team') %>% mutate(team_metric = opp_rushing_yards_per_carry_allowed_current_game),
                                                                 direction_play_vs_not_play = 'less')

blue_chip_analysis_defense_secondary = get_blue_chip_analysis(player_df = defense_data_combined %>% filter(position %in% c('CB', 'FS', 'S', 'SAF', 'DB') & defense_pct > 0.2) %>%
                                                                   rename('player_metric' = 'def_pass_defend_and_int_score',
                                                                          'avg_player_metric' = 'avg_def_pass_defend_and_int_score',
                                                                          'last_season_avg_player_metric' = 'Last_Season_avg_def_pass_defend_and_int_score',
                                                                          'two_seasons_ago_avg_player_metric' = 'Two_Seasons_Ago_avg_def_pass_defend_and_int_score',
                                                                          'pct_share' = 'pct_share_of_pass_defense_and_int'),
                                                                 team_df = opp_data_combined %>% rename('team' = 'opponent_team') %>% mutate(team_metric = opp_passing_yards_per_attempt_allowed_current_game),
                                                              direction_play_vs_not_play = 'less') 

#next steps:
#team data
#kickers
#what to do about kicker being out?

#remove passing yards and rushing yards (present stats) from team data after blue chip analysis is done.
team_data_combined = team_data_combined %>% select(-team_passing_yards, -team_rushing_yards)
#check names:
opp_data_combined = opp_data_combined %>%
  select(-opp_defense_passing_yards_allowed, -opp_defense_rushing_yards_allowed, -opp_defense_sacks_suffered_forced, -opp_defense_attempts_allowed, -opp_defense_carries_allowed,
         -opp_passing_yards_per_attempt_allowed_current_game, -opp_sacks_forced_per_attempt_allowed_current_game, -opp_rushing_yards_per_carry_allowed_current_game) %>%
  select(-matches('carries_allowed|attempts_allowed'))
player_data_combined = player_data_combined %>% select(-offense_pct)


passing_threshold = quantile(blue_chip_analysis_passing$avg_metric_this_season_two_previous_seasons, 0.66, na.rm=TRUE)
passing_threshold_higher = quantile(blue_chip_analysis_passing$avg_metric_this_season_two_previous_seasons, 0.8, na.rm=TRUE)
rushing_threshold = quantile(blue_chip_analysis_rushing$avg_metric_this_season_two_previous_seasons, 0.66, na.rm=TRUE)
rushing_threshold_higher = quantile(blue_chip_analysis_rushing$avg_metric_this_season_two_previous_seasons, 0.8, na.rm=TRUE)
receiving_threshold = quantile(blue_chip_analysis_receiving$avg_metric_this_season_two_previous_seasons, 0.66, na.rm=TRUE)
receiving_threshold_higher = quantile(blue_chip_analysis_receiving$avg_metric_this_season_two_previous_seasons, 0.8, na.rm=TRUE)

pass_rushers_threshold = quantile(blue_chip_analysis_defense_pass_rushers$avg_metric_this_season_two_previous_seasons, 0.66, na.rm=TRUE)
pass_rushers_threshold_higher = quantile(blue_chip_analysis_defense_pass_rushers$avg_metric_this_season_two_previous_seasons, 0.8, na.rm=TRUE)
rush_tackles_threshold = quantile(blue_chip_analysis_defense_rush_tackles$avg_metric_this_season_two_previous_seasons, 0.66, na.rm=TRUE)
rush_tackles_threshold_higher = quantile(blue_chip_analysis_defense_rush_tackles$avg_metric_this_season_two_previous_seasons, 0.8, na.rm=TRUE)
secondary_threshold = quantile(blue_chip_analysis_defense_secondary$avg_metric_this_season_two_previous_seasons, 0.66, na.rm=TRUE)
secondary_threshold_higher = quantile(blue_chip_analysis_defense_secondary$avg_metric_this_season_two_previous_seasons, 0.8, na.rm=TRUE)


blue_chip_analysis_df = blue_chip_analysis_passing %>% mutate() %>%
  mutate(passing_blue_chip = case_when(
    is.na(avg_metric_this_season_two_previous_seasons) ~ FALSE,
    avg_metric_this_season_two_previous_seasons < passing_threshold  ~ FALSE,
    pvalue_play_vs_not_play > 0.1 ~ FALSE, #if the team did just as well with and without you (no stat significant difference) then you're not a blue chip
    changing_teams_pvalue_worse < 0.1 ~ FALSE, #if you changed teams and you play worse now (with stat significance), then you're not a blue chip
    is.na(pvalue_play_vs_not_play) & avg_metric_this_season_two_previous_seasons < passing_threshold_higher  ~ FALSE, #if there is insufficient data on how the team plays with and without you, you must be above 80th percentile to be a blue chip
    .default = TRUE
)) %>% select(gsis_id, week, season, team, passing_blue_chip) %>% full_join(
  blue_chip_analysis_rushing %>% mutate(rushing_blue_chip = case_when(
    is.na(avg_metric_this_season_two_previous_seasons) ~ FALSE,
    avg_metric_this_season_two_previous_seasons < rushing_threshold  ~ FALSE,
    pvalue_play_vs_not_play > 0.15 ~ FALSE, #if the team did just as well with and without you (no stat significant difference) then you're not a blue chip
    changing_teams_pvalue_worse < 0.15 ~ FALSE, #if you changed teams and you play worse now (with stat significance), then you're not a blue chip
    pct_share < 0.4 ~ FALSE, #if you have less than 40% of the rushing yards on the team overall on average then you're not a blue chip
    is.na(pvalue_play_vs_not_play) & avg_metric_this_season_two_previous_seasons < rushing_threshold_higher  ~ FALSE, #if there is insufficient data on how the team plays with and without you, you must be above 80th percentile to be a blue chip
    .default = TRUE
  )) %>% select(gsis_id, week, season, team, rushing_blue_chip), join_by('gsis_id', 'week', 'season', 'team')) %>% 
  full_join(blue_chip_analysis_receiving %>% mutate(receiving_blue_chip = case_when(
    is.na(avg_metric_this_season_two_previous_seasons) ~ FALSE,
    avg_metric_this_season_two_previous_seasons < receiving_threshold  ~ FALSE, 
    pvalue_play_vs_not_play > 0.15 ~ FALSE, #if the team did just as well with and without you (no stat significant difference) then you're not a blue chip
    changing_teams_pvalue_worse < 0.15 ~ FALSE, #if you changed teams and you play worse now (with stat significance), then you're not a blue chip
    pct_share < 0.2 ~ FALSE,
    is.na(pvalue_play_vs_not_play) & avg_metric_this_season_two_previous_seasons < receiving_threshold_higher  ~ FALSE, #if there is insufficient data on how the team plays with and without you, you must be above 80th percentile to be a blue chip
    .default = TRUE
  )) %>% select(gsis_id, week, season, team, receiving_blue_chip), join_by('gsis_id', 'week', 'season', 'team')) %>% filter(season >= (min_year+2)) %>% 
  full_join(blue_chip_analysis_defense_pass_rushers %>% mutate(pass_rushers_blue_chip = case_when(
    is.na(avg_metric_this_season_two_previous_seasons) ~ FALSE,
    avg_metric_this_season_two_previous_seasons < pass_rushers_threshold  ~ FALSE, 
    pvalue_play_vs_not_play > 0.15 ~ FALSE, #if the team did just as well with and without you (no stat significant difference) then you're not a blue chip
    changing_teams_pvalue_worse < 0.15 ~ FALSE, #if you changed teams and you play worse now (with stat significance), then you're not a blue chip
    pct_share < 0.2 ~ FALSE,
    is.na(pvalue_play_vs_not_play) & avg_metric_this_season_two_previous_seasons < pass_rushers_threshold_higher  ~ FALSE, #if there is insufficient data on how the team plays with and without you, you must be above 80th percentile to be a blue chip
    .default = TRUE
  )) %>% select(gsis_id, week, season, team, pass_rushers_blue_chip), join_by('gsis_id', 'week', 'season', 'team')) %>% filter(season >= (min_year+2)) %>% 
  full_join(blue_chip_analysis_defense_rush_tackles %>% mutate(rush_tackles_blue_chip = case_when(
    is.na(avg_metric_this_season_two_previous_seasons) ~ FALSE,
    avg_metric_this_season_two_previous_seasons < rush_tackles_threshold  ~ FALSE, 
    pvalue_play_vs_not_play > 0.15 ~ FALSE, #if the team did just as well with and without you (no stat significant difference) then you're not a blue chip
    changing_teams_pvalue_worse < 0.15 ~ FALSE, #if you changed teams and you play worse now (with stat significance), then you're not a blue chip
    pct_share < 0.2 ~ FALSE,
    is.na(pvalue_play_vs_not_play) & avg_metric_this_season_two_previous_seasons < rush_tackles_threshold_higher  ~ FALSE, #if there is insufficient data on how the team plays with and without you, you must be above 80th percentile to be a blue chip
    .default = TRUE
  )) %>% select(gsis_id, week, season, team, rush_tackles_blue_chip), join_by('gsis_id', 'week', 'season', 'team')) %>% filter(season >= (min_year+2)) %>% 
  full_join(blue_chip_analysis_defense_secondary %>% mutate(secondary_blue_chip = case_when(
    is.na(avg_metric_this_season_two_previous_seasons) ~ FALSE,
    avg_metric_this_season_two_previous_seasons < secondary_threshold  ~ FALSE, 
    pvalue_play_vs_not_play > 0.15 ~ FALSE, #if the team did just as well with and without you (no stat significant difference) then you're not a blue chip
    changing_teams_pvalue_worse < 0.15 ~ FALSE, #if you changed teams and you play worse now (with stat significance), then you're not a blue chip
    pct_share < 0.2 ~ FALSE,
    is.na(pvalue_play_vs_not_play) & avg_metric_this_season_two_previous_seasons < secondary_threshold_higher  ~ FALSE, #if there is insufficient data on how the team plays with and without you, you must be above 80th percentile to be a blue chip
    .default = TRUE
  )) %>% select(gsis_id, week, season, team, secondary_blue_chip), join_by('gsis_id', 'week', 'season', 'team')) %>% filter(season >= (min_year+2))




injuries = load_injuries(seasons = min_year:max_year) %>% group_by(season, week, gsis_id, team) %>% slice(1) %>% ungroup() %>%
  mutate(illness = str_detect(tolower(practice_primary_injury), 'illness|covid|appendix|appendicitis|headache'),
         out_not_injury_related = str_detect(tolower(practice_primary_injury), 'not injury related|personal|team decision|travel|suspension|coach|jury duty|rest')) %>%
  mutate(less_practice = !(practice_status == 'Full Participation in Practice')) %>%
  select(season, week, gsis_id,  team, report_status, less_practice, illness, out_not_injury_related) %>%
  full_join(load_rosters_weekly((min_year+2):(max_year)) %>% filter(status != 'ACT') %>% select(season, week, status, gsis_id) %>% mutate(report_status = 'Out'),join_by('gsis_id','season','week')) %>%
  mutate(out = report_status.x == 'Out' |report_status.y == 'Out') %>% select(-report_status.x, -report_status.y)

blue_chip_players_out = blue_chip_analysis_df %>% filter((passing_blue_chip | rushing_blue_chip | receiving_blue_chip | pass_rushers_blue_chip | rush_tackles_blue_chip | secondary_blue_chip) & season >= (min_year+2)) %>%
  left_join(injuries %>% select(gsis_id,  season, week, team, out),
             join_by('season', 'week', 'gsis_id', 'team')) %>%
  mutate(out = ifelse(is.na(out), FALSE, out)) %>%
  group_by(team,season,week) %>% summarise(has_passing_blue_chip_out = any(out & passing_blue_chip),
                                           has_rushing_blue_chip_out = any(out & rushing_blue_chip),
                                           has_receiving_blue_chip_out = any(out & receiving_blue_chip),
                                           has_pass_rushers_blue_chip_out = any(out & pass_rushers_blue_chip),
                                           has_rush_tackles_blue_chip_out = any(out & rush_tackles_blue_chip),
                                           has_secondary_blue_chip_out = any(out & secondary_blue_chip)
                                           ) %>% arrange(team,season,week)

injuries = injuries %>% mutate(out = ifelse(is.na(out), FALSE, out)) %>% select(-status)

column_categories[['injuries']] = setdiff(colnames(injuries),c('season','week','gsis_id','full_name','team'))
column_categories[['blue_chip']] = c('passing_blue_chip', 'rushing_blue_chip', 'receiving_blue_chip', 'pass_rushers_blue_chip', 'rush_tackles_blue_chip', 'secondary_blue_chip',
                                     'has_passing_blue_chip_out', 'has_rushing_blue_chip_out', 'has_receiving_blue_chip_out', 'has_pass_rushers_blue_chip_out', 'has_rush_tackles_blue_chip_out', 'has_secondary_blue_chip_out')

#playoff_clinching_data = get_playoff_clinching_data(min_year+2, max_year, wk = NULL, predict_mode = FALSE)
playoff_clinching_data = get_supabase_data(schema = 'MainData', table_name = 'PlayoffClinching')

column_categories[['playoff_clinching']] = setdiff(colnames(playoff_clinching_data), c('Season','Week','Team'))

#video_youtube_transcripts = get_transcripts(team_lookup_table, date_cutoff = '2020-08-01')

#video_llm_results = get_video_llm_results(video_youtube_transcripts, player_lookup = player_data %>% ungroup() %>% select(gsis_id, display_name) %>% distinct())

#when joining on blue chip data, join on the player but also how it affects other players:
#get blue chips listed as out and add field for team members called blue_chip_team_member_out
model_data = player_data_combined %>%
  inner_join(team_data_combined %>% select(-game_id), join_by('season', 'week', 'team')) %>%
  inner_join(opp_data_combined, join_by('season', 'week', 'opponent_team')) %>%
  left_join(injuries, join_by('season', 'week', 'gsis_id', 'team')) %>%
  left_join(blue_chip_players_out, join_by('season','week','team')) %>% mutate(has_passing_blue_chip_out = ifelse(is.na(has_passing_blue_chip_out), FALSE, has_passing_blue_chip_out),
                                                                               has_rushing_blue_chip_out = ifelse(is.na(has_rushing_blue_chip_out), FALSE, has_rushing_blue_chip_out),
                                                                               has_receiving_blue_chip_out = ifelse(is.na(has_receiving_blue_chip_out), FALSE, has_receiving_blue_chip_out),
                                                                               has_pass_rushers_blue_chip_out = ifelse(is.na(has_pass_rushers_blue_chip_out), FALSE, has_pass_rushers_blue_chip_out),
                                                                               has_rush_tackles_blue_chip_out = ifelse(is.na(has_rush_tackles_blue_chip_out), FALSE, has_rush_tackles_blue_chip_out),
                                                                               has_secondary_blue_chip_out = ifelse(is.na(has_secondary_blue_chip_out), FALSE, has_secondary_blue_chip_out)) %>%
  left_join(blue_chip_analysis_df, join_by('season','week','team','gsis_id')) %>%
  left_join(playoff_clinching_data, join_by('season' == 'Season', 'week' == 'Week', 'team' == 'Team')) %>%
    filter(season >= (min_year+2)) %>% select(-team_score, -opponent_score) %>%
  mutate(game_on_birthday = substring(gameday,6,10) == substring(birth_date,6,10)) %>% select(-birth_date) %>%
  mutate(across(where(is.numeric), ~ ifelse(is.infinite(.x), NA, .x))) %>%
  select(-team_win, -team_differential) #this field is for the team models only
  #left_join(video_llm_results[[1]], join_by('season', 'week', 'team')) %>%
  #left_join(video_llm_results[[2]], join_by('season', 'week', 'gsis_id')) 

column_categories[['game_info']] = c(column_categories[['game_info']], 'game_on_birthday')

#column_categories[['youtube_llm']] = setdiff(colnames(video_llm_results), c('team','season','week'))


passing_model_data = model_data %>% filter(position == 'QB') %>% select(-rushing_yards, -receiving_yards, -anytime_td_scorer, -receptions, -team_attempts) %>% select(-any_of(c(column_categories[['rushing_current_season_stats']], column_categories[['receiving_current_season_stats']],
                                                                                                                                                                                column_categories[['rushing_past_season_stats']], column_categories[['receiving_past_season_stats']])))

rushing_model_data = model_data %>% filter(position %in% c('QB','RB')) %>% select(-passing_yards, -receiving_yards, -anytime_td_scorer, -receptions, -team_attempts) %>% select(-any_of(c(column_categories[['passing_current_season_stats']], column_categories[['receiving_current_season_stats']],
                                                                                                                                                                                          column_categories[['passing_past_season_stats']], column_categories[['receiving_past_season_stats']])))

receiving_model_data = model_data %>%
  filter(position %in% c('RB','WR','TE')) %>%
  select(-passing_yards, -rushing_yards, -anytime_td_scorer, -receptions) %>%
  select(-any_of(c(column_categories[['passing_current_season_stats']], column_categories[['rushing_current_season_stats']],
                   column_categories[['passing_past_season_stats']], column_categories[['rushing_past_season_stats']])))

#figure out how to combine redzone receiving and rushing targets as an efficiency metric

touchdown_model_data = model_data %>%
  filter(position %in% c('QB', 'RB', 'WR', 'TE')) %>%
  select(-passing_yards, -rushing_yards, -receiving_yards, -receptions) %>%
  select(-any_of(c(column_categories[['passing_current_season_stats']], column_categories[['passing_past_season_stats']])))

reception_model_data = model_data %>%
  filter(position %in% c('RB', 'WR', 'TE')) %>%
  select(-passing_yards, -rushing_yards, -receiving_yards, -anytime_td_scorer)  %>%
  select(-any_of(c(column_categories[['passing_current_season_stats']], column_categories[['rushing_current_season_stats']],
                   column_categories[['passing_past_season_stats']], column_categories[['rushing_past_season_stats']])))

rushing_receiving_model_data = model_data %>% mutate(rushing_receiving_yards = coalesce(rushing_yards,0) + coalesce(receiving_yards,0)) %>%
  filter(position %in% c('QB', 'RB', 'WR', 'TE')) %>% 
  select(-passing_yards, -rushing_yards, -receiving_yards, -anytime_td_scorer, -receptions) %>%
  select(-any_of(c(column_categories[['passing_current_season_stats']], column_categories[['passing_past_season_stats']])))

qb_summaries = passing_model_data %>% mutate(avg_passing_yards_this_qb = avg_passing_yards,
                                             avg_aggressiveness_this_qb = avg_aggressiveness,
                                             air_yards_per_attempt_this_qb = air_yards_per_attempt,
                                             air_yards_to_sticks_per_attempt_this_qb = air_yards_to_sticks_per_attempt,
                                             passing_pct_bad_throws_this_qb = passing_pct_bad_throws,
                                             avg_sacks_this_qb = avg_sacks_suffered,
                                             avg_attempts_this_qb = avg_attempts
                                             ) %>%
  select(season, week, team, gsis_id, contains('this_qb'))

column_categories[['receivers_qb_stats']] = setdiff(colnames(qb_summaries), c('season','week','team','gsis_id'))

receiving_model_data = receiving_model_data %>% left_join(qb_summaries,join_by('team_qb_id' == 'gsis_id','season','week','team'))
reception_model_data = reception_model_data %>% left_join(qb_summaries,join_by('gsis_id','season','week','team'))
rushing_receiving_model_data = rushing_receiving_model_data %>% left_join(qb_summaries,join_by('gsis_id','season','week','team'))
touchdown_model_data = touchdown_model_data %>% left_join(qb_summaries,join_by('gsis_id','season','week','team'))

#PROMOTION/DEMOTION ANALYSIS:

pass_attempts_high_lower_bound = quantile(passing_model_data$avg_attempts, 0.66, na.rm=TRUE) #comes out to 35
pass_attempts_medium_lower_bound = quantile(passing_model_data$avg_attempts, 0.33, na.rm=TRUE) #comes out to 25
rush_attempts_high_lower_bound = quantile(rushing_model_data$avg_team_carries, 0.66, na.rm=TRUE)
rush_attempts_medium_lower_bound = quantile(rushing_model_data$avg_team_carries, 0.33, na.rm=TRUE)

get_receiving_rookie_estimates = function(df)
{
  return(df %>% group_by(team_coach) %>% arrange(season, week) %>%
    mutate(games_coached = (row_number()-1)) %>%
    group_by(team_coach, season) %>% mutate(games_coached_this_season = (row_number()-1)) %>%
    ungroup() %>% group_by(team_coach) %>%
    mutate(avg_team_pass_attempts_this_coach_past_30_games = case_when(
      games_coached > games_coached_this_season ~ slide_dbl(team_attempts, ~ ifelse(length(.x) == 0, NA_real_, mean(.x, na.rm = TRUE)), .before = 30, .after = -1, .complete = FALSE),
      .default = NA)) %>%
    ungroup() %>%
    mutate(pass_attempt_volume = case_when(
      !is.na(avg_passing_yards_this_qb) ~ avg_passing_yards_this_qb,
      !is.na(avg_team_pass_attempts_this_coach_past_30_games) ~ avg_team_pass_attempts_this_coach_past_30_games,
      .default = Last_Season_avg_team_attempts
    )) %>%
    mutate(pass_attempt_volume_category = case_when(
      pass_attempt_volume > pass_attempts_high_lower_bound ~ 'High',
      pass_attempt_volume > pass_attempts_medium_lower_bound ~ 'Medium',
      .default = 'Low'
    )) %>% select(-games_coached_this_season, -avg_team_pass_attempts_this_coach_past_30_games, -games_coached))
}

get_rushing_rookie_estimates = function(df)
{
  return(df %>% group_by(team_coach) %>% arrange(season, week) %>%
           mutate(games_coached = (row_number()-1)) %>%
           group_by(team_coach, season) %>% mutate(games_coached_this_season = (row_number()-1)) %>%
           ungroup() %>% group_by(team_coach) %>%
           mutate(avg_team_rush_attempts_this_coach_past_30_games = case_when(
             games_coached > games_coached_this_season ~ slide_dbl(avg_team_carries*games_played_this_season, ~ ifelse(length(.x) == 0, NA_real_, mean(.x, na.rm = TRUE)), .before = 30, .after = -1, .complete = FALSE),
             .default = NA)) %>%
           ungroup() %>%
           mutate(rush_attempt_volume = case_when(
             !is.na(avg_team_rush_attempts_this_coach_past_30_games) ~ avg_team_rush_attempts_this_coach_past_30_games,
             .default = Last_Season_avg_team_carries
           )) %>%
           mutate(rush_attempt_volume_category = case_when(
             rush_attempt_volume > rush_attempts_high_lower_bound ~ 'High',
             rush_attempt_volume > rush_attempts_medium_lower_bound ~ 'Medium',
             .default = 'Low'
           ))  %>% select(-games_coached_this_season, -avg_team_rush_attempts_this_coach_past_30_games, -games_coached)
  )
}

rookie_receiving_analysis =  receiving_model_data %>%
  get_receiving_rookie_estimates() %>%
  group_by(draft_round, pass_attempt_volume_category) %>% summarise(avg_target_share_rookie_group = mean(pct_share_of_targets, na.rm = TRUE)) %>%
  mutate(draft_round = ifelse(is.na(draft_round), 'Undrafted', as.character(draft_round)))

rookie_rushing_analysis = rushing_model_data %>%
  get_rushing_rookie_estimates() %>%
  group_by(draft_round, rush_attempt_volume_category) %>% summarise(avg_rush_share_rookie_group = mean(pct_share_of_carries, na.rm = TRUE)) %>%
  mutate(draft_round = ifelse(is.na(draft_round), 'Undrafted', as.character(draft_round)))

#true talent baseline:
receiving_model_data = receiving_model_data %>%
  mutate(pct_share_of_targets = ifelse(avg_targets == 0, 0, pct_share_of_targets),
         Last_Season_pct_share_of_targets = ifelse(Last_Season_avg_targets == 0, 0, Last_Season_pct_share_of_targets)) %>%
  get_receiving_rookie_estimates() %>%
  mutate(draft_round = ifelse(is.na(draft_round), 'Undrafted', as.character(draft_round))) %>%
  left_join(rookie_receiving_analysis, join_by('draft_round', 'pass_attempt_volume_category')) %>%
  group_by(gsis_id) %>%
  mutate(true_talent_baseline_receiving = case_when(
    games_played_this_season > 5 ~ pct_share_of_targets,
    games_played_this_season > 0 & games_played > games_played_this_season ~ (games_played_this_season/6)*pct_share_of_targets + (1-games_played_this_season/6)*Last_Season_pct_share_of_targets,
    games_played > games_played_this_season ~ Last_Season_pct_share_of_targets,
  #rookies:
  games_played_this_season > 0 ~  (games_played_this_season/6)*pct_share_of_targets + (1-games_played_this_season/6)*avg_target_share_rookie_group,
  .default = avg_target_share_rookie_group
)) %>% select(-avg_target_share_rookie_group, -pass_attempt_volume_category) %>%
  ungroup() %>% filter(is.na(out) | !out) %>%
  group_by(season, week, team) %>% mutate(total_active_true_talent_baseline = sum(true_talent_baseline_receiving, na.rm = TRUE)) %>%
  mutate(adjusted_target_share = true_talent_baseline_receiving/total_active_true_talent_baseline,
         delta_adjusted_share_receiving = adjusted_target_share - true_talent_baseline_receiving,
         recent_form_delta_receiving = case_when(
           games_played_this_season == 0 ~ 0,
           .default = last3_pct_share_of_targets - true_talent_baseline_receiving
         ))  %>% select(-total_active_true_talent_baseline, -pass_attempt_volume, -team_attempts)

reception_model_data = reception_model_data %>%
  mutate(pct_share_of_targets = ifelse(avg_targets == 0, 0, pct_share_of_targets),
         Last_Season_pct_share_of_targets = ifelse(Last_Season_avg_targets == 0, 0, Last_Season_pct_share_of_targets)) %>%
  get_receiving_rookie_estimates() %>%
  mutate(draft_round = ifelse(is.na(draft_round), 'Undrafted', as.character(draft_round))) %>%
  left_join(rookie_receiving_analysis, join_by('draft_round', 'pass_attempt_volume_category')) %>%
  group_by(gsis_id) %>%
  mutate(true_talent_baseline_receiving = case_when(
    games_played_this_season > 5 ~ pct_share_of_targets,
    games_played_this_season > 0 & games_played > games_played_this_season ~ (games_played_this_season/6)*pct_share_of_targets + (1-games_played_this_season/6)*Last_Season_pct_share_of_targets,
    games_played > games_played_this_season ~ Last_Season_pct_share_of_targets,
    #rookies:
    games_played_this_season > 0 ~  (games_played_this_season/6)*pct_share_of_targets + (1-games_played_this_season/6)*avg_target_share_rookie_group,
    .default = avg_target_share_rookie_group
  )) %>% select(-avg_target_share_rookie_group, -pass_attempt_volume_category) %>%
  ungroup() %>% filter(is.na(out) | !out) %>%
  group_by(season, week, team) %>% mutate(total_active_true_talent_baseline = sum(true_talent_baseline_receiving, na.rm = TRUE)) %>%
  mutate(adjusted_target_share = true_talent_baseline_receiving/total_active_true_talent_baseline,
         delta_adjusted_share_receiving = adjusted_target_share - true_talent_baseline_receiving,
         recent_form_delta_receiving = case_when(
           games_played_this_season == 0 ~ 0,
           .default = last3_pct_share_of_targets - true_talent_baseline_receiving
         ))  %>% select(-total_active_true_talent_baseline, -pass_attempt_volume, -team_attempts)

rushing_model_data = rushing_model_data %>%
  mutate(pct_share_of_carries = ifelse(avg_carries == 0, 0, pct_share_of_carries),
         Last_Season_pct_share_of_carries = ifelse(Last_Season_avg_carries == 0, 0, Last_Season_pct_share_of_carries)) %>%
  mutate(draft_round = ifelse(is.na(draft_round), 'Undrafted', as.character(draft_round))) %>%
  get_rushing_rookie_estimates() %>%
  left_join(rookie_rushing_analysis, join_by('draft_round', 'rush_attempt_volume_category')) %>%
  group_by(gsis_id) %>%
  mutate(true_talent_baseline_rushing = case_when(
    games_played_this_season > 5 ~ pct_share_of_carries,
    games_played_this_season > 0 & games_played > games_played_this_season ~ (games_played_this_season/6)*pct_share_of_carries + (1-games_played_this_season/6)*Last_Season_pct_share_of_carries,
    games_played > games_played_this_season ~ Last_Season_pct_share_of_carries,
    #rookies:
    games_played_this_season > 0 ~  (games_played_this_season/6)*pct_share_of_carries + (1-games_played_this_season/6)*avg_rush_share_rookie_group,
    .default = avg_rush_share_rookie_group
  )) %>% select(-avg_rush_share_rookie_group, -rush_attempt_volume_category) %>%
  ungroup() %>% filter(is.na(out) | !out) %>%
  group_by(season, week, team) %>% mutate(total_active_true_talent_baseline = sum(true_talent_baseline_rushing, na.rm = TRUE)) %>%
  mutate(adjusted_rush_share = true_talent_baseline_rushing/total_active_true_talent_baseline,
         delta_adjusted_share_rushing = adjusted_rush_share - true_talent_baseline_rushing,
         recent_form_delta_rushing = case_when(
           games_played_this_season == 0 ~ 0,
           .default = last3_pct_share_of_carries - true_talent_baseline_rushing
         )) %>% select(-total_active_true_talent_baseline, -rush_attempt_volume)

rushing_receiving_model_data = rushing_receiving_model_data %>%
  mutate(pct_share_of_carries = ifelse(avg_carries == 0, 0, pct_share_of_carries),
         Last_Season_pct_share_of_carries = ifelse(Last_Season_avg_carries == 0, 0, Last_Season_pct_share_of_carries),
         pct_share_of_targets = ifelse(avg_targets == 0, 0, pct_share_of_targets),
         Last_Season_pct_share_of_targets = ifelse(Last_Season_avg_targets == 0, 0, Last_Season_pct_share_of_targets)) %>%
  mutate(draft_round = ifelse(is.na(draft_round), 'Undrafted', as.character(draft_round))) %>%
  get_rushing_rookie_estimates() %>%
  get_receiving_rookie_estimates() %>%
  left_join(rookie_rushing_analysis, join_by('draft_round', 'rush_attempt_volume_category')) %>%
  left_join(rookie_receiving_analysis, join_by('draft_round', 'pass_attempt_volume_category')) %>%
  group_by(gsis_id) %>%
  mutate(true_talent_baseline_rushing = case_when(
    games_played_this_season > 5 ~ pct_share_of_carries,
    games_played_this_season > 0 & games_played > games_played_this_season ~ (games_played_this_season/6)*pct_share_of_carries + (1-games_played_this_season/6)*Last_Season_pct_share_of_carries,
    games_played > games_played_this_season ~ Last_Season_pct_share_of_carries,
    #rookies:
    games_played_this_season > 0 ~  (games_played_this_season/6)*pct_share_of_carries + (1-games_played_this_season/6)*avg_rush_share_rookie_group,
    .default = avg_rush_share_rookie_group
  ),
  true_talent_baseline_receiving = case_when(
    games_played_this_season > 5 ~ pct_share_of_targets,
    games_played_this_season > 0 & games_played > games_played_this_season ~ (games_played_this_season/6)*pct_share_of_targets + (1-games_played_this_season/6)*Last_Season_pct_share_of_targets,
    games_played > games_played_this_season ~ Last_Season_pct_share_of_targets,
    #rookies:
    games_played_this_season > 0 ~  (games_played_this_season/6)*pct_share_of_targets + (1-games_played_this_season/6)*avg_target_share_rookie_group,
    .default = avg_target_share_rookie_group
  )) %>% select(-avg_rush_share_rookie_group, -rush_attempt_volume_category, -avg_target_share_rookie_group, -pass_attempt_volume_category) %>%
  ungroup() %>% filter(is.na(out) | !out) %>%
  group_by(season, week, team) %>% mutate(total_active_true_talent_baseline_rushing = sum(true_talent_baseline_rushing, na.rm = TRUE),
                                          total_active_true_talent_baseline_receiving = sum(true_talent_baseline_receiving, na.rm = TRUE)) %>%
  mutate(adjusted_rush_share = true_talent_baseline_rushing/total_active_true_talent_baseline_rushing,
         delta_adjusted_share_rushing = adjusted_rush_share - true_talent_baseline_rushing,
         recent_form_delta_rushing = case_when(
           games_played_this_season == 0 ~ 0,
           .default = last3_pct_share_of_carries - true_talent_baseline_rushing
         ),
         adjusted_target_share = true_talent_baseline_receiving/total_active_true_talent_baseline_receiving,
         delta_adjusted_share_receiving = adjusted_target_share - true_talent_baseline_receiving,
         recent_form_delta_receiving = case_when(
           games_played_this_season == 0 ~ 0,
           .default = last3_pct_share_of_targets - true_talent_baseline_receiving
         ))  %>% select(-total_active_true_talent_baseline_rushing, -total_active_true_talent_baseline_receiving, -rush_attempt_volume, -pass_attempt_volume, -team_attempts)


touchdown_model_data = touchdown_model_data %>%
  mutate(pct_share_of_carries = ifelse(avg_carries == 0, 0, pct_share_of_carries),
         Last_Season_pct_share_of_carries = ifelse(Last_Season_avg_carries == 0, 0, Last_Season_pct_share_of_carries),
         pct_share_of_targets = ifelse(avg_targets == 0, 0, pct_share_of_targets),
         Last_Season_pct_share_of_targets = ifelse(Last_Season_avg_targets == 0, 0, Last_Season_pct_share_of_targets)) %>%
  mutate(draft_round = ifelse(is.na(draft_round), 'Undrafted', as.character(draft_round))) %>%
  get_rushing_rookie_estimates() %>%
  get_receiving_rookie_estimates() %>%
  left_join(rookie_rushing_analysis, join_by('draft_round', 'rush_attempt_volume_category')) %>%
  left_join(rookie_receiving_analysis, join_by('draft_round', 'pass_attempt_volume_category')) %>%
  group_by(gsis_id) %>%
  mutate(true_talent_baseline_rushing = case_when(
    games_played_this_season > 5 ~ pct_share_of_carries,
    games_played_this_season > 0 & games_played > games_played_this_season ~ (games_played_this_season/6)*pct_share_of_carries + (1-games_played_this_season/6)*Last_Season_pct_share_of_carries,
    games_played > games_played_this_season ~ Last_Season_pct_share_of_carries,
    #rookies:
    games_played_this_season > 0 ~  (games_played_this_season/6)*pct_share_of_carries + (1-games_played_this_season/6)*avg_rush_share_rookie_group,
    .default = avg_rush_share_rookie_group
  ),
  true_talent_baseline_receiving = case_when(
    games_played_this_season > 5 ~ pct_share_of_targets,
    games_played_this_season > 0 & games_played > games_played_this_season ~ (games_played_this_season/6)*pct_share_of_targets + (1-games_played_this_season/6)*Last_Season_pct_share_of_targets,
    games_played > games_played_this_season ~ Last_Season_pct_share_of_targets,
    #rookies:
    games_played_this_season > 0 ~  (games_played_this_season/6)*pct_share_of_targets + (1-games_played_this_season/6)*avg_target_share_rookie_group,
    .default = avg_target_share_rookie_group
  )) %>% select(-avg_rush_share_rookie_group, -rush_attempt_volume_category, -avg_target_share_rookie_group, -pass_attempt_volume_category) %>%
  ungroup() %>% filter(is.na(out) | !out) %>%
  group_by(season, week, team) %>% mutate(total_active_true_talent_baseline_rushing = sum(true_talent_baseline_rushing, na.rm = TRUE),
                                          total_active_true_talent_baseline_receiving = sum(true_talent_baseline_receiving, na.rm = TRUE)) %>%
  mutate(adjusted_rush_share = true_talent_baseline_rushing/total_active_true_talent_baseline_rushing,
         delta_adjusted_share_rushing = adjusted_rush_share - true_talent_baseline_rushing,
         recent_form_delta_rushing = case_when(
           games_played_this_season == 0 ~ 0,
           .default = last3_pct_share_of_carries - true_talent_baseline_rushing
         ),
         adjusted_target_share = true_talent_baseline_receiving/total_active_true_talent_baseline_receiving,
         delta_adjusted_share_receiving = adjusted_target_share - true_talent_baseline_receiving,
         recent_form_delta_receiving = case_when(
           games_played_this_season == 0 ~ 0,
           .default = last3_pct_share_of_targets - true_talent_baseline_receiving
         )) %>% select(-total_active_true_talent_baseline_rushing, -total_active_true_talent_baseline_receiving, -team_attempts, -pass_attempt_volume, -rush_attempt_volume)


column_categories[['usage_and_depth']] = c(column_categories[['usage_and_depth']], 'true_talent_baseline_receiving', 'true_talent_baseline_rushing', 'adjusted_target_share', 'delta_adjusted_share_receiving', 'recent_form_delta_receiving', 'adjusted_rush_share', 'delta_adjusted_share_rushing', 'recent_form_delta_rushing')

setdiff(colnames(passing_model_data), unlist(column_categories[-which(names(column_categories) %in% c('rushing_current_season_stats', 'receiving_current_season_stats', 'rushing_past_season_stats', 'receiving_past_season_stats', 'receivers_qb_stats'))]))
setdiff(colnames(rushing_model_data), unlist(column_categories[-which(names(column_categories) %in% c('passing_current_season_stats', 'receiving_current_season_stats', 'passing_past_season_stats', 'receiving_past_season_stats', 'receivers_qb_stats'))]))
setdiff(colnames(receiving_model_data), unlist(column_categories[-which(names(column_categories) %in% c('passing_current_season_stats', 'rushing_current_season_stats', 'passing_past_season_stats', 'rushing_past_season_stats'))]))
setdiff(colnames(reception_model_data), unlist(column_categories[-which(names(column_categories) %in% c('passing_current_season_stats', 'rushing_current_season_stats', 'passing_past_season_stats', 'rushing_past_season_stats'))]))
setdiff(colnames(rushing_receiving_model_data), unlist(column_categories[-which(names(column_categories) %in% c('passing_current_season_stats', 'passing_past_season_stats'))]))
setdiff(colnames(touchdown_model_data), unlist(column_categories[-which(names(column_categories) %in% c('passing_current_season_stats', 'passing_past_season_stats'))]))


setdiff(unlist(column_categories[-which(names(column_categories) %in% c('rushing_current_season_stats', 'receiving_current_season_stats', 'rushing_past_season_stats', 'receiving_past_season_stats', 'receivers_qb_stats'))]), c(colnames(passing_model_data),
                                                                                                                                                                                                                                  column_categories[['usage_and_depth']][str_detect(column_categories[['usage_and_depth']], 'rushing|receiving|target|rush_share')],
                                                                                                                                                                                                                                  column_categories[['matchup_history']][str_detect(column_categories[['matchup_history']], 'rushing|receiving|td')]))
setdiff(unlist(column_categories[-which(names(column_categories) %in% c('passing_current_season_stats', 'receiving_current_season_stats', 'passing_past_season_stats', 'receiving_past_season_stats', 'receivers_qb_stats'))]), c(colnames(rushing_model_data),
                                                                                                                                                                                                                                  column_categories[['usage_and_depth']][str_detect(column_categories[['usage_and_depth']], 'receiving|target')],
                                                                                                                                                                                                                                  column_categories[['matchup_history']][str_detect(column_categories[['matchup_history']], 'passing|receiving|td')]))
        
setdiff(unlist(column_categories[-which(names(column_categories) %in% c('passing_current_season_stats', 'rushing_current_season_stats', 'passing_past_season_stats', 'rushing_past_season_stats'))]), c(colnames(receiving_model_data),
                                                                                                                                                                                                        column_categories[['usage_and_depth']][str_detect(column_categories[['usage_and_depth']], 'rush')],
                                                                                                                                                                                                        column_categories[['matchup_history']][str_detect(column_categories[['matchup_history']], 'passing|rushing|td')]
                                                                                                                                                                                                        ))
setdiff(unlist(column_categories[-which(names(column_categories) %in% c('passing_current_season_stats', 'rushing_current_season_stats', 'passing_past_season_stats', 'rushing_past_season_stats'))]), c(colnames(reception_model_data),
                                                                                                                                                                                                        column_categories[['usage_and_depth']][str_detect(column_categories[['usage_and_depth']], 'passing|rushing|rush_share|passing')],
                                                                                                                                                                                                        column_categories[['matchup_history']][str_detect(column_categories[['matchup_history']], 'passing|rushing|td')]
                                                                                                                                                                                                        ))
setdiff(unlist(column_categories[-which(names(column_categories) %in% c('passing_current_season_stats', 'passing_past_season_stats'))]), c(colnames(rushing_receiving_model_data),
                                                                                                                                           column_categories[['matchup_history']][str_detect(column_categories[['matchup_history']], 'passing|td')]))
setdiff(unlist(column_categories[-which(names(column_categories) %in% c('passing_current_season_stats', 'passing_past_season_stats'))]), c(colnames(touchdown_model_data), column_categories[['matchup_history']][str_detect(column_categories[['matchup_history']], 'passing')]))




#reception model response variable labels: 4,6,8,10
#rushing+receiving model: 40,70,100,130
#moneyline response variable: just win
#alternate spreads: +/- 2.5, 3.5, 6.5. 7.5
#over/under was thinking of spreading it out (37.5, 43.5, 51.5, 58.5) but due to the fact that the total score is generally in increments of 3, 7, etc, this might require a different technique. Maybe drive models.




team_offense = team_data_combined
opp_defense = opp_data_combined
team_defense = opp_data_combined %>% select(-opp_short_week, -opp_long_week) %>% rename_with(.fn = ~gsub('opp', 'team', .x), .cols = contains('opp_')) %>% rename('team' = 'opponent_team') 
opp_offense = team_data_combined %>% rename_with(.fn = ~gsub('team', 'opp_offense', .x), .cols = contains('team_')) %>% select(-short_week, -long_week) %>% select(-opponent_team) %>% rename('opponent_team' = 'team') 

team_stats = team_offense %>% inner_join(team_defense, join_by('season','week', 'team'))
opp_stats = opp_offense %>% inner_join(opp_defense, join_by('season', 'week', 'opponent_team'))

team_model_data = team_stats %>% inner_join(opp_stats %>% select(-any_of(setdiff(colnames(opp_offense)[!str_detect(colnames(opp_offense), 'opp')], c('season', 'week')))), join_by('season','week', 'opponent_team')) %>%
  left_join(blue_chip_players_out, join_by('season','week','team')) %>% mutate(has_passing_blue_chip_out = ifelse(is.na(has_passing_blue_chip_out), FALSE, has_passing_blue_chip_out),
                                                                               has_rushing_blue_chip_out = ifelse(is.na(has_rushing_blue_chip_out), FALSE, has_rushing_blue_chip_out),
                                                                               has_receiving_blue_chip_out = ifelse(is.na(has_receiving_blue_chip_out), FALSE, has_receiving_blue_chip_out),
                                                                               has_pass_rushers_blue_chip_out = ifelse(is.na(has_pass_rushers_blue_chip_out), FALSE, has_pass_rushers_blue_chip_out),
                                                                               has_rush_tackles_blue_chip_out = ifelse(is.na(has_rush_tackles_blue_chip_out), FALSE, has_rush_tackles_blue_chip_out),
                                                                               has_secondary_blue_chip_out = ifelse(is.na(has_secondary_blue_chip_out), FALSE, has_passing_blue_chip_out)) %>%
  left_join(blue_chip_players_out %>% mutate(opp_has_passing_blue_chip_out = ifelse(is.na(has_passing_blue_chip_out), FALSE, has_passing_blue_chip_out),
                                             opp_has_rushing_blue_chip_out = ifelse(is.na(has_rushing_blue_chip_out), FALSE, has_rushing_blue_chip_out),
                                             opp_has_receiving_blue_chip_out = ifelse(is.na(has_receiving_blue_chip_out), FALSE, has_receiving_blue_chip_out),
                                             opp_has_pass_rushers_blue_chip_out = ifelse(is.na(has_pass_rushers_blue_chip_out), FALSE, has_pass_rushers_blue_chip_out),
                                             opp_has_rush_tackles_blue_chip_out = ifelse(is.na(has_rush_tackles_blue_chip_out), FALSE, has_rush_tackles_blue_chip_out),
                                             opp_has_secondary_blue_chip_out = ifelse(is.na(has_secondary_blue_chip_out), FALSE, has_secondary_blue_chip_out)), join_by('season','week','opponent_team' == 'team')) %>%
  filter(season >= (min_year + 2))  %>%
  left_join(playoff_clinching_data, join_by('season' == 'Season', 'week' == 'Week', 'team' == 'Team')) %>%
  mutate(across(where(is.numeric), ~ ifelse(is.infinite(.x), NA, .x)))



#response variable: win
moneyline_model_data = team_model_data %>% select(-team_differential, -team_attempts)

spread_model_data = team_model_data %>% select(-team_win, -team_attempts)

# saveRDS(model_data, 'model_data.rds')
# saveRDS(team_model_data, 'team_model_data.rds')

write.csv(passing_model_data, 'passing_model_data.csv')
write.csv(rushing_model_data, 'rushing_model_data.csv')
write.csv(receiving_model_data, 'receiving_model_data.csv')
write.csv(touchdown_model_data, 'touchdown_model_data.csv')
write.csv(moneyline_model_data, 'moneyline_model_data.csv')