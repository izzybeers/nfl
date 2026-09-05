library(nflreadr)
library(nflseedR)
library(dplyr)
library(jsonlite)
library(lubridate)
#library(gemini.R)
library(stringr)
library(httr)
library(purrr)
library(ggplot2)
library(tidyr)
library(scorecard)
library(httr2)
library(arrow)
library(furrr)
source('model/data_prep_functions.R')
source('data_collection/scripts/global.R')
source('data_collection/scripts/player_data.R')
source('data_collection/scripts/team_game_data.R')
source('data_collection/scripts/get_injuries.R')
source('data_collection/scripts/get_playoff_clinching_data.R')
#source('data_collection/scripts/nlp.R')
source('data_collection/scripts/blue_chip_analysis.R')


data_collection = function(mode, min_year, max_year, wk, num_iv_winners = 10, test_mode)
{
  schedules_raw = get_schedules(min_year, max_year)
  if (mode != 'predict')
  {
    wk = NULL
  }
  
  if (!is.null(wk))
  {
    mode = 'predict'
  }
  
  if (mode == 'predict' & is.null(wk))
  {
    stop("wk cannot be null when mode is predict")
  }
  
  options(dplyr.summarise.inform = FALSE)
  
  t1 = Sys.time()
  
  print('Pulling team data...')
  
  #in week 2, check on the writing to supabase part:
  team_data_all =  pull_all_team_stats(min_year, max_year, wk = wk, test_mode = test_mode, schedules_raw = schedules_raw)
  team_data_combined = team_data_all[[1]]
  opp_data_combined= team_data_all[[2]]
  team_column_categories = team_data_all[[3]]
  team_redzone_drives = team_data_all[[4]]
  
  print('Pulling player data...')
  
  player_data_all = pull_all_player_stats(min_year, max_year, team_redzone_drives = team_redzone_drives, test_mode = test_mode, wk = wk, schedules_raw = schedules_raw)
  player_data_combined = player_data_all[[1]]
  defense_data_combined = player_data_all[[2]]
  player_column_categories = player_data_all[[3]]
  
  column_categories = c(team_column_categories, player_column_categories)

  #Blue chip analysis looks at a player's stats in the current year (up until the week of the dataset's row) and two previous seasons. Only players who have been playing that long are considered.
  #3 metrics are used to assess whether a player is a blue chip:
  #1) Avg yards per game must be above a certain threshold
  #2) If they missed any games, the team's performance in that area (passing or rushing) must have been a statistically significant difference from when they played vs not played.
  #3) If they played on more than one team during this timeframe, there was no statistically significant difference in their performance on different teams.

  print('Running blue chip analysis...')
  
  if (mode != 'predict')
  {
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
  
    
    blue_chip_analysis_defense_pass_rushers = get_blue_chip_analysis(player_df = defense_data_combined %>% rename('gsis_id' = 'player_id') %>% filter(position %in% c('DE', 'DT', 'DL', 'LB', 'OLB') & defense_pct > 0.2) %>%
                                                            rename('player_metric' = 'def_pressure_score',
                                                                   'avg_player_metric' = 'avg_def_pressure_score',
                                                                   'last_season_avg_player_metric' = 'Last_Season_avg_def_pressure_score',
                                                                   'two_seasons_ago_avg_player_metric' = 'Two_Seasons_Ago_avg_def_pressure_score',
                                                                   'pct_share' = 'pct_share_of_pressures'),
                                                          team_df = opp_data_combined %>% rename('team' = 'opponent_team') %>% mutate(team_metric = opp_sacks_forced_per_attempt_allowed_current_game),
                                                          direction_play_vs_not_play = 'greater')
    
    blue_chip_analysis_defense_rush_tackles = get_blue_chip_analysis(player_df = defense_data_combined %>% rename('gsis_id' = 'player_id') %>% filter(position %in% c('DT', 'NT', 'MLB', 'ILB', 'DL', 'LB', 'DE', 'OLB') & defense_pct > 0.2)%>%
                                                                       rename('player_metric' = 'def_tackles_score',
                                                                              'avg_player_metric' = 'avg_def_tackles_score',
                                                                              'last_season_avg_player_metric' = 'Last_Season_avg_def_tackles_score',
                                                                              'two_seasons_ago_avg_player_metric' = 'Two_Seasons_Ago_avg_def_tackles_score',
                                                                              'pct_share' = 'pct_share_of_tackles'),
                                                                     team_df = opp_data_combined %>% rename('team' = 'opponent_team') %>% mutate(team_metric = opp_rushing_yards_per_carry_allowed_current_game),
                                                                     direction_play_vs_not_play = 'less')
    
    blue_chip_analysis_defense_secondary = get_blue_chip_analysis(player_df = defense_data_combined %>% rename('gsis_id' = 'player_id') %>%  filter(position %in% c('CB', 'FS', 'S', 'SAF', 'DB') & defense_pct > 0.2) %>%
                                                                       rename('player_metric' = 'def_pass_defend_and_int_score',
                                                                              'avg_player_metric' = 'avg_def_pass_defend_and_int_score',
                                                                              'last_season_avg_player_metric' = 'Last_Season_avg_def_pass_defend_and_int_score',
                                                                              'two_seasons_ago_avg_player_metric' = 'Two_Seasons_Ago_avg_def_pass_defend_and_int_score',
                                                                              'pct_share' = 'pct_share_of_pass_defense_and_int'),
                                                                     team_df = opp_data_combined %>% rename('team' = 'opponent_team') %>% mutate(team_metric = opp_passing_yards_per_attempt_allowed_current_game),
                                                                  direction_play_vs_not_play = 'less') 
  } else {
    #quarterbacks
    #offense_pct > 0.5 means we are only considering games where the QB played more than half the snaps in the game. Any less will be considered a game they did not play for purposes of the analysis.
    blue_chip_analysis_passing = get_blue_chip_analysis(player_df = player_data_combined %>% mutate(offense_cutoff = ifelse(week == 1, Last_Season_avg_offense_pct, avg_offense_pct)) %>% filter(position_group == 'QB' & offense_cutoff > 0.5) %>% 
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
    blue_chip_analysis_rushing = get_blue_chip_analysis(player_df = player_data_combined %>% mutate(offense_cutoff = ifelse(week == 1, Last_Season_avg_offense_pct, avg_offense_pct),
                                                                                                    pct_share = ifelse(week == 1, Last_Season_pct_share_of_rushing_yards, pct_share_of_rushing_yards)) %>%
                                                          filter(position_group == 'RB' & offense_cutoff > 0.2) %>% 
                                                          select(gsis_id, display_name, season, team, week, rushing_yards, avg_rushing_yards, Last_Season_avg_rushing_yards, Two_Seasons_Ago_avg_rushing_yards, Last_Season_weeks_active, Two_Seasons_Ago_weeks_active,
                                                                 pct_share_of_carries, pct_share) %>%
                                                          rename('player_metric' = 'rushing_yards',
                                                                 'avg_player_metric' = 'avg_rushing_yards',
                                                                 'last_season_avg_player_metric' = 'Last_Season_avg_rushing_yards',
                                                                 'two_seasons_ago_avg_player_metric' = 'Two_Seasons_Ago_avg_rushing_yards'),
                                                        team_df = team_data_combined %>% select(team, season, week, team_rushing_yards) %>%
                                                          rename('team_metric' = 'team_rushing_yards'),
                                                        direction_play_vs_not_play = 'greater')
    
    #receiving
    #offense_pct > 0.2 means we are only considering games where the player played more than 20% of snaps. We are also only considering wide receivers and tight ends.
    blue_chip_analysis_receiving = get_blue_chip_analysis(player_df = player_data_combined %>% mutate(offense_cutoff = ifelse(week == 1, Last_Season_avg_offense_pct, avg_offense_pct),
                                                                                                      pct_share = ifelse(week == 1, Last_Season_pct_share_of_intended_air_yards, pct_share_of_intended_air_yards)) %>% filter(position_group %in% c('WR', 'TE') & offense_cutoff > 0.2) %>% 
                                                            select(gsis_id, display_name, season, team, week, receiving_yards, avg_receiving_yards, Last_Season_avg_receiving_yards, Two_Seasons_Ago_avg_receiving_yards, Last_Season_weeks_active, Two_Seasons_Ago_weeks_active,
                                                                   pct_share_of_targets, pct_share) %>%
                                                            rename('player_metric' = 'receiving_yards',
                                                                   'avg_player_metric' = 'avg_receiving_yards',
                                                                   'last_season_avg_player_metric' = 'Last_Season_avg_receiving_yards',
                                                                   'two_seasons_ago_avg_player_metric' = 'Two_Seasons_Ago_avg_receiving_yards'),
                                                          team_df = team_data_combined %>% select(team, season, week, team_passing_yards) %>%
                                                            rename('team_metric' = 'team_passing_yards'),
                                                          direction_play_vs_not_play = 'greater')
  
    
    blue_chip_analysis_defense_pass_rushers = get_blue_chip_analysis(player_df = defense_data_combined %>% mutate(defense_cutoff = ifelse(week == 1, Last_Season_avg_defense_pct, avg_defense_pct),
                                                                                                                  pct_share = ifelse(week == 1, Last_Season_pct_share_of_pressures, pct_share_of_pressures)) %>% rename('gsis_id' = 'player_id') %>% filter(position %in% c('DE', 'DT', 'DL', 'LB', 'OLB') & defense_cutoff > 0.2) %>%
                                                                       rename('player_metric' = 'def_pressure_score',
                                                                              'avg_player_metric' = 'avg_def_pressure_score',
                                                                              'last_season_avg_player_metric' = 'Last_Season_avg_def_pressure_score',
                                                                              'two_seasons_ago_avg_player_metric' = 'Two_Seasons_Ago_avg_def_pressure_score'),
                                                                     team_df = opp_data_combined %>% rename('team' = 'opponent_team') %>% mutate(team_metric = opp_sacks_forced_per_attempt_allowed_current_game),
                                                                     direction_play_vs_not_play = 'greater')
    
    blue_chip_analysis_defense_rush_tackles = get_blue_chip_analysis(player_df = defense_data_combined %>% mutate(defense_cutoff = ifelse(week == 1, Last_Season_avg_defense_pct, avg_defense_pct),
                                                                                                                  pct_share = ifelse(week == 1, Last_Season_pct_share_of_tackles, pct_share_of_tackles)) %>%
                                                                       rename('gsis_id' = 'player_id') %>% filter(position %in% c('DT', 'NT', 'MLB', 'ILB', 'DL', 'LB', 'DE', 'OLB') & defense_cutoff > 0.2) %>%   
                                                                       rename('player_metric' = 'def_tackles_score',
                                                                              'avg_player_metric' = 'avg_def_tackles_score',
                                                                              'last_season_avg_player_metric' = 'Last_Season_avg_def_tackles_score',
                                                                              'two_seasons_ago_avg_player_metric' = 'Two_Seasons_Ago_avg_def_tackles_score'),
                                                                     team_df = opp_data_combined %>% rename('team' = 'opponent_team') %>% mutate(team_metric = opp_rushing_yards_per_carry_allowed_current_game),
                                                                     direction_play_vs_not_play = 'less')
    
    blue_chip_analysis_defense_secondary = get_blue_chip_analysis(player_df = defense_data_combined %>% mutate(defense_cutoff = ifelse(week == 1, Last_Season_avg_defense_pct, avg_defense_pct),
                                                                                                               pct_share = ifelse(week == 1, Last_Season_pct_share_of_pass_defense_and_int, pct_share_of_pass_defense_and_int)) %>%
                                                                    rename('gsis_id' = 'player_id') %>% filter(position %in% c('CB', 'FS', 'S', 'SAF', 'DB') & defense_cutoff > 0.2) %>%
                                                                    rename('player_metric' = 'def_pass_defend_and_int_score',
                                                                           'avg_player_metric' = 'avg_def_pass_defend_and_int_score',
                                                                           'last_season_avg_player_metric' = 'Last_Season_avg_def_pass_defend_and_int_score',
                                                                           'two_seasons_ago_avg_player_metric' = 'Two_Seasons_Ago_avg_def_pass_defend_and_int_score'),
                                                                  team_df = opp_data_combined %>% rename('team' = 'opponent_team') %>% mutate(team_metric = opp_passing_yards_per_attempt_allowed_current_game),
                                                                  direction_play_vs_not_play = 'less') 
  }
  
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
    )) %>% select(gsis_id, week, season, team, secondary_blue_chip), join_by('gsis_id', 'week', 'season', 'team')) %>%
    filter(season >= (min_year+2))  %>% arrange(season, gsis_id, week) %>%
    mutate(across(ends_with("_blue_chip"), ~ coalesce(.x, FALSE)))
  
  if (mode == 'predict' & !test_mode)
  {
    upsert_to_supabase('MainData', 'BlueChip', blue_chip_analysis_df %>% filter(season == max_year & week == wk) %>% mutate(updated_at = Sys.time()), c('gsis_id','season','week'))
  }
  
  if (mode != 'predict')
  {
    #playoff_clinching_data = get_playoff_clinching_data(min_year+2, max_year, wk = NULL, predict_mode = FALSE)
    playoff_clinching_data = get_supabase_data(schema = 'MainData', table_name = 'PlayoffClinching')
    column_categories[['playoff_clinching']] = setdiff(colnames(playoff_clinching_data), c('Season','Week','Team'))
  } else if (!is.null(wk)) {
    playoff_clinching_data = get_playoff_clinching_data(max_year, max_year, wk = wk, predict_mode = TRUE)
    if (!(test_mode) & sum(!is.na(playoff_clinching_data$Div_Ranking)) > 0)
    {
      write_to_supabase('MainData', 'PlayoffClinching', playoff_clinching_data %>% filter(!is.na(Team)))
    }
  }

  print('Pulling injuries data and applying to blue chip analysis...')
  if (mode == 'predict')
  {
    injuries = tryCatch(
      get_injuries_data(mode == 'predict', max_year, max_year, wk = wk, testmode = test_mode),
      error = function(e) tibble(
        season = numeric(),
        week = numeric(),
        gsis_id = character(),
        team = character(),
        less_practice = logical(),
        illness = logical(),
        out_not_injury_related = logical(),
        out = logical()
      )
    )
  } else {
    injuries = get_injuries_data(mode == 'predict', min_year, max_year, testmode = test_mode)
  }
  blue_chip_players_out = combine_injuries_with_blue_chip(blue_chip_analysis_df, injuries)
  
  if(mode != 'predict')
  {
    column_categories[['injuries']] = setdiff(colnames(injuries),c('season','week','gsis_id','full_name','team'))
    column_categories[['blue_chip']] = c('passing_blue_chip', 'rushing_blue_chip', 'receiving_blue_chip', 'pass_rushers_blue_chip', 'rush_tackles_blue_chip', 'secondary_blue_chip',
                                         'has_passing_blue_chip_out', 'has_rushing_blue_chip_out', 'has_receiving_blue_chip_out', 'has_pass_rushers_blue_chip_out', 'has_rush_tackles_blue_chip_out', 'has_secondary_blue_chip_out')
  }
  
  print('Joining tables together...')
  
  if (mode == 'predict')
  {
    starting_qbs = player_data_combined %>% filter(position == 'QB', depth_rank == 1, week == wk, season == max_year) %>% select(gsis_id, season, week, team) %>% rename(team_qb_id_2 = gsis_id) %>% distinct()
    team_data_combined = team_data_combined %>% left_join(starting_qbs, join_by('season','week','team')) %>%
      mutate(team_qb_id = coalesce(team_qb_id, team_qb_id_2)) %>% select(-team_qb_id_2)
  }
  
  
  model_data = player_data_combined %>%
    inner_join(team_data_combined %>% select(-game_id), join_by('season', 'week', 'team')) %>%
    inner_join(opp_data_combined, join_by('season', 'week', 'opponent_team')) %>%
    left_join(injuries %>% select(-team), join_by('season', 'week', 'gsis_id')) %>%
    left_join(blue_chip_players_out, join_by('season','week','team')) %>% mutate(has_passing_blue_chip_out = ifelse(is.na(has_passing_blue_chip_out), FALSE, has_passing_blue_chip_out),
                                                                                 has_rushing_blue_chip_out = ifelse(is.na(has_rushing_blue_chip_out), FALSE, has_rushing_blue_chip_out),
                                                                                 has_receiving_blue_chip_out = ifelse(is.na(has_receiving_blue_chip_out), FALSE, has_receiving_blue_chip_out),
                                                                                 has_pass_rushers_blue_chip_out = ifelse(is.na(has_pass_rushers_blue_chip_out), FALSE, has_pass_rushers_blue_chip_out),
                                                                                 has_rush_tackles_blue_chip_out = ifelse(is.na(has_rush_tackles_blue_chip_out), FALSE, has_rush_tackles_blue_chip_out),
                                                                                 has_secondary_blue_chip_out = ifelse(is.na(has_secondary_blue_chip_out), FALSE, has_secondary_blue_chip_out)) %>%
    left_join(blue_chip_analysis_df, join_by('season','week','team','gsis_id')) %>%
    left_join(playoff_clinching_data, join_by('season' == 'Season', 'week' == 'Week', 'team' == 'Team')) %>%
    filter(season >= (min_year+2)) %>%
    mutate(game_on_birthday = substring(gameday,6,10) == substring(birth_date,6,10),
           age = as.numeric(difftime(gameday, birth_date))/365) %>% select(-birth_date) %>%
    mutate(across(where(is.numeric), ~ ifelse(is.infinite(.x), NA, .x)),
           out = coalesce(out, FALSE),
           out_not_injury_related = coalesce(out_not_injury_related, FALSE),
           illness = coalesce(illness, FALSE),
           less_practice = coalesce(less_practice, FALSE),
           passing_blue_chip = coalesce(passing_blue_chip, FALSE),
           rushing_blue_chip = coalesce(rushing_blue_chip, FALSE),
           receiving_blue_chip = coalesce(receiving_blue_chip, FALSE),
           pass_rushers_blue_chip = coalesce(pass_rushers_blue_chip, FALSE),
           rush_tackles_blue_chip = coalesce(rush_tackles_blue_chip, FALSE),
           secondary_blue_chip = coalesce(secondary_blue_chip, FALSE)) %>%
    select(-team_win, -team_differential) #this field is for the team models only
  #left_join(video_llm_results[[1]], join_by('season', 'week', 'team')) %>%
  #left_join(video_llm_results[[2]], join_by('season', 'week', 'gsis_id'))
  
  if (mode == 'predict')
  {
    if (test_mode)
    {
      data_with_weather = get_weather(model_data %>% select(-wind, -temp) %>% filter(season == max_year, week == wk) %>% mutate(actual_gameday = gameday, gameday = Sys.Date() + 1), team_lookup_table) %>%
        select(-gameday) %>% rename('gameday' = 'actual_gameday')
    } else if (mode == 'predict'){
      data_with_weather = get_weather(model_data %>% select(-wind, -temp) %>% filter(season == max_year, week == wk), team_lookup_table)
    }
    model_data = model_data %>% filter(!(season == max_year & week == wk)) %>%
      bind_rows(data_with_weather %>%
                  inner_join(team_lookup_table %>% select(NFLReadr_Team_Abbr, Used_To_Cold, Used_To_Hot, Coast), join_by('team' == 'NFLReadr_Team_Abbr')) %>%
                  inner_join(team_lookup_table %>% select(NFLReadr_Team_Abbr, Used_To_Cold, Used_To_Hot, Coast), join_by('opponent_team' == 'NFLReadr_Team_Abbr')) %>%
                  mutate(familiar_temperature = !((temp < 40 & !Used_To_Cold.x) | (temp > 80 & !Used_To_Hot.x)) | is.na(temp),
                         opp_familiar_temperature = !((temp < 40 & !Used_To_Cold.y) | (temp > 80 & !Used_To_Hot.y)) | is.na(temp)) %>%
                  select(-matches('Used_To|Coast'))
      )
  }

  column_categories[['game_info']] = c(column_categories[['game_info']], 'game_on_birthday')
  column_categories[['bio_data']] = c(column_categories[['bio_data']], 'age')
  
  #column_categories[['youtube_llm']] = setdiff(colnames(video_llm_results), c('team','season','week'))
  
  if (mode == "predict") {
    model_data = model_data %>%
      mutate(across(any_of(c("passing_yards", "rushing_yards", "receiving_yards", "receptions", "anytime_td_scorer")),
        ~ if_else(season == max_year & week == wk, NA_real_, as.numeric(.x))
      ))
  }
  
  passing_model_data = model_data %>% filter(position == 'QB') %>% select(-rushing_yards, -receiving_yards, -anytime_td_scorer, -receptions, -team_attempts) %>% select(-any_of(c(column_categories[['rushing_current_season_stats']], column_categories[['receiving_current_season_stats']],
                                                                                                                                                                                  column_categories[['rushing_past_season_stats']], column_categories[['receiving_past_season_stats']])))
  
  rushing_model_data = model_data %>% filter(position %in% c('QB','RB')) %>% select(-passing_yards, -receiving_yards, -anytime_td_scorer, -receptions, -team_attempts) %>% select(-any_of(c(column_categories[['passing_current_season_stats']], column_categories[['receiving_current_season_stats']],
                                                                                                                                                                                            column_categories[['passing_past_season_stats']], column_categories[['receiving_past_season_stats']])))
  
  receiving_model_data = model_data %>%
    filter(position %in% c('RB','WR','TE')) %>%
    select(-passing_yards, -rushing_yards, -anytime_td_scorer, -receptions) %>%
    select(-any_of(c(column_categories[['passing_current_season_stats']], column_categories[['rushing_current_season_stats']],
                     column_categories[['passing_past_season_stats']], column_categories[['rushing_past_season_stats']])))
  
  touchdown_model_data = model_data %>%
    filter(position %in% c('QB', 'RB', 'WR', 'TE')) %>%
    select(-passing_yards, -rushing_yards, -receiving_yards, -receptions)
    # select(-any_of(c(column_categories[['passing_current_season_stats']], column_categories[['passing_past_season_stats']])))
  
  reception_model_data = model_data %>%
    filter(position %in% c('RB', 'WR', 'TE')) %>%
    select(-passing_yards, -rushing_yards, -receiving_yards, -anytime_td_scorer)  %>%
    select(-any_of(c(column_categories[['passing_current_season_stats']], column_categories[['rushing_current_season_stats']],
                     column_categories[['passing_past_season_stats']], column_categories[['rushing_past_season_stats']])))
  
  rushing_receiving_model_data = model_data %>% mutate(rushing_receiving_yards = if (mode == 'predict') { NA_real_ } else { coalesce(rushing_yards, 0) + coalesce(receiving_yards, 0) }) %>%
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
  reception_model_data = reception_model_data %>% left_join(qb_summaries,join_by('team_qb_id' == 'gsis_id','season','week','team'))
  rushing_receiving_model_data = rushing_receiving_model_data %>% left_join(qb_summaries,join_by('team_qb_id' == 'gsis_id','season','week','team'))
  touchdown_model_data = touchdown_model_data %>% left_join(qb_summaries,join_by('team_qb_id' == 'gsis_id','season','week','team'))
  
  #PROMOTION/DEMOTION ANALYSIS:
  
  pass_attempts_high_lower_bound = quantile(passing_model_data$avg_attempts, 0.66, na.rm=TRUE) #comes out to 35
  pass_attempts_medium_lower_bound = quantile(passing_model_data$avg_attempts, 0.33, na.rm=TRUE) #comes out to 25
  rush_attempts_high_lower_bound = quantile(rushing_model_data$avg_team_carries, 0.66, na.rm=TRUE)
  rush_attempts_medium_lower_bound = quantile(rushing_model_data$avg_team_carries, 0.33, na.rm=TRUE)
  
  get_receiving_rookie_estimates = function(df)
  {
    coach_games = df %>%
      select(team_coach, team, season, week, team_attempts) %>%
      group_by(team_coach, team, season, week) %>%
      slice(1) %>%
      ungroup() %>%
      group_by(team_coach) %>%
      arrange(season, week) %>%
      mutate(games_coached = row_number() - 1) %>%
      group_by(team_coach, season) %>%
      mutate(games_coached_this_season = row_number() - 1) %>%
      ungroup() %>%
      group_by(team_coach) %>%
      mutate(
        avg_team_pass_attempts_this_coach_past_30_games = case_when(
          games_coached > games_coached_this_season ~
            slide_dbl(
              team_attempts,
              ~ ifelse(length(.x) == 0, NA_real_, mean(.x, na.rm = TRUE)),
              .before = 30,
              .after = -1,
              .complete = FALSE
            ),
          .default = NA_real_
        )
      ) %>%
      ungroup() %>%
      select(
        team_coach,
        team,
        season,
        week,
        avg_team_pass_attempts_this_coach_past_30_games
      )
    
    return(
      df %>% left_join(coach_games, join_by(team_coach, team, season, week) ) %>%
        mutate(pass_attempt_volume = case_when(!is.na(avg_attempts_this_qb) ~ avg_attempts_this_qb,
                                               !is.na(avg_team_pass_attempts_this_coach_past_30_games) ~ avg_team_pass_attempts_this_coach_past_30_games,
                                               .default = Last_Season_avg_team_attempts)) %>%
        mutate(pass_attempt_volume_category = case_when(pass_attempt_volume > pass_attempts_high_lower_bound ~ 'High',
                                                        pass_attempt_volume > pass_attempts_medium_lower_bound ~ 'Medium',
                                                        .default = 'Low')) %>%
        select(-avg_team_pass_attempts_this_coach_past_30_games)
    )
  }
  
  get_rushing_rookie_estimates = function(df)
  {
    coach_games = df %>%
      select(team_coach, team, season, week, avg_team_carries, games_played_this_season) %>%
      group_by(team_coach, team, season, week) %>%
      slice(1) %>% ungroup() %>% group_by(team_coach) %>% arrange(season, week) %>%
      mutate(games_coached = row_number() - 1) %>%
      group_by(team_coach, season) %>% mutate(games_coached_this_season = row_number() - 1) %>% ungroup() %>%
      group_by(team_coach) %>%
      mutate(
        avg_team_rush_attempts_this_coach_past_30_games = case_when(
          games_coached > games_coached_this_season ~
            slide_dbl(avg_team_carries * games_played_this_season, ~ ifelse(length(.x) == 0, NA_real_, mean(.x, na.rm = TRUE)),
              .before = 30, .after = -1, .complete = FALSE),
          .default = NA_real_)) %>%
      ungroup() %>%
      select(team_coach, team, season, week, avg_team_rush_attempts_this_coach_past_30_games)
    
    return(
      df %>%
        left_join(
          coach_games,
          join_by(team_coach, team, season, week)
        ) %>%
        mutate(
          rush_attempt_volume = case_when(
            !is.na(avg_team_rush_attempts_this_coach_past_30_games) ~
              avg_team_rush_attempts_this_coach_past_30_games,
            .default = Last_Season_avg_team_carries
          ),
          rush_attempt_volume_category = case_when(
            rush_attempt_volume > rush_attempts_high_lower_bound ~ 'High',
            rush_attempt_volume > rush_attempts_medium_lower_bound ~ 'Medium',
            .default = 'Low'
          )
        ) %>%
        select(-avg_team_rush_attempts_this_coach_past_30_games)
    )
  }
  
  print('Running promotion demotion analysis...')
  
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
      games_played_this_season > 0 & games_played > games_played_this_season ~ (games_played_this_season/6)*pct_share_of_targets + (1-games_played_this_season/6)*coalesce(Last_Season_pct_share_of_targets,Two_Seasons_Ago_pct_share_of_targets, avg_target_share_rookie_group),
      games_played > games_played_this_season ~ coalesce(Last_Season_pct_share_of_targets,Two_Seasons_Ago_pct_share_of_targets, avg_target_share_rookie_group),
      #rookies:
      games_played_this_season > 0 ~  (games_played_this_season/6)*pct_share_of_targets + (1-games_played_this_season/6)*avg_target_share_rookie_group,
      .default = avg_target_share_rookie_group
    )) %>% select(-avg_target_share_rookie_group, -pass_attempt_volume_category) %>%
    ungroup() %>% 
    filter(is.na(out) | !out) %>% 
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
      games_played_this_season > 0 & games_played > games_played_this_season ~ (games_played_this_season/6)*pct_share_of_targets + (1-games_played_this_season/6)*coalesce(Last_Season_pct_share_of_targets, Two_Seasons_Ago_pct_share_of_targets, avg_target_share_rookie_group),
      games_played > games_played_this_season ~ coalesce(Last_Season_pct_share_of_targets, Two_Seasons_Ago_pct_share_of_targets, avg_target_share_rookie_group),
      #rookies:
      games_played_this_season > 0 ~  (games_played_this_season/6)*pct_share_of_targets + (1-games_played_this_season/6)*avg_target_share_rookie_group,
      .default = avg_target_share_rookie_group
    )) %>% select(-avg_target_share_rookie_group, -pass_attempt_volume_category) %>%
    ungroup() %>% 
    filter(is.na(out) | !out) %>%
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
      games_played_this_season > 0 & games_played > games_played_this_season ~ (games_played_this_season/6)*pct_share_of_carries + (1-games_played_this_season/6)*coalesce(Last_Season_pct_share_of_carries,Two_Seasons_Ago_pct_share_of_carries, avg_rush_share_rookie_group),
      games_played > games_played_this_season ~ coalesce(Last_Season_pct_share_of_carries,Two_Seasons_Ago_pct_share_of_carries,avg_rush_share_rookie_group),
      #rookies:
      games_played_this_season > 0 ~  (games_played_this_season/6)*pct_share_of_carries + (1-games_played_this_season/6)*avg_rush_share_rookie_group,
      .default = avg_rush_share_rookie_group
    )) %>% select(-avg_rush_share_rookie_group, -rush_attempt_volume_category) %>%
    ungroup() %>% 
    filter(is.na(out) | !out) %>%
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
    mutate(rush_rookie_fallback = ifelse(position %in% c('QB', 'RB'), coalesce(avg_rush_share_rookie_group, 0), 0),
           receiving_rookie_fallback = ifelse(position %in% c('RB', 'WR', 'TE'), coalesce(avg_target_share_rookie_group, 0), 0)) %>%
    group_by(gsis_id) %>%
    mutate(true_talent_baseline_rushing = case_when(
      games_played_this_season > 5 ~ pct_share_of_carries,
      games_played_this_season > 0 & games_played > games_played_this_season ~ (games_played_this_season/6)*pct_share_of_carries + (1-games_played_this_season/6)*coalesce(Last_Season_pct_share_of_carries, Two_Seasons_Ago_pct_share_of_carries, rush_rookie_fallback),
      games_played > games_played_this_season ~ coalesce(Last_Season_pct_share_of_carries, Two_Seasons_Ago_pct_share_of_carries, rush_rookie_fallback),
      # rookies
      games_played_this_season > 0 ~ (games_played_this_season/6)*pct_share_of_carries + (1-games_played_this_season/6)*rush_rookie_fallback,
      .default = rush_rookie_fallback
    ),
    true_talent_baseline_receiving = case_when(
      games_played_this_season > 5 ~ pct_share_of_targets,
      games_played_this_season > 0 & games_played > games_played_this_season ~
        (games_played_this_season/6)*pct_share_of_targets + (1-games_played_this_season/6)*coalesce(Last_Season_pct_share_of_targets, Two_Seasons_Ago_pct_share_of_targets, receiving_rookie_fallback),
      games_played > games_played_this_season ~ coalesce(Last_Season_pct_share_of_targets, Two_Seasons_Ago_pct_share_of_targets, receiving_rookie_fallback),
      # rookies
      games_played_this_season > 0 ~ (games_played_this_season/6)*pct_share_of_targets + (1-games_played_this_season/6)*receiving_rookie_fallback,
      .default = receiving_rookie_fallback
    )) %>% select(-avg_rush_share_rookie_group, -rush_attempt_volume_category, -avg_target_share_rookie_group, -pass_attempt_volume_category, -rush_rookie_fallback, -receiving_rookie_fallback) %>%
    ungroup() %>% 
    filter(is.na(out) | !out) %>%
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
    mutate(rush_rookie_fallback = ifelse(position %in% c('QB', 'RB'), coalesce(avg_rush_share_rookie_group, 0), 0),
           receiving_rookie_fallback = ifelse(position %in% c('RB', 'WR', 'TE'), coalesce(avg_target_share_rookie_group, 0), 0)) %>%
    group_by(gsis_id) %>%
    mutate(true_talent_baseline_rushing = case_when(
      games_played_this_season > 5 ~ pct_share_of_carries,
      games_played_this_season > 0 & games_played > games_played_this_season ~ (games_played_this_season/6)*pct_share_of_carries + (1-games_played_this_season/6)*coalesce(Last_Season_pct_share_of_carries, Two_Seasons_Ago_pct_share_of_carries, rush_rookie_fallback),
      games_played > games_played_this_season ~ coalesce(Last_Season_pct_share_of_carries, Two_Seasons_Ago_pct_share_of_carries, rush_rookie_fallback),
      # rookies
      games_played_this_season > 0 ~ (games_played_this_season/6)*pct_share_of_carries + (1-games_played_this_season/6)*rush_rookie_fallback,
      .default = rush_rookie_fallback
    ),
    true_talent_baseline_receiving = case_when(
      games_played_this_season > 5 ~ pct_share_of_targets,
      games_played_this_season > 0 & games_played > games_played_this_season ~
        (games_played_this_season/6)*pct_share_of_targets + (1-games_played_this_season/6)*coalesce(Last_Season_pct_share_of_targets, Two_Seasons_Ago_pct_share_of_targets, receiving_rookie_fallback),
      games_played > games_played_this_season ~
        coalesce(Last_Season_pct_share_of_targets, Two_Seasons_Ago_pct_share_of_targets, receiving_rookie_fallback),
      # rookies
      games_played_this_season > 0 ~ (games_played_this_season/6)*pct_share_of_targets + (1-games_played_this_season/6)*receiving_rookie_fallback,
      .default = receiving_rookie_fallback
    )) %>% select(-avg_rush_share_rookie_group, -rush_attempt_volume_category, -avg_target_share_rookie_group, -pass_attempt_volume_category, -rush_rookie_fallback, -receiving_rookie_fallback) %>%
    ungroup() %>% 
    filter(is.na(out) | !out) %>%
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
  
  
  
  print('Finalizing datasets...')
  
  if (mode != 'predict')
  {
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
    
  }

  team_offense = team_data_combined
  opp_defense = opp_data_combined
  team_defense = opp_data_combined %>% ungroup() %>% select(-opp_short_week, -opp_long_week) %>% rename_with(~str_replace(.x, 'opp_defense', 'team_defense')) %>% rename('team' = 'opponent_team') 
  opp_offense = team_data_combined %>% rename_with(.fn = ~gsub('team', 'opp_offense', .x), .cols = contains('team_')) %>% rename('opp_coach_previous_weeks_with_team' = 'coach_previous_weeks_with_team') %>% select(-opponent_team) %>% rename('opponent_team' = 'team')
  opp_offense = opp_offense %>% select(-any_of(setdiff(colnames(opp_offense)[!(str_detect(colnames(opp_offense), 'opp_offense|opp_defense'))], c('season','week','opponent_team', 'opp_coach_previous_weeks_with_team'))))
  
  team_stats = team_offense %>% inner_join(team_defense, join_by('season','week', 'team'))
  opp_stats = opp_offense %>% inner_join(opp_defense, join_by('season', 'week', 'opponent_team'))
  
  team_model_data = team_stats %>% inner_join(opp_stats %>% select(-any_of(setdiff(colnames(opp_offense)[!str_detect(colnames(opp_offense), 'opp')], c('season', 'week')))), join_by('season','week', 'opponent_team')) %>%
    left_join(blue_chip_players_out, join_by('season','week','team')) %>% mutate(has_passing_blue_chip_out = ifelse(is.na(has_passing_blue_chip_out), FALSE, has_passing_blue_chip_out),
                                                                                 has_rushing_blue_chip_out = ifelse(is.na(has_rushing_blue_chip_out), FALSE, has_rushing_blue_chip_out),
                                                                                 has_receiving_blue_chip_out = ifelse(is.na(has_receiving_blue_chip_out), FALSE, has_receiving_blue_chip_out),
                                                                                 has_pass_rushers_blue_chip_out = ifelse(is.na(has_pass_rushers_blue_chip_out), FALSE, has_pass_rushers_blue_chip_out),
                                                                                 has_rush_tackles_blue_chip_out = ifelse(is.na(has_rush_tackles_blue_chip_out), FALSE, has_rush_tackles_blue_chip_out),
                                                                                 has_secondary_blue_chip_out = ifelse(is.na(has_secondary_blue_chip_out), FALSE, has_secondary_blue_chip_out)) %>%
    left_join(blue_chip_players_out %>% mutate(opp_has_passing_blue_chip_out = ifelse(is.na(has_passing_blue_chip_out), FALSE, has_passing_blue_chip_out),
                                               opp_has_rushing_blue_chip_out = ifelse(is.na(has_rushing_blue_chip_out), FALSE, has_rushing_blue_chip_out),
                                               opp_has_receiving_blue_chip_out = ifelse(is.na(has_receiving_blue_chip_out), FALSE, has_receiving_blue_chip_out),
                                               opp_has_pass_rushers_blue_chip_out = ifelse(is.na(has_pass_rushers_blue_chip_out), FALSE, has_pass_rushers_blue_chip_out),
                                               opp_has_rush_tackles_blue_chip_out = ifelse(is.na(has_rush_tackles_blue_chip_out), FALSE, has_rush_tackles_blue_chip_out),
                                               opp_has_secondary_blue_chip_out = ifelse(is.na(has_secondary_blue_chip_out), FALSE, has_secondary_blue_chip_out)) %>%
                select(-has_passing_blue_chip_out, -has_rushing_blue_chip_out, -has_receiving_blue_chip_out, -has_pass_rushers_blue_chip_out, -has_rush_tackles_blue_chip_out, -has_secondary_blue_chip_out), join_by('season','week','opponent_team' == 'team')) %>% 
    mutate(across(contains('blue_chip_out'), ~ coalesce(.x, FALSE))) %>%
    filter(season >= (min_year + 2) )  %>%
    left_join(playoff_clinching_data, join_by('season' == 'Season', 'week' == 'Week', 'team' == 'Team')) %>%
    mutate(across(where(is.numeric), ~ ifelse(is.infinite(.x), NA, .x)))
  
  if (mode != 'predict')
  {
    column_categories[['team_current_season_stats']] = c(column_categories[['team_current_season_stats']],
                                                         setdiff(colnames(team_defense)[!str_detect(colnames(team_defense), 'Last_Season|Two_Seasons')],
                                                                 c('season', 'team', 'week')))
    column_categories[['team_historical_season_stats']] = c(column_categories[['team_historical_season_stats']],
                                                            colnames(team_defense)[str_detect(colnames(team_defense), 'Last_Season|Two_Seasons')])
    column_categories[['opp_current_season_stats']] = c(column_categories[['opp_current_season_stats']], 
                                                        setdiff(colnames(opp_offense)[!str_detect(colnames(opp_offense), 'Last_Season|Two_Seasons')],
                                                                c('season', 'opponent_team', 'week', 'opp_offense_coach', 'opp_offense_qb_id')))
    column_categories[['opp_historical_season_stats']] = c(column_categories[['opp_historical_season_stats']], colnames(opp_offense)[str_detect(colnames(opp_offense), 'Last_Season|Two_Seasons')])
    
    column_categories[['blue_chip']] = c(column_categories[['blue_chip']],
                                         colnames(team_model_data)[str_detect(colnames(team_model_data), 'blue_chip') & str_detect(colnames(team_model_data), 'opp')])
  }
  if (mode == 'predict')
  {
    if (test_mode)
    {
      team_data_with_weather = get_weather(team_model_data %>% select(-wind, -temp) %>% filter(season == max_year & week == wk) %>% mutate(actual_gameday = gameday, gameday = Sys.Date() + 1), team_lookup_table) %>%
        select(-gameday) %>% rename('gameday' = 'actual_gameday')
    } else {
      team_data_with_weather = get_weather(team_model_data %>% select(-wind, -temp) %>% filter(season == max_year & week == wk), team_lookup_table)
    }
    team_model_data = team_model_data %>% filter(!(season == max_year & week == wk)) %>% bind_rows(team_data_with_weather %>%
                                                                                                     inner_join(team_lookup_table %>% select(NFLReadr_Team_Abbr, Used_To_Cold, Used_To_Hot, Coast), join_by('team' == 'NFLReadr_Team_Abbr')) %>%
                                                                                                     inner_join(team_lookup_table %>% select(NFLReadr_Team_Abbr, Used_To_Cold, Used_To_Hot, Coast), join_by('opponent_team' == 'NFLReadr_Team_Abbr')) %>%
                                                                                                     mutate(familiar_temperature = !((temp < 40 & !Used_To_Cold.x) | (temp > 80 & !Used_To_Hot.x)) | is.na(temp),
                                                                                                            opp_familiar_temperature = !((temp < 40 & !Used_To_Cold.y) | (temp > 80 & !Used_To_Hot.y)) | is.na(temp)) %>%
                                                                                                     select(-matches('Used_To|Coast')))
  }
  
  
  #response variable: win
  moneyline_model_data = team_model_data %>% select(-team_differential, -team_attempts, -opp_offense_attempts, -opp_offense_qb_id, -opp_offense_coach, -team_coach, -team_qb_id)
  
  spread_model_data = team_model_data %>% select(-team_win, -team_attempts, -opp_offense_attempts, -opp_offense_qb_id, -opp_offense_coach, -team_coach, -team_qb_id)
  
  if (test_mode)
  {
    write.csv(passing_model_data, 'validation_files/passing.csv', row.names = FALSE)
    write.csv(rushing_model_data, 'validation_files/rushing.csv', row.names = FALSE)
    write.csv(receiving_model_data, 'validation_files/receiving.csv', row.names = FALSE)
    write.csv(touchdown_model_data, 'validation_files/touchdown.csv', row.names = FALSE)
    write.csv(rushing_receiving_model_data, 'validation_files/rushrec.csv', row.names = FALSE)
    write.csv(reception_model_data, 'validation_files/receptions.csv', row.names = FALSE)
    write.csv(moneyline_model_data, 'validation_files/moneyline.csv', row.names = FALSE)
    write.csv(spread_model_data, 'validation_files/spread.csv', row.names = FALSE)
  }
  
  if (mode == 'predict')
  {
    passing_model_data = passing_model_data %>% filter(season == max_year & week == wk)
    rushing_model_data = rushing_model_data %>% filter(season == max_year & week == wk)
    receiving_model_data = receiving_model_data %>% filter(season == max_year & week == wk)
    touchdown_model_data = touchdown_model_data %>% filter(season == max_year & week == wk)
    rushing_receiving_model_data = rushing_receiving_model_data %>% filter(season == max_year & week == wk)
    reception_model_data = reception_model_data %>% filter(season == max_year & week == wk)
    moneyline_model_data = moneyline_model_data %>% filter(season == max_year & week == wk)
    spread_model_data = spread_model_data %>% filter(season == max_year & week == wk)
    
      # if (test_mode)
      # {
      #   dir_name = paste0("data_collection/testing_checkpoint_data/", max_year,'week',wk,'/')
      #   dir.create(dir_name, recursive = TRUE, showWarnings = FALSE)
      #   saveRDS(blue_chip_analysis_df, paste0(dir_name, "/testmode_blue_chip_analysis_df_checkpoint.rds"))
      # } else {
      #   dir_name = paste0("data_collection/checkpoint_data/", max_year,'week',wk,'/')
      #   dir.create(dir_name, recursive = TRUE, showWarnings = FALSE)
      # }
      # saveRDS(passing_model_data, paste0(dir_name, '/passing_model_data_checkpoint.rds'))
      # saveRDS(rushing_model_data, paste0(dir_name, '/rushing_model_data_checkpoint.rds'))
      # saveRDS(receiving_model_data, paste0(dir_name, '/receiving_model_data_checkpoint.rds'))
      # saveRDS(touchdown_model_data, paste0(dir_name, '/touchdown_model_data_checkpoint.rds'))
      # saveRDS(rushing_receiving_model_data, paste0(dir_name, '/rushing_receiving_model_data_checkpoint.rds'))
      # saveRDS(reception_model_data, paste0(dir_name, '/reception_model_data_checkpoint.rds'))
      # saveRDS(spread_model_data, paste0(dir_name, '/spread_model_data_checkpoint.rds'))
      # saveRDS(moneyline_model_data, paste0(dir_name, '/moneyline_model_data_checkpoint.rds'))
  }

    
  #things that need to be updated periodically throughout the week (injuries and weather forecast)
  # if (midweek_update)
  # {
  #     if (test_mode)
  #     {
  #       dir_name = paste0("data_collection/testing_checkpoint_data/", max_year,'week',wk,'/')
  #     } else {
  #       dir_name = paste0("data_collection/checkpoint_data/", max_year,'week',wk,'/')
  #     }
  #     passing_model_data = readRDS(paste0(dir_name, '/passing_model_data_checkpoint.rds'))
  #     rushing_model_data = readRDS(paste0(dir_name, '/rushing_model_data_checkpoint.rds'))
  #     receiving_model_data = readRDS(paste0(dir_name, '/receiving_model_data_checkpoint.rds'))
  #     touchdown_model_data = readRDS(paste0(dir_name, '/touchdown_model_data_checkpoint.rds'))
  #     rushing_receiving_model_data = readRDS(paste0(dir_name, '/rushing_receiving_model_data_checkpoint.rds'))
  #     reception_model_data = readRDS(paste0(dir_name, '/reception_model_data_checkpoint.rds'))
  #     spread_model_data = readRDS(paste0(dir_name, '/spread_model_data_checkpoint.rds'))
  #     moneyline_model_data = readRDS(paste0(dir_name, '/moneyline_model_data_checkpoint.rds'))
  #     
  #     print('Pulling injuries data and applying to blue chip analysis...')
  #     injuries = get_injuries_data(mode == 'predict', max_year, max_year, wk = wk, testmode = test_mode)
  #     if (!test_mode)
  #     {
  #       blue_chip_analysis_df = get_supabase_data(schema = 'MainData', table_name = 'BlueChip', additional_sql = list(season = paste0('eq.', max_year),
  #                                                                                                                     week = paste0('eq.', wk)))
  #     } else {
  #       blue_chip_analysis_df = readRDS(paste0(dir_name, "testmode_blue_chip_analysis_df_checkpoint.rds"))
  #     }
  #     
  #     blue_chip_players_out = combine_injuries_with_blue_chip(blue_chip_analysis_df, injuries)
  #     
  #     #player models
  #     
  #     update_data = function(data, type)
  #     {
  #       rows_before_change = nrow(data)
  #       cols_before_change = ncol(data)
  #       if (type == 'player')
  #       {
  #         depth_charts = load_depth_charts(max_year:max_year) %>% group_by(gsis_id) %>% arrange(desc(dt)) %>% slice(1) %>% ungroup() %>% #get the latest depth chart info for a player in a week
  #           select(gsis_id, team, pos_rank) %>% rename('new_depth_rank' = 'pos_rank')
  #         
  #         cols_to_update = c("less_practice", "illness", "out_not_injury_related", "out", colnames(data)[str_detect(colnames(data), 'blue_chip_out')], 'team', 'depth_rank')
  #         data = data %>% select(-any_of(cols_to_update)) %>% 
  #           left_join(depth_charts, join_by('gsis_id')) %>%
  #           left_join(injuries %>% select(-team), join_by('gsis_id','season','week')) %>%
  #           left_join(blue_chip_players_out, join_by('season','week','team')) %>% mutate(has_passing_blue_chip_out = ifelse(is.na(has_passing_blue_chip_out), FALSE, has_passing_blue_chip_out),
  #                                                                                        has_rushing_blue_chip_out = ifelse(is.na(has_rushing_blue_chip_out), FALSE, has_rushing_blue_chip_out),
  #                                                                                        has_receiving_blue_chip_out = ifelse(is.na(has_receiving_blue_chip_out), FALSE, has_receiving_blue_chip_out),
  #                                                                                        has_pass_rushers_blue_chip_out = ifelse(is.na(has_pass_rushers_blue_chip_out), FALSE, has_pass_rushers_blue_chip_out),
  #                                                                                        has_rush_tackles_blue_chip_out = ifelse(is.na(has_rush_tackles_blue_chip_out), FALSE, has_rush_tackles_blue_chip_out),
  #                                                                                        has_secondary_blue_chip_out = ifelse(is.na(has_secondary_blue_chip_out), FALSE, has_secondary_blue_chip_out))  %>%
  #           mutate(across(where(is.numeric), ~ ifelse(is.infinite(.x), NA, .x)),
  #                  out = coalesce(out, FALSE),
  #                  out_not_injury_related = coalesce(out_not_injury_related, FALSE),
  #                  illness = coalesce(illness, FALSE),
  #                  less_practice = coalesce(less_practice, FALSE),
  #                  passing_blue_chip = coalesce(passing_blue_chip, FALSE),
  #                  rushing_blue_chip = coalesce(rushing_blue_chip, FALSE),
  #                  receiving_blue_chip = coalesce(receiving_blue_chip, FALSE),
  #                  pass_rushers_blue_chip = coalesce(pass_rushers_blue_chip, FALSE),
  #                  rush_tackles_blue_chip = coalesce(rush_tackles_blue_chip, FALSE),
  #                  secondary_blue_chip = coalesce(secondary_blue_chip, FALSE))
  #           
  #         
  #       } else if (type == 'team') {
  #         cols_to_update = colnames(data)[str_detect(colnames(data), 'blue_chip')]
  #         data = data %>% select(-any_of(cols_to_update)) %>%
  #           left_join(blue_chip_players_out, join_by('season','week','team')) %>% mutate(has_passing_blue_chip_out = ifelse(is.na(has_passing_blue_chip_out), FALSE, has_passing_blue_chip_out),
  #                                                                                        has_rushing_blue_chip_out = ifelse(is.na(has_rushing_blue_chip_out), FALSE, has_rushing_blue_chip_out),
  #                                                                                        has_receiving_blue_chip_out = ifelse(is.na(has_receiving_blue_chip_out), FALSE, has_receiving_blue_chip_out),
  #                                                                                        has_pass_rushers_blue_chip_out = ifelse(is.na(has_pass_rushers_blue_chip_out), FALSE, has_pass_rushers_blue_chip_out),
  #                                                                                        has_rush_tackles_blue_chip_out = ifelse(is.na(has_rush_tackles_blue_chip_out), FALSE, has_rush_tackles_blue_chip_out),
  #                                                                                        has_secondary_blue_chip_out = ifelse(is.na(has_secondary_blue_chip_out), FALSE, has_secondary_blue_chip_out)) %>%
  #           left_join(blue_chip_players_out %>% mutate(opp_has_passing_blue_chip_out = ifelse(is.na(has_passing_blue_chip_out), FALSE, has_passing_blue_chip_out),
  #                                                      opp_has_rushing_blue_chip_out = ifelse(is.na(has_rushing_blue_chip_out), FALSE, has_rushing_blue_chip_out),
  #                                                      opp_has_receiving_blue_chip_out = ifelse(is.na(has_receiving_blue_chip_out), FALSE, has_receiving_blue_chip_out),
  #                                                      opp_has_pass_rushers_blue_chip_out = ifelse(is.na(has_pass_rushers_blue_chip_out), FALSE, has_pass_rushers_blue_chip_out),
  #                                                      opp_has_rush_tackles_blue_chip_out = ifelse(is.na(has_rush_tackles_blue_chip_out), FALSE, has_rush_tackles_blue_chip_out),
  #                                                      opp_has_secondary_blue_chip_out = ifelse(is.na(has_secondary_blue_chip_out), FALSE, has_secondary_blue_chip_out)) %>%
  #                       select(-has_passing_blue_chip_out, -has_rushing_blue_chip_out, -has_receiving_blue_chip_out, -has_pass_rushers_blue_chip_out, -has_rush_tackles_blue_chip_out, -has_secondary_blue_chip_out), join_by('season','week','opponent_team' == 'team')) %>%
  #         mutate(across(contains('blue_chip_out'), ~ coalesce(.x, FALSE)))
  #         
  #         
  #         
  #       } else {
  #         stop("type must be 'player' or 'team'")
  #       }
  #       if (test_mode)
  #       {
  #         data_with_weather = get_weather(data %>% select(-wind, -temp) %>% filter(season == max_year & week == wk) %>% mutate(actual_gameday = gameday, gameday = Sys.Date() + 1), team_lookup_table) %>%
  #           select(-gameday) %>% rename('gameday' = 'actual_gameday')
  #       } else{
  #         data_with_weather = get_weather(data %>% select(-wind, -temp) %>% filter(season == max_year & week == wk), team_lookup_table)
  #       }
  #       
  #       if((nrow(data_with_weather) != rows_before_change) | (ncol(data_with_weather) != cols_before_change))
  #       {
  #         stop(paste('Dimensions changed. Before:', rows_before_change, 'rows and', cols_before_change, 'columns. After:', nrow(data_with_weather), 'rows and', ncol(data_with_weather), 'columns.'))
  #       }
  #       return(data_with_weather %>%
  #                inner_join(team_lookup_table %>% select(NFLReadr_Team_Abbr, Used_To_Cold, Used_To_Hot, Coast), join_by('team' == 'NFLReadr_Team_Abbr')) %>%
  #                inner_join(team_lookup_table %>% select(NFLReadr_Team_Abbr, Used_To_Cold, Used_To_Hot, Coast), join_by('opponent_team' == 'NFLReadr_Team_Abbr')) %>%
  #                mutate(familiar_temperature = !((temp < 40 & !Used_To_Cold.x) | (temp > 80 & !Used_To_Hot.x)) | is.na(temp),
  #                       opp_familiar_temperature = !((temp < 40 & !Used_To_Cold.y) | (temp > 80 & !Used_To_Hot.y)) | is.na(temp)) %>%
  #                select(-contains('Used_To')))
  #     }
  #     
  #     passing_model_data = update_data(passing_model_data, 'player')
  #     
  #     rushing_model_data = update_data(rushing_model_data, 'player') %>%
  #       filter(is.na(out) | !out) %>%
  #       group_by(season, week, team) %>% mutate(total_active_true_talent_baseline_rushing = sum(true_talent_baseline_rushing, na.rm = TRUE)) %>%
  #       mutate(adjusted_rush_share = true_talent_baseline_rushing/total_active_true_talent_baseline_rushing,
  #              delta_adjusted_share_rushing = adjusted_rush_share - true_talent_baseline_rushing) %>%
  #              select(-total_active_true_talent_baseline_rushing)
  #     
  #     receiving_model_data = update_data(receiving_model_data, 'player') %>%
  #       filter(is.na(out) | !out) %>%
  #       group_by(season, week, team) %>% mutate(total_active_true_talent_baseline_receiving = sum(true_talent_baseline_receiving, na.rm = TRUE)) %>%
  #       mutate(adjusted_target_share = true_talent_baseline_receiving/total_active_true_talent_baseline_receiving,
  #              delta_adjusted_share_receiving = adjusted_target_share - true_talent_baseline_receiving) %>%
  #       select(-total_active_true_talent_baseline_receiving)
  #     
  #     touchdown_model_data = update_data(touchdown_model_data, 'player') %>%
  #       filter(is.na(out) | !out) %>%
  #       group_by(season, week, team) %>%
  #       mutate(total_active_true_talent_baseline_rushing = sum(true_talent_baseline_rushing, na.rm = TRUE),
  #              total_active_true_talent_baseline_receiving = sum(true_talent_baseline_receiving, na.rm = TRUE)) %>%
  #       mutate(adjusted_target_share = true_talent_baseline_receiving/total_active_true_talent_baseline_receiving,
  #              delta_adjusted_share_receiving = adjusted_target_share - true_talent_baseline_receiving,
  #              adjusted_rush_share = true_talent_baseline_rushing/total_active_true_talent_baseline_rushing,
  #              delta_adjusted_share_rushing = adjusted_rush_share - true_talent_baseline_rushing) %>%
  #       select(-total_active_true_talent_baseline_rushing, -total_active_true_talent_baseline_receiving)
  #     
  #     rushing_receiving_model_data = update_data(rushing_receiving_model_data, 'player') %>%
  #       filter(is.na(out) | !out) %>%
  #       group_by(season, week, team) %>%
  #       mutate(total_active_true_talent_baseline_rushing = sum(true_talent_baseline_rushing, na.rm = TRUE),
  #              total_active_true_talent_baseline_receiving = sum(true_talent_baseline_receiving, na.rm = TRUE)) %>%
  #       mutate(adjusted_target_share = true_talent_baseline_receiving/total_active_true_talent_baseline_receiving,
  #              delta_adjusted_share_receiving = adjusted_target_share - true_talent_baseline_receiving,
  #              adjusted_rush_share = true_talent_baseline_rushing/total_active_true_talent_baseline_rushing,
  #              delta_adjusted_share_rushing = adjusted_rush_share - true_talent_baseline_rushing) %>%
  #       select(-total_active_true_talent_baseline_rushing, -total_active_true_talent_baseline_receiving)
  #     
  #     reception_model_data = update_data(reception_model_data, 'player') %>%
  #       filter(is.na(out) | !out) %>%
  #       group_by(season, week, team) %>%
  #       mutate(total_active_true_talent_baseline_receiving = sum(true_talent_baseline_receiving, na.rm = TRUE)) %>%
  #       mutate(adjusted_target_share = true_talent_baseline_receiving/total_active_true_talent_baseline_receiving,
  #              delta_adjusted_share_receiving = adjusted_target_share - true_talent_baseline_receiving) %>%
  #       select(-total_active_true_talent_baseline_receiving)
  #     
  #     spread_model_data = update_data(spread_model_data, 'team')
  #     moneyline_model_data = update_data(moneyline_model_data, 'team')
  #     
  #     saveRDS(passing_model_data, paste0(dir_name, '/passing_model_data_checkpoint.rds'))
  #     saveRDS(rushing_model_data, paste0(dir_name, '/rushing_model_data_checkpoint.rds'))
  #     saveRDS(receiving_model_data, paste0(dir_name, '/receiving_model_data_checkpoint.rds'))
  #     saveRDS(touchdown_model_data, paste0(dir_name, '/touchdown_model_data_checkpoint.rds'))
  #     saveRDS(rushing_receiving_model_data, paste0(dir_name, '/rushing_receiving_model_data_checkpoint.rds'))
  #     saveRDS(reception_model_data, paste0(dir_name, '/reception_model_data_checkpoint.rds'))
  #     saveRDS(spread_model_data, paste0(dir_name, '/spread_model_data_checkpoint.rds'))
  #     saveRDS(moneyline_model_data, paste0(dir_name, '/moneyline_model_data_checkpoint.rds'))
  # }

  # passing_model_data = readRDS(paste0('data_collection/checkpoint_data/',max_year,'week',wk,'/passing_model_data_checkpoint.rds'))
  # rushing_model_data = readRDS(paste0('data_collection/checkpoint_data/',max_year,'week',wk,'/rushing_model_data_checkpoint.rds'))
  # receiving_model_data = readRDS(paste0('data_collection/checkpoint_data/',max_year,'week',wk,'/receiving_model_data_checkpoint.rds'))
  # rushing_receiving_model_data = readRDS(paste0('data_collection/checkpoint_data/',max_year,'week',wk,'/rushing_receiving_model_data_checkpoint.rds'))
  # touchdown_model_data = readRDS(paste0('data_collection/checkpoint_data/',max_year,'week',wk,'/touchdown_model_data_checkpoint.rds'))
  # reception_model_data = readRDS(paste0('data_collection/checkpoint_data/',max_year,'week',wk,'/reception_model_data_checkpoint.rds'))
  # spread_model_data = readRDS(paste0('data_collection/checkpoint_data/',max_year,'week',wk,'/spread_model_data_checkpoint.rds'))
  # moneyline_model_data = readRDS(paste0('data_collection/checkpoint_data/',max_year,'week',wk,'/moneyline_model_data_checkpoint.rds'))
  # 

  data_list = list(passing_model_data %>% filter(is.na(out) | !out),
                   rushing_model_data %>% filter(is.na(out) | !out),
                   receiving_model_data %>% filter(is.na(out) | !out),
                   touchdown_model_data %>% filter(is.na(out) | !out),
                   rushing_receiving_model_data %>% filter(is.na(out) | !out),
                   reception_model_data  %>% filter(is.na(out) | !out),
                   moneyline_model_data,
                   spread_model_data)


  t1 = Sys.time()

  if (mode == 'train')
  {
    dir.create("model/ml_ready_data/train", showWarnings = FALSE, recursive = TRUE)
    dir.create("model/ml_ready_data/test", showWarnings = FALSE, recursive = TRUE)
    dir.create("model/ml_ready_data/final_test", showWarnings = FALSE, recursive = TRUE)
  } else if (mode == 'full_fit') {
    dir.create('model/ml_ready_data/fulldata', showWarnings = FALSE)
  } else { #predict
    dir.create(paste0('model/ml_ready_data/predictions/week',wk), showWarnings = FALSE, recursive = TRUE)
  }

  prep_lookup_table = data.frame(
    response_var = c('passing_yards', 'rushing_yards', 'receiving_yards', 'anytime_td_scorer', 'rushing_receiving_yards', 'receptions', 'team_win', 'team_differential'),
    numbers = c(paste(passing_numbers, collapse = '|'),
                paste(rushing_numbers, collapse = '|'),
                paste(receiving_numbers, collapse = '|'),
                NA,
                paste(rushing_receiving_numbers, collapse = '|'),
                paste(reception_numbers, collapse = '|'),
                NA,
                paste(spread_numbers, collapse = '|')),
    current_season_column_category = c("passing_current_season_stats", "rushing_current_season_stats", "receiving_current_season_stats", "touchdown_current_season_stats",
                                       "rushing_current_season_stats|receiving_current_season_stats", 'receiving_current_season_stats', NA, NA),
    historical_season_column_category = c("passing_past_season_stats", "rushing_past_season_stats", "receiving_past_season_stats", "touchdown_past_season_stats",
                                          "rushing_past_season_stats|receiving_past_season_stats", 'receiving_past_season_stats', NA, NA)
  )
  model_mapping = get_supabase_data('predictions','ModelSelections')
  player_preds = data.frame()
  team_preds = data.frame()
  for (i in 1:length(data_list))
  {
    response_var = prep_lookup_table$response_var[i]
    print(paste('Prepping data for:', response_var))
    prepped_data = model_prep(data_to_prep = data_list[[i]],
                              column_categories = column_categories,
                              train_mode = (mode == "train"),
                              numbers = as.numeric(unlist(strsplit(prep_lookup_table$numbers[[i]], '\\|'))),
                              response_var = response_var,
                              current_season_column_category = prep_lookup_table$current_season_column_category[i],
                              historical_season_column_category = prep_lookup_table$historical_season_column_category[i],
                              acceptable_predictive_power = 'Suspicious,High,Medium,Low',
                              num_winners = 10
    )
    if ('gsis_id' %in% colnames(data_list[[i]]))
    {

      identifier_data = data_list[[i]] %>%
        select(season, week, gsis_id, display_name, team, opponent_team,
               weekday, gameday, gametime, home_stadium, neutral_field,
               position, jersey_number, draft_round, draft_pick, draft_year) %>%
        mutate(time_of_day = case_when(
          gametime < '12:00' ~ 'Morning',
          gametime < '14:00'~ 'Early Window',
          gametime < '19:00' ~ 'Late Window',
          .default = 'Night'
        ),
        game_location = ifelse(home_stadium, 'Home', ifelse(neutral_field, 'Neutral', 'Away'))) %>% select(-home_stadium, -neutral_field)
    } else {
      identifier_data = data_list[[i]] %>%
        select(season, week, team, opponent_team,
               weekday, gameday, gametime, home_stadium, neutral_field) %>%
        mutate(time_of_day = case_when(
          gametime < '12:00' ~ 'Morning',
          gametime < '14:00'~ 'Early Window',
          gametime < '19:00' ~ 'Late Window',
          .default = 'Night'
        ),
        game_location = ifelse(home_stadium, 'Home', ifelse(neutral_field, 'Neutral', 'Away'))) %>% select(-home_stadium, -neutral_field)
    }

    if(mode == 'train')
    {
      for (j in 1:length(prepped_data[[1]]))
      {
        write_parquet(prepped_data[[1]][[j]], paste0('model/ml_ready_data/train/', names(prepped_data[[1]])[j], '.parquet'))
        write_parquet(prepped_data[[2]][[j]], paste0('model/ml_ready_data/test/', names(prepped_data[[1]])[j], '.parquet'))
        write_parquet(prepped_data[[3]][[j]], paste0('model/ml_ready_data/final_test/', names(prepped_data[[1]])[j], '.parquet'))
      }
    } else if (mode == 'full_fit') {
        for (j in 1:length(prepped_data))
        {
          write_parquet(prepped_data[[j]], paste0('model/ml_ready_data/fulldata/', names(prepped_data)[j], '.parquet'))
        }
    } else { #predict
      for (j in names(prepped_data))
      {
        t1 = Sys.time()
        this_df = prepped_data[[j]] %>% ungroup()
        this_data = as.list(this_df)
        #these_preds = request("http://127.0.0.1:8000/predict") %>%
        these_preds = request("https://nfl-api-1062278650130.us-east4.run.app/predict") %>%
          req_body_json(list(response_var = j, data = this_data), na = 'null') %>%
          req_perform() %>%
          resp_body_json(simplifyVector = TRUE) %>%
          as.data.frame()
        if (str_detect(j, 'team'))
        {
          these_preds = these_preds %>% mutate(season = max_year) %>%
            left_join(identifier_data, join_by(season, 'Week' == 'week', team)) %>%
            select(response_var, season, Week, team, opponent_team, Model_Probability, everything())
          team_preds = bind_rows(team_preds, these_preds)
        } else {
          these_preds = these_preds %>% mutate(season = max_year) %>%
            left_join(identifier_data, join_by(season, 'Week' == 'week', gsis_id)) %>%
            mutate(draft_round = as.numeric(draft_round)) %>%
            select(response_var, season, Week, gsis_id, Model_Probability, everything())
          player_preds = bind_rows(player_preds, these_preds)
        }
        print('API time:')
        print(Sys.time() - t1)
      }
    }
  }

  if (mode == 'predict')
  {
    write_to_supabase('predictions', 'PlayerPredictions', player_preds %>% mutate(updated_at = Sys.time()))
    write_to_supabase('predictions', 'TeamPredictions', team_preds %>% mutate(updated_at = Sys.time()))
  }

  print(paste('Total time to prep:', difftime(Sys.time(), t1, units = 'hours'), 'hours'))
}

if (test_mode)
{
  #portfolio:
  preds = bind_rows(player_preds %>% rename('label' = 'gsis_id', 'Position' = 'position'),
                    team_preds %>% mutate(label = team, 'Position' = 'team')) %>% mutate(Odds = 500) %>% select(response_var, Week, label, Odds, Model_Probability, Position, team, opponent_team)
  bets_with_evs = request("https://nfl-api-1062278650130.us-east4.run.app/get_bets_with_ev") %>%
    req_body_json(list(season = max_year,data = as.list(preds))) %>%
    req_perform() %>%
    resp_body_json(simplifyVector = TRUE) %>%
    as.data.frame()
  portfolios = request("https://nfl-api-1062278650130.us-east4.run.app/get_portfolio") %>%
    req_body_json(list(season = max_year, data = as.list(bets_with_evs %>% slice(1:100)), max_bets = 20, total_amount = 50, sd_cap = 2.0)) %>%
   req_perform() %>%
    resp_body_json(simplifyVector = TRUE) %>%
    as.data.frame()
}




#video_youtube_transcripts = get_transcripts(team_lookup_table, date_cutoff = '2020-08-01')

#video_llm_results = get_video_llm_results(video_youtube_transcripts, player_lookup = player_data %>% ungroup() %>% select(gsis_id, display_name) %>% distinct())