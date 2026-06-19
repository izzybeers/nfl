
get_team_data = function(min_year, max_year, schedules_df, team_lookup_df)
{
 schedules = schedules_df %>%
    mutate(stadium = ifelse(location == 'home', team, ifelse(location == 'away', opponent, NA)),
           home_stadium = location == 'home',
           neutral_field = location == 'neutral') %>%
    inner_join(team_lookup_df %>% select(NFLReadr_Team_Abbr, Grass_Type, Roof, Used_To_Cold, Used_To_Hot, Coast), join_by('team' == 'NFLReadr_Team_Abbr')) %>%
    inner_join(team_lookup_df %>% select(NFLReadr_Team_Abbr, Used_To_Cold, Used_To_Hot, Coast), join_by('opponent' == 'NFLReadr_Team_Abbr')) %>%
    left_join(team_lookup_df %>% select(NFLReadr_Team_Abbr, Grass_Type, Roof, Altitude, Stadium_Capacity, Loudest_Stadiums, Coast) %>% rename('stadium_grass_type' = 'Grass_Type',
                                                                                                                                                 'stadium_roof' = 'Roof',
                                                                                                                                                 'Game_Coast' = 'Coast'),join_by('stadium' == 'NFLReadr_Team_Abbr')) %>%
    mutate(short_week = team_rest < 6, long_week = team_rest > 8,
           familiar_temperature = !((temp < 40 & !Used_To_Cold.x) | (temp > 80 & !Used_To_Hot.x)) | is.na(temp),
           opp_familiar_temperature = !((temp < 40 & !Used_To_Cold.y) | (temp > 80 & !Used_To_Hot.y)) | is.na(temp),
           familiar_grass_type = Grass_Type == stadium_grass_type,
           familiar_roof_type = Roof == stadium_roof,
           team_win = team_score > opponent_score,
           team_differential = team_score - opponent_score,
           long_travel = !is.na(Game_Coast) & !is.na(Coast.x) & Game_Coast != Coast.x,
           opp_long_travel = !is.na(Game_Coast) & !is.na(Coast.y) & Game_Coast != Coast.y) %>%
    select(game_id, season, week, team, team_score, opponent_score, team_win, team_differential, home_stadium, neutral_field, short_week, long_week, gameday, gametime, weekday, wind, temp, familiar_temperature, opp_familiar_temperature, stadium_grass_type, stadium_roof,
           familiar_grass_type, familiar_roof_type, long_travel, opp_long_travel, referee, team_coach, team_qb_id, div_game, overtime)
  
  
  orig_teamgl = load_team_stats(seasons = min_year:max_year) %>% select(season, week, team, opponent_team, 
                                                                   completions, attempts, passing_yards, passing_tds, passing_interceptions, sacks_suffered, sack_yards_lost, sack_fumbles, sack_fumbles_lost, passing_air_yards, passing_yards_after_catch, passing_first_downs, passing_2pt_conversions, 
                                                                   carries, rushing_yards, rushing_tds, rushing_fumbles, rushing_fumbles_lost, rushing_first_downs, rushing_2pt_conversions,
                                                                   receiving_fumbles, receiving_fumbles_lost,
                                                                   special_teams_tds, penalties, penalty_yards)
  colnames(orig_teamgl)[-which(colnames(orig_teamgl) %in% c('season', 'week', 'team', 'opponent_team'))] = paste0('team_', colnames(orig_teamgl)[-which(colnames(orig_teamgl) %in% c('season', 'week', 'team', 'opponent_team'))])
  
  

  teamgl = orig_teamgl %>% left_join(schedules, join_by('season', 'week', 'team')) 
  
  play_details = load_pbp((max(min_year,2022)):max_year)  %>% filter(!is.na(posteam))
  
  team_redzone_drives = play_details %>%
    group_by(season, week, game_id, posteam, drive) %>% summarise(drive_in_redzone = any(yardline_100 < 20 & !(play_type %in% c('extra_point', 'kickoff','no_play', 'qb_kneel', 'qb_spike'))),
                                                        td_in_redzone = any(yardline_100 < 20 & touchdown == 1),
                                                        team_redzone_plays = sum(play_type %in% c('pass', 'punt', 'run') & yardline_100 < 20, na.rm=TRUE),
                                                        team_redzone_receiving_plays = sum(play_type == 'pass' & yardline_100 < 20, na.rm=TRUE),
                                                        team_redzone_rushing_plays = sum(play_type == 'run' & yardline_100 < 20, na.rm=TRUE),
                                                        fg_attempts = sum(field_goal_attempt,na.rm=T), .groups = "drop") %>%
    group_by(game_id, season, week, posteam) %>%
    summarise(team_drives = length(unique(drive)),
              team_drives_in_redzone = sum(drive_in_redzone, na.rm = TRUE),
              team_touchdowns_in_redzone = sum(td_in_redzone, na.rm=TRUE),
              team_redzone_plays = sum(team_redzone_plays, na.rm=TRUE),
              team_redzone_receiving_plays = sum(team_redzone_receiving_plays, na.rm=TRUE),
              team_redzone_rushing_plays = sum(team_redzone_rushing_plays, na.rm=TRUE),
              team_total_fg_attempts = sum(fg_attempts,na.rm=T), .groups = "drop")
  
  #game and team level table. some of these are defensive stats.
  time_of_possession = play_details %>%
    mutate(drive_time_of_possession = as.numeric(sapply(strsplit(drive_time_of_possession, ':'), function(x) x[1])) + as.numeric(sapply(strsplit(drive_time_of_possession, ':'), function(x) x[2]))/60) %>%
    filter(!is.na(drive_game_clock_start)) %>%
    group_by(game_id, posteam, drive_game_clock_start) %>% summarise(drive_time_of_possession = max(drive_time_of_possession,na.rm=T), .groups = "drop") %>%
    group_by(game_id, posteam) %>% summarise(team_time_of_possession = sum(drive_time_of_possession,na.rm=T), .groups = "drop")
  
  
  aggregated_play_data = play_details %>%
    left_join(load_ftn_charting(max(min_year,2022):max_year) %>% select(-season, -week, -ftn_play_id), join_by( 'play_id'== 'nflverse_play_id', 'game_id' == 'nflverse_game_id')) %>%
    group_by(game_id, posteam) %>%
    summarise(team_qb_sneaks = sum(is_qb_sneak, na.rm = TRUE),
              team_catchable_balls = sum(is_catchable_ball, na.rm = TRUE),
              team_catchable_balls_caught =sum(is_catchable_ball & complete_pass, na.rm = TRUE),
              team_play_action = sum(is_play_action, na.rm = TRUE),
              team_screen_passes = sum(is_screen_pass, na.rm = TRUE),
              team_trick_plays = sum(is_trick_play, na.rm = TRUE),
              team_total_pass_rushers =sum(n_pass_rushers, na.rm = TRUE),
              team_total_blitzers = sum(n_blitzers, na.rm = TRUE),
              team_total_run_pass = sum(is_rpo, na.rm = TRUE),
              team_sum_qb_under_center = sum(qb_location == 'U', na.rm = TRUE),
              team_sum_qb_shotgun = sum(qb_location == 'S', na.rm = TRUE),
              team_sum_qb_pistol = sum(qb_location == 'P', na.rm = TRUE),
              team_qb_fault_sacks = sum(is_qb_fault_sack, na.rm = TRUE),
              team_total_offense_backfield = sum(n_offense_backfield, na.rm = TRUE),
              team_total_defense_box = sum(n_defense_box, na.rm = TRUE),
              team_num_plays = length(unique(play_id)),
              team_converted_third_downs = sum(third_down_converted, na.rm = TRUE),
              team_failed_third_downs = sum(third_down_failed, na.rm = TRUE),
              team_converted_fourth_downs = sum(fourth_down_converted, na.rm = TRUE),
              team_failed_fourth_downs = sum(fourth_down_failed, na.rm = TRUE),
              team_total_third_downs_attempted = team_converted_third_downs + team_failed_third_downs,
              team_total_fourth_downs_attempted = team_converted_fourth_downs + team_failed_fourth_downs, .groups = "drop") %>%
    left_join(time_of_possession, join_by('game_id', 'posteam')) %>% 
    left_join(team_redzone_drives %>% select(-season, -week), join_by('game_id', 'posteam'))
  
  teamgl = teamgl %>% left_join(aggregated_play_data, join_by('game_id', 'team' == 'posteam'))
  
  oppgl = teamgl %>% select(season, week, opponent_team, team_win, team_differential, team_completions, team_attempts, team_passing_yards, team_passing_tds, team_passing_interceptions, team_sacks_suffered, team_sack_yards_lost, team_sack_fumbles, team_sack_fumbles_lost, team_passing_yards_after_catch, team_passing_first_downs, team_passing_2pt_conversions,
                            team_carries, team_rushing_yards, team_rushing_tds, team_rushing_fumbles, team_rushing_fumbles_lost, team_rushing_first_downs, team_rushing_2pt_conversions, team_receiving_fumbles, team_receiving_fumbles_lost, team_converted_third_downs, team_converted_fourth_downs, team_total_third_downs_attempted, team_total_fourth_downs_attempted, team_total_blitzers, team_total_pass_rushers, team_total_defense_box, short_week, long_week, team_num_plays,
                            team_drives, team_touchdowns_in_redzone, team_drives_in_redzone, team_drives, team_total_fg_attempts)
  
  colnames(oppgl) = gsub('team_', 'opp_defense_', colnames(oppgl))
  oppgl = oppgl %>%  rename('opp_short_week' = 'short_week', 'opp_long_week' = 'long_week')
  allowed_colnames = setdiff(1:ncol(oppgl), c(1:3, which(str_detect(colnames(oppgl), 'fumble|sack|interception|week|box|rushers|blitzers|num_plays'))))
  forced_colnames = which(str_detect(colnames(oppgl), 'fumble|sack|interception'))
  colnames(oppgl)[allowed_colnames] = paste0(colnames(oppgl)[allowed_colnames], '_allowed')
  colnames(oppgl)[forced_colnames] = paste0(colnames(oppgl)[forced_colnames], '_forced')
  
  
  teamgl = teamgl %>% select(-team_total_blitzers, -team_total_pass_rushers, -team_total_defense_box)
  
  #matchup stats:
  team_data_with_matchups = teamgl %>%
    arrange(team, season, week) %>%
    group_by(team) %>%
    mutate(team_average_passing_past_3_years = slide_dbl(.x = team_passing_yards, .f = mean, na.rm = TRUE, .before = 51, .after = -1),
           team_average_rushing_past_3_years = slide_dbl(.x = team_rushing_yards, .f = mean, na.rm = TRUE, .before = 51, .after = -1)) %>%
    group_by(team, opponent_team) %>%
    mutate(team_average_passing_against_opp_past_3_years = slide_dbl(.x = team_passing_yards, .f = mean, na.rm = TRUE, .before = 51, .after = -1),
           team_average_rushing_against_opp_past_3_years = slide_dbl(.x = team_rushing_yards,  .f = mean, na.rm = TRUE, .before = 51, .after = -1),
           team_total_games_against_opp_past_3_years = slide_dbl(.x = game_id, .f = length, .before = 51, .after = -1),
           team_passing_matchup_average_against_opp = (team_average_passing_against_opp_past_3_years*team_total_games_against_opp_past_3_years +
                                                         team_average_passing_past_3_years * 4)/(4+team_total_games_against_opp_past_3_years),
           team_rushing_matchup_average_against_opp = (team_average_rushing_against_opp_past_3_years*team_total_games_against_opp_past_3_years +
                                                         team_average_rushing_past_3_years * 4)/(4+team_total_games_against_opp_past_3_years),
           team_passing_weighted_matchup_ratio_against_opp = team_passing_matchup_average_against_opp/team_average_passing_past_3_years,
           team_rushing_weighted_matchup_ratio_against_opp = team_rushing_matchup_average_against_opp/team_average_rushing_past_3_years
           ) %>%
    ungroup() %>%
    select(-team_average_passing_past_3_years, -team_average_rushing_past_3_years, -team_total_games_against_opp_past_3_years, -team_average_passing_against_opp_past_3_years, -team_average_rushing_against_opp_past_3_years, -team_passing_matchup_average_against_opp, -team_rushing_matchup_average_against_opp)
  
  teamgl = team_data_with_matchups
  
  return(list(teamgl, oppgl, team_redzone_drives %>% select(-game_id), schedules))
}

summarize_current_season_team_stats = function(team_data, opp_data, team_calc_metrics, opp_calc_metrics, schedules)
{
  team_column_categories = list()
  team_cols_for_historical_calculations = c('team_win', 'team_differential', setdiff(colnames(team_data), c(colnames(schedules), 'opponent_team', 'season_type', 'team_num_plays', colnames(team_data)[str_detect(colnames(team_data), 'total_|short_week|long_week|matchup_ratio')])))
  opp_cols_for_historical_calculations = setdiff(colnames(opp_data), c('season', 'week', 'opp_short_week', 'opp_long_week', 'opponent_team', 'opp_total_defense_box', 'opp_defense_total_pass_rushers', 'opp_defense_total_blitzers', 'opp_defense_num_plays','opp_defense_carries_allowed','opp_defense_attempts_allowed', 'opp_defense_total_fg_attempts_allowed'))
  
  team_data_with_historical_calculations = team_data %>%
    arrange(season, team) %>%
    group_by(season, team) %>%
    group_modify(~ compute_slider_cumulatives(.x, team_cols_for_historical_calculations, cumulative_only = FALSE)) %>%
    group_modify(~ compute_slider_cumulatives(.x, c('team_num_plays', colnames(team_data)[str_detect(colnames(team_data), 'total_')]), cumulative_only = TRUE)) %>%
    mutate(ot_road = overtime & !home_stadium,
           team_last_week_ot_road = lag(ot_road)) %>%
    select(-overtime, -ot_road) %>% select(-contains("cv_team_differential"))
  
  avg_columns = colnames(team_data_with_historical_calculations)[str_detect(tolower(colnames(team_data_with_historical_calculations)), 'avg')]
  sd_columns = colnames(team_data_with_historical_calculations)[str_detect(tolower(colnames(team_data_with_historical_calculations)), 'sd')]
  
  overlap_sd = sd_columns[which(gsub('sd_|_sd|SD_|_SD','', sd_columns) %in% gsub('_avg|avg_|Avg_|_Avg', '', avg_columns))]
  
  for(sd_col in overlap_sd)
  {
    column_name = avg_columns[which(gsub('_avg|avg_|Avg_|_Avg', '', avg_columns) == gsub('sd_|_sd|SD_|_SD','', sd_col))]
    cv_col_name = paste0('cv_',gsub('sd_|_sd|SD_|_SD','', sd_col))
    team_data_with_historical_calculations = team_data_with_historical_calculations %>% mutate(!!cv_col_name := !!sym(sd_col)/!!sym(column_name))
  }
  
  opp_data_with_historical_calculations = opp_data %>%
    arrange(season, opponent_team) %>%
    group_by(season, opponent_team) %>%
    group_modify(~ compute_slider_cumulatives(.x, opp_cols_for_historical_calculations, cumulative_only = FALSE)) %>%
    group_modify(~ compute_slider_cumulatives(.x, c('opp_defense_num_plays', 'opp_defense_attempts_allowed', 'opp_defense_carries_allowed', 'opp_defense_total_fg_attempts_allowed', colnames(opp_data)[str_detect(colnames(opp_data), 'total_')]), cumulative_only = TRUE))
  
  avg_columns = colnames(opp_data_with_historical_calculations)[str_detect(tolower(colnames(opp_data_with_historical_calculations)), 'avg')]
  sd_columns = colnames(opp_data_with_historical_calculations)[str_detect(tolower(colnames(opp_data_with_historical_calculations)), 'sd')]
  
  overlap_sd = sd_columns[which(gsub('sd_|_sd|SD_|_SD','', sd_columns) %in% gsub('_avg|avg_|Avg_|_Avg', '', avg_columns))]
  
  for(sd_col in overlap_sd)
  {
    column_name = avg_columns[which(gsub('_avg|avg_|Avg_|_Avg', '', avg_columns) == gsub('sd_|_sd|SD_|_SD','', sd_col))]
    cv_col_name = paste0('cv_',gsub('sd_|_sd|SD_|_SD','', sd_col))
    opp_data_with_historical_calculations = opp_data_with_historical_calculations %>% mutate(!!cv_col_name := !!sym(sd_col)/!!sym(column_name))
  }
  
  team_data_with_historical_calculations_and_efficiency_metrics = calculate_efficiency_metrics(df = team_data_with_historical_calculations,
                                                                                          calc_metrics = team_calc_metrics,
                                                                                          include_last3 = TRUE) %>%
    select(!matches('cumulative'))
  
  opp_data_with_historical_calculations_and_efficiency_metrics = calculate_efficiency_metrics(df = opp_data_with_historical_calculations,
                                                                                               calc_metrics = opp_calc_metrics,
                                                                                               include_last3 = TRUE)
  
  team_present_stats = c('team_differential', 'opponent_score', 'team_score',
                         setdiff(colnames(team_data_with_historical_calculations_and_efficiency_metrics),
                                 c('opponent_team', 'season_type', 'team_last_week_ot_road', colnames(schedules), colnames(team_data_with_historical_calculations_and_efficiency_metrics)[str_detect(tolower(colnames(team_data_with_historical_calculations_and_efficiency_metrics)),'last3|avg|sd|min_|max_|median_|cv_|per_|pct_|matchup_ratio')])))
  opp_present_stats = c('team_differential', 'opponent_score', 'team_score', setdiff(colnames(opp_data_with_historical_calculations_and_efficiency_metrics),
                               c('opponent_team', 'season_type', 'opp_long_week', 'opp_short_week', colnames(schedules), colnames(opp_data_with_historical_calculations_and_efficiency_metrics)[str_detect(tolower(colnames(opp_data_with_historical_calculations_and_efficiency_metrics)),'last3|avg|sd|min_|max_|median_|cv_|per_|pct_|cumulative')])))
  
  team_data_with_historical_calculations_and_efficiency_metrics = team_data_with_historical_calculations_and_efficiency_metrics %>%
    select(-any_of(setdiff(team_present_stats, c('team_passing_yards', 'team_rushing_yards', 'team_attempts', 'team_win', 'team_differential'))))
  opp_data_with_historical_calculations_and_efficiency_metrics = opp_data_with_historical_calculations_and_efficiency_metrics %>%
    select(-any_of(setdiff(opp_present_stats, c('opp_defense_passing_yards_allowed', 'opp_defense_rushing_yards_allowed', 'opp_defense_sacks_suffered_forced', 'opp_defense_attempts_allowed', 'opp_defense_carries_allowed')))) %>%
    mutate(opp_passing_yards_per_attempt_allowed_current_game = opp_defense_passing_yards_allowed/opp_defense_attempts_allowed,
           opp_sacks_forced_per_attempt_allowed_current_game = opp_defense_sacks_suffered_forced/opp_defense_attempts_allowed,
           opp_rushing_yards_per_carry_allowed_current_game = opp_defense_rushing_yards_allowed/opp_defense_carries_allowed)  %>%
    select(!matches('cumulative'))
  
  team_uninformative_stats_results = remove_uninformative_stats(df = team_data,
                                                           column_list = c('team_win', 'team_differential', setdiff(colnames(team_data), c('opponent_team', 'season_type', colnames(schedules)))),
                                                           missing_threshold = 0.9)
  team_data_with_historical_calculations_and_efficiency_metrics = team_data_with_historical_calculations_and_efficiency_metrics   %>% select(-any_of(team_uninformative_stats_results[[1]])) %>%
    mutate(team_pass_rush_ratio = ifelse(is.na(avg_team_rushing_yards) | avg_team_rushing_yards == 0, NA, avg_team_passing_yards/avg_team_rushing_yards))
  
  opp_uninformative_stats_results = remove_uninformative_stats(df = opp_data,
                                                                column_list = setdiff(colnames(opp_data), c('opponent_team', 'opp_long_week', 'opp_short_week', colnames(schedules))),
                                                                missing_threshold = 0.9)
  opp_data_with_historical_calculations_and_efficiency_metrics = opp_data_with_historical_calculations_and_efficiency_metrics  %>% select(-any_of(opp_uninformative_stats_results[[1]]))
  
  team_columns_0_1 = team_uninformative_stats_results[[2]]
  team_low_medians = team_uninformative_stats_results[[3]]
  opp_columns_0_1 = opp_uninformative_stats_results[[2]]
  opp_low_medians = opp_uninformative_stats_results[[3]]
  
  if(length(team_columns_0_1) > 0)
  {
    team_data_with_historical_calculations_and_efficiency_metrics = team_data_with_historical_calculations_and_efficiency_metrics %>% select(-any_of(c(paste0('sd_', team_columns_0_1), paste0('median_', team_columns_0_1), paste0('min_', team_columns_0_1),paste0('max_', team_columns_0_1),paste0('cv_', team_columns_0_1))))
    team_data_with_historical_calculations_and_efficiency_metrics = team_data_with_historical_calculations_and_efficiency_metrics %>% select(-any_of(c(paste0('last3_sd_', team_columns_0_1), paste0('last3_median_', team_columns_0_1), paste0('last3_min_', team_columns_0_1),paste0('last3_max_', team_columns_0_1), paste0('last3_cv_', team_columns_0_1))))
  }
  if(length(opp_columns_0_1) > 0)
  {
    opp_data_with_historical_calculations_and_efficiency_metrics = opp_data_with_historical_calculations_and_efficiency_metrics %>% select(-any_of(c(paste0('sd_', opp_columns_0_1), paste0('median_', opp_columns_0_1), paste0('min_', opp_columns_0_1),paste0('max_', opp_columns_0_1))))
    opp_data_with_historical_calculations_and_efficiency_metrics = opp_data_with_historical_calculations_and_efficiency_metrics %>% select(-any_of(c(paste0('last3_sd_', opp_columns_0_1), paste0('last3_median_', opp_columns_0_1), paste0('last3_min_', opp_columns_0_1),paste0('last3_max_', opp_columns_0_1))))
  }
  if(length(team_low_medians) > 0)
  {
    team_data_with_historical_calculations_and_efficiency_metrics = team_data_with_historical_calculations_and_efficiency_metrics  %>% select(-any_of(c(paste0('median_', team_low_medians), paste0('min_', team_low_medians))))
    team_data_with_historical_calculations_and_efficiency_metrics = team_data_with_historical_calculations_and_efficiency_metrics  %>% select(-any_of(c(paste0('last3_median_', team_low_medians), paste0('last3_min_', team_low_medians))))
  }
  if(length(opp_low_medians) > 0)
  {
    opp_data_with_historical_calculations_and_efficiency_metrics = opp_data_with_historical_calculations_and_efficiency_metrics  %>% select(-any_of(c(paste0('median_', opp_low_medians), paste0('min_', opp_low_medians))))
    opp_data_with_historical_calculations_and_efficiency_metrics = opp_data_with_historical_calculations_and_efficiency_metrics  %>% select(-any_of(c(paste0('last3_median_', opp_low_medians), paste0('last3_min_', opp_low_medians))))
  }
  
  team_column_categories[['team_drives']] = colnames(team_data_with_historical_calculations_and_efficiency_metrics)[str_detect(colnames(team_data_with_historical_calculations_and_efficiency_metrics), 'redzone|drives|fg') & str_detect(colnames(team_data_with_historical_calculations_and_efficiency_metrics), 'team')]
  team_column_categories[['team_matchup_data']] = c('team_passing_weighted_matchup_ratio_against_opp', 'team_rushing_weighted_matchup_ratio_against_opp')
  team_column_categories[['team_current_season_stats']] = setdiff(c(colnames(team_data_with_historical_calculations_and_efficiency_metrics)[str_detect(colnames(team_data_with_historical_calculations_and_efficiency_metrics), 'max_|min_|avg_|sd_|cv_|median_|pct_|_pct|per_|differential|rating|average_|mean_|cv_|last3_|lag[0-9]|ratio')], 'team_pass_rush_ratio'),
                                                                  c('team_rushing_yards', 'team_passing_yards', 'team_differential', 'team_win', team_column_categories[['team_drives']]))
  team_column_categories[['opp_current_season_stats']] = setdiff(colnames(opp_data_with_historical_calculations_and_efficiency_metrics)[str_detect(colnames(opp_data_with_historical_calculations_and_efficiency_metrics), 'max_|min_|avg_|sd_|median_|cv_|pct_|_pct|per_|differential|rating|average_|mean_|cv_|last3_|lag[0-9]|ratio')],
                                                                 c('opp_defense_passing_yards_allowed', 'opp_defense_rushing_yards_allowed', 'opp_defense_sacks_suffered_forced', 'opp_defense_attempts_allowed', 'opp_defense_carries_allowed',
                                                                   'opp_passing_yards_per_attempt_allowed_current_game', 'opp_sacks_forced_per_attempt_allowed_current_game', 'opp_rushing_yards_per_carry_allowed_current_game',
                                                                   colnames(opp_data_with_historical_calculations_and_efficiency_metrics)[str_detect(colnames(opp_data_with_historical_calculations_and_efficiency_metrics), 'defense_attempts_allowed|defense_carries_allowed|total_fg_attempts_allowed')]))
  
  return(list(team_data_with_historical_calculations_and_efficiency_metrics,
              opp_data_with_historical_calculations_and_efficiency_metrics,
              team_column_categories))
}

calculate_team_seasonal_historical_stats = function(team_data, opp_data, team_calc_metrics, opp_calc_metrics, schedules, team_column_categories)
{
  team_seasonal_stats = team_data %>%
    group_by(team, season) %>%
    summarise(across(all_of(c('team_differential', setdiff(colnames(team_data), c('opponent_team', 'season_type', colnames(schedules))))),
                     .fns = list(sum = ~sum(.x, na.rm = TRUE),
                                 mean = ~mean(.x, na.rm = TRUE),
                                 median = ~median(.x, na.rm = TRUE),
                                 max = ~max(.x, na.rm = TRUE),
                                 min = ~min(.x, na.rm = TRUE),
                                 sd = ~sd(.x, na.rm = TRUE)
                     ), .names = "{.fn}_{.col}"), .groups = 'drop') %>%
    group_by(team, season) %>%
    ungroup() %>%
    rename_with(~ gsub("sum_", "cumulative_", .x)) %>%
    rename_with(~ gsub("mean_", "avg_", .x)) %>%
    rename_with(~ gsub('Sd_|SD_', 'sd_', .x)) %>%
    mutate(across(where(is.numeric), ~ ifelse(is.infinite(.x), NA, .x))) %>%
    mutate(team_pass_rush_ratio = ifelse(is.na(avg_team_rushing_yards) | avg_team_rushing_yards == 0, NA, avg_team_passing_yards/avg_team_rushing_yards)) %>%
    calculate_efficiency_metrics(calc_metrics = team_calc_metrics, include_last3 = FALSE) %>%
    select(!matches('sum_|cumulative_'))
  
  opp_seasonal_stats = opp_data %>%
    group_by(opponent_team, season) %>%
    summarise(across(all_of(setdiff(colnames(opp_data), c('opponent_team', 'season_type', 'opp_long_week', 'opp_short_week', colnames(schedules)))),
                     .fns = list(sum = ~sum(.x, na.rm = TRUE),
                                 mean = ~mean(.x, na.rm = TRUE),
                                 median = ~median(.x, na.rm = TRUE),
                                 max = ~max(.x, na.rm = TRUE),
                                 min = ~min(.x, na.rm = TRUE),
                                 sd = ~sd(.x, na.rm = TRUE)
                     ), .names = "{.fn}_{.col}"), .groups = 'drop') %>%
    group_by(opponent_team, season) %>%
    ungroup() %>%
    rename_with(~ gsub("sum_", "cumulative_", .x)) %>%
    rename_with(~ gsub("mean_", "avg_", .x)) %>%
    rename_with(~ gsub('Sd_|SD_', 'sd_', .x)) %>%
    mutate(across(where(is.numeric), ~ ifelse(is.infinite(.x), NA, .x))) %>%
    calculate_efficiency_metrics(calc_metrics = opp_calc_metrics, include_last3 = FALSE) %>%
    select(!matches('sum_|cumulative_'))
  
  team_column_categories[['game_info']] = c(setdiff(colnames(schedules), c('season', 'team', 'team_win', 'game_id', 'team_score', 'opponent_score', 'overtime', 'team_differential')),
                                            'opponent_team', 'team_last_week_ot_road', 'opp_long_week', 'opp_short_week')
  team_column_categories[['team_historical_seasons_stats']] = setdiff(colnames(team_seasonal_stats), c('season','team'))
  team_column_categories[['opp_historical_seasons_stats']] = setdiff(colnames(opp_seasonal_stats), c('opponent_team', 'season',
                                                                                                     colnames(opp_seasonal_stats)[str_detect(colnames(opp_seasonal_stats), 'defense_attempts_allowed|defense_carries_allowed|total_fg_attempts_allowed')]))
  
  return(list(team_seasonal_stats,
              opp_seasonal_stats,
              team_column_categories))
}

pull_all_team_stats = function(min_year, max_year, recalculate_seasonal = FALSE)
{
  team_opp_data = get_team_data(min_year, max_year, schedules_df = schedules_raw, team_lookup_df = team_lookup_table)
  team_data = team_opp_data[[1]]
  opp_data = team_opp_data[[2]]
  team_redzone_drives = team_opp_data[[3]]
  cleaned_schedules_df = team_opp_data[[4]]
  
  team_opp_current_season_stats = summarize_current_season_team_stats(team_data = team_data, opp_data = opp_data,
                                                                        team_calc_metrics = team_calc_metrics, opp_calc_metrics = opp_calc_metrics,
                                                                        schedules = cleaned_schedules_df)
  team_current_season_stats = team_opp_current_season_stats[[1]]
  opp_current_season_stats = team_opp_current_season_stats[[2]]
  team_column_categories_current_season = team_opp_current_season_stats[[3]]
  
  if(recalculate_seasonal == TRUE)
  {
    team_opp_historical_season_stats = calculate_team_seasonal_historical_stats(team_data = team_data, opp_data = opp_data,
                                                                                team_calc_metrics = team_calc_metrics, opp_calc_metrics = opp_calc_metrics,
                                                                                schedules = cleaned_schedules_df,
                                                                                team_column_categories = team_column_categories_current_season)
    team_historical_season_stats = team_opp_historical_season_stats[[1]]
    opp_historical_season_stats = team_opp_historical_season_stats[[2]]
    team_column_categories = team_opp_historical_season_stats[[3]]
    
    for(name in c('team_historical_seasons_stats', 'opp_historical_seasons_stats'))
    {
      team_column_categories[[name]] = 
        c(paste0('Last_Season_', team_column_categories[[name]]),
          paste0('Two_Seasons_Ago_', team_column_categories[[name]]))
    }
  } else {
    #team_historical_season_stats = #read from supabase
    #opp_historical_season_stats = #read from supabase
    #team_column_categories_historical_seasons = #read from somewhere
  }
    
  team_data_combined = team_current_season_stats %>%
    left_join(team_historical_season_stats %>% mutate(season_to_match = season + 1) %>% select(-season) %>%
                rename_with(
                  .fn = ~paste0('Last_Season_', .x), 
                  .cols = -c(team, season_to_match)), join_by('team', 'season' == 'season_to_match')) %>%
    left_join(team_historical_season_stats %>% mutate(season_to_match = season + 2) %>% select(-season) %>%
                rename_with(
                  .fn = ~paste0('Two_Seasons_Ago_', .x), 
                  .cols = -c(team, season_to_match)
                ), join_by('team', 'season' == 'season_to_match')) %>%
    arrange(team, season, week) %>%
    group_by(team, team_coach) %>%
    mutate(coach_previous_weeks_with_team = row_number() - 1) %>%
    ungroup()
  
  opp_data_combined = opp_current_season_stats %>%
    left_join(opp_historical_season_stats %>% mutate(season_to_match = season + 1) %>%  select(-season) %>% rename_with(
      .fn = ~paste0('Last_Season_', .x), 
      .cols = -c(opponent_team, season_to_match)
    ), join_by('opponent_team', 'season' == 'season_to_match')) %>%
    left_join(opp_historical_season_stats %>% mutate(season_to_match = season + 2) %>%  select(-season) %>% rename_with(
      .fn = ~paste0('Two_Seasons_Ago_', .x), 
      .cols = -c(opponent_team, season_to_match)
    ), join_by('opponent_team', 'season' == 'season_to_match'))
  
  team_column_categories[['team_current_season_stats']] = c(team_column_categories[['team_current_season_stats']], 'coach_previous_weeks_with_team')
  
  return(list(team_data_combined, opp_data_combined, team_column_categories, team_redzone_drives))
}
