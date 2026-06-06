
get_player_data = function(min_year, max_year, team_redzone_drives, schedules_data)
{
  
  column_categories = list()
  
  player_bios = load_players() %>% filter((position_group %in% c('QB', 'RB', 'WR', 'TE') | ngs_position_group %in% c('QB', 'RB', 'WR', 'TE')) & (last_season >= min_year)) %>%
    select(display_name, position, position_group, ngs_position_group, birth_date, height, weight, college_name, college_conference, jersey_number, rookie_season, draft_year, latest_team, draft_round, draft_pick, draft_team, gsis_id, esb_id, nfl_id, pfr_id, pff_id, otc_id, espn_id, smart_id)
  
  #these tables use gsis_id to join:
  passing_extra_stats = load_nextgen_stats(seasons = min_year:max_year, stat_type = 'passing') %>%
    mutate(total_time_to_throw = avg_time_to_throw*attempts,
           total_completed_air_yards = avg_completed_air_yards*completions,
           total_intended_air_yards = avg_intended_air_yards*attempts,
           total_air_yards_differential = total_completed_air_yards - total_intended_air_yards,
           total_aggressiveness = aggressiveness*attempts,
           total_air_yards_to_sticks = avg_air_yards_to_sticks*attempts) %>%
    filter(week != 0) %>%
    select(season, week, team_abbr, total_time_to_throw, total_completed_air_yards, total_intended_air_yards, aggressiveness, total_air_yards_differential, total_air_yards_to_sticks, attempts, passer_rating, player_gsis_id)
  
  rushing_extra_stats = load_nextgen_stats(seasons = min_year:max_year, stat_type = 'rushing') %>%
    filter(week != 0) %>%
    mutate(total_time_before_line_of_scrimmage_seconds = avg_time_to_los*rush_attempts/10,
           total_efficiency_units = efficiency*rush_attempts,
           total_attempts_8_defenders = percent_attempts_gte_eight_defenders/100*rush_attempts) %>%
    select(season, week, team_abbr, player_gsis_id, total_time_before_line_of_scrimmage_seconds, total_efficiency_units, total_attempts_8_defenders)
  
  receiving_extra_stats = load_nextgen_stats(seasons = min_year:max_year, stat_type = 'receiving') %>%
    filter(week != 0) %>%
    left_join(passing_extra_stats %>% group_by(team_abbr,season,week) %>% summarise(total_intended_air_yards_from_passer_game = sum(total_intended_air_yards)), join_by('season','week','team_abbr')) %>%
    mutate(total_cushion = avg_cushion*targets,
           total_separation = avg_separation*targets
    ) %>% select(season, week, team_abbr, total_cushion, total_separation, total_intended_air_yards_from_passer_game, player_gsis_id)
  
  #combining the above 3 tables
  added_nextgen_stats = passing_extra_stats %>% select(-attempts) %>% full_join(rushing_extra_stats, join_by('player_gsis_id', 'week', 'season', 'team_abbr')) %>% full_join(receiving_extra_stats, join_by('player_gsis_id', 'week', 'season', 'team_abbr'))  %>%
    mutate(team_abbr = ifelse(team_abbr == 'LAR', 'LA', team_abbr))
  
  #uses gsis_id to join:
  playergl = load_player_stats(seasons = min_year:max_year) %>% select(player_id,
                                                                       season, week, team, 
                                                                       completions, attempts, passing_yards, passing_tds, passing_interceptions, sacks_suffered, sack_yards_lost, sack_fumbles, sack_fumbles_lost, passing_air_yards, passing_yards_after_catch, passing_first_downs, passing_2pt_conversions,
                                                                       carries, rushing_yards, rushing_tds, rushing_fumbles, rushing_fumbles_lost, rushing_first_downs, rushing_2pt_conversions,
                                                                       receptions, targets, receiving_yards, receiving_tds, receiving_fumbles, receiving_fumbles_lost, receiving_air_yards, receiving_yards_after_catch, receiving_first_downs, receiving_2pt_conversions, penalties, penalty_yards, special_teams_tds) %>%
    left_join(added_nextgen_stats, join_by('player_id' == 'player_gsis_id', 'week', 'season', 'team' == 'team_abbr')) %>%
    group_by(season, week, team) %>% mutate(team_total_carries = sum(carries, na.rm=T),
                                            team_total_rushing_yards = sum(rushing_yards, na.rm=T),
                                            team_total_passing_yards = sum(passing_yards, na.rm=T),
                                            team_total_passing_attempts = sum(attempts, na.rm=T),
                                            team_total_air_yards = sum(passing_air_yards, na.rm=T),
                                            team_total_intended_air_yards = sum(total_intended_air_yards)) %>% ungroup()
  
  defense_and_kickers = load_players() %>% filter(position %in% c('DE','DT','DL','LB','OLB','NT','MLB','ILB','CB','FS','S','SAF','DB') & (last_season >= min_year)) %>% select(gsis_id, pfr_id)
  
  defensegl = load_player_stats(seasons = min_year:max_year) %>% inner_join(defense_and_kickers, join_by('player_id' == 'gsis_id')) %>%
    mutate(def_pressure_score = def_sacks + def_qb_hits,
           def_tackles_score = def_tackles_solo + def_tackle_assists + def_tackles_for_loss,
           def_pass_defend_and_int_score = def_pass_defended + def_interceptions) %>%
    select(player_id, pfr_id, position, season, week, team, def_pressure_score, def_tackles_score, def_pass_defend_and_int_score, fg_made, fg_att) %>%
    group_by(season, week, team) %>% mutate(total_team_def_pressure_score = sum(def_pressure_score),
                                            total_team_def_tackles_score = sum(def_tackles_score),
                                            total_team_def_pass_defend_and_int_score = sum(def_pass_defend_and_int_score)) %>%
    ungroup()
  
  play_details = load_pbp((max(min_year,2022)):max_year)  %>% filter(!is.na(posteam))
  
  #nflverse game id and gsis player id:
  receiver_pbp_data = play_details %>% filter(!is.na(receiver_player_id)) %>%
    left_join(load_ftn_charting(max(min_year,2022):max_year) %>% select(-season, -week, -ftn_play_id), join_by( 'play_id'== 'nflverse_play_id', 'game_id' == 'nflverse_game_id')) %>%
    group_by(season, week, receiver_player_id, posteam) %>%
    summarise(catchable_balls = sum(is_catchable_ball, na.rm = TRUE),
           catchable_balls_caught = sum(is_catchable_ball & complete_pass, na.rm = TRUE),
           redzone_targets = sum(!is.na(receiver_player_id) & yardline_100 < 20)
    )
  
  rusher_pbp_data = play_details %>% filter(!is.na(rusher_player_id)) %>% filter(yardline_100 < 20) %>%
    group_by(season, week, rusher_player_id, posteam) %>%
    summarise(redzone_carries = n())
  
  time_spent_with_team = load_rosters_weekly(min_year:max_year) %>%
    filter(gsis_id != '') %>%
    select(season, week, gsis_id, team) %>%
    arrange(gsis_id, season, week) %>%
    group_by(gsis_id) %>% 
    mutate( weeks_on_current_team = sequence(rle(as.character(team))$lengths),
            is_midseason_arrival = coalesce((team != lag(team)) & (season == lag(season)), FALSE)
    ) %>%
    group_by(gsis_id, team) %>%
    mutate(first_season_with_team = min(season)) %>%
    ungroup() %>%
    mutate(is_first_year_with_team = season == first_season_with_team) %>% select(-first_season_with_team)
  
  #uses pfr id to join:
  snaps_data = load_snap_counts(seasons = min_year:max_year)  %>%
    select(season, week, team, pfr_player_id, offense_pct, offense_snaps, defense_snaps, st_snaps) %>%
    mutate(also_played_defense = ifelse(defense_snaps > 0, 1, 0),
           also_played_st = ifelse(st_snaps >0, 1, 0)) %>% select(-defense_snaps, -st_snaps) %>%
    rename(total_offense_snaps = offense_snaps) %>%
    group_by(season, team, week, pfr_player_id) %>% arrange(desc(offense_pct)) %>% slice(1) %>% ungroup()
  
  #uses pfr_player_id:
  player_advanced_passing = load_pfr_advstats(min_year:max_year, stat_type = 'pass') %>% select(pfr_player_id, season, week, team, passing_drops, passing_bad_throws, times_blitzed, times_hurried, times_hit, times_pressured)
  player_advanced_rushing = load_pfr_advstats(min_year:max_year, stat_type = 'rush') %>% select(pfr_player_id, season, week, team, rushing_yards_before_contact, rushing_yards_after_contact, rushing_broken_tackles)
  player_advanced_receiving = load_pfr_advstats(min_year:max_year, stat_type = 'rec') %>% select(pfr_player_id, season, week, team, receiving_broken_tackles, receiving_drop, receiving_int)
  
  player_adv_stats = player_advanced_passing %>% full_join(player_advanced_rushing, join_by('pfr_player_id', 'season', 'week', 'team')) %>% full_join(player_advanced_receiving, join_by('pfr_player_id', 'season', 'week', 'team'))
  #group this by game to get some high level game stats per player:
  #Roll up by team/game.
  
  
  depth_charts = load_depth_charts(min_year:max_year) %>%
    mutate(season = case_when(
      !is.na(season) ~ season,
      month(dt) %in% c(3,4,5,6,7,8,9,10,11,12) ~ year(dt),
      month(dt) %in% c(1,2) ~ year(dt) - 1,
      .default = NA),
      dt = as.POSIXct(dt),
      team = ifelse(is.na(team), club_code, team),
      depth_rank = ifelse(!is.na(depth_team), depth_team, pos_rank)) %>%
    left_join(week_dates, join_by('season', 'dt' <= 'week_end', 'dt' >= 'week_start', 'team')) %>%
    mutate(week = ifelse(is.na(week.x), week.y, week.x)) %>%
    filter(!is.na(week) & !is.na(depth_rank)) %>%
    group_by(gsis_id, team, season, week) %>% arrange(desc(dt)) %>% slice(1) %>% ungroup() %>% #get the latest depth chart info for a player in a week
    select(gsis_id, team, season, week, depth_rank) 
  
  
  data = player_bios %>%
    inner_join(snaps_data, join_by('pfr_id' == 'pfr_player_id')) %>% filter(offense_pct > 0) %>%
    left_join(playergl, join_by('gsis_id' == 'player_id', 'season', 'week', 'team')) %>%
    mutate(
      carries = coalesce(carries, 0),
      targets = coalesce(targets, 0),
      rushing_yards = coalesce(rushing_yards, 0),
      receiving_yards = coalesce(receiving_yards, 0),
      passing_yards = coalesce(passing_yards, 0),
      attempts = coalesce(attempts, 0),
      completions = coalesce(completions, 0),
      passing_tds = coalesce(passing_tds, 0),
      rushing_tds = coalesce(rushing_tds, 0),
      receiving_tds = coalesce(receiving_tds, 0)
    ) %>%
    group_by(gsis_id) %>% arrange(season, week) %>% mutate(games_played = (row_number()-1)) %>%
    ungroup() %>%
    group_by(gsis_id, season) %>% arrange(week) %>% mutate(games_played_this_season = (row_number() - 1)) %>%
    ungroup() %>%
    left_join(receiver_pbp_data, join_by('gsis_id' == 'receiver_player_id', 'team' == 'posteam', 'season', 'week')) %>%
    left_join(rusher_pbp_data, join_by('gsis_id' == 'rusher_player_id', 'team' == 'posteam', 'season', 'week')) %>%
    left_join(team_redzone_drives, join_by('team' == 'posteam', 'season', 'week')) %>%
    left_join(time_spent_with_team, join_by('gsis_id', 'season', 'week', 'team')) %>%
    left_join(player_adv_stats, join_by('pfr_id' == 'pfr_player_id', 'season', 'week', 'team')) %>%
    left_join(depth_charts, join_by('gsis_id', 'team', 'season', 'week')) %>%
    mutate(tds = rushing_tds + receiving_tds + special_teams_tds,
           anytime_td_scorer = tds > 0)
  
  #matchups against particular opponents:
  player_data_with_matchups = data %>% left_join(schedules_data %>% select(season, week, team, opponent) %>% distinct(), join_by('season', 'week', 'team')) %>%
    arrange(gsis_id, season, week) %>%
    group_by(gsis_id) %>%
    mutate(average_passing_career = slide_dbl(.x = passing_yards, .f = mean, na.rm = TRUE, .before = Inf, .after = -1),
           average_rushing_career = slide_dbl(.x = rushing_yards, .f = mean, na.rm = TRUE, .before = Inf, .after = -1),
           average_receiving_career = slide_dbl(.x = receiving_yards, .f = mean, na.rm = TRUE, .before = Inf, .after = -1),
           average_tds_career = slide_dbl(.x = tds, .f = mean, na.rm = TRUE, .before = Inf, .after = -1)) %>%
    group_by(gsis_id, opponent) %>%
    mutate(average_passing_against_opp = slide_dbl(.x = passing_yards, .f = mean, na.rm = TRUE, .before = Inf, .after = -1),
           average_rushing_against_opp = slide_dbl(.x = rushing_yards,  .f = mean, na.rm = TRUE, .before = Inf, .after = -1),
           average_receiving_against_opp = slide_dbl(.x = receiving_yards,  .f = mean, na.rm = TRUE, .before = Inf, .after = -1),
           average_tds_against_opp= slide_dbl(.x = tds,  .f = mean, na.rm = TRUE, .before = Inf, .after = -1),
           total_games_against_opp = slide_dbl(.x = passing_yards, .f = length, .before = Inf, .after = -1),
           passing_matchup_average_against_opp = (average_passing_against_opp*total_games_against_opp +
                                                    average_passing_career * 2)/(2+total_games_against_opp),
           rushing_matchup_average_against_opp = (average_rushing_against_opp*total_games_against_opp +
                                                    average_rushing_career * 2)/(2+total_games_against_opp),
           receiving_matchup_average_against_opp = (average_receiving_against_opp*total_games_against_opp +
                                                      average_receiving_career * 2)/(2+total_games_against_opp),
           td_matchup_average_against_opp = (average_tds_against_opp*total_games_against_opp +
                                               average_tds_career * 2)/(2+total_games_against_opp),
           passing_weighted_matchup_ratio_against_opp = passing_matchup_average_against_opp/average_passing_career,
           rushing_weighted_matchup_ratio_against_opp = rushing_matchup_average_against_opp/average_rushing_career,
           receiving_weighted_matchup_ratio_against_opp = receiving_matchup_average_against_opp/average_receiving_career,
           td_weighted_matchup_ratio_against_opp = td_matchup_average_against_opp/average_tds_career
    ) %>% ungroup() %>% select(-average_passing_career, -average_rushing_career, -average_receiving_career, -average_tds_career,
                 -average_passing_against_opp, -average_rushing_against_opp,-average_receiving_against_opp, -average_tds_against_opp,
                 -passing_matchup_average_against_opp, -rushing_matchup_average_against_opp, -receiving_matchup_average_against_opp, -td_matchup_average_against_opp, -opponent)
  
  
  data = player_data_with_matchups
  
  uninformative_stats_results = remove_uninformative_stats(df = data,
                                                           column_list = setdiff(colnames(data), c(colnames(player_bios), colnames(time_spent_with_team), colnames(depth_charts), 'game_id')),
                                                           missing_threshold = 0.98)
  
  columns_to_remove = uninformative_stats_results[[1]]
  data = data %>% select(-any_of(columns_to_remove))
  
  cols_for_historical_calculations = setdiff(colnames(data), c(colnames(player_bios), colnames(time_spent_with_team), colnames(depth_charts), 'game_id',
                                                               colnames(data)[str_detect(colnames(data), 'total_|games_played|matchup_ratio')]))
  
  column_categories[['identifiers']] = c(
    'season', 'team', 'game_id', 'gsis_id', 'display_name'
  )
  
  column_categories[['bio_data']] = setdiff(colnames(player_bios)[!str_detect(colnames(player_bios), '_id')], c(column_categories[['identifiers']], 'birth_date'))
  
  column_categories[['usage_and_depth']] = c(
    'games_played', 'games_played_this_season',
    setdiff(colnames(depth_charts), c(column_categories[['identifiers']], 'week')),
    setdiff(colnames(time_spent_with_team), c(column_categories[['identifiers']], 'week'))
  )
  
  
  return(list(data, defensegl, column_categories, uninformative_stats_results, cols_for_historical_calculations))
}


summarize_current_season_player_stats = function(data, defense_data, calc_metrics, column_categories, uninformative_stats_results, cols_for_historical_calculations)
{
  
  columns_0_1 = c(uninformative_stats_results[[2]], 'offense_pct')
  low_medians = uninformative_stats_results[[3]]
  
  data_with_historical_calculations = data %>%
    arrange(season, gsis_id, week) %>%
    group_by(season, gsis_id) %>%
    group_modify(~ compute_slider_cumulatives(.x, cols_for_historical_calculations, cumulative_only = FALSE)) %>%
    group_modify(~ compute_slider_cumulatives(.x, colnames(data)[str_detect(colnames(data), 'total_')], cumulative_only = TRUE)) %>%
    arrange(week) %>% mutate(receiving_yards_lag1 = lag(receiving_yards),
                             receiving_yards_lag2 = lag(receiving_yards, n = 2),
                             receiving_yards_lag3 = lag(receiving_yards, n = 3),
                             passing_yards_lag1 = lag(passing_yards),
                             passing_yards_lag2 = lag(passing_yards, n = 2),
                             passing_yards_lag3 = lag(passing_yards, n = 3),
                             rushing_yards_lag1 = lag(rushing_yards),
                             rushing_yards_lag2 = lag(rushing_yards, n = 2),
                             rushing_yards_lag3 = lag(rushing_yards, n = 3),
                             tds_lag1 = lag(tds),
                             tds_lag2 = lag(tds, n = 2),
                             tds_lag3 = lag(tds, n = 3))
  
  
  defense_with_historical_calculations = defense_data %>%
    arrange(season, player_id, week) %>%
    group_by(season, player_id, pfr_id, position) %>%
    group_modify(~ compute_slider_cumulatives(.x, setdiff(colnames(defense_data), c('player_id', 'pfr_id', 'position','season','week','team', colnames(defense_data)[str_detect(colnames(defense_data), 'total_')])), cumulative_only = FALSE)) %>%
    group_modify(~ compute_slider_cumulatives(.x, colnames(defense_data)[str_detect(colnames(defense_data), 'total_')], cumulative_only = TRUE))
  
  if(length(columns_0_1) > 0)
  {
    data_with_historical_calculations = data_with_historical_calculations %>% select(-any_of(c(paste0('sd_', columns_0_1), paste0('median_', columns_0_1), paste0('min_', columns_0_1),paste0('max_', columns_0_1))))
    data_with_historical_calculations = data_with_historical_calculations %>% select(-any_of(c(paste0('last3_sd_', columns_0_1), paste0('last3_median_', columns_0_1), paste0('last3_min_', columns_0_1),paste0('last3_max_', columns_0_1))))
  }
  if(length(low_medians) > 0)
  {
    data_with_historical_calculations = data_with_historical_calculations  %>% select(-any_of(c(paste0('median_', low_medians), paste0('min_', low_medians))))
    data_with_historical_calculations = data_with_historical_calculations  %>% select(-any_of(c(paste0('last3_median_', low_medians), paste0('last3_min_', low_medians))))
  }
  
  data_with_historical_calculations_and_efficiency_metrics = calculate_efficiency_metrics(df = data_with_historical_calculations,
                                                                                          calc_metrics = calc_metrics,
                                                                                          include_last3 = TRUE) %>%
    mutate(air_yards_differential = air_yards_per_attempt - air_yards_per_completion) %>%
    select(!matches('cumulative_|sum_')) %>%
    select(!matches('team_drives|team_touchdowns|team_redzone'))
  
  defense_with_historical_calculations_and_efficiency_metrics = calculate_efficiency_metrics(df = defense_with_historical_calculations,
                                                                                             calc_metrics = list(fg_rate_this_season = c('cumulative_fg_made', 'cumulative_fg_att'),
                                                                                                                 pct_share_of_pressures = c('cumulative_def_pressure_score','cumulative_total_team_def_pressure_score'),
                                                                                                                 pct_share_of_tackles = c('cumulative_def_tackles_score', 'cumulative_total_team_def_tackles_score'),
                                                                                                                 pct_share_of_pass_defense_and_int = c('cumulative_def_pass_defend_and_int_score', 'cumulative_total_team_def_pass_defend_and_int_score')),
                                                                                             include_last3 = FALSE) %>%
    select(!matches('cumulative_|sum_|sd_|cv_|min_|max_|median_|last3|total')) %>% select(-avg_fg_made, -avg_fg_att)
  
  column_categories[['usage_and_depth']] = c(column_categories[['usage_and_depth']],
                                             colnames(data_with_historical_calculations_and_efficiency_metrics)[str_detect(colnames(data_with_historical_calculations_and_efficiency_metrics), 'offense_pct|also_played')])
  
  # Opponent Matchup History (Calculated explicitly in your data pipeline)
  column_categories[['matchup_history']] = c(
    'total_games_against_opp',
    'passing_weighted_matchup_ratio_against_opp',
    'rushing_weighted_matchup_ratio_against_opp',
    'receiving_weighted_matchup_ratio_against_opp',
    'td_weighted_matchup_ratio_against_opp'
  )
  
  #do one for stats and seasonal stats and ranking:
  
  #column_categories[['past_season_summary_stats']] = c(paste0('Last_Season_',setdiff(colnames(player_seasonal_stats), c('gsis_id','season'))),
  #paste0('Two_Seasons_Ago_',setdiff(colnames(player_seasonal_stats), c('gsis_id','season'))))
  
  present_stats_to_remove = setdiff(
    gsub('max_|min_|avg_|sd_|median_|pct_|_pct|_per_.*|average_|mean_|cv_|last3_|_lag[0-9]', '',
         colnames(data_with_historical_calculations_and_efficiency_metrics)[str_detect(colnames(data_with_historical_calculations_and_efficiency_metrics), 'max_|min_|avg_|sd_|median_|pct_|_pct|per_|differential|rating|average_|mean_|cv_|last3_|lag[0-9]|ratio|team_|total_')]),
    c('passing_yards', 'rushing_yards', 'receiving_yards', 'anytime_td_scorer', 'receptions', 'games_played', 'games_played_this_season', 'total_games_against_opp',
      colnames(data_with_historical_calculations_and_efficiency_metrics)[str_detect(colnames(data_with_historical_calculations_and_efficiency_metrics), 'weighted_matchup')],
      names(calc_metrics))
  )
  
  player_dat = data_with_historical_calculations_and_efficiency_metrics %>% select(-any_of(present_stats_to_remove))
  
  stats_fields = colnames(player_dat)[str_detect(colnames(player_dat), 'max_|min_|avg_|sd_|median_|pct_|_pct|per_|differential|rating|average_|mean_|cv_|last3_|lag[0-9]|ratio')]
  column_categories[['passing_current_season_stats']] = setdiff(stats_fields[str_detect(stats_fields, 'passing|passer|passes|completion|attempt|air_yards|aggressive|sack|hurried|blitz|pressure|_hit') &
                                                                               !str_detect(stats_fields, 'rushing|receiving')], c('pct_share_of_intended_air_yards', 'last3_pct_share_of_intended_air_yards'))
  column_categories[['rushing_current_season_stats']] = stats_fields[str_detect(stats_fields, 'rushing|carries')]
  column_categories[['receiving_current_season_stats']] = c(setdiff(stats_fields[str_detect(stats_fields, 'receiving|reception|target|catchable|separation|opportunity_rating|share_of_intended')], c('average_depth_of_target_passer', 'last3_average_depth_of_target_passer')),
                                                            'pct_share_of_intended_air_yards', 'last3_pct_share_of_intended_air_yards')
  column_categories[['other_current_season_stats']] = stats_fields[str_detect(stats_fields, 'td|penal') & !str_detect(stats_fields, 'passing|rushing|receiving')] 
  
  #make sure nothing is in remaining or it won't be captured in the column categories:
  missing_cols = setdiff(colnames(player_dat),
          c('passing_yards','rushing_yards','receiving_yards','receptions','anytime_td_scorer',
            column_categories[['identifiers']],
            column_categories[['bio_data']],
            column_categories[['usage_and_depth']],
            column_categories[['matchup_history']],
            column_categories[['passing_past_season_stats']],
            column_categories[['rushing_past_season_stats']],
            column_categories[['receiving_past_season_stats']],
            column_categories[['other_past_season_stats']],
            column_categories[['past_season_usage_and_depth']],
            column_categories[['passing_current_season_stats']],
            column_categories[['rushing_current_season_stats']],
            column_categories[['receiving_current_season_stats']],
            column_categories[['other_current_season_stats']]))
  if(length(missing_cols) > 0)
  {
    print('Columnsnot accounted for in column categories:')
    print(missing_cols)
  }
  
  snaps_data = load_snap_counts(seasons = min(data$season):max(data$season)) %>%
    select(season, week, pfr_player_id, defense_pct) 
  
  return(list(player_dat %>% select(-any_of(c("esb_id", "nfl_id", "pfr_id", "pff_id", "otc_id", "espn_id", "smart_id", 'team_abbr', 'posteam'))),
               defense_with_historical_calculations_and_efficiency_metrics %>% left_join(snaps_data %>% select(pfr_player_id, season, week, defense_pct), join_by('season', 'week','pfr_id' == 'pfr_player_id')),
              column_categories))
  
}

calculate_player_seasonal_historical_stats = function(data, defense_data, column_categories, calc_metrics, cols_for_historical_calculations)
{
  
  player_seasonal_stats = data %>%
    group_by(gsis_id, season) %>%
    summarise(weeks_active = length(unique(week)),
              across(all_of(setdiff(c(colnames(data)[str_detect(colnames(data), 'total_')], cols_for_historical_calculations), c('games_played', 'games_played_this_season'))),
                     .fns = list(sum = ~sum(.x, na.rm = TRUE),
                                 mean = ~mean(.x, na.rm = TRUE),
                                 median = ~median(.x, na.rm = TRUE),
                                 max = ~max(.x, na.rm = TRUE),
                                 min = ~min(.x, na.rm = TRUE),
                                 sd = ~sd(.x, na.rm = TRUE)
                     ), .names = "{.fn}_{.col}"), .groups = "drop") %>%
    group_by(gsis_id, season) %>%
    ungroup() %>%
    rename_with(~ gsub("sum_", "cumulative_", .x), starts_with("sum_")) %>%
    rename_with(~ gsub("mean_", "avg_", .x), starts_with("mean_")) %>%
    calculate_efficiency_metrics(calc_metrics = calc_metrics,
                                 include_last3 = FALSE) %>%
    mutate(air_yards_differential = air_yards_per_attempt - air_yards_per_completion,
           weighted_opportunity_rating = 0.7*pct_share_of_targets + 0.3*pct_share_of_intended_air_yards) %>%
    select(!matches('cumulative_|sum_|total_')) %>%
    select(!matches('team_drives|team_touchdowns|team_redzone'))
  
  defense_seasonal_stats = defense_data %>%
    group_by(player_id, position, season) %>%
    summarise(weeks_active = length(unique(week)),
              across(all_of(setdiff(colnames(defense_data), c('player_id','pfr_id','position','season','week','team'))),
                     .fns = list(sum = ~sum(.x, na.rm = TRUE),
                                 mean = ~mean(.x, na.rm = TRUE),
                                 median = ~median(.x, na.rm = TRUE),
                                 max = ~max(.x, na.rm = TRUE),
                                 min = ~min(.x, na.rm = TRUE),
                                 sd = ~sd(.x, na.rm = TRUE)
                     ), .names = "{.fn}_{.col}"), .groups = "drop") %>%
                       calculate_efficiency_metrics(calc_metrics = list(fg_rate = c('sum_fg_made', 'sum_fg_att'),
                                                                        pct_share_of_pressures = c('sum_def_pressure_score','sum_total_team_def_pressure_score'),
                                                                        pct_share_of_tackles = c('sum_def_tackles_score', 'sum_total_team_def_tackles_score'),
                                                                        pct_share_of_pass_defense_and_int = c('sum_def_pass_defend_and_int_score', 'sum_total_team_def_pass_defend_and_int_score')),
                                                    include_last3 = FALSE) %>%
                       select(!matches('cumulative_|sum_|sd_|cv_|min_|max_|median_|last3|total')) %>% rename_with(.fn = ~gsub('mean','avg', .x)) %>% select(-avg_fg_made, -avg_fg_att)
    
  
  seasonal_stats_fields = colnames(player_seasonal_stats)[str_detect(colnames(player_seasonal_stats), 'max_|min_|avg_|sd_|median_|pct_|_pct|per_|differential|rating|average_|mean_|cv_|last3_|lag[0-9]|ratio')]
  column_categories[['passing_past_season_stats']] = setdiff(seasonal_stats_fields[str_detect(seasonal_stats_fields, 'passing|passer|passes|completion|attempt|air_yards|aggressive|sack|hurried|blitz|pressure|_hit') & !str_detect(seasonal_stats_fields,'rushing|receiving|total_intended_air')], 'pct_share_of_intended_air_yards')
  column_categories[['rushing_past_season_stats']] = seasonal_stats_fields[str_detect(seasonal_stats_fields, 'rushing|carries')]
  column_categories[['receiving_past_season_stats']] = c(setdiff(seasonal_stats_fields[str_detect(seasonal_stats_fields, 'receiving|reception|cushion|target|catchable|opportunity_rating|separation|share_of_intended')], 'average_depth_of_target_passer'),
                                                         'pct_share_of_intended_air_yards')
  column_categories[['other_past_season_stats']] = seasonal_stats_fields[str_detect(seasonal_stats_fields, 'td|penal') & !str_detect(seasonal_stats_fields, 'passing|rushing|receiving')]
  column_categories[['past_season_usage_and_depth']] = c('weeks_active', seasonal_stats_fields[str_detect(seasonal_stats_fields, 'offense_pct|also_played')])
  
  missing_cols = setdiff(colnames(player_seasonal_stats),
          c(column_categories[['passing_past_season_stats']],
            column_categories[['rushing_past_season_stats']],
            column_categories[['receiving_past_season_stats']],
            column_categories[['other_past_season_stats']],
            column_categories[['past_season_usage_and_depth']],
            'gsis_id','season'
          ))
  if(length(missing_cols) > 0)
  {
    print('player seasonal stats missing from column categories:')
    print(missing_cols)
  }
  
  
  
  return(list(player_seasonal_stats,
              defense_seasonal_stats,
              column_categories
  ))
}


pull_all_player_stats = function(min_year, max_year, team_redzone_drives, recalculate_seasonal = FALSE)
{
  player_data = get_player_data(min_year, max_year, schedules_data = schedules_raw, team_redzone_drives = team_redzone_drives)
  offense_data = player_data[[1]]
  defense_data = player_data[[2]]
  
  player_current_season_stats = summarize_current_season_player_stats(data = offense_data, defense_data = defense_data, calc_metrics = calc_metrics,
                                                                      column_categories = player_data[[3]], uninformative_stats_results = player_data[[4]], cols_for_historical_calculations = player_data[[5]])
  offense_current_season_stats = player_current_season_stats[[1]]
  defense_current_season_stats = player_current_season_stats[[2]]
  column_categories_current_season = player_current_season_stats[[3]]

  if(recalculate_seasonal == TRUE)
  {
    player_historical_season_stats = calculate_player_seasonal_historical_stats(data = offense_data, defense_data = defense_data, column_categories = player_current_season_stats[[3]], calc_metrics = calc_metrics, cols_for_historical_calculations = player_data[[5]])
    offense_historical_season_stats = player_historical_season_stats[[1]]
    defense_historical_season_stats = player_historical_season_stats[[2]]
    column_categories_historical_seasons = player_historical_season_stats[[3]]
    
    for(name in c('passing_past_season_stats', 'rushing_past_season_stats', 'receiving_past_season_stats', 'other_past_season_stats', 'past_season_usage_and_depth'))
    {
      column_categories_historical_seasons[[name]] = 
        c(paste0('Last_Season_', column_categories_historical_seasons[[name]]),
          paste0('Two_Seasons_Ago_', column_categories_historical_seasons[[name]]))
    }
  } else {
    #offense_historical_season_stats = #read from supabase
    #defense_historical_season_stats = #read from supabase
    #column_categories_historical_seasons = #read from somewhere
  }
  
  player_data_combined = offense_current_season_stats%>%
    left_join(offense_historical_season_stats %>% mutate(season_to_match = season + 1)  %>% select(-season) %>% rename_with(~paste0('Last_Season_', .x), -any_of(c('gsis_id', 'season_to_match'))), join_by('gsis_id', 'season' == 'season_to_match')) %>%
    left_join(offense_historical_season_stats %>% mutate(season_to_match = season + 2) %>% select(-season) %>% rename_with(~paste0('Two_Seasons_Ago_',.x), -any_of(c('gsis_id', 'season_to_match'))), join_by('gsis_id', 'season' == 'season_to_match'))
  
  player_column_categories = c(column_categories_current_season, column_categories_historical_seasons)
  
  defense_data_combined = defense_current_season_stats %>%
    left_join(defense_historical_season_stats %>% mutate(season_to_match = season + 1)  %>% select(-season) %>% rename_with(~paste0('Last_Season_', .x), -any_of(c('player_id', 'season_to_match'))), join_by('player_id', 'season' == 'season_to_match')) %>%
    left_join(defense_historical_season_stats %>% mutate(season_to_match = season + 2) %>% select(-season) %>% rename_with(~paste0('Two_Seasons_Ago_',.x), -any_of(c('player_id', 'season_to_match'))), join_by('player_id', 'season' == 'season_to_match')) %>%
    rename('gsis_id' = 'player_id')
  
  return(list(player_data_combined, defense_data_combined, player_column_categories))
}
