library(slider)
library(dplyr)
library(nflreadr)
library(jsonlite)

min_year = 2020
max_year = 2025


compute_slider_cumulatives = function(df, cols_to_include, cumulative_only) {
  df = df %>% arrange(week)
  
  for (c in cols_to_include)
  {
    vals = df[[c]]
    
    if(cumulative_only == FALSE)
    {
      # Cumulative up to each game
      df[[paste0("cumulative_", c)]] = slide_dbl(vals, ~ ifelse(length(.x) == 0, NA_real_, sum(.x, na.rm = TRUE)), .before = Inf, .after = -1, .complete = TRUE)
      df[[paste0("avg_", c)]] = slide_dbl(vals, ~ ifelse(length(.x) == 0, NA_real_, mean(.x, na.rm = TRUE)), .before = Inf, .after = -1, .complete = TRUE)
      df[[paste0("median_", c)]]     = slide_dbl(vals, ~ ifelse(length(.x) == 0, NA_real_, median(.x, na.rm = TRUE)), .before = Inf, .after = -1,  .complete = TRUE)
      df[[paste0("min_", c)]]        = slide_dbl(vals, ~ ifelse(length(.x) == 0, NA_real_, min(.x, na.rm = TRUE)), .before = Inf,  .after = -1, .complete = TRUE)
      df[[paste0("max_", c)]]        = slide_dbl(vals, ~ ifelse(length(.x) == 0, NA_real_,max(.x, na.rm = TRUE)), .before = Inf, .after = -1,  .complete = TRUE)
      df[[paste0("sd_", c)]]         = slide_dbl(vals, ~ ifelse(length(.x) == 0, NA_real_, sd(.x, na.rm = TRUE)), .before = Inf, .after = -1,  .complete = TRUE)
      df[[paste0("cv_", c)]]         = ifelse(is.na(df[[paste0("avg_", c)]]), NA, (df[[paste0("sd_", c)]]/df[[paste0("avg_", c)]]))
      
      # Last 3 games
      df[[paste0("last3_cumulative_", c)]] = slide_dbl(vals, ~ ifelse(length(.x) == 0, NA_real_, sum(.x, na.rm = TRUE)), .before = 3, .after = -1, .complete = TRUE)
      df[[paste0("last3_avg_", c)]]        = slide_dbl(vals, ~ ifelse(length(.x) == 0, NA_real_, mean(.x, na.rm = TRUE)), .before = 3, .after = -1,  .complete = TRUE)
      df[[paste0("last3_median_", c)]]     = slide_dbl(vals, ~ ifelse(length(.x) == 0, NA_real_, median(.x, na.rm = TRUE)), .before = 3, .after = -1,   .complete = TRUE)
      df[[paste0("last3_min_", c)]]        = slide_dbl(vals, ~ ifelse(length(.x) == 0, NA_real_, min(.x, na.rm = TRUE)), .before = 3, .after = -1,  .complete = TRUE)
      df[[paste0("last3_max_", c)]]        = slide_dbl(vals, ~ ifelse(length(.x) == 0, NA_real_, max(.x, na.rm = TRUE)), .before = 3, .after = -1,  .complete = TRUE)
      df[[paste0("last3_sd_", c)]]         = slide_dbl(vals, ~ ifelse(length(.x) == 0, NA_real_, sd(.x, na.rm = TRUE)), .before = 3, .after = -1,  .complete = TRUE)
      df[[paste0("last3_cv_", c)]]         = ifelse(is.na(df[[paste0("last3_avg_", c)]]), NA, (df[[paste0("last3_sd_", c)]]/df[[paste0("last3_avg_", c)]]))
    } else{
      df[[paste0("cumulative_", c)]] = slide_dbl(vals, ~ ifelse(length(.x) == 0, NA_real_, sum(.x, na.rm = TRUE)), .before = Inf, .after = -1, .complete = TRUE)
      df[[paste0("last3_cumulative_", c)]] = slide_dbl(vals, ~ ifelse(length(.x) == 0, NA_real_, sum(.x, na.rm = TRUE)), .before = 3, .after = -1, .complete = TRUE)
    }
  }
  
  return(df)
}


SUPABASE_URL <- "https://tvvhvzodwrbkgdpzzrxq.supabase.co"
SUPABASE_KEY <- "sb_publishable_K2dD8bfEwXpx0koy8t4tLA_mZ5BJN_Z"

get_supabase_data = function(schema, table_name, additional_sql = '', select = '*')
{
  url <- paste0(SUPABASE_URL, "/rest/v1/", table_name, "?select=", select, additional_sql)
  
  # Make the Request
  response <- GET(
    url,
    add_headers(
      "apikey" = SUPABASE_KEY,
      "Authorization" = paste("Bearer", SUPABASE_KEY),
      # CRITICAL: This tells Supabase which schema to look in!
      "Accept-Profile" = schema 
    )
  )
  return(fromJSON(content(response, 'text', encoding = 'UTF-8')))
}

clean_names = function(name)
{
  return(tolower(name) %>% str_remove_all("[[:punct:]]+") %>% str_remove("\\b(jr|sr|i{1,3}|iv|v|vi{1,3}|ix|x|xi{1,3})\\b") %>% str_squish() %>% trimws())
}

calculate_efficiency_metrics = function(df, week, calc_metrics, include_last3 = FALSE)
{
  for (metric_name in names(calc_metrics)) {
    vars = calc_metrics[[metric_name]]
    if (all(vars %in% names(df))) {
      df[[metric_name]] = df[[vars[1]]] / df[[vars[2]]]
      if(include_last3)
      {
        df[[paste0('last3_', metric_name)]] = df[[paste0('last3_', vars[1])]] / df[[paste0('last3_', vars[2])]]
      }
    }
  }
  return(df)
}

remove_uninformative_stats = function(df, column_list, missing_threshold)
{
  columns_to_remove = c()
  columns_0_1 = c()
  low_medians = c()
  for (c in column_list)
  {
    pct_nonmissing = mean(!is.na(df[,c]))
    unique_values = df %>% select(!!sym(c)) %>% pull() %>% unique() %>% na.omit() %>% as.numeric()
    median = df %>% select(!!sym(c)) %>% pull() %>% median(na.rm =TRUE)
    mean = df %>% select(!!sym(c)) %>% pull() %>% mean(na.rm = TRUE)
    if((pct_nonmissing > (1-missing_threshold)) & (length(unique_values) > 1))
    {
      if(all(unique_values[which(!is.na(unique_values))] %in% c(0,1, TRUE, FALSE)))
      {
        columns_0_1 = c(columns_0_1, c)
      } else if (!is.na(median) && (median == 0 & mean < 1 & length(unique_values) < 10))
      {
        low_medians = c(low_medians, c)
      }
    } else {
      columns_to_remove = c(columns_to_remove, c)
    }
  }
  return(list(columns_to_remove, columns_0_1, low_medians))
}


#data:

schedules_raw = load_schedules(min_year:max_year) %>% clean_homeaway() 

#fix the case for the week after a bye week
week_end_dates_by_team = schedules_raw %>% group_by(season, week, team) %>% summarise(week_end = max(paste(gameday, gametime)), .groups = 'drop')
previous_game_info = week_end_dates_by_team %>% 
  ungroup() %>% 
  group_by(season, team) %>% 
  arrange(week) %>%
  mutate(previous_game_end = lag(week_end)) %>% 
  ungroup() %>% select(-week_end)

week_dates = week_end_dates_by_team %>% 
  left_join(previous_game_info, join_by('season', 'team', 'week')) %>%
  mutate(
    week_start = case_when(
      is.na(previous_game_end) ~ as.POSIXct(paste0(season, '-08-01 00:00:00')),
      TRUE ~ as.POSIXct(previous_game_end) + 1
    ),
    week_end = as.POSIXct(week_end)
  ) %>% select(-previous_game_end)

team_lookup_table = get_supabase_data(schema = 'MainData', table_name = 'TeamLookup')


calc_metrics = list(
  passing_comp_pct = c("cumulative_completions", "cumulative_attempts"),
  receiving_catch_pct = c("cumulative_receptions", "cumulative_targets"),
  passing_yards_per_completion = c("cumulative_passing_yards", "cumulative_completions"),
  passing_yards_per_attempt = c("cumulative_passing_yards", "cumulative_attempts"),
  passing_pct_bad_throws = c('cumulative_passing_bad_throws', 'cumulative_attempts'),
  rushing_yards_per_attempt = c("cumulative_rushing_yards", 'cumulative_carries'),
  rushing_first_downs_per_attempt = c("cumulative_rushing_first_downs", "cumulative_carries"),
  rushing_yards_after_contact_per_attempt = c("cumulative_rushing_yards_after_contact", "cumulative_carries"),
  receiving_yards_per_target = c("cumulative_receiving_yards", "cumulative_targets"),
  receiving_yards_per_reception = c("cumulative_receiving_yards", "cumulative_receptions"),
  receiving_first_downs_per_target = c("cumulative_receiving_first_downs", "cumulative_targets"),
  receiving_first_downs_per_reception = c("cumulative_receiving_first_downs", "cumulative_receptions"),
  receiving_air_yards_per_target = c("cumulative_receiving_air_yards", "cumulative_targets"),
  receiving_air_yards_per_reception = c("cumulative_receiving_air_yards", "cumulative_receptions"),
  receiving_YAC_per_target = c("cumulative_receiving_yards_after_catch", "cumulative_targets"),
  receiving_YAC_per_reception = c("cumulative_receiving_yards_after_catch", "cumulative_receptions"),
  receiving_cushion_per_play = c("cumulative_total_cushion", "cumulative_targets"),
  receiving_separation_per_target = c("cumulative_total_separation", "cumulative_targets"),
  rushing_efficiency_per_play = c('cumulative_total_efficiency_units', 'cumulative_carries'),
  time_to_throw_per_attempt = c('cumulative_total_time_to_throw', 'cumulative_attempts'),
  air_yards_per_completion = c('cumulative_passing_air_yards', 'cumulative_completions'),
  air_yards_per_attempt = c('cumulative_total_intended_air_yards', 'cumulative_attempts'),
  aggressive_passes_pct = c('cumulative_aggressiveness', 'cumulative_attempts'),
  air_yards_to_sticks_per_attempt = c('cumulative_total_air_yards_to_sticks', 'cumulative_attempts'),
  time_before_line_of_scrimmage_per_rushing_play = c('cumulative_total_time_before_line_of_scrimmage_seconds', 'cumulative_carries'),
  rushing_yards_per_carry_facing_8_defenders = c('cumulative_rushing_yards', 'cumulative_total_attempts_8_defenders'),
  pct_share_of_targets = c('cumulative_targets', 'cumulative_team_total_passing_attempts'),
  pct_share_of_targets_redzone = c('cumulative_redzone_targets', 'cumulative_team_redzone_receiving_plays'),
  pct_share_of_intended_air_yards = c('cumulative_receiving_air_yards', 'cumulative_total_intended_air_yards_from_passer_game'),
  pct_share_of_carries = c('cumulative_carries', 'cumulative_team_total_carries'),
  pct_share_of_carries_redzone = c('cumulative_redzone_carries', 'cumulative_team_redzone_rushing_plays'),
  pct_share_of_rushing_yards = c('cumulative_rushing_yards', 'cumulative_team_total_rushing_yards'),
  average_depth_of_target_receiver = c('cumulative_receiving_air_yards', 'cumulative_targets'),
  average_depth_of_target_passer = c('cumulative_total_intended_air_yards', 'cumulative_attempts'),
  receiving_yards_per_snap = c('cumulative_receiving_yards', 'cumulative_total_offense_snaps'),
  receiving_air_yards_conversion_ratio = c('cumulative_receiving_yards', 'cumulative_receiving_air_yards')
)  

team_calc_metrics = list(
  team_differential_per_win = c('cumulative_team_differential', 'cumulative_team_win'),
  team_pct_plays_third_down = c('cumulative_team_total_third_downs', 'cumulative_team_num_plays'),
  team_pct_third_down_conversion = c('cumulative_team_converted_third_downs', 'cumulative_team_total_third_downs_attempted'),
  team_pct_plays_fourth_down_attempts = c('cumulative_team_total_fourth_downs_attempted', 'cumulative_team_num_plays'),
  team_pct_fourth_down_conversion = c('cumulative_team_converted_fourth_downs', 'cumulative_team_total_fourth_downs_attempted'),
  team_run_pass_per_play = c('cumulative_team_total_run_pass', 'cumulative_team_num_plays'),
  team_offense_backfield_per_play = c('cumulative_team_total_offense_backfield', 'cumulative_team_num_plays'),
  team_pct_plays_qb_under_center = c('cumulative_team_sum_qb_under_center', 'cumulative_team_num_plays'),
  team_pct_plays_qb_shotgun = c('cumulative_team_sum_qb_shotgun', 'cumulative_team_num_plays'),
  team_pct_plays_screen_passes = c('cumulative_team_screen_passes', 'cumulative_team_num_plays'),
  team_pct_plays_qb_sneaks = c('cumulative_team_qb_sneaks', 'cumulative_team_num_plays'),
  team_pct_pct_catchable_balls = c('cumulative_team_catchable_balls', 'cumulative_team_attempts'),
  team_pct_trick_plays = c('cumulative_team_trick_plays', 'cumulative_team_num_plays'),
  team_pct_plays_qb_pistol = c('cumulative_team_sum_qb_pistol', 'cumulative_team_num_plays'),
  team_pct_plays_play_action = c('cumulative_team_play_action', 'cumulative_team_num_plays'),
  team_pct_redzone_drives_td_result = c('cumulative_team_touchdowns_in_redzone', 'cumulative_team_drives_in_redzone'),
  team_pct_drives_in_redzone = c('cumulative_team_drives_in_redzone', 'cumulative_team_drives'),
  team_fg_dependency = c('cumulative_team_total_fg_attempts','cumulative_team_drives'),
  team_pace = c('cumulative_team_time_of_possession', 'cumulative_team_num_plays')
) 

opp_calc_metrics = list(
  opp_defense_pct_converted_third_downs_allowed = c('cumulative_opp_defense_converted_third_downs_allowed', 'cumulative_opp_defense_total_third_downs_attempted_allowed'),
  opp_defense_pct_converted_fourth_downs_allowed = c('cumulative_opp_defense_converted_fourth_downs_allowed', 'cumulative_opp_defense_total_fourth_downs_attempted_allowed'),
  opp_defense_blitzers_per_play = c('cumulative_opp_defense_total_blitzers', 'cumulative_opp_defense_num_plays'),
  opp_defense_pass_rushers_per_play = c('cumulative_opp_defense_total_pass_rushers', 'cumulative_opp_defense_num_plays'),
  opp_defense_box_per_play = c('cumulative_opp_total_defense_box', 'cumulative_opp_defense_num_plays'),
  opp_pct_redzone_drives_td_result = c('cumulative_opp_defense_touchdowns_in_redzone_allowed', 'cumulative_opp_defense_drives_in_redzone_allowed'),
  opp_pct_drives_in_redzone = c('cumulative_opp_defense_drives_in_redzone_allowed', 'cumulative_opp_defense_drives_allowed'),
  opp_passing_yards_per_attempt_allowed = c('cumulative_opp_defense_passing_yards_allowed', 'cumulative_opp_defense_attempts_allowed'),
  opp_sacks_forced_per_attempt_allowed = c('cumulative_opp_defense_sacks_suffered_forced', 'cumulative_opp_defense_attempts_allowed'),
  opp_rushing_yards_per_carry_allowed = c('cumulative_opp_defense_rushing_yards_allowed', 'cumulative_opp_defense_carries_allowed'),
  opp_fg_dependency_allowed = c('cumulative_opp_defense_total_fg_attempts_allowed','cumulative_opp_defense_drives_allowed')
)