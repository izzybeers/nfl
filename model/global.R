library(slider)
library(dplyr)
library(nflreadr)

#functions: 

compute_slider_cumulatives = function(df, cols_to_include, cumulative_only) {
  df = df %>% arrange(week)
  
  for (c in cols_to_include)
  {
    
    vals = df[[c]]
    
    if(cumulative_only == FALSE)
    {
      # Cumulative up to each game
      df[[paste0("Cumulative_", c)]] = slide_dbl(vals, ~ ifelse(length(.x) == 0, NA_real_, sum(.x, na.rm = TRUE)), .before = Inf, .after = -1, .complete = TRUE)
      df[[paste0("Avg_", c)]] = slide_dbl(vals, ~ ifelse(length(.x) == 0, NA_real_, mean(.x, na.rm = TRUE)), .before = Inf, .after = -1, .complete = TRUE)
      df[[paste0("Median_", c)]]     = slide_dbl(vals, ~ ifelse(length(.x) == 0, NA_real_, median(.x, na.rm = TRUE)), .before = Inf, .after = -1,  .complete = TRUE)
      df[[paste0("Min_", c)]]        = slide_dbl(vals, ~ ifelse(length(.x) == 0, NA_real_, min(.x, na.rm = TRUE)), .before = Inf,  .after = -1, .complete = TRUE)
      df[[paste0("Max_", c)]]        = slide_dbl(vals, ~ ifelse(length(.x) == 0, NA_real_,max(.x, na.rm = TRUE)), .before = Inf, .after = -1,  .complete = TRUE)
      df[[paste0("SD_", c)]]         = slide_dbl(vals, ~ ifelse(length(.x) == 0, NA_real_, sd(.x, na.rm = TRUE)), .before = Inf, .after = -1,  .complete = TRUE)
      
      # Last 3 games
      df[[paste0("Last3_Cumulative_", c)]] = slide_dbl(vals, ~ ifelse(length(.x) == 0, NA_real_, sum(.x, na.rm = TRUE)), .before = 3, .after = -1, .complete = TRUE)
      df[[paste0("Last3_Avg_", c)]]        = slide_dbl(vals, ~ ifelse(length(.x) == 0, NA_real_, mean(.x, na.rm = TRUE)), .before = 3, .after = -1,  .complete = TRUE)
      df[[paste0("Last3_Median_", c)]]     = slide_dbl(vals, ~ ifelse(length(.x) == 0, NA_real_, median(.x, na.rm = TRUE)), .before = 3, .after = -1,   .complete = TRUE)
      df[[paste0("Last3_Min_", c)]]        = slide_dbl(vals, ~ ifelse(length(.x) == 0, NA_real_, min(.x, na.rm = TRUE)), .before = 3, .after = -1,  .complete = TRUE)
      df[[paste0("Last3_Max_", c)]]        = slide_dbl(vals, ~ ifelse(length(.x) == 0, NA_real_, max(.x, na.rm = TRUE)), .before = 3, .after = -1,  .complete = TRUE)
      df[[paste0("Last3_SD_", c)]]         = slide_dbl(vals, ~ ifelse(length(.x) == 0, NA_real_, sd(.x, na.rm = TRUE)), .before = 3, .after = -1,  .complete = TRUE)
    } else{
      df[[paste0("Cumulative_", c)]] = slide_dbl(vals, ~ ifelse(length(.x) == 0, NA_real_, sum(.x, na.rm = TRUE)), .before = Inf, .after = -1, .complete = TRUE)
      df[[paste0("Last3_Cumulative_", c)]] = slide_dbl(vals, ~ ifelse(length(.x) == 0, NA_real_, sum(.x, na.rm = TRUE)), .before = 3, .after = -1, .complete = TRUE)
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


#data:

week_end_dates_by_team = schedules %>% group_by(season, week, team) %>% summarise(week_end = max(paste(gameday, gametime)))
week_dates = week_end_dates_by_team %>% left_join(week_end_dates_by_team %>% mutate(next_week = week + 1) %>% rename('previous_week_end' = 'week_end') %>% select(season, team, next_week, previous_week_end),
                                                  join_by('season', 'week' == 'next_week', 'team')) %>%
  mutate(week_start = case_when(
    is.na(previous_week_end) ~ as.POSIXct('2020-08-01 00:00:00'),
    .default = as.POSIXct(previous_week_end) + 1),
    week_end = as.POSIXct(week_end)) %>% select(season, week, team, week_start, week_end)

team_lookup_table = get_supabase_data(schema = 'MainData', table_name = 'TeamLookup')

schedules_raw = load_schedules(min_year:max_year) %>% clean_homeaway() 
