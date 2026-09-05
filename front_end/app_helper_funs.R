library(shiny)
library(shinyWidgets)
library(httr)
library(googlesheets4)
library(dplyr)
library(jsonlite)
library(shinycssloaders)
library(furrr)
library(googlesheets4)
library(DT)
library(shinyjs)
library(lubridate)
library(quadprog)
library(stringr)
library(Matrix)
library(tidyr)
library(httr2)
library(plotly)
library(nflreadr)


if (file.exists(".Renviron")) {
  readRenviron(".Renviron")
}
SUPABASE_URL = Sys.getenv('SUPABASE_URL')
SUPABASE_KEY = Sys.getenv('SUPABASE_KEY')
api_base_url = Sys.getenv('API_BASE_URL')
dk_proxy_url = Sys.getenv('CLOUDFLARE_URL')

lines_considered = list(
  team_differential = sapply(c('-',''), function(x) paste0(x, c('7.5','6.5','3.5','2.5'))) %>% paste() %>% sort(),
  passing_yards = seq(150,360,30),
  rushing_yards = c(25, seq(40,140,20)),
  receiving_yards = c(25, seq(40,140,20)),
  rushing_receiving_yards = seq(40,140,30),
  receptions = seq(4,10,2)
)

get_supabase_data = function(schema, table_name, additional_sql = list(),
                              select = "*", page_size = 500, order_by = NULL)
{
  url = paste0(SUPABASE_URL, "/rest/v1/", table_name)
  all_data = list()
  offset = 0
  
  repeat {
    query_params = c(
      list(
        select = select,
        limit = page_size,
        offset = offset
      ),
      additional_sql
    )
    
    if (!is.null(order_by)) {
      query_params$order = order_by
    }
    
    response = GET(
      url,
      query = query_params,
      add_headers(
        "apikey" = SUPABASE_KEY,
        "Authorization" = paste("Bearer", SUPABASE_KEY),
        "Accept-Profile" = schema
      )
    )
    
    if (http_error(response)) {
      stop(content(response, "text", encoding = "UTF-8"))
    }
    
    this_data = fromJSON(content(response, "text", encoding = "UTF-8"))
    
    if (length(all_data) == 0 && !is.null(this_data) && length(this_data) == 0) {
      return(NULL)
    }
    
    all_data[[length(all_data) + 1]] = this_data
    
    if (nrow(this_data) < page_size) {
      break
    }
    
    offset = offset + page_size
  }
  
  return(bind_rows(all_data))
}

team_lookup = get_supabase_data('MainData', 'TeamLookup')
alternate_names = get_supabase_data('MainData', 'AlternatePlayerNames')
existing_error_logs = get_supabase_data('betting', 'ErrorLogs')



write_to_supabase = function(schema, table_name, df, batch_size = 500)
{
  url = paste0(SUPABASE_URL, "/rest/v1/", table_name)
  
  for (start_row in seq(1, nrow(df), by = batch_size))
  {
    end_row = min(start_row + batch_size - 1, nrow(df))
    
    batch = df[start_row:end_row, ]
    
    body_data = toJSON(
      batch,
      dataframe = "rows",
      auto_unbox = TRUE,
      na = "null",
      null = "null",
      digits = NA
    )
    
    response = POST(
      url,
      add_headers(
        "apikey" = SUPABASE_KEY,
        "Authorization" = paste("Bearer", SUPABASE_KEY),
        "Content-Type" = "application/json",
        "Content-Profile" = schema,
        "Prefer" = "return=minimal"
      ),
      body = body_data
    )
    
    if (http_error(response))
    {
      stop(
        paste(
          "Failed on rows", start_row, "to", end_row, ":",
          content(response, "text", encoding = "UTF-8")
        )
      )
    }
    
    print(paste("Wrote rows", start_row, "to", end_row))
  }
  
  print(paste("Successfully wrote", nrow(df), "rows to", table_name))
  return(TRUE)
}


upsert_to_supabase = function(schema, table_name, df, conflict_cols)
{
  if (length(conflict_cols) == 0) {
    stop("conflict_cols must contain at least one column name")
  }
  
  missing_cols = setdiff(conflict_cols, names(df))
  if (length(missing_cols) > 0) {
    stop(paste(
      "These conflict columns are missing from df:",
      paste(missing_cols, collapse = ", ")
    ))
  }
  
  url = modify_url(
    paste0(SUPABASE_URL, "/rest/v1/", table_name),
    query = list(
      on_conflict = paste(conflict_cols, collapse = ",")
    )
  )
  
  body_data = toJSON(df, dataframe = "rows", auto_unbox = TRUE, na = "null")
  
  response = POST(
    url,
    add_headers(
      "apikey" = SUPABASE_KEY,
      "Authorization" = paste("Bearer", SUPABASE_KEY),
      "Content-Type" = "application/json",
      "Content-Profile" = schema,
      "Prefer" = "resolution=merge-duplicates,return=minimal"
    ),
    body = body_data
  )
  
  if (http_error(response)) {
    stop(paste(
      "Failed to upsert to Supabase:",
      content(response, "text", encoding = "UTF-8")
    ))
  } else {
    print(paste("Successfully upserted", nrow(df), "rows to", table_name))
    return(TRUE)
  }
}


clean_names = function(name)
{
  idx = match(name, alternate_names$Name1)
  name[!is.na(idx)] = alternate_names$NameAlternate[idx[!is.na(idx)]]
  
  return(tolower(gsub('\\(.*\\)', '', name)) %>% str_remove_all("[[:punct:]]+") %>% str_remove("\\b(jr|sr|i{1,3}|iv|v|vi{1,3}|ix|x|xi{1,3})\\b") %>% str_squish() %>% trimws())
}


pull_prediction_data = function()
{
  player_info = get_supabase_data('predictions', 'PlayerPredictions') %>%
    group_by(response_var, season, Week, gsis_id) %>%
    arrange(desc(updated_at)) %>% 
    slice(1) %>% ungroup() %>%
    mutate(cleaned_name = clean_names(display_name)) %>%
    rename('Position' = 'position') %>%
    mutate(timeslot = paste(weekday, time_of_day)) %>% select(-time_of_day)
  
  team_info = get_supabase_data('predictions', 'TeamPredictions') %>%
    group_by(response_var, season, Week, team) %>%
    arrange(desc(updated_at))  %>% 
    slice(1) %>% ungroup() %>%
    mutate(timeslot = paste(weekday, time_of_day)) %>% select(-time_of_day)
  
  return(list(player_info, team_info))
}

get_bet_ids_by_category = function()
{
  response = httr::GET(paste0(dk_proxy_url, "/get_bet_ids_by_category"), httr::timeout(30))
  if(httr::status_code(response) != 200)
  {
    stop("Bet category API failed. Status: ", httr::status_code(response), ". Response: ",
         substr(httr::content(response, as = "text", encoding = "UTF-8"), 1, 500))
  }
  props_lookup = jsonlite::fromJSON(httr::content(response, as = "text", encoding = "UTF-8"))
  return(props_lookup)
}

get_props = function() {
  
  props_lookup = get_bet_ids_by_category()
  
  if (is.null(props_lookup) || nrow(props_lookup) == 0) {
    return(NULL)
  }
  lines_df = data.frame()
  for (i in 1:nrow(props_lookup))
  {
    bet_id = props_lookup$id[i]
    bet_name = props_lookup$name[i]
    response_var_root = props_lookup$response_var_root[i]
    
    lines_response = httr::GET(paste0(dk_proxy_url, "/lines_by_bet_id"), query = list(bet_id = bet_id), httr::timeout(30))
    data = lines_response %>% httr::content(as = "text", encoding = "UTF-8") %>% jsonlite::fromJSON(flatten = TRUE)
    if (length(data$selections) > 0)
    {
      lines = data$selections %>% select(any_of(c('marketId', 'label', 'displayOdds.american', 'points'))) %>%
        left_join(data$markets %>% select(id,eventId,name), join_by(marketId == id)) %>%
        rename('Odds' = `displayOdds.american`) %>%
        left_join(data$events %>% select(id, startEventDate), join_by('eventId' == 'id'))
    }
    if(is.null(lines))
    {
      return (NULL)
      message("DK lines request failed for ", response_var_root, ", block 3. Status: ", status_code(response))
      message("Response: ", substr(content(response, as = "text", encoding = "UTF-8"), 1, 500))
    }
    lines$Odds <- gsub("\u2212", "-", lines$Odds)
    if(bet_name == 'TD Scorer')
    {
      lines = lines %>% filter(name == 'Anytime TD Scorer' & !str_detect(label, 'D/ST')) %>%
        rename('bet_type' = 'name') %>%
        mutate(response_var = response_var_root,
               cleaned_name = clean_names(label),
               bet_display_name = paste(label, bet_type)) %>% 
        select(-label) %>% rename('label' = 'cleaned_name') %>%
        select(bet_display_name, bet_type, label, response_var, Odds, startEventDate)
    } else if (bet_name == 'Game') {
      moneyline = lines %>% filter(name == 'Moneyline') %>%
        rename('bet_type' = 'name') %>%
        mutate(response_var = response_var_root, bet_display_name = paste(label, bet_type))
      moneyline$team_shortname = sapply(strsplit(moneyline$label, ' '), function(x) x[2:length(x)])
      moneyline = moneyline %>% left_join(team_lookup %>% select(ShortName, TV_abbr), join_by('team_shortname' == 'ShortName')) %>% rename('team' = 'TV_abbr') %>%
        select(-label) %>% rename('label' = 'team') %>%
        select(bet_display_name, bet_type, label, response_var, Odds, startEventDate)
      
      #most of the spread bets will be under alternate spread, but just in case the actual spread number is one of the numbers the model looks for (+/- 2.5, +/- 3.5, +/- 6.5, +/- 7.5)
      spread = lines %>% filter(name == 'Spread') %>% 
        rename('bet_type' = 'name') %>%
        filter(points %in% lines_considered[['team_differential']]) %>%
        mutate(response_var = paste0('team_differential_', gsub('-','minus',(-1)*as.numeric(points))), bet_display_name = paste0(label, ' ', bet_type, ' ', ifelse(points>0, '+',''), points))
      spread$team_shortname = sapply(strsplit(spread$label, ' '), function(x) x[2:length(x)])
      spread = spread %>% left_join(team_lookup %>% select(ShortName, TV_abbr), join_by('team_shortname' == 'ShortName')) %>% rename('team' = 'TV_abbr') %>%
        select(-label) %>% rename('label' = 'team') %>%
        select(bet_display_name, bet_type, label, response_var, Odds, startEventDate)
      
      lines = bind_rows(moneyline, spread)
    } else if (bet_name == 'Alternate Spread') {
      lines$team_shortname = sapply(strsplit(lines$label, ' '), function(x) x[2:length(x)])
      lines = lines %>% filter(points %in% lines_considered[[response_var_root]]) %>%
        rename('bet_type' = 'name') %>%
        mutate(response_var = paste0(response_var_root, '_', gsub('-','minus',(-1)*as.numeric(points))), bet_display_name = paste0(label, ' ', bet_type, ' ', ifelse(points>0, '+',''), points)) %>%
        left_join(team_lookup %>% select(ShortName, TV_abbr), join_by('team_shortname' == 'ShortName')) %>% rename('team' = 'TV_abbr') %>%
        select(-label) %>% rename('label' = 'team') %>%
        select(bet_display_name, bet_type, label, response_var, Odds, startEventDate)
    } else {
      lines = lines %>%
        mutate(label_num = gsub('\\+','', label),
               response_var = paste0(response_var_root, '_', label_num),
               player_name = gsub('( [A-Za-z]+( \\+ [A-Za-z]+)* Yards| Receptions)$', '', lines$name),
               cleaned_name = clean_names(player_name),
               bet_display_name = paste(name, label),
               bet_type = bet_name) %>% 
        select(-label) %>% rename(label = cleaned_name) %>%
        filter(label_num %in% lines_considered[[response_var_root]]) %>%
        select(bet_display_name, bet_type, label, response_var, Odds, startEventDate)
    }
    lines = lines %>% mutate(gameday = as.Date(lubridate::with_tz(lubridate::ymd_hms(startEventDate), "America/New_York"), tz = "America/New_York")) %>%
      select(-startEventDate)
    lines_df = bind_rows(lines_df, lines)
  }
  return(lines_df)
}

get_spreads = function()
{
  bet_id_spread = get_bet_ids_by_category() %>% filter(name == 'Game') %>% pull(id)
  
  spread_lines_response = httr::GET(paste0(dk_proxy_url, "/lines_by_bet_id"), query = list(bet_id = bet_id_spread), httr::timeout(30))
  spread_data = spread_lines_response %>% httr::content(as = "text", encoding = "UTF-8") %>% jsonlite::fromJSON(flatten = TRUE)
  
  if (length(spread_data$selections) > 0)
  {
    spread_lines = spread_data$selections %>% select(any_of(c('marketId', 'label', 'displayOdds.american', 'points'))) %>%
      left_join(spread_data$markets %>% select(id,eventId,name), join_by(marketId == id)) %>%
      rename('Odds' = `displayOdds.american`) %>%
      left_join(spread_data$events %>% select(id, startEventDate), join_by('eventId' == 'id'))
  }
  
  spread = spread_lines %>% filter(name == 'Spread') %>% 
    select(label, points, startEventDate)
  first_game_per_team = spread %>% group_by(label) %>% summarize(startEventDate = min(startEventDate))
  spread = spread %>% inner_join(first_game_per_team, join_by('label', 'startEventDate'))
  spread$team_shortname = sapply(strsplit(spread$label, ' '), function(x) x[2:length(x)])
  spread = spread %>% left_join(team_lookup %>% select(ShortName, TV_abbr), join_by('team_shortname' == 'ShortName')) %>% rename('team' = 'TV_abbr') %>%
    select(label, points, team)
  return(spread)

}


join_preds_and_props = function(player_preds, team_preds)
{
  props = get_props()
  
  if (is.null(props))
  {
    return(NULL)
  }
  
  preds_to_match = bind_rows(player_preds %>% rename('label' = 'cleaned_name') %>% mutate(team = ifelse(team == 'LA', 'LAR', team)) %>% select(response_var, Week, label, Model_Probability, Position, team, opponent_team, timeslot, gameday, gametime, game_location, gsis_id),
                             team_preds %>% mutate(label = team) %>% mutate(label = ifelse(label == 'LA', 'LAR', label), team = ifelse(team == 'LA', 'LAR', team)) %>% mutate(Position = 'team') %>% select(response_var, Week, label, Model_Probability, Position, team, opponent_team, timeslot, gameday, gametime, game_location)) %>%
    mutate(posix_timestamp = as.POSIXct(paste0(gameday, gametime),format = "%Y-%m-%d %H:%M",tz = "America/New_York")) %>% select(-gametime)
  
  joined = props %>% left_join(preds_to_match %>% mutate(gameday = as.Date(gameday)), join_by('response_var', 'label', 'gameday')) %>%
    mutate(Betting_Line_Implied_Prob = ifelse(as.numeric(Odds) < 0, (-1)*as.numeric(Odds) / ((-1)*as.numeric(Odds) + 100), 100 / (as.numeric(Odds) + 100)),
    ) %>%
    select(bet_display_name, response_var, Week, label, Model_Probability, Position, Odds, Betting_Line_Implied_Prob, bet_type, team, opponent_team, timeslot, game_location, posix_timestamp, gsis_id)
  names_missing = joined %>% group_by(label) %>% summarize(num_nonmissing = sum(!is.na(Model_Probability))) %>% filter(num_nonmissing == 0) %>% pull(label) %>% unique()
  joined = joined %>% filter(posix_timestamp > Sys.time() - 3600)
  if(!is.null(existing_error_logs))
  {
    names_missing_unaccounted_for = names_missing[-which(names_missing %in% existing_error_logs$error_description[existing_error_logs$error_type == 'Missing Player'])]
  } else {
    names_missing_unaccounted_for = names_missing
  }
  if (length(names_missing_unaccounted_for) > 0)
  {
    write_to_supabase('betting','ErrorLogs', data.frame(error_type = 'Missing Player', error_description = names_missing_unaccounted_for))
  }
  joined = joined %>% filter(!is.na(opponent_team))
  
  return(joined)
}

pull_details = function(bets, bios, player_stats, team_stats, opp_stats, stats_season) {
  
  last_stats_season = stats_season - 1
  original_bet_cols = names(bets)
  
  # HELPERS --------------------------------------------------------------------
  
  mean_or_na = function(x) if (length(x) == 0 || all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
  sum_or_na = function(x) if (length(x) == 0 || all(is.na(x))) NA_real_ else sum(x, na.rm = TRUE)
  is_regular_season = function(season, week) week <= ifelse(season >= 2021, 18, 17)
  
  collapse_values = function(x, digits = 0, percent = FALSE) {
    x = x[!is.na(x)]
    if (length(x) == 0) return("")
    if (percent) return(paste0(paste(round(100*x, digits), collapse = "%, "), "%"))
    paste(round(x, digits), collapse = ", ")
  }
  
  html_join = function(...) {
    x = unlist(list(...), use.names = FALSE)
    x = x[!is.na(x) & nzchar(x)]
    paste(x, collapse = "<br><br>")
  }
  
  html_section = function(title, body) {
    if (is.na(body) || !nzchar(body)) return("")
    paste0("<h4>", title, "</h4>", body)
  }
  
  format_date_long = function(x) sub(" 0", " ", format(as.Date(x), "%B %d, %Y"))
  
  season_description = function(season_label, win_rate, location_win_rate, location_phrase,
                                no_location_games_phrase, avg_differential, avg_differential_per_win) {
    if (is.na(win_rate)) return("")
    location_text = if (is.na(location_win_rate)) no_location_games_phrase else
      paste0(round(100*location_win_rate), "% ", location_phrase)
    
    paste0("<b>", season_label, ":</b> ", round(100*win_rate), "% win rate (", location_text, ")<br>",
           "Average point differential: ", round(avg_differential, 1), " points",
           ifelse(is.na(avg_differential_per_win), "",
                  paste0(" (", round(avg_differential_per_win, 1), " differential points per win)")))
  }
  
  
  # HEADER / PLAYER BIO ---------------------------------------------------------
  
  bios_for_join = bios %>%
    filter(season == max(season, na.rm = TRUE)) %>%
    rename(Week = week) %>%
    select(gsis_id, Week, position, draft_round, draft_year, depth_rank, jersey_number, weeks_on_current_team) %>%
    distinct(gsis_id, Week, .keep_all = TRUE)
  
  bets_with_header = bets %>%
    left_join(bios_for_join, join_by(gsis_id, Week)) %>%
    mutate(
      weeks_on_current_team_num = suppressWarnings(as.numeric(weeks_on_current_team)),
      player_position = coalesce(position, Position),
      header = paste0(
        "<h2>", bet_display_name, "</h2>",
        "<b>",team, " vs ", opponent_team, " (", game_location, ") - ", timeslot, "</b><br>",
        "<b>Model Probability: </b>", round(100*Model_Probability, 1), "%<br>",
        "<b>Odds: </b>", Odds, " (Market Implied Probability: ",
        round(100*Betting_Line_Implied_Prob, 1), "%)<br><br>",
        ifelse(is.na(gsis_id), "",
               paste0(player_position, " - ",
                      ifelse(is.na(draft_round) | draft_round == "", "Undrafted",
                             paste0("Round ", draft_round, " Draft Pick in ", draft_year)), "<br>",
                      ifelse(is.na(depth_rank) | depth_rank == "", "Not listed on depth chart",
                             paste0("Depth chart rank: ", depth_rank)), "<br>",
                      ifelse(is.na(weeks_on_current_team_num), "",
                             paste0("# of in-season weeks with team: ", weeks_on_current_team_num-1,
                                    ifelse(weeks_on_current_team_num == 2, " week", " weeks"), "<br>"))))
      )
    )
  
  
  # TEAM MATCHUP HISTORY --------------------------------------------------------
  
  matchup_counts = team_stats %>%
    filter(season >= 2020) %>%
    group_by(team, opponent_team) %>%
    summarise(
      num_matchups_since_2020 = n_distinct(game_id),
      pct_win = mean(team_win, na.rm = TRUE),
      .groups = "drop"
    )
  
  most_recent_matchup = team_stats %>%
    filter(season >= 2020, gameday < Sys.Date()) %>%
    mutate(gameday = as.Date(gameday)) %>%
    group_by(team, opponent_team) %>%
    arrange(desc(gameday), .by_group = TRUE) %>%
    slice_head(n = 1) %>%
    ungroup() %>%
    mutate(
      most_recent_game = gameday,
      most_recent_result = paste0(
        team_score, " - ", opponent_score, " (",
        ifelse(team_win, "Win",
               ifelse(team_score == opponent_score, "Tie", "Loss")),
        ")"
      )
    ) %>%
    select(team, opponent_team, most_recent_game, most_recent_result)
  
  matchup_df = matchup_counts %>%
    left_join(most_recent_matchup, join_by(team, opponent_team)) %>%
    mutate(
      Matchup_Description = paste0(
        num_matchups_since_2020,
        ifelse(num_matchups_since_2020 == 1, " matchup", " matchups"),
        " against opponent since 2020 (", round(100*pct_win), "% win rate)<br>",
        "Most recent matchup: ", format_date_long(most_recent_game), ", ", most_recent_result
      )
    ) %>%
    select(team, opponent_team, Matchup_Description)
  
  
  # TEAM WIN RATE / DIFFERENTIAL STATS -----------------------------------------
  
  team_stats_this_season = team_stats %>%
    filter(season == stats_season, is_regular_season(season, week)) %>%
    group_by(team) %>%
    summarise(
      this_season_win_rate = mean(team_win, na.rm = TRUE),
      this_season_win_rate_at_home_stadium =
        ifelse(sum(home_stadium, na.rm = TRUE) == 0, NA_real_,
               mean(team_win[home_stadium], na.rm = TRUE)),
      this_season_win_rate_at_visitor_stadium =
        ifelse(sum(!home_stadium & !neutral_field, na.rm = TRUE) == 0, NA_real_,
               mean(team_win[!home_stadium & !neutral_field], na.rm = TRUE)),
      this_season_win_rate_at_neutral_field =
        ifelse(sum(neutral_field, na.rm = TRUE) == 0, NA_real_,
               mean(team_win[neutral_field], na.rm = TRUE)),
      this_season_avg_differential = mean(team_differential, na.rm = TRUE),
      this_season_avg_differential_per_win =
        ifelse(sum(team_win, na.rm = TRUE) == 0, NA_real_,
               mean(team_differential[team_win], na.rm = TRUE)),
      .groups = "drop"
    )
  
  team_stats_last_season = team_stats %>%
    filter(season == last_stats_season, is_regular_season(season, week)) %>%
    group_by(team) %>%
    summarise(
      last_season_win_rate = mean(team_win, na.rm = TRUE),
      last_season_win_rate_at_home_stadium =
        ifelse(sum(home_stadium, na.rm = TRUE) == 0, NA_real_,
               mean(team_win[home_stadium], na.rm = TRUE)),
      last_season_win_rate_at_visitor_stadium =
        ifelse(sum(!home_stadium & !neutral_field, na.rm = TRUE) == 0, NA_real_,
               mean(team_win[!home_stadium & !neutral_field], na.rm = TRUE)),
      last_season_win_rate_at_neutral_field =
        ifelse(sum(neutral_field, na.rm = TRUE) == 0, NA_real_,
               mean(team_win[neutral_field], na.rm = TRUE)),
      last_season_avg_differential = mean(team_differential, na.rm = TRUE),
      last_season_avg_differential_per_win =
        ifelse(sum(team_win, na.rm = TRUE) == 0, NA_real_,
               mean(team_differential[team_win], na.rm = TRUE)),
      .groups = "drop"
    )
  
  team_info = team_stats_this_season %>%
    full_join(team_stats_last_season, join_by(team)) %>%
    rowwise() %>%
    mutate(
      team_stats_home_description = html_join(
        season_description("This season", this_season_win_rate,
                           this_season_win_rate_at_home_stadium,
                           "at home", "no games at home",
                           this_season_avg_differential,
                           this_season_avg_differential_per_win),
        season_description("Last season", last_season_win_rate,
                           last_season_win_rate_at_home_stadium,
                           "at home", "no games at home",
                           last_season_avg_differential,
                           last_season_avg_differential_per_win)
      ),
      team_stats_away_description = html_join(
        season_description("This season", this_season_win_rate,
                           this_season_win_rate_at_visitor_stadium,
                           "during away games", "no away games",
                           this_season_avg_differential,
                           this_season_avg_differential_per_win),
        season_description("Last season", last_season_win_rate,
                           last_season_win_rate_at_visitor_stadium,
                           "during away games", "no away games",
                           last_season_avg_differential,
                           last_season_avg_differential_per_win)
      ),
      team_stats_neutral_description = html_join(
        season_description("This season", this_season_win_rate,
                           this_season_win_rate_at_neutral_field,
                           "during neutral field games", "no neutral field games",
                           this_season_avg_differential,
                           this_season_avg_differential_per_win),
        season_description("Last season", last_season_win_rate,
                           last_season_win_rate_at_neutral_field,
                           "during neutral field games", "no neutral field games",
                           last_season_avg_differential,
                           last_season_avg_differential_per_win)
      )
    ) %>%
    ungroup() %>%
    select(team, team_stats_home_description,
           team_stats_away_description,
           team_stats_neutral_description)
  
  opp_team_info = team_info %>%
    rename(
      opponent_team = team,
      opp_stats_home_description = team_stats_home_description,
      opp_stats_away_description = team_stats_away_description,
      opp_stats_neutral_description = team_stats_neutral_description
    )
  
  
  # OPPONENT YARDAGE / TD DEFENSIVE RANK ---------------------------------------
  
  opp_yardage_stats_this_season = opp_stats %>%
    filter(season == stats_season, is_regular_season(season, week)) %>%
    mutate(
      total_tds_this_game = ifelse(
        is.na(opp_defense_rushing_tds_allowed) &
          is.na(opp_defense_passing_tds_allowed),
        NA_real_,
        coalesce(as.numeric(opp_defense_rushing_tds_allowed), 0) +
          coalesce(as.numeric(opp_defense_passing_tds_allowed), 0)
      )
    ) %>%
    group_by(opponent_team) %>%
    summarise(
      total_pass_allowed = sum_or_na(as.numeric(opp_defense_passing_yards_allowed)),
      total_completions_allowed = sum_or_na(as.numeric(opp_defense_completions_allowed)),
      total_rushing_yards_allowed = sum_or_na(as.numeric(opp_defense_rushing_yards_allowed)),
      total_tds_allowed = sum_or_na(total_tds_this_game),
      .groups = "drop"
    ) %>%
    mutate(
      pass_allowed_rank = min_rank(total_pass_allowed),
      completions_allowed_rank = min_rank(total_completions_allowed),
      rushing_allowed_rank = min_rank(total_rushing_yards_allowed),
      tds_allowed_rank = min_rank(total_tds_allowed),
      
      pass_defense_description = ifelse(
        is.na(pass_allowed_rank), "",
        paste0(
          "<b>This season:</b> #", pass_allowed_rank,
          " ranked defense in passing yards allowed, #",
          completions_allowed_rank,
          " ranked defense in passing completions allowed"
        )
      ),
      
      rush_defense_description = ifelse(
        is.na(rushing_allowed_rank), "",
        paste0(
          "<b>This season:</b> #", rushing_allowed_rank,
          " ranked defense in rushing yards allowed"
        )
      ),
      
      td_defense_description = ifelse(
        is.na(tds_allowed_rank), "",
        paste0(
          "<b>This season:</b> #", tds_allowed_rank,
          " ranked defense in touchdowns allowed"
        )
      )
    ) %>%
    select(
      opponent_team,
      pass_defense_description,
      rush_defense_description,
      td_defense_description
    )
  
  opp_yardage_stats_last_season = opp_stats %>%
    filter(season == last_stats_season, is_regular_season(season, week)) %>%
    mutate(
      total_tds_this_game = ifelse(
        is.na(opp_defense_rushing_tds_allowed) &
          is.na(opp_defense_passing_tds_allowed),
        NA_real_,
        coalesce(as.numeric(opp_defense_rushing_tds_allowed), 0) +
          coalesce(as.numeric(opp_defense_passing_tds_allowed), 0)
      )
    ) %>%
    group_by(opponent_team) %>%
    summarise(
      total_pass_allowed = sum_or_na(as.numeric(opp_defense_passing_yards_allowed)),
      total_completions_allowed = sum_or_na(as.numeric(opp_defense_completions_allowed)),
      total_rushing_yards_allowed = sum_or_na(as.numeric(opp_defense_rushing_yards_allowed)),
      total_tds_allowed = sum_or_na(total_tds_this_game),
      .groups = "drop"
    ) %>%
    mutate(
      pass_allowed_rank = min_rank(total_pass_allowed),
      completions_allowed_rank = min_rank(total_completions_allowed),
      rushing_allowed_rank = min_rank(total_rushing_yards_allowed),
      tds_allowed_rank = min_rank(total_tds_allowed),
      
      last_season_pass_defense_description = ifelse(
        is.na(pass_allowed_rank), "",
        paste0(
          "<b>Last season:</b> #", pass_allowed_rank,
          " ranked defense in passing yards allowed, #",
          completions_allowed_rank,
          " ranked defense in passing completions allowed"
        )
      ),
      
      last_season_rush_defense_description = ifelse(
        is.na(rushing_allowed_rank), "",
        paste0(
          "<b>Last season:</b> #", rushing_allowed_rank,
          " ranked defense in rushing yards allowed"
        )
      ),
      
      last_season_td_defense_description = ifelse(
        is.na(tds_allowed_rank), "",
        paste0(
          "<b>Last season:</b> #", tds_allowed_rank,
          " ranked defense in touchdowns allowed"
        )
      )
    ) %>%
    select(
      opponent_team,
      last_season_pass_defense_description,
      last_season_rush_defense_description,
      last_season_td_defense_description
    )
  
  opp_yardage_info = opp_yardage_stats_this_season %>%
    full_join(opp_yardage_stats_last_season, join_by(opponent_team))
  
  
  # PLAYER — THREE MOST RECENT GAMES -------------------------------------------
  
  player_stats_this_season = player_stats %>%
    filter(season == stats_season) %>%
    mutate(
      completion_rate = ifelse(
        !is.na(attempts) & attempts > 0,
        completions/attempts,
        NA_real_
      ),
      total_tds = ifelse(
        is.na(rushing_tds) &
          is.na(receiving_tds) &
          is.na(special_teams_tds),
        NA_real_,
        coalesce(as.numeric(rushing_tds), 0) +
          coalesce(as.numeric(receiving_tds), 0) +
          coalesce(as.numeric(special_teams_tds), 0)
      )
    ) %>%
    group_by(gsis_id) %>%
    arrange(desc(week), .by_group = TRUE) %>%
    slice_head(n = 3) %>%
    summarise(
      passing_recent_games_description =
        ifelse(
          all(is.na(passing_yards)), "",
          paste0(
            "Passing Yards In Recent Games: ", collapse_values(passing_yards), "<br>",
            "# of Completions In Recent Games: ", collapse_values(completions), "<br>",
            "Completion % Rate In Recent Games: ",
            collapse_values(completion_rate, percent = TRUE)
          )
        ),
      
      rushing_recent_games_description =
        ifelse(
          all(is.na(rushing_yards)), "",
          paste0(
            "Rushing Yards In Recent Games: ", collapse_values(rushing_yards), "<br>",
            "# of Rushing First Downs In Recent Games: ",
            collapse_values(rushing_first_downs), "<br>",
            "# of Carries In Recent Games: ", collapse_values(carries)
          )
        ),
      
      receiving_recent_games_description =
        ifelse(
          all(is.na(receiving_yards)), "",
          paste0(
            "Receiving Yards In Recent Games: ",
            collapse_values(receiving_yards), "<br>",
            "# of Receptions In Recent Games: ",
            collapse_values(receptions), "<br>",
            "# of Targets In Recent Games: ",
            collapse_values(targets)
          )
        ),
      
      tds_recent_games_description =
        ifelse(
          all(is.na(total_tds)), "",
          paste0(
            "# of Touchdowns In Recent Games: ",
            collapse_values(total_tds)
          )
        ),
      
      passing_yards_recent_description =
        ifelse(
          all(is.na(passing_yards)), "",
          paste0(
            "Passing Yards In Recent Games: ",
            collapse_values(passing_yards)
          )
        ),
      
      rushing_yards_recent_description =
        ifelse(
          all(is.na(rushing_yards)), "",
          paste0(
            "Rushing Yards In Recent Games: ",
            collapse_values(rushing_yards)
          )
        ),
      
      receiving_yards_recent_description =
        ifelse(
          all(is.na(receiving_yards)), "",
          paste0(
            "Receiving Yards In Recent Games: ",
            collapse_values(receiving_yards)
          )
        ),
      
      .groups = "drop"
    )
  
  
  # PLAYER — LAST-SEASON AVERAGES ----------------------------------------------
  
  player_stats_last_season = player_stats %>%
    filter(season == last_stats_season) %>%
    group_by(gsis_id) %>%
    summarise(
      last_season_avg_passing_yards = mean_or_na(passing_yards),
      last_season_avg_completions = mean_or_na(completions),
      
      last_season_avg_completion_rate =
        ifelse(
          sum(attempts, na.rm = TRUE) == 0,
          NA_real_,
          sum(completions, na.rm = TRUE) /
            sum(attempts, na.rm = TRUE)
        ),
      
      last_season_avg_rushing_yards = mean_or_na(rushing_yards),
      last_season_avg_carries = mean_or_na(carries),
      last_season_avg_rushing_1ds = mean_or_na(rushing_first_downs),
      last_season_avg_receiving_yards = mean_or_na(receiving_yards),
      last_season_avg_targets = mean_or_na(targets),
      last_season_avg_receptions = mean_or_na(receptions),
      
      last_season_avg_tds =
        mean_or_na(
          ifelse(
            is.na(rushing_tds) &
              is.na(receiving_tds) &
              is.na(special_teams_tds),
            NA_real_,
            coalesce(as.numeric(rushing_tds), 0) +
              coalesce(as.numeric(receiving_tds), 0) +
              coalesce(as.numeric(special_teams_tds), 0)
          )
        ),
      
      .groups = "drop"
    ) %>%
    mutate(
      last_year_passing_description =
        ifelse(
          is.na(last_season_avg_passing_yards), "",
          paste0(
            "Last Season - Avg Passing Yards Per Game: ",
            round(last_season_avg_passing_yards), "<br>",
            "Last Season - Avg Completions Per Game: ",
            round(last_season_avg_completions, 1), "<br>",
            "Last Season - Avg Passing Completion Rate: ",
            round(100*last_season_avg_completion_rate), "%"
          )
        ),
      
      last_year_rushing_description =
        ifelse(
          is.na(last_season_avg_rushing_yards), "",
          paste0(
            "Last Season - Avg Rushing Yards Per Game: ",
            round(last_season_avg_rushing_yards), "<br>",
            "Last Season - Avg Carries Per Game: ",
            round(last_season_avg_carries, 1), "<br>",
            "Last Season - Avg Rushing First Downs Per Game: ",
            round(last_season_avg_rushing_1ds, 1)
          )
        ),
      
      last_year_receiving_description =
        ifelse(
          is.na(last_season_avg_receiving_yards), "",
          paste0(
            "Last Season - Avg Receiving Yards Per Game: ",
            round(last_season_avg_receiving_yards), "<br>",
            "Last Season - Avg Receptions Per Game: ",
            round(last_season_avg_receptions, 1), "<br>",
            "Last Season - Avg Targets Per Game: ",
            round(last_season_avg_targets, 1)
          )
        ),
      
      last_year_td_description =
        ifelse(
          is.na(last_season_avg_tds), "",
          paste0(
            "Last Season - Avg Touchdowns Per Game: ",
            round(last_season_avg_tds, 2)
          )
        ),
      
      last_year_passing_yards_description =
        ifelse(
          is.na(last_season_avg_passing_yards), "",
          paste0(
            "Last Season - Avg Passing Yards Per Game: ",
            round(last_season_avg_passing_yards)
          )
        ),
      
      last_year_rushing_yards_description =
        ifelse(
          is.na(last_season_avg_rushing_yards), "",
          paste0(
            "Last Season - Avg Rushing Yards Per Game: ",
            round(last_season_avg_rushing_yards)
          )
        ),
      
      last_year_receiving_yards_description =
        ifelse(
          is.na(last_season_avg_receiving_yards), "",
          paste0(
            "Last Season - Avg Receiving Yards Per Game: ",
            round(last_season_avg_receiving_yards)
          )
        )
    ) %>%
    select(
      gsis_id,
      last_year_passing_description,
      last_year_rushing_description,
      last_year_receiving_description,
      last_year_td_description,
      last_year_passing_yards_description,
      last_year_rushing_yards_description,
      last_year_receiving_yards_description
    )
  
  
  # PLAYER HISTORY AGAINST CURRENT OPPONENT ------------------------------------
  
  game_lookup = team_stats %>%
    select(season, week, team, opponent_team, gameday) %>%
    distinct()
  
  player_matchup = player_stats %>%
    left_join(game_lookup, join_by(season, week, team)) %>%
    filter(!is.na(opponent_team)) %>%
    mutate(
      gameday = as.Date(gameday),
      total_tds = ifelse(
        is.na(rushing_tds) &
          is.na(receiving_tds) &
          is.na(special_teams_tds),
        NA_real_,
        coalesce(as.numeric(rushing_tds), 0) +
          coalesce(as.numeric(receiving_tds), 0) +
          coalesce(as.numeric(special_teams_tds), 0)
      )
    ) %>%
    group_by(gsis_id, opponent_team) %>%
    arrange(desc(gameday), .by_group = TRUE) %>%
    summarise(
      times_played = n(),
      avg_passing_yards = mean_or_na(passing_yards),
      avg_completion_rate = ifelse(sum(attempts, na.rm = TRUE) == 0, NA_real_, sum(completions, na.rm = TRUE) / sum(attempts, na.rm = TRUE)),
      avg_rushing_yards = mean_or_na(rushing_yards),
      avg_carries = mean_or_na(carries),
      avg_receiving_yards = mean_or_na(receiving_yards),
      avg_receptions = mean_or_na(receptions),
      avg_tds = mean_or_na(total_tds),
      gameday = first(gameday),
      passing_yards = first(passing_yards),
      rushing_yards = first(rushing_yards),
      receiving_yards = first(receiving_yards),
      tds = first(total_tds),
      .groups = "drop"
    ) %>%
    mutate(
      matchup_passing_description = paste0(
        times_played, ifelse(times_played == 1, " matchup", " matchups"),
        " against this opponent (avg ", round(avg_passing_yards), " passing yards",
        ifelse(is.na(avg_completion_rate), "", paste0(", ", round(100*avg_completion_rate), "% completion rate")), ")<br>",
        "Most recent matchup: ", format_date_long(gameday), " - ", passing_yards, " passing yards"
      ),
      
      matchup_rushing_description = paste0(
        times_played, ifelse(times_played == 1, " matchup", " matchups"),
        " against this opponent (avg ", round(avg_rushing_yards), " rushing yards",
        ifelse(is.na(avg_carries), "", paste0(", ", round(avg_carries, 1), " carries")), ")<br>",
        "Most recent matchup: ", format_date_long(gameday), " - ", rushing_yards, " rushing yards"
      ),
      
      matchup_receiving_description = paste0(
        times_played, ifelse(times_played == 1, " matchup", " matchups"),
        " against this opponent (avg ", round(avg_receiving_yards), " receiving yards",
        ifelse(is.na(avg_receptions), "", paste0(", ", round(avg_receptions, 1), " receptions")), ")<br>",
        "Most recent matchup: ", format_date_long(gameday), " - ", receiving_yards, " receiving yards"
      ),
      
      matchup_touchdown_description = paste0(
        times_played,
        ifelse(times_played == 1, " matchup", " matchups"),
        " against this opponent (avg ",
        round(avg_tds, 2),
        " touchdowns)<br>",
        "Most recent matchup: ",
        format_date_long(gameday),
        " - ", tds,
        ifelse(tds == 1, " touchdown", " touchdowns")
      )
    )
  
  
  # JOIN EVERYTHING ONTO BETS --------------------------------------------------
  
  display_info = bets_with_header %>%
    left_join(matchup_df, join_by(team, opponent_team)) %>%
    left_join(team_info, join_by(team)) %>%
    left_join(opp_team_info, join_by(opponent_team)) %>%
    left_join(opp_yardage_info, join_by(opponent_team)) %>%
    left_join(player_stats_this_season, join_by(gsis_id)) %>%
    left_join(player_stats_last_season, join_by(gsis_id)) %>%
    left_join(player_matchup, join_by(gsis_id, opponent_team)) %>%
    mutate(
      is_team_bet = is.na(gsis_id) |
        coalesce(tolower(Position) == "team", FALSE),
      
      bet_key = tolower(paste(response_var, bet_type)),
      
      bet_category = case_when(
        is_team_bet ~ "team",
        grepl("rushing_receiving", bet_key) ~ "rushing_receiving",
        grepl("anytime_td|td_scorer|touchdown", bet_key) ~ "td",
        grepl("passing|completion", bet_key) ~ "passing",
        grepl("receiving|reception|target", bet_key) ~ "receiving",
        grepl("rushing|carries", bet_key) ~ "rushing",
        TRUE ~ "player"
      ),
      
      td_position_stat = case_when(
        toupper(player_position) == "QB" ~ "passing",
        toupper(player_position) %in% c("RB", "FB") ~ "rushing",
        toupper(player_position) %in% c("WR", "TE") ~ "receiving",
        TRUE ~ "receiving"
      )
    ) %>%
    rowwise() %>%
    mutate(
      team_location_description = case_when(
        tolower(game_location) == "home" ~ team_stats_home_description,
        tolower(game_location) == "away" ~ team_stats_away_description,
        grepl("neutral", tolower(game_location)) ~ team_stats_neutral_description,
        TRUE ~ ""
      ),
      
      opponent_location_description = case_when(
        tolower(game_location) == "home" ~ opp_stats_away_description,
        tolower(game_location) == "away" ~ opp_stats_home_description,
        grepl("neutral", tolower(game_location)) ~ opp_stats_neutral_description,
        TRUE ~ ""
      ),
      
      td_recent_yardage_description = case_when(
        td_position_stat == "passing" ~ passing_yards_recent_description,
        td_position_stat == "rushing" ~ rushing_yards_recent_description,
        td_position_stat == "receiving" ~ receiving_yards_recent_description,
        TRUE ~ ""
      ),
      
      td_last_year_yardage_description = case_when(
        td_position_stat == "passing" ~ last_year_passing_yards_description,
        td_position_stat == "rushing" ~ last_year_rushing_yards_description,
        td_position_stat == "receiving" ~ last_year_receiving_yards_description,
        TRUE ~ ""
      ),
      
      td_matchup_yardage_description = case_when(
        td_position_stat == "passing" ~ matchup_passing_description,
        td_position_stat == "rushing" ~ matchup_rushing_description,
        td_position_stat == "receiving" ~ matchup_receiving_description,
        TRUE ~ ""
      ),
      
      td_position_defense_description = case_when(
        td_position_stat == "passing" ~
          html_join(pass_defense_description,
                    last_season_pass_defense_description),
        
        td_position_stat == "rushing" ~
          html_join(rush_defense_description,
                    last_season_rush_defense_description),
        
        td_position_stat == "receiving" ~
          html_join(pass_defense_description,
                    last_season_pass_defense_description),
        
        TRUE ~ ""
      ),
      
      player_matchup_description = case_when(
        bet_category == "passing" ~
          coalesce(
            matchup_passing_description,
            "No historical matchup data found"
          ),
        
        bet_category == "rushing" ~
          coalesce(
            matchup_rushing_description,
            "No historical matchup data found"
          ),
        
        bet_category == "receiving" ~
          coalesce(
            matchup_receiving_description,
            "No historical matchup data found"
          ),
        
        bet_category == "td" ~
          html_join(
            coalesce(
              matchup_touchdown_description,
              "No historical matchup data found"
            ),
            td_matchup_yardage_description
          ),
        
        bet_category == "rushing_receiving" ~
          html_join(
            coalesce(
              matchup_rushing_description,
              "No historical matchup data found"
            ),
            coalesce(
              matchup_receiving_description,
              "No historical matchup data found"
            )
          ),
        
        TRUE ~ ""
      ),
      
      player_stats_description = case_when(
        bet_category == "passing" ~
          html_join(
            passing_recent_games_description,
            last_year_passing_description
          ),
        
        bet_category == "rushing" ~
          html_join(
            rushing_recent_games_description,
            last_year_rushing_description
          ),
        
        bet_category == "receiving" ~
          html_join(
            receiving_recent_games_description,
            last_year_receiving_description
          ),
        
        bet_category == "td" ~
          html_join(
            tds_recent_games_description,
            last_year_td_description,
            td_recent_yardage_description,
            td_last_year_yardage_description
          ),
        
        bet_category == "rushing_receiving" ~
          html_join(
            rushing_recent_games_description,
            receiving_recent_games_description,
            last_year_rushing_description,
            last_year_receiving_description
          ),
        
        TRUE ~ ""
      ),
      
      opponent_yardage_description = case_when(
        bet_category == "passing" ~
          html_join(
            pass_defense_description,
            last_season_pass_defense_description
          ),
        
        bet_category == "receiving" ~
          html_join(
            pass_defense_description,
            last_season_pass_defense_description
          ),
        
        bet_category == "rushing" ~
          html_join(
            rush_defense_description,
            last_season_rush_defense_description
          ),
        
        bet_category == "td" ~
          html_join(
            td_defense_description,
            last_season_td_defense_description,
            td_position_defense_description
          ),
        
        bet_category == "rushing_receiving" ~
          html_join(
            pass_defense_description,
            rush_defense_description,
            last_season_pass_defense_description,
            last_season_rush_defense_description
          ),
        
        TRUE ~ ""
      ),
      
      team_extra_info = html_join(
        html_section("Team Performance", team_location_description),
        html_section("Opponent Performance", opponent_location_description),
        html_section(
          "Matchup History",
          coalesce(
            Matchup_Description,
            "No matchups against this opponent since 2020"
          )
        )
      ),
      
      player_extra_info = html_join(
        html_section("Player Performance", player_stats_description),
        html_section("History Against Opponent", player_matchup_description),
        html_section("Opponent Defense", opponent_yardage_description)
      ),
      
      extra_info = paste0(
        header,
        "<br>",
        ifelse(is_team_bet, team_extra_info, player_extra_info)
      )
    ) %>%
    ungroup() %>%
    select(bet_display_name, depth_rank, position, jersey_number, extra_info)
  
  return(display_info)
}