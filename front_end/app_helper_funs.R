library(supabase)

readRenviron(".Renviron")
SUPABASE_URL = Sys.getenv('SUPABASE_URL')
SUPABASE_KEY = Sys.getenv('SUPABASE_KEY')

team_lookup_table = get_supabase_data('MainData', 'TeamLookup')

get_supabase_data <- function(schema, table_name, additional_sql = list(), select = "*")
{
  url <- paste0(SUPABASE_URL, "/rest/v1/", table_name)
  
  response <- GET(
    url,
    query = c(
      list(select = select),
      additional_sql
    ),
    add_headers(
      "apikey" = SUPABASE_KEY,
      "Authorization" = paste("Bearer", SUPABASE_KEY),
      "Accept-Profile" = schema
    )
  )
  
  if (http_error(response)) {
    stop(content(response, "text", encoding = "UTF-8"))
  }
  
  fromJSON(content(response, "text", encoding = "UTF-8"))
}


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

pull_prediction_data = function()
{
  player_info = get_supabase_data('predictions', 'PlayerPredictions') %>%
    group_by(response_var, season, Week, gsis_id) %>%
    arrange(updated_at, descending = TRUE) %>% 
    slice(1) %>% ungroup()
  
  team_info = get_supabase_data('predictions', 'TeamPredictions') %>%
    group_by(response_var, season, Week, team) %>%
    arrange(updated_at, descending = TRUE) %>% 
    slice(1) %>% ungroup()
  
  predictions = player_info %>% rename('BettingOn' = 'gsis_id') %>% select(response_var, season, Week, BettingOn, Model_Probability, team, opponent_team, gmeday, time_of_day) %>%
    bind_rows(team_info %>% rename('BettingOn' = 'team') %>% select(response_var, season, Week, BettingOn, Model_Probability, team, opponent_team, gmeday, time_of_day))
  
  return(list(player_info, team_info, predictions))
}

get_props = function(bet_category) {
  bet_id = case_when(
    bet_category == 'Receiving' ~ '16570',
    bet_category == 'Receptions' ~ '16821',
    bet_category == 'Rushing' ~ '16571',
    bet_category == 'RushingAttempts' ~ '16820',
    bet_category == 'RushRec' ~ '16572',
    bet_category == 'Touchdown' ~ '12438',
    bet_category == 'Passing' ~ '16569',
    bet_category == 'Team' ~ '4518',
    bet_category == 'SpreadAlternate' ~ '13195'
  )
  base_url = paste0("https://sportsbook-nash.draftkings.com/sites/US-NJ-SB/api/sportscontent/controldata/league/leagueSubcategory/v1/markets?isBatchable=false&templateVars=%2C",bet_id,"&eventsQuery=%24filter%3DleagueId%20eq%20%2788808%27%20AND%20clientMetadata%2FSubcategories%2Fany%28s%3A%20s%2FId%20eq%20%27", bet_id, "%27%29&marketsQuery=%24filter%3DclientMetadata%2FsubCategoryId%20eq%20%27",bet_id,"%27%20AND%20tags%2Fall%28t%3A%20t%20ne%20%27SportcastBetBuilder%27%29&include=Events&entity=events")
  lines = tryCatch({
    fromJSON(content(GET(base_url), as = "text", encoding = "UTF-8"), flatten = TRUE)$selections %>% select(marketId, label, `displayOdds.american`) %>%
      left_join(fromJSON(content(GET(base_url), as = "text", encoding = "UTF-8"))$markets %>% select(id,name), join_by(marketId == id)) %>%
      rename('Odds' = `displayOdds.american`)
  }, error = function(e) {
    print(e$message)
    return(NULL)
  })
  if(!is.null(lines))
  {
    lines$Odds <- gsub("\u2212", "-", lines$Odds)
    if(bet_category == 'Touchdown')
    {
      lines = lines %>% filter(name == 'Anytime TD Scorer') %>% rename('Type' = 'name', 'name' = 'label') %>% mutate(label = 'Anytime TD Scorer')
      lines = lines %>% mutate(name = gsub(paste(bet_category, ifelse(bet_category == 'Touchdown', 'Anytime TD Scorer', 'Yards')), '', name) %>% trimws())
    } else {
      lines = lines %>% mutate(Type = bet_category)
      lines = lines %>% mutate(name = gsub(paste(bet_category, 'Yards'), '', name) %>% trimws())
    }
    
    lines = lines %>% mutate(profit_per_100 = ifelse(as.numeric(Odds) < 0, (100*100/abs(as.numeric(Odds))), as.numeric(Odds)))
    
    return(lines)
  } else{
    return(NULL)
  }
}

clean_names = function(name)
{
  return(tolower(name) %>% str_remove_all("[[:punct:]]+") %>% str_remove("\\b(jr|sr|i{1,3}|iv|v|vi{1,3}|ix|x|xi{1,3})\\b") %>% str_squish() %>% trimws())
}

join_preds_and_props = function(player_preds, team_preds, props)
{
  preds$type = ifelse(preds$response_var == 'anytime_td_scorer', 'anytime TD scorer', sapply(strsplit(preds$response_var, '_'), function(x) x[1]))
  joined = preds %>% mutate(cleaned_names = clean_names(names)) %>% left_join(props %>% mutate(cleaned_name = clean_names(name)), join_by('cleaned_names' == 'cleaned_name', 'label' == 'label', 'Type' == 'Type')) %>% filter(!is.na(marketId)) %>%
    mutate(Betting_Line_Implied_Prob = ifelse(as.numeric(Odds) < 0, (-1)*as.numeric(Odds) / ((-1)*as.numeric(Odds) + 100), 100 / (as.numeric(Odds) + 100)),
           Timeslot = paste(gameday, time_of_day)) %>%
    rename('Player' = 'names') %>%
    mutate(posix_timestamp = as.POSIXct(paste0(gameday, " ", Time, ':--'),format = "%B %d, %Y %I:%M %p",tz = "America/New_York")) %>%
    filter(posix_timestamp > Sys.time() - 3600) %>%
    select(display_name, position, type, label, team, opponent_team, gameday, time_of_day, posix_timestamp, Odds, Model_Probability, Betting_Line_Implied_Prob, profit_per_100)
  return(joined)
}


# display_extra_info = function(df, bet_type, player_name, week, season)
# {
#   print(week)
#   print(season)
#   print(extra_rushing_info)
#   print(bet_type)
#   print(player_name)
#   if(bet_type == 'passing')
#   {
#     print('entered passing')
#     extra_info = df %>% filter(Name == player_name)
#     extra_info_min_year = paste0('In NFL since: ', extra_info$min_year)
#     extra_info_draft = ifelse(!is.na(extra_info$draft_round),
#                               paste0('Drafted Round ', extra_info$draft_round, ' (Pick ', extra_info$draft_pick, ') to team ', team_lookup_table$FullName[team_lookup_table$Team == extra_info$original_draft_team]),
#                               'Undrafted, or no draft info available')
#     extra_info_home = ifelse(extra_info$International == 1, 'International Game',
#                              ifelse(extra_info$Home == 1, 'Home Game', 'Away Game'))
#     extra_info_depth = paste('Depth:', extra_info$Depth)
#     if(week > 1)
#     {
#       if(!is.na(extra_info$Pct_Active) && extra_info$Pct_Active > 0)
#       {
#         extra_info_pct_active_gs = paste0('This season, Active for ', round(100*extra_info$Pct_Active),'% of games, and Starter for ', round(100*extra_info$Pct_GS), '% of games')
#         opp_defense_passyd  = round(extra_info$Opp_Avg_Defense_PassY_Allowed)
#         opp_defense_passyd_score = ifelse(opp_defense_passyd <= 130, 'Very Good Pass Defense',
#                                           ifelse(opp_defense_passyd <= 202, 'Pretty Good Pass Defense',
#                                                  ifelse(opp_defense_passyd <= 241, 'Okay Pass Defense',
#                                                         ifelse(opp_defense_passyd <= 320, 'Not Good Pass Defense',
#                                                                'Terrible Pass Defense'))))
#         extra_info_stats_this_season = paste0('Is the team\'s QB1: ', ifelse(extra_info$Is_Qb1 == 1, 'Yes', 'No'), '<br>',
#                                               'Passing Yds Previous Game: ', extra_info$Passing_Yds_Lag1, '<br>',
#                                               'This Season, Average Passing Yds Per Game: ', round(extra_info$Avg_Passing_Yds), '<br>',
#                                               'This Season, Average Passing Attempts Per Game: ', round(extra_info$Avg_Passing_Att, 1), '<br>',
#                                               'This Season, Average Passing 1st Downs Per Game: ', round(extra_info$Avg_Passing_1D, 1), '<br>',
#                                               'This Season, Average Passing Completions Per Game: ', round(extra_info$Avg_Passing_Cmp,1), '<br>',
#                                               'This Season, Average Passing TD Per Game: ', round(extra_info$Avg_Passing_TD,1), '<br>',
#                                               'This Season, Opponent\'s Average Passing Yards Allowed Per Game: ', opp_defense_passyd, ' (', opp_defense_passyd_score,')', '<br>')
#       } else {
#         extra_info_stats_this_season = 'Player had no active games this season, so no current stats to show.'
#       }
#     } else {
#       extra_info_stats_this_season = 'Since it is only week 1, there are no current season stats to show.'
#     }
#     if(extra_info$min_year < season) {
#       if(!is.na(extra_info$Last_Season_Pct_Active) && extra_info$Last_Season_Pct_Active > 0)
#       {
#         extra_info_stats_last_season = paste0('Last Season, Percent of Games Active: ', round(100*extra_info$Last_Season_Pct_Active), '%<br>',
#                                               'Last Season, Median Passing Yds Per Game: ', round(extra_info$Last_Season_Passing_Yds_median), '<br>',
#                                               'Last Season, Passing Completion Percent: ', round(100*extra_info$Last_Season_Passing_Comp_Pct), '%<br>',
#                                               'Last Season, Average Passing TD Per Game: ', round(extra_info$Last_Season_Passing_TD_mean,1), '<br>')
#       } else {
#         extra_info_stats_last_season = 'Player had no active games last season, or last season stats unavailable.'
#       }
#     } else {
#       extra_info_stats_last_season = 'This is the player\'s first season in the NFL, so no previous season stats to show.'
#     }
#     
#   } else if(bet_type == 'rushing')
#   {
#     print('entered rushing')
#     extra_info = df %>% filter(Name == player_name)
#     print(extra_info)
#     extra_info_min_year = paste0('In NFL since: ', extra_info$min_year)
#     extra_info_draft = ifelse(!is.na(extra_info$draft_round),
#                               paste0('Drafted Round ', extra_info$draft_round, ' (Pick ', extra_info$draft_pick, ') to team ', team_lookup_table$FullName[team_lookup_table$Team == extra_info$original_draft_team]),
#                               'Undrafted, or no draft info available')
#     extra_info_home = ifelse(extra_info$International == 1, 'International Game',
#                              ifelse(extra_info$Home == 1, 'Home Game', 'Away Game'))
#     extra_info_depth = paste('Depth:', extra_info$Depth)
#     if(week > 1)
#     {
#       extra_info_pct_active_gs = paste0('This season, Active for ', round(100*extra_info$Pct_Active),'% of games, and Starter for ', round(100*extra_info$Pct_GS), '% of games')
#       opp_defense_rushyd  = round(extra_info$Opp_Avg_Defense_RushY_Allowed)
#       opp_defense_rushyd_score = ifelse(opp_defense_rushyd <= 58, 'Very Good Rush Defense',
#                                         ifelse(opp_defense_rushyd <= 99, 'Pretty Good Rush Defense',
#                                                ifelse(opp_defense_rushyd <= 124, 'Okay Rush Defense',
#                                                       ifelse(opp_defense_rushyd <= 185, 'Not Good Rush Defense',
#                                                              'Terrible Rush Defense'))))
#       if(!is.na(extra_info$Pct_Active) && extra_info$Pct_Active > 0)
#       {
#         extra_info_stats_this_season = paste0('This Season, Average Rushing Yds Per Game: ',  round(extra_info$Avg_Rushing_Yds,1),'<br>',
#                                               'This Season, Average Rushing Attempts Per Game: ', round(extra_info$Avg_Rushing_Att,1),'<br>',
#                                               'This Season, Average Rushing 1st Downs Per Game: ', round(extra_info$Avg_Rushing_1D, 1), '<br>',
#                                               'Rushing Yds Previous Game: ', extra_info$Rushing_Yds_Lag1, '<br>',
#                                               'This Season, Opponent\'s Defense Avg Rush Yards Allowed Per Game: ', opp_defense_rushyd, ' (', opp_defense_rushyd_score, ')')
#       } else {
#         extra_info_stats_this_season = 'Player had no active games this year, so there are no current season stats to show.'
#       }
#     } else {
#       extra_info_stats_this_season = 'Since it is only week 1, there are no current season stats to show.'
#     }
#     if(extra_info$min_year < season) {
#       if(!is.na(extra_info$Last_Season_Pct_Active) && extra_info$Last_Season_Pct_Active > 0)
#       {
#         extra_info_stats_last_season = paste0('Last Season, Percent of Games Active: ', round(100*extra_info$Last_Season_Pct_Active), '%<br>',
#                                               'Last Season, Average Rushing Yds Per Game: ', round(extra_info$Last_Season_Rushing_Yds_mean, 1), '<br>',
#                                               'Last Season, Highest Rushing Yds in a Game: ', round(extra_info$Last_Season_Rushing_Yds_max), '<br>',
#                                               'Last Season, Average Rushing TD Per Game: ', round(extra_info$Last_Season_Rushing_TD_mean,1), '<br>',
#                                               'Last Season, Average Rushing Attempts Per Game: ', round(extra_info$Last_Season_Rushing_Att_mean,1), '<br>')
#         
#       } else {
#         extra_info_stats_last_season = 'Player had no active games last year, so no previous season stats to show.'
#       }
#     } else {
#       extra_info_stats_last_season = 'This is the player\'s first season in the NFL, so no previous season stats to show.'
#     }
#     
#     print(extra_info_min_year)
#     print(extra_info_draft)
#     print(extra_info_home)
#     print(extra_info_stats_this_season)
#     print(extra_info_stats_last_season)
#   } else if(bet_type == 'receiving')
#   {
#     print('entered receiving')
#     extra_info = df  %>% filter(Name == player_name)
#     extra_info_min_year = paste0('In NFL since: ', extra_info$min_year)
#     extra_info_draft = ifelse(!is.na(extra_info$draft_round),
#                               paste0('Drafted Round ', extra_info$draft_round, ' (Pick ', extra_info$draft_pick, ') to team ', team_lookup_table$FullName[team_lookup_table$Team == extra_info$original_draft_team]),
#                               'Undrafted, or no draft info available')
#     extra_info_home = ifelse(extra_info$International == 1, 'International Game',
#                              ifelse(extra_info$Home == 1, 'Home Game', 'Away Game'))
#     extra_info_depth = paste('Depth:', extra_info$Depth)
#     if(week > 1)
#     {
#       extra_info_pct_active_gs = paste0('This season, Active for ', round(100*extra_info$Pct_Active),'% of games, and Starter for ', round(100*extra_info$Pct_GS), '% of games')
#       if(!is.na(extra_info$Pct_Active) && extra_info$Pct_Active > 0)
#       {
#         extra_info_stats_this_season = paste0('This Season, Average Receiving Yds Per Game: ',  round(extra_info$Avg_Receiving_Yds,1), '<br>',
#                                               'This Season, Average Targets Per Game: ',  round(extra_info$Avg_Receiving_Tgt,1), '<br>',
#                                               'This Season, Average Receiving 1st Downs Per Game: ', round(extra_info$Avg_Receiving_1D, 1), '<br>',
#                                               'This Season, Average Receptions Per Game: ', round(extra_info$Avg_Receiving_Rec,1))
#       } else {
#         extra_info_stats_this_season = 'Player had no active games this year, so there are no current season stats to show.'
#       }
#     } else {
#       extra_info_stats_this_season = 'Since it is only week 1, there are no current season stats to show.'
#     }
#     if(extra_info$min_year < season) {
#       if(!is.na(extra_info$Last_Season_Pct_Active) && extra_info$Last_Season_Pct_Active > 0)
#       {
#         extra_info_stats_last_season = paste0('Last Season, Percent of Games Active: ', round(100*extra_info$Last_Season_Pct_Active), '%<br>',
#                                               'Last Season, Average Receiving Yds Per Game: ', round(extra_info$Last_Season_Receiving_Yds_mean, 1), '<br>',
#                                               'Last Season, Average Targets Per Game: ', round(extra_info$Last_Season_Receiving_Tgt_mean, 1), '<br>',
#                                               'Last Season, Average Receiving 1st Downs Per Game: ', round(extra_info$Last_Season_Receiving_1D_mean, 2), '%<br>',
#                                               'Last Season, Average Receiving Yards Before Catch Per Game: ', round(extra_info$Last_Season_Receiving_YBC_mean,1), '<br>',
#                                               'Last Season, Highest Receiving Yards Before Catch Per Game: ', round(extra_info$Last_Season_Receiving_YBC_max), '<br>')
#         
#       } else {
#         extra_info_stats_last_season = 'Player had no active games last year, so no previous season stats to show.'
#       }
#     } else {
#       extra_info_stats_last_season = 'This is the player\'s first season in the NFL, so no previous season stats to show.'
#     }
#   } else {
#     print('entered touchdown')
#     extra_info = df %>% filter(Name == player_name)
#     extra_info_min_year = paste0('In NFL since: ', extra_info$min_year)
#     extra_info_draft = ifelse(!is.na(extra_info$draft_round),
#                               paste0('Drafted Round ', extra_info$draft_round, ' (Pick ', extra_info$draft_pick, ') to team ', team_lookup_table$FullName[team_lookup_table$Team == extra_info$original_draft_team]),
#                               'Undrafted, or no draft info available')
#     extra_info_home = ifelse(extra_info$International == 1, 'International Game',
#                              ifelse(extra_info$Home == 1, 'Home Game', 'Away Game'))
#     extra_info_depth = paste('Depth:', extra_info$Depth)
#     if(week > 1)
#     {
#       extra_info_pct_active_gs = paste0('This season, Active for ', round(100*extra_info$Pct_Active),'% of games, and Starter for ', round(100*extra_info$Pct_GS), '% of games')
#       if(!is.na(extra_info$Pct_Active) && extra_info$Pct_Active > 0)
#       {
#         extra_info_stats_this_season = paste0('This Season, Average Touchdowns (Rushing/Receiving) Per Game: ',  round(extra_info$Avg_Total_Touchdowns,1), '<br>',
#                                               'This Season, Average Targets Per Game: ',  round(extra_info$Avg_Receiving_Tgt,1), '<br>',
#                                               'This Season, Average Receptions Per Game: ', round(extra_info$Avg_Receiving_Rec, 1), '<br>',
#                                               'This Season, Average Receiving Yards After Catch Per Game: ', round(extra_info$Avg_Receiving_YAC, 1), '<br>',
#                                               'This Season, Average Rushing Yards After Catch Per Game: ', round(extra_info$Avg_Rushing_YAC, 1), '<br>')
#       } else {
#         extra_info_stats_this_season = 'Player had no active games this year, so there are no current season stats to show.'
#       }
#     } else {
#       extra_info_stats_this_season = 'Since it is only week 1, there are no current season stats to show.'
#     }
#     if(extra_info$min_year < season) {
#       if(!is.na(extra_info$Last_Season_Pct_Active) && extra_info$Last_Season_Pct_Active > 0)
#       {
#         extra_info_stats_last_season = paste0('Last Season, Percent of Games Active: ', round(100*extra_info$Last_Season_Pct_Active), '%<br>',
#                                               'Last Season, Average Touchdowns (Rushing/Receiving) Per Game: ', round(extra_info$Last_Season_Total_Touchdowns_mean, 1), '<br>',
#                                               'Last Season, Standard Deviation of Touchdowns (Rushing/Receiving) Per Game: ', round(extra_info$Last_Season_Total_Touchdowns_sd, 2), '<br>',
#                                               'Last Season, Average Receiving 1st Downs Per Target: ', round(extra_info$Last_Season_Receiving_1D_Per_Tgt, 2), '%<br>',
#                                               'Last Season, Average Receiving Yards After Catch Per Game : ', extra_info$Last_Season_Receiving_YAC_max, '<br>')
#         
#       } else {
#         extra_info_stats_last_season = 'Player had no active games last year, so no previous season stats to show.'
#       }
#     } else {
#       extra_info_stats_last_season = 'This is the player\'s first season in the NFL, so no previous season stats to show.'
#     }
#   }
#   print(c(extra_info_min_year,
#           extra_info_draft,
#           extra_info_home,
#           extra_info_pct_active_gs,
#           extra_info_stats_this_season,
#           extra_info_stats_last_season,
#           extra_info_depth))
#   return(c(extra_info_min_year,
#            extra_info_draft,
#            extra_info_home,
#            extra_info_pct_active_gs,
#            extra_info_stats_this_season,
#            extra_info_stats_last_season,
#            extra_info_depth))
# }



