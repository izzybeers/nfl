library(httr)
library(RSelenium)
library(wdman)
library(chromote)
library(stringr)
library(xml2)
library(dplyr)
library(slider)
library(lubridate)
library(rvest)

options(chromote.headless = "new")
Sys.setenv(CHROMOTE_CHROME = "/Users/izzybeers/chrome-headless-shell/mac-136.0.7103.49/chrome-headless-shell-mac-x64/chrome-headless-shell")

team_lookup_table = read.csv('https://docs.google.com/spreadsheets/d/e/2PACX-1vT9_LcNO2d8L5kzbJQZZti9kxfAZRFRAl2oJz5WlpusfvL1txbkc8OU6BSlB54TA9HCBHRlIxi9MpuT/pub?gid=0&single=true&output=csv')

#any changes to here also need to be made in app.r since published apps can't access this file:
passing_numbers = seq(150,360,30)
rushing_numbers = c(25,seq(40,160,20))
receiving_numbers = c(25,seq(40,160,20))

passing_response = c()
for(n in passing_numbers)
{
  passing_response = c(passing_response, paste0('Passing_Yds_', n))
}
rushing_response = c()
for(n in rushing_numbers)
{
  rushing_response = c(rushing_response, paste0('Rushing_Yds_', n))
}
receiving_response = c()
for(n in rushing_numbers)
{
  receiving_response = c(receiving_response, paste0('Receiving_Yds_', n))
}
touchdown_response = 'Anytime_Touchdown'

get_html_content = function(url, max_retries = 3, skip_to_chromote = FALSE, extra_wait = 0) {
  retries = 0
 
for (i in 1:max_retries) {
  if(!skip_to_chromote)
  {
    try_result = try({
      session <- session(url, user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 Chrome/113.0.0.0 Safari/537.36"))
      page <- read_html(session)
      if(length(html_children(page)) > 0)
      {
        return(page)  # Success — exit function
      } else {
        page = NULL
        stop("Empty content detected — triggering fallback to Chromote")
      }
    }, silent = TRUE)
  }
  
  # If the above failed, try with Chromote
  try_result = try({
    message("Using chromote")
    b <- ChromoteSession$new()
    b$Page$navigate(url)
    Sys.sleep(extra_wait) #if certain sites take a few more seconds to load before pulling result, like nfl's site
    result <- b$Runtime$evaluate("document.documentElement.outerHTML")
    html_content <- result$result$value
    page = read_html(html_content)
    
    return(page)  # Success — exit function
  })

  if (!is.null(try_result)) return(try_result)
      
    # Only wait if both methods failed
    wait_time <- runif(1, 2, 4)
    message(paste("Both methods failed. Waiting", round(wait_time, 1), "seconds before retrying..."))
    Sys.sleep(wait_time)
}


stop(paste(max_retries, "attempts failed to retrieve", url))

}

conditionally_remove = function(df, col_list) #normally, to remove a columnm you would do %>% select(-col_name), but if that col name doesn't exist, it gives error. so this conditionally removes if it exists.
{
  for (c in col_list)
  {
     if (c %in% colnames(df))
     {
       df = df %>% select(-!!sym(c))
     }
  }
  return(df)
}

get_player_bio = function(player_row)
{

  url = paste0("https://www.pro-football-reference.com",player_row$links)
  html_response = get_html_content(url = url)
  if (any(str_detect(html_response %>% html_nodes("p") %>% as.character(), 'block traffic')))
  {
    print("Site detected scraping. Try again in one hour.")
    Sys.sleep(65*60)
    html_response = get_html_content(url = url)
  }
  
  #player bio:
  info = html_response %>% html_nodes("p") %>% html_text(trim = TRUE)
  
  
  
  name = player_row$names
  if(length(info) > 0)
  {
    height_weight = info[str_detect(info, '[0-9]lb|[0-9]cm|[0-9]kg')]
    if(length(height_weight) == 0)
    {
      height = NA
      weight = NA
    } else{
      height_string = str_extract(height_weight,"[0-9]+-[0-9]*")
      ft = sapply(strsplit(height_string, '-'), function(x) x[1])
      inch = sapply(strsplit(height_string, '-'), function(x) x[2])
      height = 12*as.numeric(ft)+as.numeric(inch)
      weight = gsub('lb','',str_extract(height_weight, "[0-9]+lb"))
    }
    current_team = gsub('Team:', '', info[which(str_detect(info, 'Team:'))]) %>% trimws()
    if(length(current_team)  == 0)
    {
      current_team = NA
    }
    college = trimws(gsub('College:|\\n|\\t|\\(College Stats\\)','',info[str_detect(info,'College')]), which = "both")
    if(length(college) > 0)
    {
      college = ifelse(!is.null(college) && length(college) >= 1, gsub("\u00A0", "", college)[1], NA) #remove the invisible space at the end
    } else {
      college = NA
    }
    birthday_text = info[str_detect(info, 'Born:')]
    if(length(birthday_text) > 0)
    {
       birth_year = str_extract(birthday_text, '[0-9]{4}')
       birth_day = str_pad(gsub(',', '', str_extract(birthday_text, '[0-9]{1,2},')), width = 2, side = 'left', pad = '0')
       birth_month = str_extract(birthday_text, paste0(month.name, collapse =  '|')) %>% match(month.name)
       birthday = paste0(birth_year,'-',birth_month,'-',birth_day)
    } else{
      birthday = NA
    }
    draft = info[str_detect(info, 'Draft:')]
    if(length(draft) > 0)
    {
      draft_round = draft %>% str_extract('[0-9]+.*round') %>% str_extract('[0-9]+') %>% as.numeric()
      draft_pick = draft %>% str_extract('\\([0-9]+.*overall') %>% str_extract('[0-9]+') %>% as.numeric()
      year_drafted = draft %>% str_extract('[0-9]+ NFL Draft') %>% str_extract('[0-9]+') %>% as.numeric()
      t = draft %>% str_extract(paste(team_lookup_table$FullName, collapse = '|'))
      original_draft_team = unique(team_lookup_table$Team[team_lookup_table$FullName == t])
    } else {
      draft_round = NA
      draft_pick = NA
      year_drafted = NA
      original_draft_team = NA
    }
    
  } else {
    print('no info')
    height = NA
    weight = NA
    college = NA
    draft_round = NA
    draft_pick = NA
    year_drafted = NA
    birthday = NA
    original_draft_team = NA
  }
  return(data.frame(name, current_team, birthday, height, weight, college, year_drafted, draft_round, draft_pick, original_draft_team))
}

prepare_gamelog_table = function(df, df_advanced_p = NULL, df_advanced_rr = NULL, player_row, yr)
  #this function takes a gamelog table, plus optional advanced passing tables and advanced rushing&receiving tables, and scrapes and cleans/prepares them
{
    colnames(df) = ifelse(colnames(df) != '',
                                   paste(colnames(df), df[1,], sep = '_'),
                                   df[1,])
    df = df[2:nrow(df),] 
    colnames(df)[which(colnames(df) %in% c('', 'NA'))] = 'Game_Location'

    
    if(sum(colnames(df) == 'Passing_Yds') == 2)
    {
      colnames(df)[which(colnames(df) == 'Passing_Yds')] = c('Passing_Yds', 'Sacked_Yds')
    }
    
    if('Snap Counts_OffSnp' %in% colnames(df) & 'Snap Counts_Off%' %in% colnames(df))
    {
      df = df %>% mutate(Total_Team_Off_Snaps = as.numeric(`Snap Counts_OffSnp`)/(as.numeric(`Snap Counts_Off%`)/100))
    }
    if('Snap Counts_STSnp' %in% colnames(df) & 'Snap Counts_ST%' %in% colnames(df))
    {
      df = df %>% mutate(Total_Team_ST_Snaps = as.numeric(`Snap Counts_STSnp`)/(as.numeric(`Snap Counts_ST%`)/100))
    }
    
    df = df[,-which(str_detect(colnames(df), '\\/|%'))] %>% conditionally_remove(c('Rk', 'Gcar'))
    df = df %>% filter(Week != '' & !is.na(Week) & !str_detect(Week, '[A-Za-z]'))
    
    if (any(str_detect(colnames(df), 'Receiving') | str_detect(colnames(df), 'Rushing') |  str_detect(colnames(df), 'Passing')))
    {
      if(!is.null(df_advanced_rr) && nrow(df_advanced_rr) > 0)
      {
      
        colnames(df_advanced_rr) = ifelse(colnames(df_advanced_rr) != '',
                                         paste(colnames(df_advanced_rr), df_advanced_rr[1,], sep = '_'),
                                         df_advanced_rr[1,])
        df_advanced_rr = df_advanced_rr[2:nrow(df_advanced_rr),]
        colnames(df_advanced_rr)[which(colnames(df_advanced_rr) == '')] = 'Game_Location'
        df_advanced_rr = df_advanced_rr[,-which(str_detect(colnames(df_advanced_rr), '\\/|%'))] %>% conditionally_remove(c('Rk', 'Gcar', 'Snap Counts_DefSnp')) %>%
          filter(GS %in% c('*', '') & (Gtm != '' & !is.na(Gtm)))
      }
        
      if(!is.null(df_advanced_p) && nrow(df_advanced_p) > 0)
      {
        colnames(df_advanced_p) =  ifelse(colnames(df_advanced_p) != '',
                                                        paste(colnames(df_advanced_p), df_advanced_p[1,], sep = '_'),
                                                        df_advanced_p[1,])
        df_advanced_p = df_advanced_p[2:nrow(df_advanced_p),]
        colnames(df_advanced_p)[which(colnames(df_advanced_p) == '')] = 'Game_Location'
        df_advanced_p = df_advanced_p[,-which(str_detect(colnames(df_advanced_p), '\\/|%'))] %>%
          filter(GS %in% c('*', '') & (Gtm != '' & !is.na(Gtm))) %>%
          conditionally_remove(c('Rk', 'Gcar', 'Week', 'Date', 'Team', 'Game_Location', 'Opp', 'Result', 'GS', 'Snap Counts_DefSnp'))
          
      }
      
      if((!is.null(df_advanced_rr) && nrow(df_advanced_rr) > 0) & (!is.null(df_advanced_p) && nrow(df_advanced_p) > 0))
      {
        shared_columns = setdiff(intersect(colnames(df_advanced_rr), colnames(df_advanced_p)), 'Gtm')
        df_advanced = df_advanced_rr %>% left_join(df_advanced_p %>% select(-shared_columns), join_by('Gtm'))
      } else if ((!is.null(df_advanced_rr) && nrow(df_advanced_rr) > 0))
      {
        df_advanced = df_advanced_rr
      } else if ((!is.null(df_advanced_p) && nrow(df_advanced_p) > 0)) {
        df_advanced = df_advanced_p
      } else {
        df_advanced = NULL
      }
      
      if(!is.null(df_advanced))
      {
        shared_columns = intersect(colnames(df), colnames(df_advanced))
        combined_df = df  %>% left_join(df_advanced %>% select(-!!setdiff(shared_columns,c('Gtm', 'Team'))), join_by('Gtm', 'Team'))
      } else {
        combined_df = df
      }
      
      combined_df = combined_df[,which(colnames(combined_df) != 'NA')]
      combined_df = combined_df %>%
        mutate(Active = ifelse(str_detect(GS, '[A-Za-z]+'), 0, 1),
               GS = ifelse(is.na(GS), NA, ifelse(GS == '*', 1, 0))) %>%
        filter(!is.na(Week) & Week != '')
      
      if('total_broken_tackles' %in% colnames(combined_df))
      {
        combined_df$total_broken_tackles[combined_df$Active == 0] = NA
      }
      
      combined_df[combined_df == 'Inactive' | combined_df == 'Did Not Play' | combined_df == 'COVID-19 List'] = NA
      touchdown_colnames = colnames(combined_df)[str_detect(colnames(combined_df), 'TD')]
      if (nrow(combined_df) > 1)
      {
        combined_df[,which(!(colnames(combined_df) %in% c('Date', 'Team', 'Game_Location', 'Opp', 'Result', 'GS')))] = sapply(combined_df[,which(!(colnames(combined_df) %in% c('Date', 'Team', 'Game_Location', 'Opp', 'Result', 'GS')))], as.numeric)
        combined_df[,touchdown_colnames] = sapply(combined_df[,touchdown_colnames], as.numeric)
        
      } else {
        combined_df[,which(!(colnames(combined_df) %in% c('Date', 'Team', 'Game_Location', 'Opp', 'Result', 'GS')))] = data.frame(lapply(combined_df[,which(!(colnames(combined_df) %in% c('Date', 'Team', 'Game_Location', 'Opp', 'Result', 'GS')))], as.numeric))
        combined_df[,touchdown_colnames] = data.frame(lapply(combined_df[,touchdown_colnames], as.numeric))
      }
      combined_df = combined_df %>% mutate(Total_Touchdowns = rowSums(across(all_of(touchdown_colnames)), na.rm = TRUE))
  
      colnames(combined_df)[which(colnames(combined_df) == 'Fumbles_Fmb')] = 'Fumbles'
      colnames(combined_df)[which(colnames(combined_df) == 'Fumbles_FL')] = 'Fumbles_Lost'
      colnames(combined_df)[which(colnames(combined_df) == 'Fumbles_FF')] = 'Fumbles_Forced'
      colnames(combined_df)[which(colnames(combined_df) == 'Fumbles_FR')] = 'Fumbles_Recovered'
      colnames(combined_df)[which(colnames(combined_df) =='Fumbles_FRTD')] = 'Fumbles_TD'
    
      combined_df = combined_df %>% mutate(Season = yr, Name = player_row$names, Position = player_row$positions, player_id = player_row$player_id)
      if('Date' %in% colnames(combined_df))
      {
        combined_df = combined_df %>% mutate(Month = Date %>% substring(6,7),
                                                 day_of_week = weekdays(as.Date(Date)))
      }
      if('Game_Location' %in% colnames(combined_df))
      {
        combined_df = combined_df %>% mutate(Game_Location = ifelse(Game_Location == '@', 'Away', 'Home'))
      }
      # if('Result' %in% colnames(combined_df))
      # {
      #   combined_df = combined_df %>% mutate(Win = sapply(strsplit(Result, ','), function(x) ifelse(x[1] == 'W', 1, 0)),
      #                                            Differential = sapply(strsplit(gsub(' \\(OT\\)', '', Result), ','), function(x) sapply(strsplit(x[2],'-'), function(y) as.numeric(y[1])-as.numeric(y[2]))))
      # }
    
      combined_df = combined_df %>% conditionally_remove("Result")
      
      return(combined_df)
    } else {
  return(NULL)
    }
}

get_current_team = function(link_suffix)
{
  url = paste0("https://www.pro-football-reference.com", link_suffix)
  html_response = get_html_content(url = url)
  #player bio:
  info = html_response %>% html_nodes("p") %>% html_text(trim = TRUE)
  team = gsub('Team:', '', info[which(str_detect(info, 'Team:'))]) %>% trimws()
  return(team)
}

get_season_schedule = function(season, wk, team = NULL)
{
  url = paste0("https://www.pro-football-reference.com/years/", season, '/games.htm')
  schedule_html = get_html_content(url = url)
  schedule_table_node = schedule_html %>% html_node('table#games')
  if (!inherits(schedule_table_node, "xml_missing"))
  {
    schedule_table = schedule_table_node %>% html_table(fill = TRUE)
  } else {
     schedule_table = NULL
  }
  if('Winner/tie' %in% colnames(schedule_table))
  {
    colnames(schedule_table) = c('Week', 'Day', 'Date', 'Time', 'Winner', 'at_symbol', 'Loser', 'boxscore', 'WinPts', 'LosePts', 'YdsW', 'TOW', 'YdsL', 'TOL') 
    schedule_table = schedule_table %>%
      mutate(Home = case_when(at_symbol == '@' ~ Loser,  .default = Winner),
             Away = case_when(at_symbol == '@' ~ Winner,  .default = Loser))
    schedule_table = schedule_table %>% filter(Week == wk) %>% select(Week, Date, Away, Home) %>%
      left_join(team_lookup_table %>% select(FullName, Team) %>% rename('HomeTeam' = 'Team'), join_by('Home' == 'FullName')) %>%
      left_join(team_lookup_table %>% select(FullName, Team) %>% rename('AwayTeam' = 'Team'), join_by('Away' == 'FullName')) %>%
      select(-c('Home','Away'))
  } else {
    colnames(schedule_table) = c('Week', 'Day', 'Date', 'Away', 'AwayPts', 'at_symbol', 'Home', 'HomePts', 'Time')
    schedule_table = schedule_table %>% filter(Week == wk) %>% select(Week, Date, Away, Home) %>%
      mutate(Date = as.Date(Date, format = "%B %d, %Y")) %>%
      left_join(team_lookup_table %>% select(FullName, Team) %>% rename('HomeTeam' = 'Team'), join_by('Home' == 'FullName')) %>%
      left_join(team_lookup_table %>% select(FullName, Team) %>% rename('AwayTeam' = 'Team'), join_by('Away' == 'FullName')) %>%
      select(-c('Home','Away'))
    
  }
  schedule_table = rbind(schedule_table %>% mutate(Team = HomeTeam, Opp = AwayTeam, Game_Location = 'Home'), schedule_table %>% mutate(Team = AwayTeam, Opp = HomeTeam, Game_Location = 'Away')) %>%
    select(Week, Date, Team, Opp, Game_Location)
  if(!is.null(team))
  {
    schedule_table = schedule_table %>% filter(Team == team)
  }
  
  return(schedule_table)
}

get_game_log = function(player_row, yr, wk = NULL, gamelog_table_tag, gamelog_playoffs_table_tag, gamelog_advanced_rushing_table_tag, gamelog_advanced_passing_table_tag, gamelog_advanced_playoffs_passing_table_tag, gamelog_advanced_playoffs_rushing_table_tag)
{
  url = paste0("https://www.pro-football-reference.com",gsub('.htm','',player_row$links),'/gamelog/',yr,'/')
  url_advanced = paste0(url, 'advanced')
  game_log_html = get_html_content(url)
  Sys.sleep(1)
  game_log_html_advanced = get_html_content(url_advanced)
  
  if (any(str_detect(game_log_html %>% html_nodes("p") %>% as.character(), 'block traffic')))
  {
    print("Site detected scraping. Try again in one hour.")
    Sys.sleep(65*60)
    game_log_html = get_html_content(url)
    Sys.sleep(1)
    game_log_html_advanced = get_html_content(url_advanced)
  }
  if (any(str_detect(game_log_html_advanced %>% html_nodes("p") %>% as.character(), 'block traffic')))
  {
    print("Site detected scraping. Try again in one hour.")
    Sys.sleep(65*60)
    game_log_html_advanced = get_html_content(url_advanced)
  }
  

  html_path_table = paste0('table#', gamelog_table_tag)
  html_path_table_playoffs =  paste0('table#', gamelog_playoffs_table_tag)
  html_path_advanced_table_rushing = paste0('table#', gamelog_advanced_rushing_table_tag)
  html_path_advanced_table_passing = paste0('table#', gamelog_advanced_passing_table_tag)
  html_path_playoffs_advanced_table_passing = paste0('table#', gamelog_advanced_playoffs_passing_table_tag)
  html_path_playoffs_advanced_table_rushing = paste0('table#', gamelog_advanced_playoffs_rushing_table_tag)
  
  gamelog_table_node = game_log_html %>% html_node(html_path_table)
  if (!inherits(gamelog_table_node, "xml_missing"))
  {
    gamelog_table = gamelog_table_node %>% html_table(fill = TRUE)
  } else {
    gamelog_table = NULL
  }
  
  gamelog_playoffs_table_node = game_log_html %>% html_node(html_path_table_playoffs)
  if (!inherits(gamelog_playoffs_table_node, "xml_missing"))
  {
    gamelog_playoffs_table = gamelog_playoffs_table_node %>% html_table(fill = TRUE)
  } else {
    gamelog_playoffs_table = NULL
  }
  
  gamelog_advanced_passing_table_node = game_log_html_advanced %>% html_node(html_path_advanced_table_passing)
  if (!inherits(gamelog_advanced_passing_table_node, "xml_missing"))
  {
    gamelog_advanced_passing_table = gamelog_advanced_passing_table_node %>% html_table(fill = TRUE)
  } else{
    gamelog_advanced_passing_table = NULL
  }
  
  gamelog_advanced_rushing_table_node = game_log_html_advanced %>% html_node(html_path_advanced_table_rushing)
  if (!inherits(gamelog_advanced_rushing_table_node, "xml_missing"))
  {
    gamelog_advanced_rushing_table = gamelog_advanced_rushing_table_node %>% html_table(fill = TRUE)
  } else{
    gamelog_advanced_rushing_table = NULL
  }
  
  gamelog_advanced_playoffs_passing_table_node = game_log_html_advanced %>% html_node(html_path_playoffs_advanced_table_passing)
  if (!inherits(gamelog_advanced_playoffs_passing_table_node, "xml_missing"))
  {
    gamelog_advanced_playoffs_passing_table = gamelog_advanced_playoffs_passing_table_node %>% html_table(fill = TRUE)
  } else{
    gamelog_advanced_playoffs_passing_table = NULL
  }
  
  gamelog_advanced_playoffs_rushing_table_node = game_log_html_advanced %>% html_node(html_path_playoffs_advanced_table_rushing)
  if (!inherits(gamelog_advanced_playoffs_rushing_table_node, "xml_missing"))
  {
    gamelog_advanced_playoffs_rushing_table = gamelog_advanced_playoffs_rushing_table_node %>% html_table(fill = TRUE)
  } else{
    gamelog_advanced_playoffs_rushing_table = NULL
  }
  
  if(!is.null(gamelog_table) && nrow(gamelog_table) > 0 && ('Receiving' %in% colnames(gamelog_table) | 'Rushing' %in% colnames(gamelog_table) | 'Passing' %in% colnames(gamelog_table)))
  {
    gamelog_table = prepare_gamelog_table(df = gamelog_table,
                                          df_advanced_rr = gamelog_advanced_rushing_table,
                                          df_advanced_p = gamelog_advanced_passing_table,
                                          player_row = player_row,
                                          yr = yr)
    
    gamelog_table = gamelog_table %>% mutate(Playoffs = 0)
    
    
    if(!is.null(gamelog_playoffs_table) && nrow(gamelog_playoffs_table) > 0)
    {
      playoffs_table = prepare_gamelog_table(df = gamelog_playoffs_table,
                                             df_advanced_rr = gamelog_advanced_playoffs_rushing_table,
                                             df_advanced_p = gamelog_advanced_playoffs_passing_table,
                                             player_row = player_row,
                                             yr = yr)
      
      if(!is.null(playoffs_table) && nrow(playoffs_table) > 0)
      {
      
        missing_columns_from_playoffs = setdiff(colnames(gamelog_table), colnames(playoffs_table))
        playoffs_table[,missing_columns_from_playoffs] = NA
        missing_columns_from_gamelog = setdiff(colnames(playoffs_table), colnames(gamelog_table))
        gamelog_table[,missing_columns_from_gamelog] = NA
        playoffs_table = playoffs_table %>% mutate(Playoffs = 1)
        
        gamelog_table = data.frame(rbind(gamelog_table, playoffs_table))
      }
    }

    return(gamelog_table %>% filter(Week != '' & !is.na(Week)))
    
    } else {
      return(NULL)
    }
}

# 
# get_cumulative = function(log, last3, skip, team)
# {
#   for (c in setdiff(colnames(log), skip))
#   {
#     rows = rbind()
#     for (g in log$Gtm)
#     {
#       if(g == 1) #first game, no historical data, NA for everything
#       {
#         result_sum = NA
#         result_avg = NA
#         result_median = NA
#         result_min = NA
#         result_max = NA
#         result_sd = NA
#       } else { #not game #1
#           if(last3 == TRUE)
#           {
#             if (g <= 4) #if there's only been 3 or less previous games, just take them all
#             {
#               previous_games = log %>% filter(Gtm < g)
#             }
#             else if (c %in% team) #team-based stats don't rely on whether player was active
#             {
#               previous_games = log %>% filter(Gtm < g & Gtm >= (g-3))
#             } else { #out of the past 5 games, choose the most recent 3 where player was active.
#               num_active_games = 0 #counter
#               games_back = 3
#               while(num_active_games < 3 & games_back <= 5)
#               {
#                 new_g = g - games_back
#                 previous_games = log %>% filter(Gtm >= max(new_g,1) & Gtm < g)
#                 num_active_games = sum(previous_games$Active)
#                 if(num_active_games < 3)
#                 {
#                   games_back = games_back + 1
#                 }
#               }
#             }
#             
#           } else {
#             
#             if (c %in% team)
#             {
#               previous_games = log %>% filter(Gtm < g)
#             } else { #player stats depend on whether player was active
#               previous_games = log %>% filter(Gtm < g & Active == 1)
#             }
#             
#           }
#         
#         column_values = previous_games %>% select(!!sym(c)) %>% pull()
#         
#         if(any(!is.na(column_values)))   
#         {
#           result_sum = column_values %>% sum(na.rm = TRUE)
#           result_avg = column_values %>% mean(na.rm = TRUE)
#           result_median = column_values %>% median(na.rm = TRUE)
#           result_min = column_values %>% min(na.rm = TRUE)
#           result_max = column_values %>% max(na.rm = TRUE)
#           result_sd = column_values %>% sd(na.rm = TRUE)
#         } else {
#           result_sum = NA
#           result_avg = NA
#           result_median = NA
#           result_min = NA
#           result_max = NA
#           result_sd = NA
#         }
#       }
#     rows = rbind(rows, c(result_sum, result_avg, result_median, result_min, result_max, result_sd))
# 
#     }
#   
#     rows = rows %>% data.frame()
#     colnames(rows) = paste0(ifelse(last3 == TRUE, 'Last3_',''), c('Cumulative_','Avg_','Median_','Min_','Max_','SD_'), c)
#     for (new_colname in colnames(rows))
#     {
#       vals = rows[,new_colname]
#       log[[new_colname]] = vals
#     }
#     
#   }
# 
#     return(log)
#   
# }

compute_slider_cumulatives = function(df, basic_cols = c()) {
  df = df %>% arrange(Week, Gtm)
  
  stat_cols = df %>%
    select(where(is.numeric)) %>%
    colnames()
  
  for (c in setdiff(stat_cols, basic_cols))
  {

    vals = df[[c]]
    
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
      if(all(unique_values[which(!is.na(unique_values))] %in% c(0,1)))
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

add_players_performance_recent_years = function(df)
{
  player_performance_by_season = df %>% group_by(name, Season) %>%
    summarise(num_active_games_last_year = sum(!is.na(gs)),
              num_games_start_last_year = sum(gs == '*', na.rm = TRUE),
              rec_yd_per_game_last_year = sum(Receiving_Yards, na.rm =TRUE)/sum(!is.na(gs)),
              rec_per_game_last_year = sum(rec, na.rm = TRUE)/sum(!is.na(gs)),
              targets_per_game_last_year = sum(targets, na.rm= TRUE)/sum(!is.na(gs)),
              rec_per_target_last_year = sum(rec, na.rm = TRUE)/sum(targets, na.rm = TRUE),
              snaps_per_game_last_year = sum(offense*off_pct, na.rm = TRUE)/sum(!is.na(gs))
    )
  
  df = df %>% mutate(last_season = Season - 1, years_ago_2 = Season - 2 ) 
  df = df %>% left_join(player_performance_by_season, join_by('name' == 'name', 'last_season' == 'Season'))
  
  df = df %>% left_join(player_performance_by_season %>% rename('num_active_games_2_years_ago' = 'num_active_games_last_year',
                                                                      'num_games_start_2_years_ago' = 'num_games_start_last_year',
                                                                      'rec_yd_per_game_2_years_ago' = 'rec_yd_per_game_last_year',
                                                                      'rec_per_game_2_years_ago' = 'rec_per_game_last_year',
                                                                      'targets_per_game_2_years_ago' = 'targets_per_game_last_year',
                                                                      'rec_per_target_2_years_ago' = 'rec_per_target_last_year',
                                                                      'snaps_per_game_2_years_ago' = 'snaps_per_game_last_year'), join_by('name' == 'name', 'years_ago_2' == 'Season'))
  
  return(df %>% data.frame())
}




get_team_game_logs = function(url, y)
{
  doc = get_html_content(url = url)
  
  if (any(str_detect(doc %>% html_nodes("p") %>% as.character(), 'block traffic')))
  {
    print("Site detected scraping. Try again in one hour.")
    Sys.sleep(65*60)
    doc = get_html_content(url = url)
  }
  
  team_gamelog_table_node =  doc %>% html_node('table#games')
  if (xml_length(team_gamelog_table_node) > 0)
  {
    team_gamelog_table = team_gamelog_table_node %>% html_table(fill = TRUE)
  } else {
    team_gamelog_table = NULL
  }
  if(!is.null(team_gamelog_table) && nrow(team_gamelog_table) > 0)
  {
    
    colnames(team_gamelog_table) = ifelse(colnames(team_gamelog_table) != '',
                                          paste(colnames(team_gamelog_table), team_gamelog_table[1,], sep = '_'),
                                          team_gamelog_table[1,])
    team_gamelog_table = team_gamelog_table[2:nrow(team_gamelog_table),]
    if(!(any(team_gamelog_table[,5] == 'boxscore'))) 
    {
      colnames(team_gamelog_table)[which(colnames(team_gamelog_table) %in% c('', 'NA'))] = c('Time', 'Game_Location', 'Result')
      team_gamelog_table = team_gamelog_table %>% mutate(Week = as.numeric(Week)) %>% filter(!is.na(Week)) #remove weeks ike Pre, Pre 2, Regular Season, etc
      team_gamelog_table = team_gamelog_table %>% mutate(Win = ifelse(Result == 'preview', NA, ifelse(unlist(sapply(strsplit(Result, ','), function(x) x[1])) == 'W', 1, 0)),
                                                         Scores = ifelse(Result == 'preview', NA, unlist(sapply(strsplit(Result, ','), function(x) x[2]))) %>% trimws(),
                                                         Score1 = ifelse(is.na(Win), NA, unlist(sapply(strsplit(Scores, '-'), function(x) x[1]))),
                                                         Score2 = ifelse(is.na(Win), NA, unlist(sapply(strsplit(Scores, '-'), function(x) x[2]))),
                                                         Winner_Score = as.numeric(ifelse(is.na(Win), NA, ifelse(Score1 >= Score2, Score1, Score2))),
                                                         Loser_Score = as.numeric(ifelse(is.na(Win), NA, ifelse(Score2 < Score1, Score2, Score1))),
                                                         Score_Tm = as.numeric(ifelse(is.na(Win), NA, ifelse(Win == 'W', Winner_Score, Loser_Score))),
                                                         Score_Opp = as.numeric(ifelse(is.na(Win), NA, ifelse(Win == 'W', Loser_Score, Winner_Score))),
                                                         Differential = as.numeric(ifelse(is.na(Win), NA, ifelse(Win == 'W', Winner_Score - Loser_Score, Loser_Score - Winner_Score))),
                                                         OT = as.numeric(NA)) %>%
        select(Week, Day, Date, Time, Game_Location, Opp, Win, Differential, OT)
    } else {
      colnames(team_gamelog_table)[which(colnames(team_gamelog_table) %in% c('', 'NA'))] = c('Time', 'Boxscore', 'Result', 'Game_Location')
      team_gamelog_table = team_gamelog_table %>% mutate(
        Win = ifelse(is.na(Result), NA, ifelse(Result == 'W', 1, 0)),
        OT = ifelse(OT == 'OT', 1, 0),
        Differential = as.numeric(Score_Tm) - as.numeric(Score_Opp),)
    }
  
    team_gamelog_table = team_gamelog_table %>% filter(Opp != 'Bye Week' & Opp != '') %>%
      rename(Opp_FullName = Opp) %>%
      mutate(Opp_FullName = ifelse(Opp_FullName == 'Washington Football Team', 'Washington Commanders', Opp_FullName),
             Time = str_extract(Time, '[0-9]+:[0-9]+(PM|AM)'),
             time_parsed = parse_date_time(Time, orders = "I:Mp"),
             Time_of_Day = case_when(hour(time_parsed) < 11 ~ 'Morning',
                                     hour(time_parsed) < 15 ~ 'Early Window',
                                     hour(time_parsed) < 19 ~ 'Late Window',
                                     TRUE ~ 'Night'),
             Game_Location = ifelse(Game_Location == '@', 'Away', 'Home'),
             Month_Name = trimws(gsub('[0-9]+','',Date)),
             Month = str_pad(match(Month_Name, month.name), width = 2, side = 'left', pad = '0'),
             Week = case_when(Week == 'Wild Card' ~ ifelse(y <= 2020, 18, 19),
                              Week == 'Division' ~ ifelse(y <= 2020, 19, 20),
                              Week == 'Conf. Champ.' ~ ifelse(y <= 2020, 20, 21),
                              Week == 'SuperBowl' ~ ifelse(y <= 2020, 21, 22),
                              .default = as.numeric(Week)),
             Playoffs = ifelse(Week >= ifelse(y <= 2020, 18, 19), 1, 0)
             ) %>%
      mutate(across(
        .cols = matches("Offense|Defense"),
        .fns = ~ as.numeric(ifelse(.x == "", 0, .x))
      )) %>% select(-any_of(c(
        "Rec", "Score_Tm", "Score_Opp", "time_parsed", "Result", "Month_Name", "Boxscore",
        "Expected Points_Offense", "Expected Points_Defense", "Expected Points_Sp. Tms"
      )))
    
    return(team_gamelog_table)
    
  }
}

get_target_rankings = function(df, y, t, injuries)
{
  filtered_table = df[df$Season == y & df$team == t,] %>% select(name, Season, week_num, Avg_Targets_Per_Game) %>%
    left_join(injuries  %>%
                select(year, week, Player, Game_Status),
              join_by('name' == 'Player', 'Season' == 'year', 'week_num' == 'week')) %>%
    filter(!(Game_Status %in% c('Doubtful','Out')))
  
  target_ranks = rbind()
  for (w in 1:max(filtered_table$week_num))
  {
    filtered_table_week = filtered_table %>% filter(week_num == w) %>% mutate(tgt_floor = floor(Avg_Targets_Per_Game)) %>%
      select(name, tgt_floor) %>% arrange(desc(tgt_floor)) %>% mutate(targets_rank = dense_rank(desc(tgt_floor)))
    if(nrow(filtered_table_week) > 0)
    {
      target_ranks = rbind(target_ranks, data.frame(y, t, week = w, filtered_table_week %>% select(name, targets_rank)))
    }
  }
  return(target_ranks)
}

get_historical_weather = function(date, time_of_day, station_link, stadium)
{
  weather_doc = get_html_content(paste0(station_link,date))
  
  min_temp = weather_doc %>% html_node('.temp_mn .value') %>% html_text(trim = TRUE)
  
  if(is.na(min_temp))
  {
    date_to_scrape = as.Date(date) - 1
    weather_doc = get_html_content(paste0(station_link,date))
    min_temp = weather_doc %>% html_node('.temp_mn .value') %>% html_text(trim = TRUE)
    mean_temp = weather_doc %>% html_node('.temp .value') %>% html_text(trim = TRUE)
    max_temp = weather_doc %>% html_node('.temp_mx .value') %>% html_text(trim = TRUE)
    visibility = NA
    mean_wind_speed = NA
  } else {
    mean_temp <- weather_doc %>% html_node('.temp .value') %>% html_text(trim = TRUE)
    max_temp <- weather_doc %>% html_node('.temp_mx .value') %>% html_text(trim = TRUE)
    visibility <- weather_doc %>% html_node('.visib .value') %>% html_text(trim = TRUE)
    mean_wind_speed <- weather_doc %>% html_node('.wdsp .value') %>% html_text(trim = TRUE)
  }
  
  timezone = ifelse(stadium %in% c('SEA','SFO','DEN'), 'Earlier', 'Later')
  

  approx_temperature = ifelse(time_of_day == 'Night' & timezone == 'Later', min_temp,
                              ifelse((time_of_day == 'Night' & timezone == 'Earlier') | time_of_day == 'Late Window' & timezone == 'Later',
                                     mean_temp, max_temp))  %>% as.numeric()
  approx_visibility = visibility %>% as.numeric()
  Approx_Wind_Speed = mean_wind_speed %>% as.numeric()

  # return(data.frame(
  #   date = date,
  #   stadium = stadium,
  #   approx_temperature = ifelse(time_of_day == 'Night' & timezone == 'Later', min_temp,
  #                               ifelse((time_of_day == 'Night' & timezone == 'Earlier') | time_of_day == 'Late Window' & timezone == 'Later',
  #                                      mean_temp, max_temp))  %>% as.numeric(),
  #   approx_visibility = visibility %>% as.numeric(),
  #   Approx_Wind_Speed = mean_wind_speed %>% as.numeric())
  # # )
  
  return(list(approx_temperature = approx_temperature, 
              approx_visibility = approx_visibility, 
              approx_wind_speed = Approx_Wind_Speed))
}

get_forecasted_weather = function(timestamp, lat, long, api_key)
{
  
  url = paste0("https://api.openweathermap.org/data/3.0/onecall",
               "?lat=", lat,
               "&lon=", long,
               "&type=hour",
               "&units=imperial",
               "&appid=", api_key)
  
  res = GET(url)
  results = content(res, as = "parsed")
  if(difftime(timestamp, Sys.time(), units = "hours") %>% as.numeric() < 47)
  {
    index = which(sapply(1:length(results$hourly), function(x) as.POSIXct(results$hourly[[x]]$dt, origin = "1970-01-01", tz = "America/New_York") == timestamp))
    temp = results$hourly[[index]]$temp
    visibility = results$hourly[[index]]$visibility
    wind_speed = results$hourly[[index]]$wind_speed
  } else {
    temp = NA %>% as.numeric()
    visibility = NA %>% as.numeric()
    wind_speed = NA %>% as.numeric()
  }
  return(list(approx_temperature = temp,
              approx_visibility = visibility,
              approx_wind_speed = wind_speed))
}


get_qb_list = function()
{
  qb_df = rbind()

  for (let in LETTERS)
  {
    player_list = get_html_content(url = paste0("https://www.pro-football-reference.com/players/",let))
    if (any(str_detect(player_list %>% html_nodes("p") %>% as.character(), 'block traffic')))
    {
      print("Site detected scraping. Try again in one hour.")
      Sys.sleep(65*60)
      break
    }
    
    player_list_content = player_list %>% html_nodes(".section_content#div_players")
    total_info = player_list_content %>% html_nodes("p") %>% html_text(trim = TRUE)
    years = str_extract(total_info, '[0-9]+-[0-9]+')
    min_year = sapply(strsplit(years, "-"), function(x) x[1])
    max_year = sapply(strsplit(years, "-"), function(x) x[2])
    positions = gsub('\\(|\\)','',str_extract(total_info, "\\(.*\\)"))
    names = gsub(' \\(', '', str_extract(total_info, '.+\\('))
    links = player_list_content %>% html_nodes("a") %>% html_attr("href")
    
    df = data.frame(names, positions, min_year, max_year,links)
    df = df %>% filter(str_detect(positions,'QB') & max_year >= year_cutoff)
    
    qb_df = rbind(qb_df, df)
  }
  return(qb_df)
}

get_qb_gamelog = function(qb_list, y)
{
  url = paste0("https://www.pro-football-reference.com",gsub('.htm','',qb_list$links),'/gamelog/',y,'/')
  game_log_html = get_html_content(url)
  
  df = data.frame(week_num = game_log_html %>% html_nodes(xpath = paste0('//tbody/tr[not(ancestor::tfoot)]/td[@data-stat="week_num"]')) %>% html_text(trim = TRUE) %>% as.numeric())
  
  
  if (any(str_detect(game_log_html %>% html_nodes("p") %>% as.character(), 'block traffic')))
  {
    print("Site detected scraping. Try again in one hour.")
    Sys.sleep(65*60)
  }
  
  if(length(game_log_html %>% html_nodes(paste0('td[data-stat="reason"]')) %>% html_text(trim = TRUE)>0))
  {
    inactive_week_nums = game_log_html %>%
      html_nodes(xpath = '//tr[td[@data-stat="reason"]]/td[@data-stat="week_num"]') %>%
      html_text(trim = TRUE)
    
    
  } else {
    inactive_week_nums = NULL
  }
  active_index =  which(!df$week_num %in% inactive_week_nums)
  started =  game_log_html %>% html_nodes(xpath = paste0('//tbody/tr[not(ancestor::tfoot)]/td[@data-stat="gs"]')) %>% html_text(trim = TRUE)
  
  
  team = game_log_html %>% html_nodes('td[data-stat="team"] a') %>% html_text(trim =TRUE)
  if(length(active_index) > 0)
  {
    qb_game_stats = data.frame(name = qb_list$names,
                               Season = y,
                               Team = team[active_index],
                               week_num =  df$week_num[active_index],
                               started = started) %>% filter(started == '*') %>% select(-started)
    return(qb_game_stats)
  } else {
    return(NULL)
  }
  
}

get_qb_data = function()
{
  qb_df = get_qb_list()
  #QB GAMES:
  qb_games_played = rbind()
  start = 1
  for (p in start:nrow(qb_df))
  {
    name = qb_df$names[p]
    first_year = min(qb_df$min_year[p])
    final_year = max(qb_df$max_year[p])
    for (y in seq(max(first_year,2022),max(2024,final_year)))
    {
      if(y == this_season)
      {
        all_weeks = 1:(current_week-1)
      } else {
        all_weeks = 1:18
      }
      print(paste(name, y))
      qb_games_played = rbind(qb_games_played, get_qb_gamelog(qb_df[p,], y,))
    }
  }
  return(qb_games_played)
}





 