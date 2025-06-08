library(rvest)
library(httr)
library(RSelenium)
library(wdman)
library(chromote)
library(stringr)
library(xml2)
library(dplyr)
library(slider)
library(lubridate)

options(chromote.headless = "new")
Sys.setenv(CHROMOTE_CHROME = "/Users/izzybeers/chrome-headless-shell/mac-136.0.7103.49/chrome-headless-shell-mac-x64/chrome-headless-shell")

team_lookup_table = read.csv('https://docs.google.com/spreadsheets/d/e/2PACX-1vT9_LcNO2d8L5kzbJQZZti9kxfAZRFRAl2oJz5WlpusfvL1txbkc8OU6BSlB54TA9HCBHRlIxi9MpuT/pub?gid=0&single=true&output=csv')

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
  return(data.frame(name, birthday, height, weight, college, year_drafted, draft_round, draft_pick, original_draft_team))
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

compute_slider_cumulatives <- function(df, basic_cols = c()) {
  df <- df %>% arrange(Week, Gtm)
  
  stat_cols <- df %>%
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



  
remove_uninformative_stats = function(df, column_list)
{
  columns_to_remove = c()
  columns_0_1 = c()
  low_medians = c()
  for (c in column_list)
  {
    pct_nonmissing = mean(!is.na(df[,c]))
    unique_values = df %>% select(!!sym(c)) %>% pull() %>% unique()
    median = df %>% select(!!sym(c)) %>% pull() %>% median(na.rm =TRUE)
    mean = df %>% select(!!sym(c)) %>% pull() %>% mean(na.rm = TRUE)
    if((pct_nonmissing > 0.05) & (length(unique_values) > 1))
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
    colnames(team_gamelog_table)[which(colnames(team_gamelog_table) %in% c('', 'NA'))] = c('Time', 'Boxscore', 'Result', 'Game_Location')
    team_gamelog_table = team_gamelog_table %>% filter(Opp != 'Bye Week' & Opp != '') %>%
      rename(Opp_FullName = Opp) %>%
      mutate(Opp_FullName = ifelse(Opp_FullName == 'Washington Football Team', 'Washington Commanders', Opp_FullName),
             Time = str_extract(Time, '[0-9]+:[0-9]+(PM|AM)'),
             time_parsed = parse_date_time(Time, orders = "I:Mp"),
             Time_of_Day = case_when(hour(time_parsed) < 11 ~ 'Morning',
                                     hour(time_parsed) < 15 ~ 'Early Window',
                                     hour(time_parsed) < 19 ~ 'Late Window',
                                     TRUE ~ 'Night'),
             Win = ifelse(Result == 'W', 1, 0),
             Game_Location = ifelse(Game_Location == '@', 'Away', 'Home'),
             OT = ifelse(OT == 'OT', 1, 0),
             Differential = as.numeric(Score_Tm) - as.numeric(Score_Opp),
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
      )
      ) %>% select(-Rec, -Score_Tm, -Score_Opp, -time_parsed, -Result, -Month_Name, -Boxscore, -`Expected Points_Offense`, -`Expected Points_Defense`, -`Expected Points_Sp. Tms`)
    
    
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

get_weather = function(date, time_of_day, station_link, stadium)
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

get_injuries_data = function(year, week_num)
{
  injuries_html = get_html_content(url = paste0('https://www.nfl.com/injuries/league/', year, '/reg', week_num))

  # Extract player names, practice participation, and game status
  players = injuries_html %>%
    html_nodes("td:nth-child(1)") %>%
    html_text(trim = TRUE)

  position = injuries_html %>%
    html_nodes("td:nth-child(2)") %>%
    html_text(trim = TRUE)

  practice_status = injuries_html %>%
    html_nodes("td:nth-child(4)") %>%
    html_text(trim = TRUE)
  
  game_status = injuries_html %>%
    html_nodes("td:nth-child(5)") %>%
    html_text(trim = TRUE)

  # Combine the data into a data frame
  injury_report = data.frame(
    year,
    week = week_num,
    Player = players,
    position = position,
    Less_Practice = ifelse(practice_status %in% c('Limited Participation in Practice', 'Did Not Participate In Practice'), 0, 1),
    Game_Status = game_status,
    On_Injury_List = 1
  ) %>% filter(position == 'WR') %>% select(-position) %>% unique()
  
  return(injury_report)
}
                   

get_receiving_props <- function() {
  # URL for NFL event group (replace "88808" if the event group ID changes)
  base_url <- "https://sportsbook.draftkings.com/sites/US-NJ-SB/api/v5/eventgroups/88808/categories/1342?format=json"
  
  response = get_html_content(base_url)
  
  script_text <- response %>% 
    html_nodes("script") %>% 
    html_text()
  
  # Find the relevant script containing JSON (inspect manually if needed)
  json_text <- response %>% html_nodes("body") %>%  # Target the body
    html_text(trim = TRUE)
  
  
  # Assuming `json_text` contains your JSON data as a string
  labels <- unlist(str_extract_all(json_text, '(?<=\"label\":\")[^\"]+'))
  labels = labels[-which(str_detect(labels, ' '))] #some extra ones with extra words that we don't want
  odds <- unlist(str_extract_all(json_text, '(?<=\"oddsAmerican\":\")[^\"]+'))
  participants <- unlist(str_extract_all(json_text, '(?<=\"participant\":\")[^\"]+'))
  
  
  # Combine into a data frame
  lines <- data.frame(
    bet_type = gsub('\\\\u002B','+',labels),
    Odds = gsub( '\\\\u002D','-', gsub('\\\\u002B','+', odds)),
    Participant = unlist(participants),
    stringsAsFactors = FALSE
  )
  lines = lines %>% mutate(payout_per_100 = ifelse(as.numeric(Odds) < 0, 100 + (100*100/abs(as.numeric(Odds))), 100 + as.numeric(Odds)))
  return(lines)
  
}



  

create_prediction_df = function(wk_num, player_df,season, df, full_df, schedule_df, future_weather, starting_qbs, stadiums, team_stats_by_game, injuries, long_travel) {
  Name = unique(df$name)
  
  team = get_html_content(paste0("https://www.pro-football-reference.com",player_df$links[player_df$names == Name])) %>%
                    html_nodes("p") %>% html_text(trim = TRUE)
  if(length(team) == 0)
  {
    team = get_html_content(paste0("https://www.pro-football-reference.com",player_df$links[player_df$names == Name])) %>%
      html_nodes("p") %>% html_text(trim = TRUE)
  }
  team_fullname = gsub(' \\(.*','',gsub('Team: ', '', str_extract(team[which(str_detect(team, 'Team'))], 'Team:.*')))
  team_fullname = team_fullname[which(!is.na(team_fullname))]
  team = unique(full_df$team[which(full_df$Team_FullName == team_fullname)])
  
  
  if((length(team) > 0 && (team_fullname %in% schedule_df$away_team | team_fullname %in% schedule_df$home_team)))
  {
    starting_qb = starting_qbs_this_week[team] %>% as.numeric()
    
    Season = season
    week17 = ifelse(wk_num == 17, 1, 0)
    week18 = ifelse(wk_num == 18, 1, 0)
    age = max(df$age) + 1/52 
    height = unique(df$height)
    weight = unique(df$weight)
    Receiving_Yards_Lag1 = ifelse(wk_num == 1, NA, df$Receiving_Yards[df$week_num == wk_num - 1])
    Receiving_Yards_Lag2 = ifelse(wk_num <= 2, NA, df$Receiving_Yards[df$week_num == wk_num - 2])
    Receiving_Yards_Lag3 = ifelse(wk_num <= 3, NA, df$Receiving_Yards[df$week_num == wk_num - 3])
  
    game_location = ifelse(team_fullname %in% schedule_df$home_team, 'Home', 'Away')
    opp_fullname = ifelse(game_location == 'Home', schedule_df$away_team[which(schedule_df$home_team == team_fullname)],
                 schedule_df$home_team[which(schedule_df$away_team == team_fullname)])
    opp = unique(full_df$team[which(full_df$Team_FullName == opp_fullname)])
    day_of_week = schedule_df$day_of_week[which(schedule_df$away_team == team_fullname | schedule_df$home_team == team_fullname)]
    time = schedule_df$time[which(schedule_df$away_team == team_fullname | schedule_df$home_team == team_fullname)]
    time = ifelse(str_detect(time,'PM'),
                  gsub('PM','',paste0(time %>% strsplit(":") %>% sapply(function(x) x[1]) %>% as.numeric() + 12, ":", time %>% strsplit(":") %>% sapply(function(x) x[2]))),gsub('AM','',time))
    time_of_day = ifelse(time < '12:00', 'Morning', ifelse(time < '16:00', 'Early Window', ifelse(time < '19:00', 'Late Window', 'Night')))
    Month = substring(schedule_df$game_date[which(schedule_df$away_team == team_fullname | schedule_df$home_team == team_fullname)],6,7)
    stadium_abbr = unique(full_df$team[which(full_df$Team_FullName == schedule_df$home_team[which(schedule_df$away_team == team_fullname | schedule_df$home_team == team_fullname)])])
    
    Games_Started_Season = ifelse(sum(!is.na(df$gs)) == 1, NA, 
                                     sum(replace(df$gs, is.na(df$gs), '') == "*"))
    Cumulative_Targets = ifelse(sum(!is.na(df$gs)) == 1, NA, 
                                   sum(replace(df$targets, is.na(df$targets), 0)))
    Cumulative_Rec = ifelse(sum(!is.na(df$gs)) == 1, NA, 
                               sum(replace(df$rec, is.na(df$rec), 0)))
    Cumulative_Rec_Yards = ifelse(sum(!is.na(df$gs)) == 1, NA, 
                                     sum(replace(df$Receiving_Yards, is.na(df$Receiving_Yards), 0)))
    Cumulative_Receiving_TDs = ifelse(sum(!is.na(df$gs)) == 1, NA, 
                                         cumsum(replace(df$rec_td, is.na(df$rec_td), 0)))
    Cumulative_Team_Snaps = ifelse(sum(!is.na(df$gs)) == 1, NA, 
                                      sum(replace(df$offense, is.na(df$offense), 0)))
    Cumulative_Snaps_Played = round(ifelse(sum(!is.na(df$gs)) == 1, NA, 
                                              sum(replace(df$offense * df$off_pct, is.na(df$offense * df$off_pct), 0))))
    Cumulative_Games_Active = ifelse(wk_num == 1, NA, 
                                        sum(!is.na(df$gs)))
    Cumulative_Games_Inactive = ifelse(wk_num == 1, NA, 
                                          sum(is.na(df$gs)))
   
    Percent_Games_Started_Season = ifelse(is.na(Cumulative_Games_Active) | Cumulative_Games_Active == 0, NA, 
                                             Games_Started_Season / Cumulative_Games_Active)
    Avg_Targets_Per_Game = ifelse(is.na(Cumulative_Games_Active) | Cumulative_Games_Active == 0, NA, 
                                            Cumulative_Targets / Cumulative_Games_Active)
    Avg_Rec_Per_Game = ifelse(is.na(Cumulative_Games_Active) | Cumulative_Games_Active == 0, NA, 
                                        Cumulative_Rec / Cumulative_Games_Active)
    Avg_Rec_Per_Target = ifelse(is.na(Cumulative_Targets) | Cumulative_Targets == 0, NA, 
                                          Cumulative_Rec / Cumulative_Targets)
    Avg_Receiving_Yards_Per_Game = ifelse(is.na(Cumulative_Games_Active) | Cumulative_Games_Active == 0, NA, 
                                                    Cumulative_Rec_Yards / Cumulative_Games_Active)
    Avg_Receiving_Yards_Per_Target = ifelse(is.na(Cumulative_Targets) | Cumulative_Targets == 0, NA, 
                                                      Cumulative_Rec_Yards / Cumulative_Targets)
    Avg_Receiving_TDs_Per_Game = ifelse(is.na(Cumulative_Games_Active) | Cumulative_Games_Active == 0, NA, 
                                               Cumulative_Receiving_TDs / Cumulative_Games_Active)
    Avg_Percent_Snaps_Played = ifelse(is.na(Cumulative_Team_Snaps), NA, 
                                                Cumulative_Snaps_Played / Cumulative_Team_Snaps) 
    
    Median_Receiving_Yards = median(df$Receiving_Yards, na.rm = TRUE)
    Median_Rec = median(df$rec, na.rm = TRUE)
    Median_Targets = median(df$targets, na.rm = TRUE)
    Median_TD = median(df$rec_td, na.rm = TRUE)
    
    
    
    #stadium predictors:
    Grass_Type = ifelse(time_of_day == 'Morning', NA, stadiums$Grass_Type[which(stadiums$Team == stadium_abbr)])
    Roof_Type = ifelse(time_of_day == 'Morning', NA, stadiums$Roof[which(stadiums$Team == stadium_abbr)])
    Home_Grass_Type = stadiums$Grass_Type[which(stadiums$Team == team)]
    Home_Roof_Type = stadiums$Roof[which(stadiums$Team == team)]
    has_roof = ifelse(Roof_Type %in% c('Dome', 'Retractable'), 1, 0)
    Familiar_Roof_Type = ifelse(Roof_Type == Home_Roof_Type, 1, 0)
    Familiar_Grass_Type = ifelse(Grass_Type == Home_Grass_Type, 1, 0)
    Altitude = ifelse(time_of_day == 'Morning', NA, stadiums$Altitude[which(stadium_abbr == ifelse(game_location == 'Home', team, opp))])
  
  
    #stats from recent years:
    num_active_games_last_year = unique(df$num_active_games_last_year)
    num_games_start_last_year = unique(df$num_games_start_last_year)
    rec_yd_per_game_last_year = unique(df$rec_yd_per_game_last_year)
    rec_per_game_last_year = unique(df$rec_per_game_last_year)
    targets_per_game_last_year = unique(df$targets_per_game_last_year)
    rec_per_target_last_year = unique(df$rec_per_target_last_year)
    snaps_per_game_last_year = unique(df$snaps_per_game_last_year)
    
    num_active_games_2_years_ago = unique(df$num_active_games_2_years_ago)
    num_games_start_2_years_ago = unique(df$num_games_start_2_years_ago)
    rec_yd_per_game_2_years_ago = unique(df$rec_yd_per_game_2_years_ago)
    rec_per_game_2_years_ago = unique(df$rec_per_game_2_years_ago)
    targets_per_game_2_years_ago = unique(df$targets_per_game_2_years_ago)
    rec_per_target_2_years_ago = unique(df$rec_per_target_2_years_ago)
    snaps_per_game_2_years_ago = unique(df$snaps_per_game_2_years_ago)
    
    
    num_games = sum(!is.na(df$gs[df$Season == this_season]))
    
    if(num_games > 0)
    {
      Median_Receiving_Yards_Recent = get_last_3_games(week_num = (wk_num-1), field_name = 'Receiving_Yards', df = full_df %>% filter(name == Name & Season == season)) #3 most recent active games. If there aren't 3 active games within the last 5, take the most possible in the last 5 games.
      Median_Rec_Recent = get_last_3_games(week_num = (wk_num-1), field_name = 'rec', df = full_df %>% filter(name == Name & Season == season))
      Median_TD_Recent = get_last_3_games(week_num = (wk_num-1), field_name = 'rec_td', df = full_df %>% filter(name == Name & Season == season))
      Median_Targets_Recent = get_last_3_games(week_num = (wk_num-1), field_name = 'targets', df = full_df %>% filter(name == Name & Season == season))
      Median_Snaps_Played_Recent = get_last_3_games(week_num = (wk_num-1), field_name = 'snaps_played', df = full_df %>% filter(name == Name & Season == season))
      

    } else {
      Median_Receiving_Yards_Recent = NA
      Median_Rec_Recent = NA
      Median_TD_Recent = NA
      Median_Targets_Recent = NA
      Median_Snaps_Played_Recent = NA
    }
    
    #predictors about team:
    Team_Conf = divisional_table$Conference[divisional_table$Team == team]
    Team_Div = divisional_table$Division[divisional_table$Team == team]
    Opp_Conf = divisional_table$Conference[divisional_table$Team == opp]
    Opp_Div = divisional_table$Division[divisional_table$Team == opp]
    Inter_Conference_Game = ifelse(Team_Conf != Opp_Conf, 1, 0)
    Divisional_Game = ifelse(Team_Conf == Opp_Conf & Team_Div == Opp_Div, 1, 0)
    
    #team stats:
    df_team = team_stats_by_game[team_stats_by_game$team == team & team_stats_by_game$Season == season,]
    df_team = df_team[which(df_team$week < wk_num),]
    win = ifelse(df_team$outcome == 'W', 1, 0)
    num_games_so_far =sum(df_team$outcome != '')
    cumulative_wins = sum(df_team$outcome == 'W')
    cumulative_nonwins = sum(df_team$outcome != 'W')
    cumulative_points = sum(df_team$points)
    cumulative_opp_points = sum(df_team$opp_points)
    cumulative_win_points = sum(ifelse(df_team$outcome == 'W', df_team$points, 0))
    cumulative_win_opp_points = sum(ifelse(df_team$outcome == 'W', df_team$opp_points, 0))
    cumulative_yards = sum(df_team$yards)
    cumulative_opp_yards = sum(df_team$opp_yards)
    cumulative_win_yards = sum(ifelse(df_team$outcome == 'W', df_team$yards, 0))
    cumulative_win_opp_yards = sum(ifelse(df_team$outcome == 'W', df_team$opp_yards, 0))
    cumulative_passing = sum(df_team$passing_yards)
    cumulative_passing_allowed = sum(df_team$passing_yards_allowed)
    cumulative_turnovers_allowed = sum(df_team$turnovers_allowed)
    cumulative_turnovers_forced = sum(df_team$turnovers_forced)
    
    win_pct = cumulative_wins/num_games_so_far
    point_diff = cumulative_points - cumulative_opp_points
    point_diff_per_game = ifelse(num_games_so_far == 0, NA, (cumulative_points - cumulative_opp_points)/num_games_so_far)
    point_diff_per_winning_games = ifelse(cumulative_wins == 0, NA,(cumulative_win_points - cumulative_win_opp_points)/cumulative_wins)
    yard_diff_per_game = ifelse(num_games_so_far == 0, NA, (cumulative_yards - cumulative_opp_yards)/num_games_so_far)
    yard_diff_per_winning_games = ifelse(cumulative_wins == 0, NA, (cumulative_win_yards - cumulative_win_opp_yards)/cumulative_wins)
    team_turnovers_allowed_per_game = ifelse(num_games_so_far == 0, NA, cumulative_turnovers_allowed/num_games_so_far)
    team_turnovers_forced_per_game =  ifelse(num_games_so_far == 0, NA, cumulative_turnovers_forced/num_games_so_far)
    team_passing_yards_per_game = ifelse(num_games_so_far == 0, NA, cumulative_passing/num_games_so_far)
    team_passing_yards_allowed_per_game = ifelse(num_games_so_far == 0, NA, cumulative_passing_allowed/num_games_so_far)
    
    team_win_pct_last_season = unique(full_df$team_win_pct_last_season[full_df$team == team & full_df$Season == season])
    team_point_diff_last_season = unique(full_df$team_point_diff_last_season[full_df$team == team & full_df$Season == season])
    team_point_diff_per_game_last_season = unique(full_df$team_point_diff_per_game_last_season[full_df$team == team & full_df$Season == season])
    team_yard_diff_last_season = unique(full_df$team_yard_diff_last_season[full_df$team == team & full_df$Season == season])
    team_yard_diff_per_game_last_season = unique(full_df$team_yard_diff_per_game_last_season[full_df$team == team & full_df$Season == season])
    team_turnovers_allowed_per_game_last_season = unique(full_df$team_turnovers_allowed_per_game_last_season[full_df$team == team & full_df$Season == season])
    team_turnovers_forced_per_game_last_season = unique(full_df$team_turnovers_forced_per_game_last_season[full_df$team == team & full_df$Season == season])
    team_point_diff_per_win_last_season = unique(full_df$team_point_diff_per_win_last_season[full_df$team == team & full_df$Season == season])
    team_yard_diff_per_win_last_season = unique(full_df$team_yard_diff_per_win_last_season[full_df$team == team & full_df$Season == season])
    team_passing_yards_last_season = unique(full_df$team_passing_yards_last_season[full_df$team == team & full_df$Season == season])
    team_passing_yards_allowed_last_season = unique(full_df$team_passing_yards_allowed_last_season[full_df$team == team & full_df$Season == season])
    team_win_pct_2_seasons_ago = unique(full_df$team_win_pct_2_seasons_ago[full_df$team == team & full_df$Season == season])
    team_point_diff_2_seasons_ago = unique(full_df$team_point_diff_2_seasons_ago[full_df$team == team & full_df$Season == season])
    team_point_diff_per_game_2_seasons_ago = unique(full_df$team_point_diff_per_game_2_seasons_ago[full_df$team == team & full_df$Season == season])
    team_yard_diff_2_seasons_ago = unique(full_df$team_yard_diff_2_seasons_ago[full_df$team == team & full_df$Season == season])
    team_yard_diff_per_game_2_seasons_ago = unique(full_df$team_yard_diff_per_game_2_seasons_ago[full_df$team == team & full_df$Season == season])
    team_turnovers_allowed_per_game_2_seasons_ago = unique(full_df$team_turnovers_allowed_per_game_2_seasons_ago[full_df$team == team & full_df$Season == season])
    team_turnovers_forced_per_game_2_seasons_ago = unique(full_df$team_turnovers_forced_per_game_2_seasons_ago[full_df$team == team & full_df$Season == season])
    team_point_diff_per_win_2_seasons_ago = unique(full_df$team_point_diff_per_win_2_seasons_ago[full_df$team == team & full_df$Season == season])
    team_yard_diff_per_win_2_seasons_ago = unique(full_df$team_yard_diff_per_win_2_seasons_ago[full_df$team == team & full_df$Season == season])
    team_passing_yards_2_seasons_ago = unique(full_df$team_passing_yards_2_seasons_ago[full_df$team == team & full_df$Season == season])
    team_passing_yards_allowed_2_seasons_ago = unique(full_df$team_passing_yards_allowed_2_seasons_ago[full_df$team == team & full_df$Season == season])
    
    #stats about opponent's defense:
    df_opp = team_stats_by_game %>% filter(team ==opp & Season == 2024 & outcome != '')
    num_games_so_far = sum(df_opp$outcome != '')
    opp_defense_passing_yards_allowed_per_game = sum(df_opp$passing_yards_allowed)/num_games_so_far
    opp_defense_turnovers_forced_per_game = sum(df_opp$turnovers_forced)/num_games_so_far
    
    #weather:
    approx_temperature = ifelse(has_roof == 1, NA, future_weather$approx_temperature[future_weather$Stadium == stadium_abbr])
    approx_visibility =  ifelse(has_roof == 1, NA, future_weather$approx_visibility[future_weather$Stadium == stadium_abbr])
    Approx_Wind_Speed = ifelse(has_roof == 1, NA, future_weather$Approx_Wind_Speed[future_weather$Stadium == stadium_abbr])
    unfamiliar_temperature =  ifelse(is.na(stadium_abbr), NA,
                                     ifelse(unique(full_df$has_roof[which(full_df$stadium == stadium_abbr)]) == 1, 0,
                                            ifelse((unique(df$Used_To_Hot) == 0 & approx_temperature >= 80) | (unique(df$Used_To_Cold) == 0 & approx_temperature < 40),
                                                   1, 0)))
    
    cross_country_travel = ifelse(length(long_travel$Coast[long_travel$Team == team]) == 0, 0,
                                         ifelse(length(long_travel$Coast[long_travel$Team == opp]), 0,
                                                       ifelse(long_travel$Coast[long_travel$Team == team] == long_travel$Coast[long_travel$Team == opp], 0, 1)))
    
   
    
    Less_Practice = injuries %>% filter(Player == Name & year == season & week == wk_num) %>% select(Less_Practice) %>% pull()
    Less_Practice = ifelse(length(Less_Practice) == 0, 0, Less_Practice)
    
    On_Injury_List = injuries %>% filter(Player == Name & year == season & week == wk_num) %>% select(On_Injury_List) %>% pull()
    On_Injury_List = ifelse(length(On_Injury_List) == 0, 0, On_Injury_List)
    
    #depth chart estimates:
   targets_rank = get_target_rankings(df = full_df, y = season, t = team, injuries = injuries) %>% filter(name == Name)
   targets_rank = targets_rank %>% filter(week == max(week)) %>%
     select(targets_rank) %>% pull()
   
   recent_weeks_with_target_rank = df %>% filter(name == Name & Season == season &
                                                           week_num < wk_num & !is.na(targets_rank)) %>%
     select(week_num) %>% pull()
   
   if(length(recent_weeks_with_target_rank) > 0) 
   {
     #take the targets rank for the last time that they had an active game:
     targets_rank = df %>% filter(name == Name & Season == season &
                                                          week_num == max(recent_weeks_with_target_rank)) %>%
       select(targets_rank) %>% pull()
   } else {
     targets_rank = NA
   }
  
   #playoffs_at_stake
   playoffs_at_stake = ifelse(wk_num < 17, NA,
                              ifelse(wk_num == 17 & paste(Season, team) %in% (clinching_data %>% filter(Week == 17) %>% select(year_team) %>% pull()), 1,
                                     ifelse(wk_num == 18 & paste(Season, team) %in% (clinching_data %>% filter(Week == 18) %>% select(year_team) %>% pull()), 1, 0)))


  
    return(data.frame(
      Name = Name,
    #predictors about game:
    game_location, Month, day_of_week, time_of_day, Inter_Conference_Game, Divisional_Game, cross_country_travel,
    week17, week18, playoffs_at_stake, starting_qb,
    #predictors about stadium:
    Grass_Type, Altitude, has_roof, Familiar_Grass_Type, Familiar_Roof_Type,
    #player biography
    age, height, weight,
    #player other info:
    targets_rank, Less_Practice, On_Injury_List,
    #player stats from recent games:
    Receiving_Yards_Lag1, Receiving_Yards_Lag2, Receiving_Yards_Lag3,
    Median_Receiving_Yards_Recent, Median_Rec_Recent, Median_TD_Recent,
    Median_Targets_Recent, Median_Snaps_Played_Recent,
    #player stats from the whole current season:
    Percent_Games_Started_Season, Avg_Targets_Per_Game, Avg_Rec_Per_Game,
    Avg_Rec_Per_Target, Avg_Receiving_Yards_Per_Game, Avg_Receiving_Yards_Per_Target,
    Avg_Receiving_TDs_Per_Game, Avg_Percent_Snaps_Played,
    Median_Receiving_Yards, Median_Rec, Median_Targets, Median_TD,
    #stats from recent years:
    num_active_games_last_year, num_games_start_last_year, rec_yd_per_game_last_year, rec_per_game_last_year,
    targets_per_game_last_year, rec_per_target_last_year, snaps_per_game_last_year,
    num_active_games_2_years_ago, num_games_start_2_years_ago, rec_yd_per_game_2_years_ago, rec_per_game_2_years_ago,
    targets_per_game_2_years_ago, rec_per_target_2_years_ago, snaps_per_game_2_years_ago,
    #predictors about team:
    Team_Conf, Team_Div,
    win_pct, point_diff_per_game, point_diff_per_winning_games, team_passing_yards_per_game,
    team_passing_yards_allowed_per_game, yard_diff_per_game, yard_diff_per_winning_games,
    team_turnovers_allowed_per_game, team_turnovers_forced_per_game,
    #team recent years data:
    team_win_pct_last_season, team_point_diff_last_season, team_point_diff_per_game_last_season,
    team_yard_diff_last_season, team_yard_diff_per_game_last_season, team_turnovers_allowed_per_game_last_season,
    team_turnovers_forced_per_game_last_season, team_point_diff_per_win_last_season, team_yard_diff_per_win_last_season,
    team_passing_yards_last_season, team_passing_yards_allowed_last_season,
    team_win_pct_2_seasons_ago, team_point_diff_2_seasons_ago, team_point_diff_per_game_2_seasons_ago, team_yard_diff_2_seasons_ago,
    team_yard_diff_per_game_2_seasons_ago, team_turnovers_allowed_per_game_2_seasons_ago, team_turnovers_forced_per_game_2_seasons_ago,
    team_point_diff_per_win_2_seasons_ago, team_yard_diff_per_win_2_seasons_ago, team_passing_yards_2_seasons_ago, team_passing_yards_allowed_2_seasons_ago,
    #stats about opponent's defense:
    opp_defense_passing_yards_allowed_per_game, opp_defense_turnovers_forced_per_game,
    #weather:
    approx_temperature, approx_visibility, Approx_Wind_Speed, unfamiliar_temperature
    ))
  }
}




 