
library(stringr)
library(dplyr)
library(purrr)
source('data_collection/scripts/global.R')


get_team_gamelogs = function(start_year, end_year, basic_cols, missing_threshold, calculate_season_end_stats = FALSE, wk = NULL, predict_mode = FALSE)
{
  t1 = Sys.time()
  
  team_abbreviations = team_lookup_table %>% select(FullName, Team, Alternate_Abbr)
  
  pull_gamelog = function(t)
  {
    link_abbr = team_lookup_table$Alternate_Abbr[t]
    team = team_lookup_table$Team[t]
    print(team)
    gamelogs_all_years = rbind()
    for (y in start_year:end_year)
    {
      print(y)
      url = paste0("https://www.pro-football-reference.com/teams/",link_abbr,"/",y,".htm#all_games")
      
      team_gamelogs = tryCatch({
        get_team_game_logs(url = url, y = y)
      }, error = function(e) {
        message("Error encountered. Waiting 5 minutes before retrying...")
        Sys.sleep(300)
        tryCatch({
          get_team_game_logs(url = url, y = y)
        }, error = function(e2) {
          message("Second attempt failed.")
          return(NULL)
        })
      })
      
      team_gamelogs$Team_FullName = team_abbreviations$FullName[t]
      team_gamelogs$Team = team
      team_gamelogs$Season = y
      team_gamelogs$Opp = as.character(unlist(sapply(team_gamelogs$Opp_FullName, function(x) team_abbreviations$Team[team_abbreviations$FullName == x])))
      
      team_gamelogs = team_gamelogs %>% select(Team, Season, Week, Date, Month, Day, Time, Time_of_Day, Game_Location, Opp, Win, Differential, OT, everything(), Team_FullName, Opp_FullName)
      team_gamelogs$Gtm = 1:nrow(team_gamelogs)
      if(predict_mode == TRUE)
      {
        team_gamelogs = team_gamelogs %>% filter(Week <= wk)
      }
      gamelogs_all_years = rbind(gamelogs_all_years, team_gamelogs)
      wait = runif(1,2,4)
      Sys.sleep(wait)
    }
    
    return(gamelogs_all_years)
  }
  team_gamelog_table = bind_rows(map(.x = 1:nrow(team_abbreviations),
                                     .f = pull_gamelog))
  
  Sys.time() - t1
  
  uninformative_stats_results = remove_uninformative_stats(df = team_gamelog_table, column_list = setdiff(colnames(team_gamelog_table), basic_cols), missing_threshold = missing_threshold)
  
  columns_to_remove = uninformative_stats_results[[1]]
  columns_0_1 = uninformative_stats_results[[2]]
  low_medians = uninformative_stats_results[[3]]
  
  
  team_gamelog_table_before_calculations = team_gamelog_table
  
  if(!is.null(wk) && predict_mode == TRUE && wk == 1)
  {
    team_gamelog_table = team_gamelog_table %>%
      mutate(Offense_TotYd = NA %>% as.numeric(),
             Offense_PassY = NA %>% as.numeric(),
             Offense_RushY = NA %>% as.numeric(),
             Offense_TO = NA %>% as.numeric(),
             Offense_1stD = NA %>% as.numeric(),
             Defense_TotYd = NA %>% as.numeric(),
             Defense_PassY = NA %>% as.numeric(),
             Defense_RushY = NA %>% as.numeric(),
             Defense_TO = NA %>% as.numeric(),
             Defense_1stD = NA %>% as.numeric())
  } else if (!is.null(wk) && predict_mode == TRUE) {
    team_gamelog_table$Offense_TotYd[team_gamelog_table$Week == wk] = NA
    team_gamelog_table$Offense_PassY[team_gamelog_table$Week == wk] = NA
    team_gamelog_table$Offense_RushY[team_gamelog_table$Week == wk] = NA
    team_gamelog_table$Offense_TO[team_gamelog_table$Week == wk] = NA
    team_gamelog_table$Offense_1stD[team_gamelog_table$Week == wk] = NA
    team_gamelog_table$Defense_TotYd[team_gamelog_table$Week == wk] = NA
    team_gamelog_table$Defense_PassY[team_gamelog_table$Week == wk] = NA
    team_gamelog_table$Defense_RushY[team_gamelog_table$Week == wk] = NA
    team_gamelog_table$Defense_TO[team_gamelog_table$Week == wk] = NA
    team_gamelog_table$Win[team_gamelog_table$Week == wk] = NA
    team_gamelog_table$Differential[team_gamelog_table$Week == wk] = NA
    team_gamelog_table$OT[team_gamelog_table$Week == wk] = NA
  }
  
  team_gamelog_table = team_gamelog_table %>%
    mutate(Differential_Win = as.numeric(ifelse(Win == 1, Differential, NA))) %>%
    arrange(Season, Team) %>%
    group_by(Season, Team) %>%
    group_modify(~ compute_slider_cumulatives(.x, c(basic_cols, 'Playoffs'))) %>%
    ungroup() %>%
    rename('Differential_Per_Win' = 'Avg_Differential_Win') %>%
    select(-Cumulative_Differential_Win, -Median_Differential_Win, -Max_Differential_Win, -Min_Differential_Win, -SD_Differential_Win,
           -Last3_Cumulative_Differential_Win, -Last3_Median_Differential_Win, -Last3_Min_Differential_Win, -Last3_Max_Differential_Win, -Last3_SD_Differential_Win)
  
  
  differential_per_win_calculation = team_gamelog_table_before_calculations %>%
    filter(Win == 1) %>%
    group_by(Season, Team) %>%
    summarise(Differential_Per_Win = mean(Differential))
  
  columns_to_use = setdiff(colnames(team_gamelog_table_before_calculations), basic_cols)
  
  if(calculate_season_end_stats == TRUE)
  {
    team_gamelog_table_season_end = team_gamelog_table_before_calculations %>%
      mutate(Superbowl_Win = ifelse(Win == 1 & Week == ifelse(Season <= 2020, 21, 22), 1, 0)) %>%
      group_by(Season, Team) %>%
      summarise(
        Won_Superbowl = max(Superbowl_Win),
        across(
          all_of(columns_to_use),
          .fns = list(
            sum = ~sum(.x, na.rm = TRUE),
            mean = ~mean(.x, na.rm = TRUE),
            median = ~median(.x, na.rm = TRUE),
            max = ~max(.x, na.rm = TRUE),
            min = ~min(.x, na.rm = TRUE),
            sd = ~sd(.x, na.rm = TRUE)
          ),
          .names = "{.col}_{.fn}"
        ), .groups = "drop") %>%
      rename('Num_Playoff_Games' = 'Playoffs_sum',
             'Made_Playoffs' = 'Playoffs_max',
             'Win_Pct' = 'Win_mean') %>%
      select(-Playoffs_mean, -Playoffs_median, -Playoffs_min, -Playoffs_sd, -Win_sum, -Win_median, -Win_max, -Win_min, -Win_sd)
    team_gamelog_table_season_end = team_gamelog_table_season_end %>% left_join(differential_per_win_calculation, join_by('Season' == 'Season', 'Team' == 'Team')) %>%
      select(-Cumulative_Differential_Win, -Median_Differential_Win, -Max_Differential_Win, -Min_Differential_Win, -SD_Differential_Win,
             -Last3_Cumulative_Differential_Win, -Last3_Median_Differential_Win, -Last3_Min_Differential_Win, -Last3_Max_Differential_Win, -Last3_SD_Differential_Win)
    
    
  } else {
    team_gamelog_table_season_end = NULL
  }
    

  
 if(predict_mode == FALSE)
 {
    if(length(columns_0_1) > 0)
    {
      team_gamelog_table = team_gamelog_table %>% select(-any_of(colnames(team_gamelog_table)[which(colnames(team_gamelog_table) %in% paste0('SD_', columns_0_1))]))
      team_gamelog_table = team_gamelog_table %>% select(-any_of(colnames(team_gamelog_table)[which(colnames(team_gamelog_table) %in% paste0('Last3_SD_', columns_0_1))]))
      team_gamelog_table = team_gamelog_table %>% select(-any_of(colnames(team_gamelog_table)[which(colnames(team_gamelog_table) %in% paste0('Median_', columns_0_1))]))
      team_gamelog_table = team_gamelog_table %>% select(-any_of(colnames(team_gamelog_table)[which(colnames(team_gamelog_table) %in% paste0('Last3_Median_', columns_0_1))]))
      team_gamelog_table = team_gamelog_table %>% select(-any_of(colnames(team_gamelog_table)[which(colnames(team_gamelog_table) %in% paste0('Max_', columns_0_1))]))
      team_gamelog_table = team_gamelog_table %>% select(-any_of(colnames(team_gamelog_table)[which(colnames(team_gamelog_table) %in% paste0('Last3_Max_', columns_0_1))]))
      team_gamelog_table = team_gamelog_table %>% select(-any_of(colnames(team_gamelog_table)[which(colnames(team_gamelog_table) %in% paste0('Min_', columns_0_1))]))
      team_gamelog_table = team_gamelog_table %>% select(-any_of(colnames(team_gamelog_table)[which(colnames(team_gamelog_table) %in% paste0('Last3_Min_', columns_0_1))]))
    }
    if(length(low_medians) > 0)
    {
      team_gamelog_table = team_gamelog_table %>% select(-any_of(colnames(team_gamelog_table)[which(colnames(team_gamelog_table) %in% paste0('Median_', low_medians))]))
      team_gamelog_table = team_gamelog_table %>% select(-any_of(colnames(team_gamelog_table)[which(colnames(team_gamelog_table) %in% paste0('Last3_Median_', low_medians))]))
      team_gamelog_table = team_gamelog_table %>% select(-any_of(colnames(team_gamelog_table)[which(colnames(team_gamelog_table) %in% paste0('Max_', columns_0_1))]))
      team_gamelog_table = team_gamelog_table %>% select(-any_of(colnames(team_gamelog_table)[which(colnames(team_gamelog_table) %in% paste0('Last3_Max_', columns_0_1))]))
    }
 }
  
  team_gamelog_table = team_gamelog_table %>%
    rename('Pct_Win' = 'Avg_Win') %>%
    select(-any_of(c('Avg_Playoffs', 'Last3_Avg_Playoffs', 'Cumulative_Playoffs', 'Last3_Cumulative_Playoffs')))
  
  if(!is.null(wk) && predict_mode == TRUE && wk == 1)
  {
    team_gamelog_table = team_gamelog_table %>%
      group_by(Season, Week) %>%
      mutate(Rank_Offensive_Yards_Season = NA,
             Rank_Passing_Yards_Season = NA,
             Rank_Rushing_Yards_Season = NA,
             Rank_Offensive_Yards_Last3 = NA,
             Rank_Passing_Yards_Last3 = NA,
             Rank_Rushing_Yards_Last3 = NA,
             Rank_Defense_Total_Yards_Allowed_Season = NA,
             Rank_Defense_Passing_Yards_Allowed_Season = NA,
             Rank_Defense_Rushing_Yards_Allowed_Season = NA,
             Rank_Defense_Total_Yards_Allowed_Last3 = NA,
             Rank_Defense_Passing_Yards_Allowed_Last3 = NA,
             Rank_Defense_Rushing_Yards_Allowed_Last3 = NA   
      ) %>%
      ungroup()
  } else {
  team_gamelog_table = team_gamelog_table %>%
    group_by(Season, Week) %>%
    mutate(Rank_Offensive_Yards_Season = dense_rank(desc(Cumulative_Offense_TotYd)),
           Rank_Passing_Yards_Season = dense_rank(desc(Cumulative_Offense_PassY)),
           Rank_Rushing_Yards_Season = dense_rank(desc(Cumulative_Offense_RushY)),
           Rank_Offensive_Yards_Last3 = dense_rank(desc(Last3_Cumulative_Offense_TotYd)),
           Rank_Passing_Yards_Last3 = dense_rank(desc(Last3_Cumulative_Offense_PassY)),
           Rank_Rushing_Yards_Last3 = dense_rank(desc(Last3_Cumulative_Offense_RushY)),
           Rank_Defense_Total_Yards_Allowed_Season = dense_rank(desc(Cumulative_Defense_TotYd)),
           Rank_Defense_Passing_Yards_Allowed_Season = dense_rank(desc(Cumulative_Defense_PassY)),
           Rank_Defense_Rushing_Yards_Allowed_Season = dense_rank(desc(Cumulative_Defense_RushY)),
           Rank_Defense_Total_Yards_Allowed_Last3 = dense_rank(desc(Last3_Cumulative_Defense_TotYd)),
           Rank_Defense_Passing_Yards_Allowed_Last3 = dense_rank(desc(Last3_Cumulative_Defense_PassY)),
           Rank_Defense_Rushing_Yards_Allowed_Last3 = dense_rank(desc(Last3_Cumulative_Defense_RushY))   
    ) %>%
    ungroup()
  }
  
  date_numerical = as.Date(paste0(ifelse(team_gamelog_table$Month == '01', team_gamelog_table$Season + 1, team_gamelog_table$Season),'-', team_gamelog_table$Month, '-', str_extract(team_gamelog_table$Date, '[0-9]+')))
  d = c(NA, diff(date_numerical))
  team_gamelog_table$Short_Week = ifelse(d < 6, 1, 0)
  team_gamelog_table$Long_Week = ifelse(d > 8, 1, 0)
  team_gamelog_table$Short_Week[which(team_gamelog_table$Week == 1)] = NA
  team_gamelog_table$Long_Week[which(team_gamelog_table$Week == 1)] = NA
  
  if (!is.null(wk))
  {
    team_gamelog_table = team_gamelog_table %>% filter(Week == wk)
    if(predict_mode == TRUE)
    {
      #only predict games that haven't started:
      team_gamelog_table = team_gamelog_table %>% filter(as.POSIXct(paste0(Date, ", ", Season, " ", Time),format = "%B %d, %Y %I:%M %p",tz = "America/New_York") > Sys.time())
    }
  }
  
  
  #Add whether game was international
  
  page = get_html_content(url = 'https://en.wikipedia.org/wiki/NFL_International_Series#Game_history') 
  
  tables = page %>% html_nodes('table.wikitable') %>% html_table(fill = TRUE)
  tables = tables[which(sapply(tables, function(x) all(c('Year', 'Date', 'Stadium') %in% colnames(x))))]
  
  international_games = lapply(tables, function(x) x %>%
                                 select(Year, Date, Designatedvisitor, `Designatedhome team`) %>%
                                 mutate(Year = as.numeric(Year))) %>%
    bind_rows() %>%
    filter(Year >= start_year & Year <= end_year) %>%
    mutate(Hometeam = gsub('\\[[0-9]+\\]', '', `Designatedhome team`),
           Awayteam = gsub('\\[[0-9]+\\]', '', `Designatedvisitor`)) %>%
    left_join(team_lookup_table %>% select(FullName, Team) %>% rename('Awayteam_abbr' = 'Team'), join_by('Awayteam' == 'FullName')) %>%
    left_join(team_lookup_table %>% select(FullName, Team) %>% rename('Hometeam_abbr' = 'Team'), join_by('Hometeam' == 'FullName')) %>%
    select(-Designatedvisitor, -`Designatedhome team`, -Hometeam, -Awayteam)
  
  team_gamelog_table = team_gamelog_table %>%
    left_join(international_games, join_by('Season' == 'Year', 'Date' == 'Date', 'Team' == 'Awayteam_abbr')) %>%
    left_join(international_games, join_by('Season' == 'Year', 'Date' == 'Date', 'Team' == 'Hometeam_abbr')) %>%
   mutate(International = ifelse(!is.na(Awayteam_abbr) | !is.na(Hometeam_abbr), 1, 0),
           Home_Stadium = ifelse(Game_Location == 'Home' & International == 0, 1, 0)) %>% select(-Awayteam_abbr, -Hometeam_abbr)
  
  return(list(team_gamelog_table, team_gamelog_table_season_end))
  
}


