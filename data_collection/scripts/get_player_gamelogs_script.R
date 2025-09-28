library(dplyr)
library(stringr)
library(purrr)
library(stats)

source('data_collection/scripts/global.R')

  
get_player_gamelogs = function(player_bios, year_cutoff, max_year_cutoff, basic_cols, missing_threshold,
                               gamelog_html_table_tag = 'stats',
                               gamelog_html_playoff_table_tag = 'stats_playoffs',
                               gamelog_advanced_html_rushing_table_tag = 'adv_rushing_and_receiving',
                               gamelog_advanced_html_passing_table_tag = 'passing_advanced',
                               gamelog_advanced_playoffs_html_passing_table_tag = 'passing_advanced_post',
                               gamelog_advanced_playoffs_html_rushing_table_tag = 'adv_rushing_and_receiving_post',
                               wk = NULL, response_only = FALSE, predict_mode = FALSE)
{
  
  t1 = Sys.time()
  
  gamelog_per_player = function(p, player_bios, schedule = NULL)
  {
    print(p)
    player_min_year = max(year_cutoff,(player_bios$min_year[p]))
    player_max_year = min(player_bios$max_year[p], max_year_cutoff) %>% max(player_min_year)
    gamelogs_all_years = rbind()
    for (y in player_min_year:player_max_year)
    {
      print(y)
      gamelog_with_stats = tryCatch({
          get_game_log(player_row = player_bios[p,],
                       yr = y,
                       gamelog_table_tag = gamelog_html_table_tag,
                       gamelog_playoffs_table_tag = gamelog_html_playoff_table_tag,
                       gamelog_advanced_rushing_table_tag = gamelog_advanced_html_rushing_table_tag,
                       gamelog_advanced_passing_table_tag = gamelog_advanced_html_passing_table_tag,
                       gamelog_advanced_playoffs_passing_table_tag = gamelog_advanced_playoffs_html_passing_table_tag,
                       gamelog_advanced_playoffs_rushing_table_tag = gamelog_advanced_playoffs_html_rushing_table_tag)
        }, error = function(e) {
            return(NULL)
          })
      if(!is.null(gamelog_with_stats) && nrow(gamelog_with_stats) > 0)
      {
        if(predict_mode == TRUE)
        {
          current_team = player_bios$current_team[p] #in full name form
          current_team_abbr = team_lookup_table$Team[team_lookup_table$FullName == current_team]
          new_gtm = gamelog_with_stats %>% filter(Week < wk) %>% select(Gtm) %>% pull() %>% max() + 1
          upcoming_schedule = schedule %>% filter(Team == current_team_abbr)
          upcoming_schedule = upcoming_schedule %>% mutate(player_id = player_bios$player_id[p],
                                                           Name = player_bios$names[p],
                                                           Season = y,
                                                           Gtm = new_gtm,
                                                           Position = player_bios$positions[p],
                                                           Week = as.numeric(Week)
                                                           )
           
           #placeholder in for the upcoming week to calculate stats on it:
          gamelog_with_stats = bind_rows(gamelog_with_stats %>% filter(Week < wk), upcoming_schedule)
        }
        gamelog_with_stats[which(gamelog_with_stats$Active == 0), setdiff(colnames(gamelog_with_stats), c(basic_cols, 'Active'))] = NA
        # gamelog_with_stats = gamelog_with_stats %>% arrange(Week) %>% get_cumulative(last3 = FALSE, skip = basic_cols, team = team_cols)
        # gamelog_with_stats = gamelog_with_stats %>% get_cumulative(last3 = TRUE, skip = c(basic_cols, colnames(gamelog_with_stats)[str_detect(colnames(gamelog_with_stats), 'Cumulative_|Avg_|Median_|Min_|Max_|SD_')]), team = team_cols)
        
        if('Receiving_Yds' %in% colnames(gamelog_with_stats))
        {
             gamelog_with_stats = gamelog_with_stats %>% mutate(
               Receiving_Yds_Lag1 = lag(Receiving_Yds, n = 1, order_by = Week),
               Receiving_Yds_Lag2 = lag(Receiving_Yds, n = 2, order_by = Week),
               Receiving_Yds_Lag3 = lag(Receiving_Yds, n = 3, order_by = Week))
        }
        if('Rushing_Yds' %in% colnames(gamelog_with_stats))
        {
          gamelog_with_stats = gamelog_with_stats %>% mutate(
               Rushing_Yds_Lag1 = lag(Rushing_Yds, n = 1, order_by = Week),
               Rushing_Yds_Lag2 = lag(Rushing_Yds, n = 2, order_by = Week),
               Rushing_Yds_Lag3 = lag(Rushing_Yds, n = 3, order_by = Week))
        }
        if('Passing_Yds' %in% colnames(gamelog_with_stats))
        {
          gamelog_with_stats = gamelog_with_stats %>% mutate(
               Passing_Yds_Lag1 = lag(Passing_Yds, n = 1, order_by = Week),
               Passing_Yds_Lag2 = lag(Passing_Yds, n = 2, order_by = Week),
               Passing_Yds_Lag3 = lag(Passing_Yds, n = 3, order_by = Week))
        }
      } else { #gamelogs is null
        if(!is.null(wk) && predict_mode == TRUE && wk == 1)
        {
          gamelog_with_stats = data.frame(player_id = player_bios$player_id[p],
                                               Season = y,
                                               Name = player_bios$names[p],
                                               Position = player_bios$positions[p],
                                               Gtm = 1, Week = 1,
                                               Team = team_lookup_table$Team[team_lookup_table$FullName == player_bios$current_team[p]])
                                               
        } else {#could have been an error. retry.
          retry = 1
          while(is.null(gamelog_with_stats) & retry <= 3)
          {
            wait = runif(1,3,5)
            Sys.sleep(wait)
            gamelog_with_stats = tryCatch({
              get_game_log(player_row = player_bios[p,],
                           yr = y,
                           gamelog_table_tag = gamelog_html_table_tag,
                           gamelog_playoffs_table_tag = gamelog_html_playoff_table_tag,
                           gamelog_advanced_rushing_table_tag = gamelog_advanced_html_rushing_table_tag,
                           gamelog_advanced_passing_table_tag = gamelog_advanced_html_passing_table_tag,
                           gamelog_advanced_playoffs_passing_table_tag = gamelog_advanced_playoffs_html_passing_table_tag,
                           gamelog_advanced_playoffs_rushing_table_tag = gamelog_advanced_playoffs_html_rushing_table_tag)
            }, error = function(e) {
              return(NULL)
            })
            retry = retry + 1 #retry increments until 3, and if gamelog_with_stats is still null, try while loop again
          }
          if(retry > 3 & is.null(gamelog_with_stats))
          {
            print(paste('Unable to scrape:',player_bios$names[p], y))
          }
        }
      }
      wait = runif(1,3,5)
      Sys.sleep(wait)
      gamelogs_all_years = bind_rows(gamelogs_all_years, gamelog_with_stats)
    }
    return(gamelogs_all_years)
  }
  if(!is.null(wk) && predict_mode == TRUE && wk == 1) #first week of season predict mode treated differently because there are no previous gamelogs for this season yet
  {
    player_bios = player_bios %>% filter(max_year >= (year_cutoff - 1) & min_year <= max_year_cutoff & !is.na(current_team) & current_team %in% team_lookup_table$FullName) #consider everyone who played last year to start, who doesn't have missing current_team.
  } else { 
    player_bios = player_bios %>% filter(max_year >= year_cutoff & min_year <= max_year_cutoff)
  }
  if(!is.null(wk)) #mid-season pull:
  {
    player_bios = player_bios %>% filter(!is.na(current_team) & current_team %in% team_lookup_table$FullName) #must be on an active team
  }
  
  
  if(predict_mode == TRUE)
  {
    upcoming_schedule = get_season_schedule(season = year_cutoff, wk = wk)
    wait = runif(1,3,5)
    Sys.sleep(wait)
    gamelogs = map(1:nrow(player_bios), gamelog_per_player, player_bios, schedule = upcoming_schedule)
  } else {
    gamelogs = map(1:nrow(player_bios), gamelog_per_player, player_bios)
  } 

  print('done scraping logs')
  gamelogs = bind_rows(gamelogs)

  if(response_only == FALSE & !(!is.null(wk) && predict_mode == TRUE && wk == 1))
  {
    gamelogs_df = gamelogs %>% arrange(player_id, Season, Week) %>%
      select(player_id, Name, Position, Season, Gtm, Week, Team, Active, GS, everything(), -any_of(c("Opp", "Game_Location", "Month")))
  
  
    #This section will do the following:
    #1. For stats that are 0/1 (like Active, Game Started), a median, sd, max, and min don't make sense. For these, we will just keep the cumulative amount and the mean.
    #2. For stats with very low medians, like number of touchdowns (which are mostly 0 or 1, and in very few cases at least 2), no reason to have min or median. Only cumulative, mean, max and maybe sd are useful.
    #3. Any columns that are all NA or only have one unique value will be removed. -- not doing this for now.
    uninformative_stats_results = remove_uninformative_stats(df = gamelogs_df, column_list = setdiff(colnames(gamelogs_df), basic_cols), missing_threshold = missing_threshold)
    columns_to_remove = uninformative_stats_results[[1]]
    columns_0_1 = uninformative_stats_results[[2]]
    low_medians = uninformative_stats_results[[3]]
    
    # gamelogs_df = gamelogs_df %>% select(-any_of(columns_to_remove))
  
  
    t1 = Sys.time()
    
    gamelogs_df = gamelogs_df %>% 
      arrange(Season, player_id) %>%
      group_by(Season, player_id) %>%
      group_modify(~ compute_slider_cumulatives(.x, basic_cols)) %>%
      ungroup()
  
  
    print(Sys.time() - t1)
  
  
    
    if(length(columns_0_1) > 0)
    {
      gamelogs_df = gamelogs_df %>% conditionally_remove(colnames(gamelogs_df)[which(colnames(gamelogs_df) %in% paste0('SD_', columns_0_1))])
      gamelogs_df = gamelogs_df %>% conditionally_remove(colnames(gamelogs_df)[which(colnames(gamelogs_df) %in% paste0('Last3_SD_', columns_0_1))])
      gamelogs_df = gamelogs_df %>% conditionally_remove(colnames(gamelogs_df)[which(colnames(gamelogs_df) %in% paste0('Median_', columns_0_1))])
      gamelogs_df = gamelogs_df %>% conditionally_remove(colnames(gamelogs_df)[which(colnames(gamelogs_df) %in% paste0('Last3_Median_', columns_0_1))])
      gamelogs_df = gamelogs_df %>% conditionally_remove(colnames(gamelogs_df)[which(colnames(gamelogs_df) %in% paste0('Max_', columns_0_1))])
      gamelogs_df = gamelogs_df %>% conditionally_remove(colnames(gamelogs_df)[which(colnames(gamelogs_df) %in% paste0('Last3_Max_', columns_0_1))])
      gamelogs_df = gamelogs_df %>% conditionally_remove(colnames(gamelogs_df)[which(colnames(gamelogs_df) %in% paste0('Min_', columns_0_1))])
      gamelogs_df = gamelogs_df %>% conditionally_remove(colnames(gamelogs_df)[which(colnames(gamelogs_df) %in% paste0('Last3_Min_', columns_0_1))])
    }
    if(length(low_medians) > 0)
    {
      gamelogs_df = gamelogs_df %>% conditionally_remove(colnames(gamelogs_df)[which(colnames(gamelogs_df) %in% paste0('Median_', low_medians))])
      gamelogs_df = gamelogs_df %>% conditionally_remove(colnames(gamelogs_df)[which(colnames(gamelogs_df) %in% paste0('Last3_Median_', low_medians))])
      gamelogs_df = gamelogs_df %>% conditionally_remove(colnames(gamelogs_df)[which(colnames(gamelogs_df) %in% paste0('Min_', low_medians))])
      gamelogs_df = gamelogs_df %>% conditionally_remove(colnames(gamelogs_df)[which(colnames(gamelogs_df) %in% paste0('Last3_Min_', low_medians))])
    }
    
    gamelogs_df = gamelogs_df %>%
      rename('Pct_GS' = 'Avg_GS',
             'Pct_Active' = 'Avg_Active')
          
  
    summary_function = function(df, c, all_pos)
    {
      column = df %>% select(!!sym(c)) %>% pull()
      class = class(column)
      pct_nonmissing = mean(!is.na(column))
      unique_values = paste(sort(unique(column)), collapse = ",")
      mean = mean(column, na.rm = TRUE)
      median = median(column, na.rm = TRUE)
      min = min(column, na.rm = TRUE)
      max = max(column, na.rm = TRUE)
      
      indices_nonmissing = which(!is.na(column))
      df_subset = df[indices_nonmissing,]
      position_rates = c()
      for (p in all_pos)
      {
        total_position_active =  sum(str_detect(df$Position, p) & df$Active == 1 & df$Week > 1)
        nonmissing_position = sum(str_detect(df_subset$Position, p) & df_subset$Active == 1 & df_subset$Week > 1)
        position_rates = c(position_rates, nonmissing_position/total_position_active)
      }
      
      return(c(c, class, unique_values, mean, median, min, max, pct_nonmissing, position_rates))
      
    }
    
    summary_table = rbind()
    all_pos =  sort(unique(unlist(strsplit(gamelogs_df$Position, '-|/'))))
    for (c in setdiff(colnames(gamelogs_df), basic_cols))
    {
      summary_table = rbind(summary_table, summary_function(df = gamelogs_df, c = c, all_pos = all_pos))
    }
    summary_table = data.frame(summary_table)
    colnames(summary_table) = c('Colname', 'Class','unique_values', 'Mean', 'Median', 'Min', 'Max', 'Pct_nonmissing', paste(all_pos,'_pct_nonmissing'))
    # View(summary_table)
    
    
    
    
    
    #in the above sections we have derived per-game stats (like average number of receiving yards per game, median number of receiving yards per game, etc) so in this section we will derive other fields like receiving yards per target, receptions per target, passing yards per attempt, passing completions per attempt, etc.
    calc_metrics = list(
      This_Season_Passing_Comp_Pct = c("Cumulative_Passing_Cmp", "Cumulative_Passing_Att"),
      This_Season_Receiving_Catch_Pct = c("Cumulative_Receiving_Rec", "Cumulative_Receiving_Tgt"),
      This_Season_Passing_Yds_Per_Completion = c("Cumulative_Passing_Yds", "Cumulative_Passing_Cmp"),
      This_Season_Passing_Yds_Per_Attempt = c("Cumulative_Passing_Yds", "Cumulative_Passing_Att"),
      This_Season_Rushing_Yds_Per_Attempt = c("Cumulative_Rushing_Yds", "Cumulative_Rushing_Att"),
      This_Season_Rushing_1D_Per_Attempt = c("Cumulative_Rushing_1D", "Cumulative_Rushing_Att"),
      This_Season_Rushing_YAC_Per_Attempt = c("Cumulative_Rushing_YAC", "Cumulative_Rushing_Att"),
      This_Season_Receiving_Yds_Per_Tgt = c("Cumulative_Receiving_Yds", "Cumulative_Receiving_Tgt"),
      This_Season_Receiving_Yds_Per_Rec = c("Cumulative_Receiving_Yds", "Cumulative_Receiving_Rec"),
      This_Season_Receiving_1D_Per_Tgt = c("Cumulative_Receiving_1D", "Cumulative_Receiving_Tgt"),
      This_Season_Receiving_1D_Per_Rec = c("Cumulative_Receiving_1D", "Cumulative_Receiving_Rec"),
      This_Season_Receiving_YBC_Per_Tgt = c("Cumulative_Receiving_YBC", "Cumulative_Receiving_Tgt"),
      This_Season_Receiving_YBC_Per_Rec = c("Cumulative_Receiving_YBC", "Cumulative_Receiving_Rec"),
      This_Season_Receiving_YAC_Per_Tgt = c("Cumulative_Receiving_YAC", "Cumulative_Receiving_Tgt"),
      This_Season_Receiving_YAC_Per_Rec = c("Cumulative_Receiving_YAC", "Cumulative_Receiving_Rec"),
      This_Season_Pct_Offensive_Snaps = c("Cumulative_Snap Counts_OffSnp", "Cumulative_Total_Team_Off_Snaps"),
      This_Season_Pct_ST_Snaps = c("Cumulative_Snap Counts_STSnp", "Cumulative_Total_Team_ST_Snaps"),
      Last3_Passing_Comp_Pct = c("Last3_Cumulative_Passing_Cmp", "Last3_Cumulative_Passing_Att"),
      Last3_Receiving_Catch_Pct = c("Last3_Cumulative_Receiving_Rec", "Last3_Cumulative_Receiving_Tgt"),
      Last3_Passing_Yds_Per_Completion = c("Last3_Cumulative_Passing_Yds", "Last3_Cumulative_Passing_Cmp"),
      Last3_Passing_Yds_Per_Attempt = c("Last3_Cumulative_Passing_Yds", "Last3_Cumulative_Passing_Att"),
      Last3_Rushing_Yds_Per_Attempt = c("Last3_Cumulative_Rushing_Yds", "Last3_Cumulative_Rushing_Att"),
      Last3_Rushing_1D_Per_Attempt = c("Last3_Cumulative_Rushing_1D", "Last3_Cumulative_Rushing_Att"),
      Last3_Rushing_YAC_Per_Attempt = c("Last3_Cumulative_Rushing_YAC", "Last3_Cumulative_Rushing_Att"),
      Last3_Receiving_Yds_Per_Tgt = c("Last3_Cumulative_Receiving_Yds", "Last3_Cumulative_Receiving_Tgt"),
      Last3_Receiving_Yds_Per_Rec = c("Last3_Cumulative_Receiving_Yds", "Last3_Cumulative_Receiving_Rec"),
      Last3_Receiving_1D_Per_Tgt = c("Last3_Cumulative_Receiving_1D", "Last3_Cumulative_Receiving_Tgt"),
      Last3_Receiving_1D_Per_Rec = c("Last3_Cumulative_Receiving_1D", "Last3_Cumulative_Receiving_Rec"),
      Last3_Receiving_YBC_Per_Tgt = c("Last3_Cumulative_Receiving_YBC", "Last3_Cumulative_Receiving_Tgt"),
      Last3_Receiving_YBC_Per_Rec = c("Last3_Cumulative_Receiving_YBC", "Last3_Cumulative_Receiving_Rec"),
      Last3_Receiving_YAC_Per_Tgt = c("Last3_Cumulative_Receiving_YAC", "Last3_Cumulative_Receiving_Tgt"),
      Last3_Receiving_YAC_Per_Rec = c("Last3_Cumulative_Receiving_YAC", "Last3_Cumulative_Receiving_Rec"),
      Last3_Pct_Offensive_Snaps = c("Last3_Cumulative_Snap Counts_OffSnp", "Last3_Cumulative_Total_Team_Off_Snaps"),
      Last3_Pct_ST_Snaps = c("Last3_Cumulative_Snap Counts_STSnp", "Last3_Cumulative_Total_Team_ST_Snaps")
    )
    
    for (metric_name in names(calc_metrics)) {
      vars = calc_metrics[[metric_name]]
      if (all(vars %in% names(gamelogs_df))) {
        gamelogs_df[[metric_name]] = gamelogs_df[[vars[1]]] / gamelogs_df[[vars[2]]]
      }
    }
    
    #duplicate removal
    
    duplicates = gamelogs_df %>% group_by(player_id, Name, Season, Week) %>% summarise(n = n()) %>% filter(n > 1)
    nrow(duplicates)
    if(nrow(duplicates) > 0)
    {
      for (d in 1:nrow(duplicates))
      {
        #likely one of them is active and one is not
        gamelogs_df = gamelogs_df %>% filter(!(player_id == duplicates$player_id[d] & Season == duplicates$Season[d] & Week == duplicates$Week[d] & Active == 0))
      }
    }
    
    persisting_duplicates = gamelogs_df %>% group_by(player_id, Name, Season, Week) %>% summarise(n = n()) %>% filter(n > 1)
    nrow(persisting_duplicates)
    
    if(!is.null(wk))
    {
      gamelogs_df = gamelogs_df %>% filter(Week == wk)
    }
  }
  else if(predict_mode == TRUE & wk == 1) {
    existing_gamelog_table = readRDS('data_collection/saved_data_files/player_gamelogs.rds')
    other_columns = setdiff(colnames(existing_gamelog_table), colnames(gamelogs))
    gamelogs[,other_columns] = NA
    gamelogs_df = gamelogs
  } else { #response only -- just grabbing the response variable
    gamelogs_df  = gamelogs %>% filter(Week == wk) %>% select(player_id, Season, Week, any_of(c('Passing_Yds', 'Rushing_Yds', 'Receiving_Yds', 'Total_Touchdowns', 'GS')))
  }
  
  # saveRDS(gamelogs_df, 'saved_data_files/player_gamelogs.rds')
  return(gamelogs_df)
}
    
    
