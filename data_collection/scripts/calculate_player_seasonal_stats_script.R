


library(stringr)
library(dplyr)
library(furrr)
source('data_collection/scripts/global.R')



plan(multisession, workers = 4)

calculate_player_seasonal_stats = function(player_gamelogs, basic_cols, missing_threshold)
{
  in_season_calculation_columns = colnames(player_gamelogs)[str_detect(tolower(colnames(player_gamelogs)),'sum|cumulative|avg|median|max|min|sd|per|pct|lag')]
  columns_to_use = setdiff(colnames(player_gamelogs), c(basic_cols, in_season_calculation_columns))
  
  uninformative_stats_results = remove_uninformative_stats(df = player_gamelogs,
                                                           column_list = columns_to_use,
                                                           missing_threshold = missing_threshold)
  columns_to_remove = uninformative_stats_results[[1]]
  player_gamelogs = player_gamelogs %>% select(-any_of(columns_to_remove))
  
  columns_0_1 = uninformative_stats_results[[2]]
  low_medians = uninformative_stats_results[[3]]
  
  calculate_group_stats = function(p, df, include_team)
  {
    if(include_team == FALSE)
    {
      df = df %>% select(player_id, Name, Season, Week, all_of(columns_to_use)) %>% filter(player_id == p)
      df %>% group_by(player_id, Name, Season) %>%
        summarise(
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
          ), .groups = "drop") 
    } else {
      df = df %>% select(player_id, Team, Name, Season, Week, all_of(columns_to_use)) %>% filter(player_id == p)
      df = df %>% group_by(player_id, Name, Team, Season) %>%
        summarise(
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
          ), .groups = "drop") 
    }
  }
  
  player_seasonal_stats = future_map(.x = unique(player_gamelogs$player_id),
                                     .f = calculate_group_stats,
                                     df = player_gamelogs,
                                     include_team = FALSE) %>% bind_rows()
  print('done calculating group stats without team')
  
  
  grouped_table_with_team = future_map(.x = player_gamelogs %>% select(player_id, Name, Team, Season, Week, all_of(columns_to_use)),
                                       .f = calculate_group_stats,
                                       df = player_gamelogs,
                                       include_team = TRUE) %>% bind_rows()
  print('done calculating group stats with team')
  
  
  #to be used in the get_target_rankings workflow, since some of the stats there are done by team, and the Weeks_active field is important:
  player_gamelogs_active_weeks = player_gamelogs %>% filter(Active == 1) %>%
    group_by(player_id, Name, Season, Team) %>%
    summarise(
      num_weeks = length(unique(Week)),
      Weeks_Active = paste(sort(unique(Week)), collapse = ",") #in case he switched teams mid-season
    ) %>% mutate(
      Weeks_Active = ifelse(num_weeks == ifelse(Season <= 2020, 16, 17), 'All', Weeks_Active)
    ) %>% select(-num_weeks)
  
  grouped_table_with_team = grouped_table_with_team %>% left_join(player_gamelogs_active_weeks, join_by('player_id', 'Team', 'Name', 'Season')) %>% select(player_id, Name, Team, Season, Weeks_Active, everything())
  
  
  
  
  #Just like with the weekly gamelogs data, we will do the following:
  #1. For stats that are 0/1 (like Active, Game Started), a median, sd, max, and min don't make sense. For these, we will just keep the cumulative amount and the mean.
  #2. For stats with very low medians, like number of touchdowns (which are mostly 0 or 1, and in very few cases at least 2), no reason to have min or median. Only cumulative, mean, max, and maybe sd are useful.
  #3. Any columns that are all NA or only have one unique value will be removed.
  
  #use the column names from player gamelogs since the above summary table comes from this anyway.
  
  
  if(length(columns_0_1) > 0)
  {
    player_seasonal_stats = player_seasonal_stats %>% select(-any_of(colnames(player_seasonal_stats)[which(colnames(player_seasonal_stats) %in% paste0(columns_0_1, '_sd'))]))
    player_seasonal_stats = player_seasonal_stats %>% select(-any_of(colnames(player_seasonal_stats)[which(colnames(player_seasonal_stats)%in% paste0(columns_0_1, '_median'))]))
    player_seasonal_stats = player_seasonal_stats %>% select(-any_of(colnames(player_seasonal_stats)[which(colnames(player_seasonal_stats) %in% paste0(columns_0_1, '_max'))]))
    player_seasonal_stats = player_seasonal_stats %>% select(-any_of(colnames(player_seasonal_stats)[which(colnames(player_seasonal_stats) %in% paste0(columns_0_1, '_min'))]))
  }
  if(length(low_medians) > 0)
  {
    player_seasonal_stats = player_seasonal_stats %>% select(-any_of(colnames(player_seasonal_stats)[which(colnames(player_seasonal_stats) %in% paste0(low_medians, '_median'))]))
    player_seasonal_stats = player_seasonal_stats %>% select(-any_of(colnames(player_seasonal_stats)[which(colnames(player_seasonal_stats) %in% paste0(columns_0_1, '_min'))]))
  }
  
  
  calc_metrics = list(
    Passing_Comp_Pct = c("Passing_Cmp_sum", "Passing_Att_sum"),
    Receiving_Catch_Pct = c("Receiving_Rec_sum", "Receiving_Tgt_sum"),
    Passing_Yds_Per_Completion = c("Passing_Yds_sum", "Passing_Cmp_sum"),
    Passing_Yds_Per_Attempt = c("Passing_Yds_sum", "Passing_Att_sum"),
    Rushing_Yds_Per_Attempt = c("Rushing_Yds_sum", "Rushing_Att_sum"),
    Rushing_1D_Per_Attempt = c("Rushing_1D_sum", "Rushing_Att_sum"),
    Rushing_YAC_Per_Attempt = c("Rushing_YAC_sum", "Rushing_Att_sum"),
    Receiving_Yds_Per_Tgt = c("Receiving_Yds_sum", "Receiving_Tgt_sum"),
    Receiving_Yds_Per_Rec = c("Receiving_Yds_sum", "Receiving_Rec_sum"),
    Receiving_1D_Per_Tgt = c("Receiving_1D_sum", "Receiving_Tgt_sum"),
    Receiving_1D_Per_Rec = c("Receiving_1D_sum", "Receiving_Rec_sum"),
    Receiving_YBC_Per_Tgt = c("Receiving_YBC_sum", "Receiving_Tgt_sum"),
    Receiving_YBC_Per_Rec = c("Receiving_YBC_sum", "Receiving_Rec_sum"),
    Receiving_YAC_Per_Tgt = c("Receiving_YAC_sum", "Receiving_Tgt_sum"),
    Receiving_YAC_Per_Rec = c("Receiving_YAC_sum", "Receiving_Rec_sum"),
    Pct_Offensive_Snaps = c("Snap Counts_OffSnp_sum", "Total_Team_Off_Snaps_sum"),
    Pct_ST_Snaps = c("Snap Counts_STSnp_sum", "Total_Team_ST_Snaps_sum")
  )
  
  
  for (metric_name in names(calc_metrics)) {
    vars = calc_metrics[[metric_name]]
    if (all(vars %in% names(player_seasonal_stats))) {
      player_seasonal_stats[[metric_name]] = player_seasonal_stats[[vars[1]]] / player_seasonal_stats[[vars[2]]]
    }
  }
  
  player_seasonal_stats = player_seasonal_stats %>%
    rename('Pct_GS' = 'GS_mean',
           'Pct_Active' = 'Active_mean')
 
  
  player_seasonal_stats = player_seasonal_stats %>% select(-any_of(colnames(player_seasonal_stats)[str_detect(colnames(player_seasonal_stats), '_sum')]))
  
  return(list(player_seasonal_stats, grouped_table_with_team))

}


