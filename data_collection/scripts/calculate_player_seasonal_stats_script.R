library(stringr)
library(dplyr)
library(furrr)
source('data_collection/scripts/global.R')



plan(multisession, workers = 4)

calculate_player_seasonal_stats = function(player_gamelogs, columns_to_use, missing_threshold, calc_metrics)
{
  uninformative_stats_results = remove_uninformative_stats(df = player_gamelogs,
                                                           column_list = columns_to_use,
                                                           missing_threshold = missing_threshold)
  columns_to_remove = uninformative_stats_results[[1]]
  player_gamelogs = player_gamelogs %>% select(-any_of(columns_to_remove))
  
  columns_0_1 = uninformative_stats_results[[2]]
  low_medians = uninformative_stats_results[[3]]
  
  player_seasonal_stats = player_gamelogs %>%
    group_by(gsis_id, season) %>%
    summarise(weeks_active = length(unique(week)),
              across(all_of(columns_to_use),
       .fns = list(sum = ~sum(.x, na.rm = TRUE),
                   mean = ~mean(.x, na.rm = TRUE),
                   median = ~median(.x, na.rm = TRUE),
                   max = ~max(.x, na.rm = TRUE),
                   min = ~min(.x, na.rm = TRUE),
                   sd = ~sd(.x, na.rm = TRUE)
        ), .names = "{str_to_title{.fn}}_{.col}", .groups = "drop")) %>%
    group_by(gsis_id, season) %>%
    ungroup() %>%
    rename_with(~ gsub("Sum_", "Cumulative_", .x), starts_with("Sum_")) %>%
    rename_with(~ gsub("Mean_", "Avg_", .x), starts_with("Mean_"))

  
  
  
  #Just like with the weekly gamelogs data, we will do the following:
  #1. For stats that are 0/1 (like Active, Game Started), a median, sd, max, and min don't make sense. For these, we will just keep the cumulative amount and the mean.
  #2. For stats with very low medians, like number of touchdowns (which are mostly 0 or 1, and in very few cases at least 2), no reason to have min or median. Only cumulative, mean, max, and maybe sd are useful.
  #3. Any columns that are all NA or only have one unique value will be removed.
  
  #use the column names from player gamelogs since the above summary table comes from this anyway.
  
  
  if(length(columns_0_1) > 0)
  {
    player_seasonal_stats = player_seasonal_stats %>% select(-any_of(colnames(player_seasonal_stats)[which(colnames(player_seasonal_stats) %in% paste0(columns_0_1, 'sd_'))]))
    player_seasonal_stats = player_seasonal_stats %>% select(-any_of(colnames(player_seasonal_stats)[which(colnames(player_seasonal_stats)%in% paste0(columns_0_1, 'median_'))]))
    player_seasonal_stats = player_seasonal_stats %>% select(-any_of(colnames(player_seasonal_stats)[which(colnames(player_seasonal_stats) %in% paste0(columns_0_1, 'max_'))]))
    player_seasonal_stats = player_seasonal_stats %>% select(-any_of(colnames(player_seasonal_stats)[which(colnames(player_seasonal_stats) %in% paste0(columns_0_1, 'min_'))]))
  }
  if(length(low_medians) > 0)
  {
    player_seasonal_stats = player_seasonal_stats %>% select(-any_of(colnames(player_seasonal_stats)[which(colnames(player_seasonal_stats) %in% paste0(low_medians, 'median_'))]))
    player_seasonal_stats = player_seasonal_stats %>% select(-any_of(colnames(player_seasonal_stats)[which(colnames(player_seasonal_stats) %in% paste0(columns_0_1, 'min_'))]))
  }
  
  
  player_seasonal_stats_with_efficiency_metrics = calculate_efficiency_metrics(df = player_seasonal_stats, calc_metrics = calc_metrics) %>%
    mutate(air_yards_differential = air_yards_per_attempt - air_yards_per_completion,
           weighted_opportunity_rating = 1.5*pct_share_of_targets + 0.7*pct_share_of_intended_air_yards) %>%
    select(-any_of(colnames(player_seasonal_stats)[str_detect(colnames(player_seasonal_stats), 'sum_')])) 
  
  return(player_seasonal_stats)

}


