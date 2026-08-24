library(stringr)
library(dplyr)
library(scorecard)
library(tidyr)
library(furrr)


passing_numbers = c(150, 180, 210, 240, 270, 300, 330, 360)
rushing_numbers = c(25, 40, 60, 80, 100, 120, 140)
receiving_numbers = c(25, 40, 60, 80, 100, 120, 140)
rushing_receiving_numbers = c(40, 70, 100, 130)
reception_numbers = c(4,6,8,10)
spread_numbers = c(-2.5, 2.5, -3.5, 3.5, -6.5, 6.5, -7.5, 7.5)

options(future.globals.maxSize = 5 * 1024^3) 
plan(multisession, workers = availableCores() - 1)

this_or_that = data.frame(cbind(option1 = c('draft_round', 'stadium_grass_type', 'stadium_roof'),
                                option2 = c('draft_pick', 'familiar_grass_type', 'familiar_roof_type')))





categorize_stats_fields = function(column_categories, column_category_current, past_season_column_category)
{
  current_categories = unlist(strsplit(na.omit(column_category_current), "\\|"))
  past_categories = unlist(strsplit(na.omit(past_season_column_category), "\\|"))
  if(length(current_categories) > 0)
  {
    player_stats_cols_df = data.frame(Stat = unlist(column_categories[c(current_categories, "other_current_season_stats", "usage_and_depth")]),
                                      scope = 'Player Stats') %>%
      filter(!(Stat %in% column_categories$usage_and_depth[!str_detect(column_categories$usage_and_depth, 'offense_pct|also_played')]))
    rownames(player_stats_cols_df) = NULL
    player_stats_cols_df = player_stats_cols_df %>% 
      mutate(Timeframe = ifelse(str_detect(Stat, '(lag)|(last3)'), 'Recent Games', 'Full Current Season'),
             RawStat = gsub('last3_|avg_|mean_|median_|min_|max_|cv_|sd_|cumulative_|_lag[0-9]', '', Stat),
             StatType = case_when(
               ((str_detect(RawStat, 'att|attempt|carries|target|tgt|snap|offense|active|also_played') & !(RawStat %in% 'average_depth_of_target_passer') & !str_detect(RawStat, 'per|pct'))) | (RawStat %in% c('offense_pct', 'time_to_throw_per_attempt', 'games_played', 'also_played_defense')) ~ 'Opportunity',
               str_detect(RawStat, 'per|pct') & !str_detect(RawStat, 'snap') & !(RawStat %in% c('offense_pct', 'time_to_throw_per_attempt')) ~ 'Efficiency',
               .default = 'Production'
             ))
    rownames(player_stats_cols_df)= NULL
  }

  if(length(past_categories) > 0)
  {
    player_recent_seasons_stats_cols_df = data.frame(Stat = unlist(column_categories[c(past_categories, "other_past_season_stats", "past_season_usage_and_depth")]),
                                                     scope = 'Player Stats') %>%
      filter(!(Stat %in% column_categories$past_season_usage_and_depth[!str_detect(column_categories$past_season_usage_and_depth, 'offense_pct|also_played')]))
    colnames(player_recent_seasons_stats_cols_df)[1] = 'Stat'
    
    player_recent_seasons_stats_cols_df = player_recent_seasons_stats_cols_df %>% 
      mutate(Timeframe = 'Historical Seasons',
             RawStat = gsub('Last_Season_|Two_Seasons_Ago_|min_|mean_|median_|max_|cv_|sd_|avg_', '', Stat),
             StatType = case_when(
               ((str_detect(RawStat, 'att|attempt|carries|target|tgt|snap|offense|active|also_played') & !(RawStat %in% 'average_depth_of_target_passer') & !str_detect(RawStat, 'per|pct'))) | (RawStat %in% c('offense_pct', 'time_to_throw_per_attempt')) ~ 'Opportunity',
               str_detect(RawStat, 'per|pct') & !str_detect(RawStat, 'snap') & !(RawStat %in% c('offense_pct', 'time_to_throw_per_attempt')) ~ 'Efficiency',
               .default = 'Production'
             ))
    rownames(player_recent_seasons_stats_cols_df)= NULL
  }
  
  team_current_stats_cols_df = data.frame(Stat = unlist(column_categories[str_detect(names(column_categories), 'team_current_season|team_drives')]),
                                          scope = 'Team Stats')
  colnames(team_current_stats_cols_df)[1] = 'Stat'
  
  team_current_stats_cols_df = team_current_stats_cols_df %>%
    filter(Stat != 'coach_previous_weeks_with_team') %>%
    mutate(Timeframe = ifelse(str_detect(Stat, 'last3'), 'Recent Games', 'Full Current Season'),
           RawStat = gsub('team_|last3_|min_|mean_|median_|max_|avg_|cumulative_|cv_|sd_|offense_', '', Stat),
           StatType = case_when(
             (RawStat == 'differential_per_win' | str_detect(RawStat, 'per_|ratio|pct|dependency|passer_rating|aggressiveness')) & !str_detect(RawStat, 'pct_plays|pct_drives|pct_trick|matchup') ~ 'Efficiency',
            str_detect(RawStat, 'att|attempt|carries|target|tgt|snap|offense|active|pct_plays|pct_drives|pct_trick|qb_shotgun|sum_qb|play_action|screen|times_') | RawStat %in% c('drives_in_redzone', 'drives', 'games_played','redzone_rushing_plays', 'redzone_receiving_plays', 'pace', 'time_of_possession', 'redzone_plays') ~ 'Opportunity',
             .default = 'Production'))
  rownames(team_current_stats_cols_df)= NULL
  
  team_historical_season_stats_cols_df = data.frame(Stat = unlist(column_categories[str_detect(names(column_categories), 'team_historical_season')]),
                                                    scope = 'Team Stats')
  
  team_historical_season_stats_cols_df = team_historical_season_stats_cols_df %>% 
    mutate(Timeframe = 'Historical Seasons',
           RawStat = gsub('Last_Season_|Two_Seasons_Ago_|team_|last3_|min_|mean_|median_|max_|avg_|_cumulative_|cv_|sd_|sum_|offense_', '', Stat),
           StatType = case_when(
             (RawStat == 'differential_per_win' | str_detect(RawStat, 'per_|ratio|pct|dependency')) & !str_detect(RawStat, 'pct_plays|pct_drives|pct_trick|matchup') ~ 'Efficiency',
             str_detect(RawStat, 'att|attempt|carries|target|tgt|snap|offense|active|pct_plays|pct_drives|qb_shotgun|sum_qb|qb_sneaks|trick|play_action|screen|backfield') | RawStat %in% c('drives_in_redzone', 'num_plays', 'drives', 'games_played','redzone_rushing_plays', 'redzone_receiving_plays', 'pace', 'time_of_possession', 'redzone_plays') ~ 'Opportunity',
             .default = 'Production'))
  rownames(team_historical_season_stats_cols_df)= NULL
  
  opp_current_stats_cols_df = data.frame(Stat = unlist(column_categories[str_detect(names(column_categories), 'opp_current_season')]),
                                         scope = 'Opp Stats')
  
  opp_current_stats_cols_df = opp_current_stats_cols_df %>% filter(!Stat %in% c('Opp_Used_To_Hot', 'Opp_Used_To_Cold', 'Opp.y')) %>%
    mutate(Timeframe = ifelse(str_detect(Stat, 'Last3'), 'Recent Games', 'Full Current Season'),
           RawStat = gsub('opp_|last3_|min_|mean_|median_|max_|avg_|cumulative_|cv_|sd_|defense_', '', Stat),
           StatType = case_when(
             str_detect(RawStat, 'attempt|drives|carries|plays|per_play') & (!str_detect(RawStat, 'per_') | str_detect(RawStat, 'per_play')) ~ 'Opportunity',
             str_detect(RawStat, 'per_|pct|dependency') & !str_detect(RawStat, 'per_play') ~ 'Efficiency',
             .default = 'Production'))
  rownames(opp_current_stats_cols_df)= NULL
  
  opp_historical_season_stats_cols_df = data.frame(Stat = unlist(column_categories[str_detect(names(column_categories), 'opp_historical_season')]),
                                                   scope = 'Opp Stats')

  opp_historical_season_stats_cols_df = opp_historical_season_stats_cols_df %>% 
    mutate(Timeframe = 'Historical Seasons',
           RawStat = gsub('Last_Season_|Two_Seasons_Ago_|opp_|last3_|min_|mean_|median_|max_|avg_|cumulative_|cv_|sd_|defense_', '', Stat),
           StatType = case_when(
             str_detect(RawStat, 'attempt|drives|carries|plays|per_play|total') & (!str_detect(RawStat, 'per_') | str_detect(RawStat, 'per_play')) ~ 'Opportunity',
             str_detect(RawStat, 'per_|pct|dependency') & !str_detect(RawStat, 'per_play') ~ 'Efficiency',
             .default = 'Production'))
  rownames(opp_historical_season_stats_cols_df)= NULL
  
  stats_columns_table = rbind(team_current_stats_cols_df,
    team_historical_season_stats_cols_df,
    opp_current_stats_cols_df,
    opp_historical_season_stats_cols_df
  )
  if (length(current_categories) > 0) {
    stats_columns_table = rbind(stats_columns_table, player_stats_cols_df)
  }
  
  if (length(past_categories) > 0) {
    stats_columns_table = rbind(
      stats_columns_table,
      player_recent_seasons_stats_cols_df
    )
  }

  return(stats_columns_table)
}

trim_columns_by_iv_correlation = function(columns_df, train_df, num_winners_per_category = 5)
{
  winners_df = data.frame()
  for(type in unique(columns_df$StatType))
  {
    winners = columns_df %>% filter(StatType == type) %>% arrange(desc(Total_IV)) %>% slice(1:max(num_winners_per_category)) %>% ungroup()
    winners_df = rbind(winners_df, winners %>% select(Stat, StatType, Total_IV))
  }
  
  if(nrow(winners_df) > num_winners_per_category)
  {
    cor_mat = cor(train_df %>% ungroup() %>% select(all_of(winners_df$Stat)), 
                  use = "pairwise.complete.obs")  %>% data.frame()
    
    removal_list = c()
    for(i in 1:nrow(cor_mat))
    {
      for(j in 1:ncol(cor_mat))
      {
        if(i < j)
        {
          col1 = rownames(cor_mat)[i]
          col2 = colnames(cor_mat)[j]
          corr = cor_mat[i,j]
          if(is.na(corr))
          {
            corr = 0
          }
          if((!(col1 %in% removal_list)) & (!(col2 %in% removal_list)))
          {
            if(corr > 0.9) #if < 0.9, keep both, so don't go throught this removal process
            {
              iv1 = winners_df %>% filter(Stat == col1) %>% pull(Total_IV)
              iv2 = winners_df %>% filter(Stat == col2) %>% pull(Total_IV)
              category1 = winners_df %>% filter(Stat == col1) %>% pull(StatType)
              category2 = winners_df %>% filter(Stat == col2) %>% pull(StatType)
              if(category1 == category2)
              {
                removal_list = c(removal_list, ifelse(iv1 < iv2, col1, col2))
              } else {
                removal_list = c(removal_list, ifelse(iv1 < iv2 - 0.15, col1,
                                                      ifelse(iv2 < iv1 - 0.15, col2,
                                                             ifelse(sample(1:2, 1) == 1, col1, col2))))
              }
            }
          }
        }
      }
    }
    
    final_winners = winners_df %>% filter(!Stat %in% removal_list)
  } else if (nrow(winners_df) > 0) {
    final_winners = winners_df
  } else {
    final_winners = NULL
  }
  
  return(final_winners)
}

#define functions for creating information value tables:

create_iv_tables = function(df, r, var_list, column_categories, specific_bins = FALSE)
{
  all_bins = rbind()
  all_ivs = rbind()
  for (var in var_list)
  {
    if (var %in% colnames(df))
    {
      overall_mean = df %>% pull(r) %>% mean()
      unique_vals = length(na.omit(unique(df[[var]])))
      pct_missing = mean(is.na(df[,var]))
      
      if(unique_vals > 1)
      {
        bins = woebin(df, y = r, x = var, bin_num_limit = 6, check_cate_num = FALSE, print_step = 0, print_info = FALSE) %>% data.frame() %>% select(!!sym(paste0(var, '.variable')), !!sym(paste0(var,'.bin')),!!sym(paste0(var,'.bin_iv')), !!sym(paste0(var,'.total_iv')), !!sym(paste0(var, '.posprob')), !!sym(paste0(var, '.count')))
        colnames(bins) = c('Variable', 'Bin', 'Bin_IV', 'Total_IV', 'Pos_Prob', 'Bin_Count')
        bins = bins %>% mutate(Response = r, overall_mean_response = overall_mean, display = ifelse(unique_vals >= 3, 1, 0), unique_vals = unique_vals, pct_missing = pct_missing, class = class(df[[var]]))
        ivs = bins %>% select(Variable, Total_IV, display, pct_missing) %>% distinct() %>% mutate(Response = r,
                                                                                     overall_mean_response = overall_mean)
        
        if(var %in% column_categories$player_bio_data)
        {
          specific_count_field = 'player_id'
        } else if(var %in% column_categories$stadium_info)
        {
          specific_count_field = 'Stadium'
        } else {
          specific_count_field = NULL
        }
        
        if(specific_bins == TRUE)
        {
          specific_count = c()
          specific_list = c()
          for (b in 1:nrow(bins))
          {
            bin = bins$Bin[b]
            if(!is.null(specific_count_field))
            {
              if(bin == 'missing')
              {
                specific_count = c(specific_count,length(na.omit(unique(df[,specific_count_field][which(is.na(df[,var]))]))))
                specific_list = c(specific_list, paste(sort(unique(df[,specific_count_field][which(is.na(df[,var]))])), collapse = ','))
                
              } else if (is.numeric(df[,var])) {
                lower_bound = trimws(sub('\\[', '', setdiff(unlist(strsplit(gsub('%', '', bin), ',')), 'missing')[1])) %>% as.numeric()
                upper_bound =   trimws(sub('\\)', '', setdiff(unlist(strsplit(gsub('%', '', bin), ',')), 'missing')[2])) %>% as.numeric()
                c = length(na.omit(unique(df[,specific_count_field][which(df[,var] >= lower_bound & df[,var] < upper_bound)])))
                l = sort(unique(df[,specific_count_field][which(df[,var] >= lower_bound & df[,var] < upper_bound)]))
                if('missing' %in% unlist(strsplit(gsub('%', '', bin), ',')))
                {
                  c = c + length(na.omit(unique(df[,specific_count_field][which(is.na(df[,var]))])))
                  l = sort(unique(c(l, sort(unique(df[,specific_count_field][which(is.na(df[,var]))])))))
                }
                specific_count = c(specific_count,c)
                specific_list = c(specific_list, paste(l, collapse = ","))
                
              } else { #categorical
                unique_vals = unlist(strsplit(gsub('%', '', bin), ','))
                c = length(na.omit(unique(df[,specific_count_field][which(df[,var] %in% unique_vals)])))
                l = paste(sort(unique(df[,specific_count_field][which(df[,var] %in% unique_vals)])), collapse = ",")
                if('missing' %in% unique_vals)
                {
                  c = c + length(na.omit(unique(df[,specific_count_field][which(is.na(df[,var]))])))
                  l = sort(unique(c(l, sort(unique(df[,specific_count_field][which(is.na(df[,var]))])))))
                }
                specific_count = c(specific_count,c)
                specific_list = c(specific_list, paste(l, collapse = ","))
                
              }
            } else {
              specific_count = c(specific_count, NA)
              specific_list = c(specific_list, NA)
            }
          
          }
          bins = bins %>% mutate(specific_count = specific_count,
                                 specific_list = specific_list)
        
          all_bins = rbind(all_bins, bins)
        } else {
          bins = NULL
        }
          all_ivs = rbind(all_ivs, ivs)
      } else {
        all_ivs = rbind(all_ivs,
                        data.frame(Variable = var,
                                   Total_IV = 0,
                                   display = 1,
                                   Response = r,
                                   pct_missing = pct_missing,
                                   overall_mean_response= overall_mean))
      }
      
    } else {
    #print(paste('Column not found in data:', var))
    }
  }

  all_ivs = all_ivs %>% mutate(Predictive_Power = case_when(
    Total_IV < 0.02 ~ 'None',
    Total_IV <= 0.1 ~ 'Low',
    Total_IV <= 0.3 ~ 'Medium',
    Total_IV <= 0.5 ~ 'High',
    .default = 'Suspicious'
  ))
  
  if(!is.null(all_bins))
  {
    all_bins = all_bins %>% mutate(Predictive_Power = case_when(
      Total_IV < 0.02 ~ 'None',
      Total_IV <= 0.1 ~ 'Low',
      Total_IV <= 0.3 ~ 'Medium',
      Total_IV <= 0.5 ~ 'High',
      .default = 'Suspicious'
    ))
  }
  return(list(all_ivs,all_bins))
}

this_or_that_results = function(ivs, this_or_that, r)
{
  this_or_that$decision = NA
  for(t in 1:nrow(this_or_that))
  {
    option1 = this_or_that$option1[t]
    option2 = this_or_that$option2[t]
    if(option1 %in% ivs$Variable & !(option2 %in% ivs$Variable))
    {
      this_or_that[t, 'decision'] = option1
    } else if (option2 %in% ivs$Variable & !(option1 %in% ivs$Variable))
    {
      this_or_that[t, 'decision'] = option2
    } else if (option1 %in% ivs$Variable & option2 %in% ivs$Variable)
    {
      filtered_table = ivs %>%
        filter(Variable %in% c(option1, option2) & Response == r)
      if(nrow(filtered_table) > 0)
      {
        this_or_that[t, 'decision']  = filtered_table %>%  arrange(desc(Total_IV)) %>%
          select(Variable) %>% 
          slice(1) %>%
          pull()
      }
    }
  } 
  return(this_or_that %>% filter(!is.na(decision)))
}

create_data_with_response_variables = function(df, numbers, response_var) {
  for(n in numbers)
  {
    df  = df %>% mutate(!!paste0(response_var, '_', gsub('-', 'minus', n)) := ifelse(!!sym(response_var) >= n, 1, 0))
  }
  return(df)
}

iv_tournament = function(i, unique_scopes_tf, column_categories_df, train, r, column_categories, missing_pct, num_winners, acceptable_predictive_power)
{
  scope = unique_scopes_tf[i,'scope']
  tf = unique_scopes_tf[i,'Timeframe']
  iv_tables = create_iv_tables(df = train, r = r, var_list = column_categories_df$Stat[column_categories_df$scope == scope & column_categories_df$Timeframe == tf],
                               column_categories, specific_bins = FALSE)
  iv_tables = iv_tables[[1]]
  
  column_categories_player_stats_with_iv = column_categories_df %>%
    inner_join(iv_tables %>% distinct() %>% select(Variable, Total_IV, Predictive_Power, pct_missing), join_by('Stat' == 'Variable')) %>%
    filter(Predictive_Power %in% unlist(strsplit(acceptable_predictive_power,',')))
  
  
  if (nrow(column_categories_player_stats_with_iv %>% filter(pct_missing < missing_pct)) > 0)
  {
    return(data.frame(scope = scope, tf = tf, trim_columns_by_iv_correlation(columns_df = column_categories_player_stats_with_iv %>% filter(pct_missing < missing_pct), train_df = train, num_winners_per_category = num_winners)))
  }
  
}

trim_data = function(train, test, final_test, r, raw_response_var, missing_pct, exclude_from_information_value, exclude_from_model, response_var_list, this_or_that, column_categories, column_categories_df, acceptable_predictive_power, num_winners)
{
  print('creating IVs and bins...')
  ivs_and_bins = create_iv_tables(df = train, r = r, var_list = setdiff(colnames(train), c(exclude_from_information_value, column_categories_df$Stat, colnames(train)[which(str_detect(colnames(train), gsub('_[0-9]+','',r)))], unlist(response_var_list))), column_categories = column_categories, specific_bins = TRUE)
  ivs = ivs_and_bins[[1]]
  bins = ivs_and_bins[[2]]
  this_or_that_table = this_or_that_results(ivs = ivs, this_or_that = this_or_that, r = r) %>%
    mutate(NotSelected = ifelse(option1 == decision, option2, option1))
  this_or_that_removals = c(setdiff(this_or_that$option1, this_or_that_table$option1), setdiff(this_or_that$option2, this_or_that_table$option2), this_or_that_table$NotSelected)
  ivs = ivs %>% filter(!(Variable %in% this_or_that_removals))
  bins = bins %>% filter(!(Variable %in% this_or_that_removals))
  
  fields_to_use_bins = bins %>% filter(Predictive_Power  %in% unlist(strsplit(acceptable_predictive_power,',')) & class =='character' & pct_missing < missing_pct & unique_vals > 10) %>% pull(Variable) %>% unique()
  fields_to_use_as_is =  bins %>% filter(Predictive_Power %in% unlist(strsplit(acceptable_predictive_power,',')) & !(class =='character' & pct_missing < missing_pct & unique_vals > 10)) %>% pull(Variable) %>% unique()
  
  unique_scopes_tf = column_categories_df %>% select(scope, Timeframe) %>% distinct()
  
  all_stats_fields = future_map(.x = 1:nrow(unique_scopes_tf), .f = iv_tournament, unique_scopes_tf, column_categories_df, train, r, column_categories, missing_pct, num_winners, acceptable_predictive_power) %>% bind_rows()
  
  #removing fields_to_use_bins because it's just college and college_conference, and it's not worth complicating the workflow for fields that can be described by draft round. 
  selected_fields = c(column_categories$identifiers, all_stats_fields$Stat, fields_to_use_as_is, setdiff(exclude_from_information_value, exclude_from_model), paste0(raw_response_var, '_lag1'), paste0(raw_response_var, '_lag2'), paste0(raw_response_var, '_lag3'))
  training_data = train %>% select(r, any_of(selected_fields))
  if(!is.null(test))
  {
    test_data = test %>% select(r, any_of(selected_fields))
    final_test_data = final_test %>% select(r, any_of(selected_fields))
  } else {
    test_data = NULL
    final_test_data = NULL
  }
  
  print(paste('Fields that don\'t exist:', paste(setdiff(selected_fields, colnames(train)), collapse = ',')))
  
  # training_data = training_data %>% iv_to_dummy(bins = bins %>% filter(Variable %in% fields_to_use_bins), r) %>% select(-any_of(fields_to_use_bins))
  # test_data = test_data %>% iv_to_dummy(bins = bins %>% filter(Variable %in% fields_to_use_bins), r) %>% select(-any_of(fields_to_use_bins))
  
  return(list(training_data, test_data, final_test_data))
}

model_prep = function(data_to_prep, column_categories, train_mode, numbers, response_var, current_season_column_category, historical_season_column_category, acceptable_predictive_power, num_winners)
{
  data_to_prep = data_to_prep %>%
    mutate(across(where(is.numeric), ~ifelse(is.infinite(.x), NA, .x))) %>%
    mutate(across(where(is.logical), ~as.factor(.x)))
  
  if (train_mode)
  {
    data_to_prep = data_to_prep %>% filter(!is.na(!!sym(response_var)))
  }

  
  if (any(!is.na(numbers)))
  {
    response_var_list = paste0(response_var, '_', gsub('-', 'minus', numbers))
    if(response_var %in% colnames(data_to_prep)) #historical data
    {
      data_to_prep = create_data_with_response_variables(data_to_prep, numbers, response_var = response_var)
    }
  } else {
    response_var_list = response_var
    if(any(!is.na(data_to_prep[[response_var]])))
    {
      if(max(as.numeric(data_to_prep[[response_var]]),na.rm=T) == 2)
      {
        data_to_prep[[response_var]] = as.factor(as.numeric(data_to_prep[[response_var]])-1)
      } else {
        data_to_prep[[response_var]] = as.factor(as.numeric(data_to_prep[[response_var]]))
      }
    }
  }
  
  data_to_prep = data_to_prep %>%
    mutate(EarlySeason = ifelse(week < 4, 1, 0),
           Playoffs = ifelse(week >= 19, 1, 0),
           LateSeason = ifelse(week >= 16 & week < 19, 1, 0),
           MidSeason = ifelse(week < 16 & week >= 4, 1, 0)
    )
  
  #Add masks for missing data:
  
  lag1_cols = colnames(data_to_prep)[str_detect(colnames(data_to_prep), 'lag1')]
  lag2_cols = colnames(data_to_prep)[str_detect(colnames(data_to_prep), 'lag2')]
  lag3_cols = colnames(data_to_prep)[str_detect(colnames(data_to_prep), 'lag3')]
  if(length(lag1_cols) > 0)
  {
    data_to_prep[['mask_lag1_missing']] = apply(data_to_prep[,lag1_cols], 1, function(x) all(is.na(x)))
  }
  if(length(lag2_cols) > 0)
  {
    data_to_prep[['mask_lag2_missing']] = apply(data_to_prep[,lag2_cols], 1, function(x) all(is.na(x)))
  }
  if(length(lag3_cols) > 0)
  {
    data_to_prep[['mask_lag3_missing']] = apply(data_to_prep[,lag3_cols], 1, function(x) all(is.na(x)))
  }
  #typically rookies:
  if(any(is.na(data_to_prep[[paste0('Last_Season_avg_', response_var)]])))
  {
    data_to_prep[['mask_last_season_missing']] = is.na(data_to_prep[[paste0('Last_Season_avg_', response_var)]])
  }
  if(any(is.na(data_to_prep[[paste0('Two_Seasons_Ago_avg_', response_var)]])))
  {
    data_to_prep[['mask_two_seasons_ago_missing']] = is.na(data_to_prep[[paste0('Two_Seasons_Ago_avg_', response_var)]])
  }
  #all models, even team models, will have missing data during week 1:
  if('games_played_this_season' %in% colnames(data_to_prep))
  {
    data_to_prep[['mask_current_season_missing']] = data_to_prep$games_played_this_season == 0
  } else if (paste0('avg_', response_var) %in% colnames(data_to_prep))
  {
    data_to_prep[['mask_current_season_missing']] = data_to_prep[[paste0('avg_', response_var)]] == 0
  }
  column_categories$mask = colnames(data_to_prep)[str_detect(colnames(data_to_prep), 'mask_')]
  column_categories$game_info = c(column_categories$game_info, 'EarlySeason', 'MidSeason', 'LateSeason', 'Playoffs')
  
  set.seed(1)
  
  if (train_mode)
  {
    train = data_to_prep %>% filter(season %in% c(2022,2023))
    test = data_to_prep %>% filter(season == 2024)
    final_test = data_to_prep %>% filter(season == 2025)
  
    column_categories_df = categorize_stats_fields(column_categories, column_category_current = current_season_column_category,
                                                   past_season_column_category = historical_season_column_category) %>% distinct() %>% select(-any_of(paste0(response_var, c('_lag1','_lag2','_lag3'))))
    
    #information value:
    
    exclude_from_model = c(column_categories$identifiers, 'jersey_number', 'latest_team', 'draft_team', 'game_id', 'opponent_team', 'gameday', 'team_qb_id', 'team_coach', "opp_offense_coach", "opp_offense_qb_id")
    exclude_from_information_value = c(exclude_from_model, 'coach_previous_weeks_with_team', "opp_coach_previous_weeks_with_team", column_categories$mask, column_categories$injuries, column_categories$usage_and_depth, column_categories$past_season_usage_and_depth, column_categories$playoff_clinching, column_categories$blue_chip, column_categories$matchup_history, column_categories$team_matchup_data, 'home_stadium', 'long_travel', 'opp_long_travel', 'div_game', 'long_week', 'opp_long_week', 'opp_long_week', 'opp_short_week', 'team_last_week_ot_road', 'game_on_birthday', 'neutral_field', 'week')
    
    list_training_data = list()
    list_test_data = list()
    list_final_test_data = list()
  
    for(r in response_var_list)
    {
      print(r)
      this_response_df = train %>% select(-any_of(setdiff(colnames(train)[which(str_detect(colnames(train), paste0(gsub('_[0-9]+','',r), '_[0-9]+')))], r)))
      this_response_df_test = test %>% select(-any_of(setdiff(colnames(test)[which(str_detect(colnames(test), paste0(gsub('_[0-9]+','',r), '_[0-9]+')))],r)))
      this_response_df_final_test = final_test %>% select(-any_of(setdiff(colnames(final_test)[which(str_detect(colnames(final_test), paste0(gsub('_[0-9]+','',r), '_[0-9]+')))],r)))
      res = trim_data(train = this_response_df, test = this_response_df_test, final_test = this_response_df_final_test, r = r, raw_response_var = response_var, missing_pct = 0.5,
                      exclude_from_information_value, exclude_from_model, response_var_list, this_or_that,
                      column_categories, column_categories_df, acceptable_predictive_power = acceptable_predictive_power, num_winners = num_winners)
      list_training_data[[r]] = res[[1]]
      list_test_data[[r]] = res[[2]]
      list_final_test_data[[r]] = res[[3]]
    }
    return(list(list_training_data, list_test_data, list_final_test_data, column_categories))
  } else {
    list_data = list()
    for(r in response_var_list)
    {
      print(r)
      this_response_var_data = data_to_prep %>% select(-any_of(setdiff(colnames(data_to_prep)[which(str_detect(colnames(data_to_prep), paste0(gsub('_[0-9]+','',r), '_[0-9]+')))], r)))
      previously_trained_model_data = read_parquet(paste0('model/ml_ready_data/train/',r,'.parquet'))
      if (r %in% colnames(this_response_var_data))
      {
        list_data[[r]] = this_response_var_data %>% select(all_of(colnames(previously_trained_model_data)))
      } else { #predict mode
        list_data[[r]] = this_response_var_data %>% select(all_of(setdiff(colnames(previously_trained_model_data), r)))
      }
      
    }
    return(list_data)
  }
  
}

#Turn IV results into dummy variables:

# iv_to_dummy = function(df, bins, r)
# {
#   new_dfs = list()
#   # for (r in response_vars)
#   # {
#     cat('Response Variable:', r)
#     #information_vars_used = bins %>% filter(Response == r) %>% filter(display == 1) %>% select(Variable) %>% distinct() %>% pull()
#     information_vars_used = bins %>% pull(Variable) %>% unique()
#     #information_vars_used = setdiff(information_vars_used, c(this_or_that_table$option1, this_or_that_table$option2))
#     #information_vars_used = c(information_vars_used, this_or_that_table[,paste0('decision_',r)])
#     #for the ones that did not score high on IV, I'll keep those in the model as is just in case the model sees a pattern in conjunction with other variables. But for the ones that did score high on IV, I will use the optimal bins.
#     
#     df_this_r = df %>% select(-na.omit(information_vars_used))
#     for(v in information_vars_used)
#     {
#       cat('Variable:', v)
#       relevant_bins = bins %>% filter(Response == r & Variable == v)
#       for (b in relevant_bins$Bin)
#       {
#         indices_to_include = c()
#         if(b == 'missing')
#         {
#           indices_to_include = c(indices_to_include, which(is.na(df[,v])))
#           new_var_name = paste0(v,'_missing')
#           
#         } else if (is.numeric(df[,v])) {
#           lower_bound = trimws(sub('\\[', '', setdiff(unlist(strsplit(gsub('%', '', b), ',')), 'missing')[1])) %>% as.numeric()
#           upper_bound =   trimws(sub('\\)', '', setdiff(unlist(strsplit(gsub('%', '', b), ',')), 'missing')[2])) %>% as.numeric()
#           indices_to_include = c(indices_to_include, which(df[,v] >= lower_bound & df[,v] < upper_bound))
#           if(!is.na(lower_bound) && lower_bound == (-Inf))
#           {
#             new_var_name = paste0(v,'_below_',upper_bound)
#           } else if (!is.na(upper_bound) && upper_bound == Inf)
#           {
#             new_var_name = paste0(v,'_',lower_bound,'_and_above')
#           } else if (!is.na(lower_bound) && !is.na(upper_bound)){
#             new_var_name = paste0(v,'_',lower_bound,'_',upper_bound)
#           } else {
#             new_var_name = paste0(v,'_',na.omit(c(lower_bound,upper_bound)))
#           }
#           if('missing' %in% unlist(strsplit(gsub('%', '', b), ',')))
#           {
#             indices_to_include = c(indices_to_include, which(is.na(df[,v])))
#             new_var_name = paste0(new_var_name,'_missing')
#           }
#           
#           
#         } else { #categorical
#           unique_vals = unlist(strsplit(gsub('%', '', b), ','))
#           indices_to_include = c(indices_to_include, which(df[,v] %in% unique_vals))
#           new_var_name = paste0(v,'_',paste(unique_vals, collapse='_'))
#           if('missing' %in% unique_vals)
#           {
#             indices_to_include = c(indices_to_include, which(is.na(df[,v])))
#           }
#           
#         }
#         
#         df_this_r[,new_var_name] = 0
#         df_this_r[indices_to_include, new_var_name] = 1
#         
#         cat('New var added:', new_var_name)
#         
#       }
#       
#     }
#     #this_or_that_not_used = setdiff(c(this_or_that_table$option1, this_or_that_table$option2), this_or_that_table[,paste0('decision_',r)])
#     print(r)
#     
#     colnames(df_this_r) = sapply(colnames(df_this_r), function(x) substring(x, 1, 1000))
#     #df_this_r = df_this_r %>% select(-any_of(c(this_or_that_not_used, setdiff(response_vars, r))))
#     #new_dfs = append(new_dfs, list(df_this_r))
#   #}
#   #names(new_dfs) = response_vars
#   return(df_this_r)
# }


 