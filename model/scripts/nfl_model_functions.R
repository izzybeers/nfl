library(stringr)

#df must have column Model_Probability
calculate_probability_ranges = function(df)
{
  return(df %>% mutate(
    ProbabilityFloor = case_when(
      Model_Probability < 0.10 ~ floor(Model_Probability / 0.02) * 0.02,
      Model_Probability < 0.30 ~ floor((Model_Probability - 0.10) / 0.05) * 0.05 + 0.10,
      TRUE                     ~ floor((Model_Probability - 0.30) / 0.10) * 0.10 + 0.30
    ),
    ProbabilityCeiling = case_when(
      Model_Probability < 0.10 ~ ProbabilityFloor + 0.02,
      Model_Probability < 0.30 ~ ProbabilityFloor + 0.05,
      TRUE                     ~ ProbabilityFloor + 0.10
    ),
    ProbabilityRange = paste0(
      round(ProbabilityFloor * 100), "% to ",
      round(ProbabilityCeiling * 100), "%"
    ),
    ProbabilityRange = factor(ProbabilityRange, 
                              levels = unique(ProbabilityRange[order(ProbabilityFloor)]))
  ))
}

#df must have columns: ProbabilityRange, ProbabilityFloor, ProbabilityCeiling (as derived in the calculate_probability_ranges function), along with BetHit (true/false) indicating whether bet won
assess_probability_ranges = function(df) {
  return(df %>% group_by(ProbabilityRange, ProbabilityFloor, ProbabilityCeiling) %>%
           summarise(PctWin = mean(BetHit), n = n()) %>%
           mutate(Assessment = case_when(
             n < 10 ~ 'Insufficient Data',
             PctWin >= ProbabilityFloor & PctWin <= ProbabilityCeiling ~ 'Inside Target Range',
             abs(PctWin - ProbabilityCeiling) < ProbabilityFloor*.1| abs(ProbabilityFloor - PctWin)  < ProbabilityFloor*.1 ~ 'Near Target Range',
             .default = 'Bad'
           )) %>% arrange(ProbabilityFloor) %>% ungroup() %>% select(-ProbabilityFloor, -ProbabilityCeiling))
}


categorize_stats_fields = function(column_categories, column_category_current)
{
  player_stats_cols_df = data.frame(Stat = column_categories[column_category_current],
                                    scope = 'Player Stats')
  colnames(player_stats_cols_df)[1] = 'Stat'
  player_stats_cols_df = player_stats_cols_df %>% 
    filter(!(Stat %in% c('GS', 'Active', 'Playoffs'))) %>%
    mutate(Timeframe = ifelse(str_detect(Stat, '(Lag)|(Last3)'), 'Recent Games', 'Full Current Season'),
           RawStat = gsub('last3_|avg_|mean_|median_|min_|max_|cv_|sd_|cumulative_|_lag[0-9]', '', Stat),
           StatType = case_when(
             (str_detect(RawStat, 'att|attempt|target|tgt|snap|offense|active') & !str_detect(RawStat, 'per|pct')) | RawStat == 'time_to_throw_per_attempt' ~ 'Opportunity',
             str_detect(RawStat, 'per|pct') & !str_detect(RawStat, 'snap') & RawStat != 'time_to_throw_per_attempt' ~ 'Efficiency',
             .default = 'Production'
           ))
  rownames(player_stats_cols_df)= NULL
  
  #for(type in unique(player_stats_cols_df$StatType))
  #{
   # print(toupper(type))
    #print(paste(unique(player_stats_cols_df$RawStat[player_stats_cols_df$StatType == type]), collapse = ", "))
  #}
  
  player_recent_seasons_stats_cols_df = data.frame(column_name = column_categories[past_season_column_category,
                                                   scope = 'Player Stats')
  colnames(player_recent_seasons_stats_cols_df)[1] = 'Stat'
  
  player_recent_seasons_stats_cols_df = player_recent_seasons_stats_cols_df %>% 
    filter(!(Stat %in% c('GS', 'Active', 'Playoffs'))) %>%
    mutate(Timeframe = ifelse(str_detect(Stat, 'Last') & !str_detect(Stat,'Two_Season'), 'Last_Season', '2 Seasons Ago'),
           RawStat = gsub('Last_Season_|Two_Seasons_Ago_|_min|_mean|_median|_max|cv_|_sd', '', Stat),
           StatType = case_when(
             str_detect(RawStat, 'Att|Attempt|Target|Tgt|Snap|Active|GS') & (str_detect(RawStat, 'Snap|Active|GS') | !str_detect(RawStat, 'Per|Pct')) ~ 'Opportunity',
             str_detect(RawStat, 'Pct_of_All|Rank') ~ 'Production',
             str_detect(RawStat, 'Per|Pct') & !str_detect(RawStat, 'Snap') ~ 'Efficiency',
             .default = 'Production'
           ))
  rownames(player_recent_seasons_stats_cols_df)= NULL
  
  # for(type in unique(player_recent_seasons_stats_cols_df$StatType))
  # {
  #   print(toupper(type))
  #   print(paste(unique(player_recent_seasons_stats_cols_df$RawStat[player_recent_seasons_stats_cols_df$StatType == type]), collapse = ", "))
  # }
  
  #Repeat for:
  #player rank stats (current season and historical)
  #team stats (current season and historical)
  #team rank stats
  #opp def stats (current season and historical)
  #opp def rank stats
  
  player_ranking_stats_cols_df = data.frame(column_name = column_categories[str_detect(names(column_categories), 'rank_columns')],
                                            scope = 'Player Rankings')
  colnames(player_ranking_stats_cols_df)[1] = 'Stat'
  
  player_ranking_stats_cols_df = player_ranking_stats_cols_df %>% 
    filter(!(Stat %in% c('GS', 'Active', 'Playoffs'))) %>%
    mutate(Timeframe = ifelse(str_detect(Stat, 'Last3'), 'Recent Games', 'Full Current Season'),
           RawStat = gsub('_Last3|Pct_Team_|_Rank|_Median|_Season', '', Stat),
           StatType = case_when(
             str_detect(RawStat, 'Att|Attempt|Target|Tgt|Snap|Active|G') & (str_detect(RawStat, 'Snap|Active|GS') | !str_detect(RawStat, 'Per|Pct')) ~ 'Opportunity',
             str_detect(RawStat, 'Yards|Yds') ~ 'Production',
             .default = 'Efficiency'
           ))
  rownames(player_ranking_stats_cols_df )= NULL
  
  # for(type in unique(player_ranking_stats_cols_df$StatType))
  # {
  #   print(toupper(type))
  #   print(paste(unique(player_ranking_stats_cols_df$RawStat[player_ranking_stats_cols_df$StatType == type]), collapse = ", "))
  # }
  
  team_current_stats_cols_df = data.frame(column_name = column_categories[str_detect(names(column_categories), 'team_current_season')],
                                          scope = 'Team Stats')
  colnames(team_current_stats_cols_df)[1] = 'Stat'
  
  team_current_stats_cols_df = team_current_stats_cols_df %>% filter(Stat!= 'current_team') %>%
    mutate(Timeframe = ifelse(str_detect(Stat, 'Last3'), 'Recent Games', 'Full Current Season'),
           RawStat = gsub('Team_|Last3_|Min_|Mean_|Median_|Max_|Avg_|Cumulative_|CV_|SD_|Offense_', '', Stat),
           StatType = case_when(
             RawStat == 'Differential_Win' ~ 'Efficiency',
             .default = 'Production'))
  rownames(team_current_stats_cols_df)= NULL
  
  # for(type in unique(team_current_stats_cols_df$StatType))
  # {
  #   print(toupper(type))
  #   print(paste(unique(team_current_stats_cols_df$RawStat[team_current_stats_cols_df$StatType == type]), collapse = ", "))
  # }
  
  team_historical_season_stats_cols_df = data.frame(column_name = column_categories[str_detect(names(column_categories), 'team_recent_seasons')],
                                                    scope = 'Team Stats')
  colnames(team_historical_season_stats_cols_df)[1] = 'Stat'
  
  team_historical_season_stats_cols_df = team_historical_season_stats_cols_df %>% 
    mutate(Timeframe = ifelse(str_detect(Stat, 'Last') & !str_detect(Stat,'Two_Season'), 'Last_Season', '2 Seasons Ago'),
           RawStat = gsub('Last_Season_|Two_Seasons_Ago_|Team_|Last3_|_min|_mean|_median|_max|_avg_|_cumulative_|CV_|_sd|_sum|Offense_', '', Stat),
           StatType = case_when(
             str_detect(Stat, 'Per') ~ 'Efficiency',
             .default = 'Production'))
  rownames(team_historical_season_stats_cols_df)= NULL
  
  # for(type in unique(team_historical_season_stats_cols_df$StatType))
  # {
  #   print(toupper(type))
  #   print(paste(unique(team_historical_season_stats_cols_df$RawStat[team_historical_season_stats_cols_df$StatType == type]), collapse = ", "))
  # }
  
  opp_current_stats_cols_df = data.frame(column_name = column_categories[str_detect(names(column_categories), 'opp_current_season')],
                                         scope = 'Opp Stats')
  colnames(opp_current_stats_cols_df)[1] = 'Stat'
  
  opp_current_stats_cols_df = opp_current_stats_cols_df %>% filter(!Stat %in% c('Opp_Used_To_Hot', 'Opp_Used_To_Cold', 'Opp.y')) %>%
    mutate(Timeframe = ifelse(str_detect(Stat, 'Last3'), 'Recent Games', 'Full Current Season'),
           RawStat = gsub('Opp_|Last3_|Min_|Mean_|Median_|Max_|Avg_|Cumulative_|CV_|SD_|Defense_', '', Stat),
           StatType = case_when(
             RawStat == 'Differential_Win' ~ 'Efficiency',
             .default = 'Production'))
  rownames(opp_current_stats_cols_df)= NULL
  
  # for(type in unique(opp_current_stats_cols_df$StatType))
  # {
  #   print(toupper(type))
  #   print(paste(unique(opp_current_stats_cols_df$RawStat[opp_current_stats_cols_df$StatType == type]), collapse = ", "))
  # }
  
  opp_historical_season_stats_cols_df = data.frame(column_name = column_categories[str_detect(names(column_categories), 'opp_recent_seasons')],
                                                   scope = 'Opp Stats')
  colnames(opp_historical_season_stats_cols_df)[1] = 'Stat'
  
  opp_historical_season_stats_cols_df = opp_historical_season_stats_cols_df %>% 
    mutate(Timeframe = ifelse(str_detect(Stat, 'Last') & !str_detect(Stat,'Two_Season'), 'Last_Season', '2 Seasons Ago'),
           RawStat = gsub('Last_Season_|Two_Seasons_Ago_|Opp_|Last3_|_min|_mean|_median|_max|_avg_|_cumulative_|CV_|_sd|_sum|Defense_', '', Stat),
           StatType = case_when(
             str_detect(Stat, 'Per') ~ 'Efficiency',
             .default = 'Production'))
  rownames(opp_historical_season_stats_cols_df)= NULL
  
  # for(type in unique(opp_historical_season_stats_cols_df$StatType))
  # {
  #   print(toupper(type))
  #   print(paste(unique(opp_historical_season_stats_cols_df$RawStat[opp_historical_season_stats_cols_df$StatType == type]), collapse = ", "))
  # }
  # 
  
  stats_columns_table = rbind(
    player_stats_cols_df,
    player_recent_seasons_stats_cols_df,
    player_ranking_stats_cols_df,
    team_current_stats_cols_df,
    team_historical_season_stats_cols_df,
    opp_current_stats_cols_df,
    opp_historical_season_stats_cols_df
  )
  return(stats_columns_table)
}

trim_columns_by_iv_correlation = function(columns_df, train_df, num_winners_per_category = 5)
{
  winners_df = data.frame()
  for(type in unique(columns_df$StatType))
  {
    winners = columns_df %>% filter(StatType == type) %>% arrange(desc(Total_IV)) %>% slice(1:num_winners_per_category) %>% ungroup()
    winners_df = rbind(winners_df, winners %>% select(Stat, StatType, Total_IV))
  }
  
  if(nrow(winners_df) > 0)
  {
    cor_mat = cor(train_df %>% select(all_of(winners_df$Stat)), 
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
  } else {
    final_winners = NULL
  }
  
  return(final_winners)
}

#define functions for creating information value tables:

create_iv_tables = function(df, response_var, var_list, column_categories, specific_bins = FALSE)
{
  all_bins = rbind()
  all_ivs = rbind()
  for(r in response_var)
  {
    for (var in var_list)
    {
      overall_mean = df %>% select(r) %>% pull() %>% mean()
      unique_vals = length(na.omit(unique(df[,var])))
      pct_missing = mean(is.na(df[,var]))
      
      if(unique_vals > 1)
      {
        bins = woebin(df, y = r, x = var, bin_num_limit = 6, check_cate_num = FALSE, print_step = 0, print_info = FALSE) %>% data.frame() %>% select(!!sym(paste0(var, '.variable')), !!sym(paste0(var,'.bin')),!!sym(paste0(var,'.bin_iv')), !!sym(paste0(var,'.total_iv')), !!sym(paste0(var, '.posprob')), !!sym(paste0(var, '.count')))
        colnames(bins) = c('Variable', 'Bin', 'Bin_IV', 'Total_IV', 'Pos_Prob', 'Bin_Count')
        bins = bins %>% mutate(Response = r, overall_mean_response = overall_mean, display = ifelse(unique_vals >= 3, 1, 0), unique_vals = unique_vals, pct_missing = pct_missing, class = class(df[,var]))
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
  }
  return(list(all_ivs,all_bins))
}

get_specific_iv_table = function(df, iv_table, bin_table, predictive_power_choice, column_categories, bin_iv_limit)
{
  if(predictive_power_choice != 'Significant')
  {
    iv_predictive_power = iv_table %>%
      filter(Predictive_Power == predictive_power_choice) %>%
      select(Variable, Response, display) %>% arrange(Variable, Response)
  } else {
    iv_predictive_power = iv_table %>%
      filter(Predictive_Power != 'None') %>%
      select(Variable, Response, display) %>% arrange(Variable, Response)
  }
  
  bins_cleaned = rbind()
  
  for(i in unique(iv_predictive_power$Variable))
  {
    if(predictive_power_choice != 'Significant')
    {
      subtable = bin_table %>% filter(Variable == i & Predictive_Power == predictive_power_choice) 
    } else {
      subtable = bin_table %>% filter(Variable == i)
    }
    if(all(!is.na(subtable$specific_count)))
    {
      if(i %in% column_categories$player_bio_data)
      {
        reference_number = length(unique(df$player_id))
      }
      if (i %in% column_categories$stadium_info)
      {
        reference_number = length(unique(df$Stadium))
      }
      subtable = subtable %>% filter(specific_count >= reference_number*0.1 & Bin_IV >= bin_iv_limit)
    } else {
      subtable = subtable %>% filter(Bin_IV >= bin_iv_limit)
    }
    bins_cleaned = rbind(bins_cleaned, subtable)
  }
  return(bins_cleaned)
}


this_or_that_results = function(ivs, this_or_that, responses)
{
  #this_or_that[,paste0('decision_',responses)] = NA
  this_or_that$decision = NA
  for(t in 1:nrow(this_or_that))
  {
    option1 = this_or_that$option1[t]
    option2 = this_or_that$option2[t]
    if(option1 %in% ivs$Variable & !(option2 %in% ivs$Variable))
    {
      this_or_that[t, 'decision'] = option1
    } else if(option2 %in% ivs$Variable & !(option1 %in% ivs$Variable))
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
    } else{
      this_or_that[t, 'decision'] = NULL
    }
  } 
  return(this_or_that)
}

#Turn IV results into dummy variables:

iv_to_dummy = function(df, bins, r)
{
  new_dfs = list()
  # for (r in response_vars)
  # {
    cat('Response Variable:', r)
    #information_vars_used = bins %>% filter(Response == r) %>% filter(display == 1) %>% select(Variable) %>% distinct() %>% pull()
    information_vars_used = bins %>% pull(Variable) %>% unique()
    #information_vars_used = setdiff(information_vars_used, c(this_or_that_table$option1, this_or_that_table$option2))
    #information_vars_used = c(information_vars_used, this_or_that_table[,paste0('decision_',r)])
    #for the ones that did not score high on IV, I'll keep those in the model as is just in case the model sees a pattern in conjunction with other variables. But for the ones that did score high on IV, I will use the optimal bins.
    
    df_this_r = df %>% select(-na.omit(information_vars_used))
    for(v in information_vars_used)
    {
      cat('Variable:', v)
      relevant_bins = bins %>% filter(Response == r & Variable == v)
      for (b in relevant_bins$Bin)
      {
        indices_to_include = c()
        if(b == 'missing')
        {
          indices_to_include = c(indices_to_include, which(is.na(df[,v])))
          new_var_name = paste0(v,'_missing')
          
        } else if (is.numeric(df[,v])) {
          lower_bound = trimws(sub('\\[', '', setdiff(unlist(strsplit(gsub('%', '', b), ',')), 'missing')[1])) %>% as.numeric()
          upper_bound =   trimws(sub('\\)', '', setdiff(unlist(strsplit(gsub('%', '', b), ',')), 'missing')[2])) %>% as.numeric()
          indices_to_include = c(indices_to_include, which(df[,v] >= lower_bound & df[,v] < upper_bound))
          if(!is.na(lower_bound) && lower_bound == (-Inf))
          {
            new_var_name = paste0(v,'_below_',upper_bound)
          } else if (!is.na(upper_bound) && upper_bound == Inf)
          {
            new_var_name = paste0(v,'_',lower_bound,'_and_above')
          } else if (!is.na(lower_bound) && !is.na(upper_bound)){
            new_var_name = paste0(v,'_',lower_bound,'_',upper_bound)
          } else {
            new_var_name = paste0(v,'_',na.omit(c(lower_bound,upper_bound)))
          }
          if('missing' %in% unlist(strsplit(gsub('%', '', b), ',')))
          {
            indices_to_include = c(indices_to_include, which(is.na(df[,v])))
            new_var_name = paste0(new_var_name,'_missing')
          }
          
          
        } else { #categorical
          unique_vals = unlist(strsplit(gsub('%', '', b), ','))
          indices_to_include = c(indices_to_include, which(df[,v] %in% unique_vals))
          new_var_name = paste0(v,'_',paste(unique_vals, collapse='_'))
          if('missing' %in% unique_vals)
          {
            indices_to_include = c(indices_to_include, which(is.na(df[,v])))
          }
          
        }
        
        df_this_r[,new_var_name] = 0
        df_this_r[indices_to_include, new_var_name] = 1
        
        cat('New var added:', new_var_name)
        
      }
      
    }
    #this_or_that_not_used = setdiff(c(this_or_that_table$option1, this_or_that_table$option2), this_or_that_table[,paste0('decision_',r)])
    print(r)
    
    colnames(df_this_r) = sapply(colnames(df_this_r), function(x) substring(x, 1, 1000))
    #df_this_r = df_this_r %>% select(-any_of(c(this_or_that_not_used, setdiff(response_vars, r))))
    #new_dfs = append(new_dfs, list(df_this_r))
  #}
  #names(new_dfs) = response_vars
  return(df_this_r)
}

data_prep = function(df, response, pre_model = FALSE)
{
  if ("weight" %in% colnames(df))
  {
    df = df %>% rename('player_weight' = 'weight')
  }
  if (pre_model == TRUE)
  {
    df = df[, sapply(df, function(x) length(unique(x)) > 1 && !is.list(x))]
    df[[response]] = as.numeric(as.character(df[,response]))
  }
  
  df = df %>%
    mutate(across(where(is.character), as.factor))
  
  df = df %>%
    mutate(across(everything(), ~ ifelse(is.nan(.x), NA, .x))) %>%
    rename_with(.cols = matches(" "),
                .fn = ~gsub(' ', '_', .x)) %>%
    select(all_of(names(.)[nchar(names(.)) <= 100]))
  
  
  colnames(df) = gsub("[^A-Za-z0-9_]", "_", colnames(df))
  
  df = df[, !duplicated(colnames(df))] #remove any duplicated column names
  
  other_responses = setdiff(colnames(df)[str_detect(colnames(df), '_Yds_[0-9]+')], response)
  df = df %>% select(-any_of(other_responses))
  
  return(df)
}

run_gbm = function(df, model_name, response, path, t_per_s, i_range, s_range, n_range, b_range)
{
  set.seed(1)
  df = data_prep(df, response, pre_model = TRUE)
  
  #random subset if too big:
  if(nrow(df)*ncol(df) > 15000000)
  {
    df = df[sample(1:nrow(df), 15000000/ncol(df)),]
    s_range = s_range[s_range <= 0.05] #no large shrinkage if data is huge
  }
  
  train_indices = sample(1:nrow(df), 0.8*nrow(df))
  train = df[train_indices,]
  valid = df[-train_indices,]
  if(mean(train[,response]) < 0.15)
  {
    weights = ifelse(train[[response]] == 1, 4, 1)  # Increase weights when low prevalence
  } else {
    weights = NULL
  }
  
  best_pct_high_medium = -Inf
  best_model = NULL
  tuning=rbind()
  
  for (i in i_range)
  {
    print(i)
    for (s in s_range)
    {
      print(s)
      t = t_per_s[which(s_range == s)]
      for (n in n_range)
      {
        print(n)
        for (bag in b_range)
        {
          print(bag)

          formula = as.formula(paste(response, "~ ."))
          
          if(!is.null(weights))
          {
            model = gbm(formula = formula,
                        data = train,
                        distribution = 'bernoulli',
                        n.trees = t,
                        interaction.depth = i,
                        shrinkage = s,
                        n.minobsinnode = n,
                        cv.folds = 5,
                        bag.fraction = bag,
                        weights = weights,
                        keep.data = FALSE)
          } else {
            model = gbm(formula = formula,
                        data = train,
                        distribution = 'bernoulli',
                        n.trees = t,
                        interaction.depth = i,
                        shrinkage = s,
                        n.minobsinnode = n,
                        cv.folds = 5,
                        bag.fraction = bag,
                        keep.data = FALSE)
          }
          print('model done')
          best_tree = gbm.perf(model, method = "cv", plot.it = FALSE)
        
          res = get_prediction_data_metrics(df = valid, model = model, tree = best_tree, type = 'gbm', response = response)
          buckets = res[[1]]
          precision = res[[2]]
          recall = res[[3]]          
          pct_high = mean(buckets$Assessment == 'Inside Target Range')
          pct_high_medium = mean(buckets$Assessment %in% c('Inside Target Range','Near Target Range'))
          
          if(!is.na(pct_high_medium) && pct_high_medium > best_pct_high_medium)
          {
            best_model = model
            best_pct_high_medium = pct_high_medium
          }
          rm(model)
          gc(FALSE)
            tuning = rbind(tuning, cbind(Response = response, trees = best_tree, idepth = i, shrink = s, nmino = n, bag = bag, precision, recall, pct_high, pct_high_medium, prevalence = mean(df[,response])))
          }
      }
    }
  }
  tuning = data.frame(tuning)
  if(!is.null(path))
  {
    #write the original tuning to rds, and the best model:
    saveRDS(tuning, tolower(paste0('model/tunings_and_models/',model_name,'/',path,'/all_tunings_',response,'.rds')))
    if(!is.null(best_model))
    {
      saveRDS(best_model, tolower(paste0('model/tunings_and_models/',model_name,'/',path,'/model_',response,'.rds')))
    }
  } else {
    return(list(tuning, best_model))
  }
}


run_rf = function(df, model_name, response, path, t, mtry, min_node_size, sample_fraction)
{
  set.seed(1)
  df = data_prep(df, response, pre_model = TRUE)
  
  train_indices = sample(1:nrow(df), 0.8*nrow(df))
  train = df[train_indices,]
  valid = df[-train_indices,]
  if(mean(train[,response] == "1") < 0.15) {
    obs_weights = ifelse(train[[response]] == "1", 4, 1) 
  } else {
    obs_weights = NULL
  }
  train[[response]] <- as.factor(train[[response]])
  valid[[response]] <- as.factor(valid[[response]])
  
  best_pct_high_medium = -Inf
  best_model = NULL
  tuning=rbind()
  
  for (m in mtry)
  {
    print(m)
    for (n in min_node_size)
    {
      print(n)
      for (s in sample_fraction)
      {
        print(s)
        
        formula = as.formula(paste(response, "~ ."))
        
        if(!is.null(obs_weights))
        {
          model = ranger(formula = formula,
                      data = train,
                      distribution = 'bernoulli',
                      n.trees = t,
                      mtry = m, 
                      min.node.size = n,
                      sample.fraction = s,
                      probability = TRUE,
                      case.weights = obs_weights,
                      importance = 'impurity')
        } else {
          model = ranger(formula = formula,
                         data = train,
                         num.trees = t,
                         mtry = m, 
                         min.node.size = n,
                         sample.fraction = s,
                         probability = TRUE,
                         importance = 'impurity')
        }
        print('model done')
        
        res = get_prediction_data_metrics(df = valid, model = model, tree = NULL, type = 'rf', response = response)
        buckets = res[[1]]
        precision = res[[2]]
        recall = res[[3]]          
        pct_high = mean(buckets$Assessment == 'Inside Target Range')
        pct_high_medium = mean(buckets$Assessment %in% c('Inside Target Range','Near Target Range'))
        
        if(!is.na(pct_high_medium) && pct_high_medium > best_pct_high_medium)
        {
          best_model = model
          best_pct_high_medium = pct_high_medium
        }
        rm(model)
        gc(FALSE)
        tuning = rbind(tuning, cbind(Response = response, mtry = m, min_node_obs = n, sample_fraction = s, precision, recall, pct_high, pct_high_medium, prevalence = mean(df[,response])))
      }
    }
  }
  tuning = data.frame(tuning)
  if(!is.null(path))
  {
    #write the original tuning to rds, and the best model:
    saveRDS(tuning, tolower(paste0('model/tunings_and_models/',model_name,'/',path,'/all_tunings_',response,'.rds')))
    if(!is.null(best_model))
    {
      saveRDS(best_model, tolower(paste0('model/tunings_and_models/',model_name,'/',path,'/model_',response,'.rds')))
    }
  } else {
    return(list(tuning, best_model))
  }
}


get_prediction_data_metrics = function(df, model, tree = NULL, type, response)
{
    #df would be a test dataset or validation dataset
  
    df = data_prep(df, response)
    if(!is.null(tree))
    {
      tree = tree
    } else if (type != 'rf') {
      tree = gbm.perf(model, method = "cv", plot.it = FALSE)
    }
    
    if(type == 'gbm')
    {
      preds = predict(model, df, n.trees = tree, type = "response")
    } else {
      preds = predict(model, data = df)$predictions[,2]
    }
    
    actual = df[[response]]
    #precision and recall
    pred_class_table = data.frame()
    for (p in seq(0.05,1,0.05))
    {
      pred_classes = ifelse(preds > p, 1, 0)
      precision = (sum(pred_classes == 1 & actual == 1))/(sum(pred_classes == 1))
      recall = (sum(pred_classes == 1 & actual == 1))/(sum(actual == 1))
      if(!is.na(precision) & precision > 0 & !is.na(recall) & recall > 0)
      {
        pred_class_table = rbind(pred_class_table, c(p, precision, recall))
      }
    }
    colnames(pred_class_table) = c('p', 'precision', 'recall')
    
    pred_class_table$diff_p_r = abs(pred_class_table$precision - pred_class_table$recall)
    optimal = pred_class_table[which.min(pred_class_table$diff_p_r), c('p','precision','recall')]
    
    # buckets = ifelse(preds < 0.01, 0, 
    #                  ifelse(preds < 0.1, 0.01, floor(preds*10)/10))
    
    df$Model_Probability = preds
    df$BetHit = df[[response]]
    buckets_df = df %>% calculate_probability_ranges() %>% assess_probability_ranges() 
    
    #weird floating point precision so have to round the seq to 1 digit to make sure it's stored correctly:
    # pct_yes = paste0(100*round(sapply(round(c(0, 0.01, seq(0.1,0.9,0.1)),1), function(b) mean(actual[which(df$ == b)], na.rm = TRUE)),2),'%')
    # size = sapply(round(c(0, 0.01, seq(0.1,0.9,0.1)),1), function(b) sum(buckets == b))
    
    # buckets_df = data.frame(pct_yes, size)
    
    
    # vec = c(0.01,seq(0.1, 0.9,0.1))
    # vec_above = c(seq(0.1,0.9,0.1), 0.99)
    # rownames(buckets_df) = c('Predicted_Less_1Pct', paste0('Predicted_',vec, '_to_',vec_above))
    
    return(list(buckets_df, optimal$precision, optimal$recall))

  
}

# evaluate_buckets = function(buckets)
# {
#   errors = c()
#   #weird floating point precision so have to round the seq to 1 digit to make sure it's stored correctly:
#   for (b in 1:length(round(c(0, 0.01, seq(0.1,0.9,0.1)),1)))
#   {
#     lower_bound = round(c(0, 0.01, seq(0.1,0.9,0.1)),1)[b]
#     upper_bound = lower_bound + 0.1
#     pct_yes = (buckets %>% mutate(pct_yes = as.numeric(gsub('%','',pct_yes))/100))[b,'pct_yes']
#     size = buckets$size[b]
#     if(size >= 10)
#     {
#       err = ifelse(pct_yes < lower_bound, (lower_bound - pct_yes),
#                    ifelse(pct_yes > upper_bound, (pct_yes - upper_bound), 0))
#     } else {
#       lower_bound_size = round(lower_bound*size)
#       upper_bound_size = round(upper_bound*size)
#       amt_yes = pct_yes*size
#       err = ifelse(size == 0, NA,
#                    ifelse(amt_yes < lower_bound_size & lower_bound_size != 0, (lower_bound_size - amt_yes) / lower_bound_size,
#                           ifelse(amt_yes > upper_bound_size & upper_bound_size !=0, (amt_yes - upper_bound_size) / upper_bound_size, 0)))
#     }
#     errors = c(errors, err)
#   }
#   avg_buckets_error = mean(errors, na.rm = TRUE)
#   median_buckets_error = median(errors, na.rm = TRUE)
#   min_buckets_error = min(errors, na.rm = TRUE)
#   max_buckets_error = max(errors, na.rm = TRUE)
#   confidence = case_when(
#     is.na(errors) ~ 'No Data',
#     errors == 0 ~ 'High',
#     errors < 0.1~ 'Medium',
#     .default = 'Low'
#   )
#   
#   return(list(avg_buckets_error, median_buckets_error, min_buckets_error, max_buckets_error, confidence))
# }
 