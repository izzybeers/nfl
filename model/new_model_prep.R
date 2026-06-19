source('model/scripts/nfl_model_functions.R')
library(dplyr)
library(stringr)
library(scorecard)
library(gbm)
library(ranger)
library(tidyr)

this_or_that = data.frame(cbind(option1 = c('draft_round', 'Grass_Type', 'Roof', 'age'),
                                option2 = c('draft_pick', 'Familiar_Grass_Type', 'Familiar_Roof_Type', 'Years_Since_Drafted')))

create_data_with_response_variables = function(df, numbers, response_var) {
  for(n in numbers)
  {
    df  = df %>% mutate(!!paste0(response_var, '_', gsub('-', 'minus', n)) := ifelse(!!sym(response_var) >= n, 1, 0))
  }
  return(df)
}


model_prep = function(data_to_prep, column_categories, numbers, response_var, current_season_column_category, historical_season_column_category, split_method = 'random', split_models = FALSE)
{

  data_to_prep = data_to_prep %>%
    mutate(across(where(is.numeric), ~ifelse(is.infinite(.x), NA, .x))) %>%
    mutate(across(where(is.logical), ~as.factor(.x))) %>%
    filter(!is.na(!!sym(response_var)))
  
  if (any(!is.na(numbers)))
  {
    response_var_list = paste0(response_var, '_', gsub('-', 'minus', numbers))
    data_to_prep = create_data_with_response_variables(data_to_prep, numbers, response_var = response_var)
    data_to_prep[[response_var]] = as.factor(data_to_prep[[response_var]])
  } else {
    response_var_list = response_var
    data_to_prep[response_var] = as.numeric(data_to_prep[[response_var]])
  }
  
  
  if(split_method == 'random')
  {
    set.seed(1)
    train_indices = sample(1:nrow(data_to_prep), 0.6*nrow(data_to_prep) ,replace = FALSE)
    test_indices = setdiff(1:nrow(data_to_prep), train_indices)
    # remaining_indices = setdiff(1:nrow(data_to_prep), train_indices)
    # test_indices = sample(remaining_indices, 0.5*length(remaining_indices))
    # final_test_indices = setdiff(remaining_indices, test_indices) #for the final logistic regression
    
    train = data_to_prep[train_indices,]
    test = data_to_prep[test_indices,]
    # final_test = data_to_prep[final_test_indices,]
  }
  
  column_categories_df = categorize_stats_fields(column_categories, column_category_current = current_season_column_category,
                                                 past_season_column_category = historical_season_column_category) %>% distinct() %>% select(-any_of(paste0(response_var, c('_lag1','_lag2','_lag3'))))

  #information value:
  
  exclude_from_model = c(column_categories$identifiers, 'jersey_number', 'latest_team', 'draft_team', 'opponent_team', 'gameday', 'team_qb_id', 'team_coach', "opp_offense_coach", "opp_offense_qb_id")
  exclude_from_information_value = c(exclude_from_model, 'coach_previous_weeks_with_team', "opp_coach_previous_weeks_with_team", column_categories$injuries, column_categories$usage_and_depth, column_categories$past_season_usage_and_depth, column_categories$playoff_clinching, column_categories$blue_chip, column_categories$matchup_history, column_categories$team_matchup_data, 'home_stadium', 'long_travel', 'opp_long_travel', 'div_game', 'long_week', 'opp_long_week', 'opp_long_week', 'opp_short_week', 'team_last_week_ot_road', 'game_on_birthday', 'neutral_field', 'week')
  
  this_response_df_list = list()
  
  for(r in response_var_list)
  {
    print(r)
    this_response_df = train %>% select(-any_of(setdiff(colnames(train)[which(str_detect(colnames(train), paste0(gsub('_[0-9]+','',r), '_[0-9]+')))], r)))
    this_response_df_test = test %>% select(-any_of(setdiff(colnames(test)[which(str_detect(colnames(test), paste0(gsub('_[0-9]+','',r), '_[0-9]+')))],r)))
    #this_response_df_final_test = final_test %>% select(-any_of(setdiff(colnames(final_test)[which(str_detect(colnames(final_test), paste0(gsub('_[0-9]+','',r), '_[0-9]+')))],r)))
    
    if (split_models == TRUE)
    {
      for(early_season in c(0,1))
      {
        if(early_season == 0)
        {
          print('main season')
          this_season_df = this_response_df %>% filter(Week >= 4)
          this_season_df_test = this_response_df_test %>% filter(Week >= 4)
          this_season_df_final_test = this_response_df_final_test %>% filter(Week >= 4)
        } else {
          print('early season')
          this_season_df = this_response_df %>% filter(Week <= 3)
          this_season_df_test = this_response_df_test %>% filter(Week <= 3)
          this_season_df_final_test = this_response_df_final_test %>% filter(Week <= 3)
        }
        for(rookie in c(0,1))
        {
          final_stats_fields_df_gbm = rbind()
          final_stats_fields_df_rf = rbind()
          if(rookie == 1)
          {
            print('rookie')
            this_df = this_season_df %>% filter(Years_Since_Drafted == 0)
            this_df_test = this_season_df_test %>% filter(Years_Since_Drafted == 0)
            this_df_final_test = this_season_df_final_test %>% filter(Years_Since_Drafted == 0)
          } else {
            print('non rookie')
            this_df = this_season_df %>% filter(Years_Since_Drafted > 0)
            this_df_test = this_season_df_test %>% filter(Years_Since_Drafted > 0)
            this_df_final_test = this_season_df_final_test %>% filter(Years_Since_Drafted > 0)
          }
          #for categorical fields and non-stat numeric fields:
          ivs_and_bins = create_iv_tables(df = this_df, r = r, var_list = setdiff(colnames(this_df), c(exclude_from_information_value, column_categories_df$Stat, colnames(this_df)[which(str_detect(colnames(this_df), gsub('_[0-9]+','',r)))])), column_categories = column_categories, specific_bins = TRUE)
          ivs = ivs_and_bins[[1]]
          bins = ivs_and_bins[[2]]
          this_or_that_table = this_or_that_results(ivs = ivs, this_or_that = this_or_that) %>%
            mutate(NotSelected = ifelse(option1 == decision, option2, option1))
          
          ivs = ivs %>% filter(!(Variable %in% this_or_that_table$NotSelected))
          bins = bins %>% filter(!(Variable %in% this_or_that_table$NotSelected))
          
          fields_to_use_bins_gbm = bins %>% filter(Predictive_Power != 'None' & class =='character' & pct_missing < 0.5 & unique_vals > 10) %>% pull(Variable) %>% unique()
          fields_to_use_as_is_gbm =  bins %>% filter(Predictive_Power != 'None'  & !(class =='character' & pct_missing < 0.5 & unique_vals > 10)) %>% pull(Variable) %>% unique()
          fields_to_use_bins_rf = bins %>% filter(Predictive_Power != 'None' & class =='character' & pct_missing < 0.05 & unique_vals > 10) %>% pull(Variable) %>% unique()
          fields_to_use_as_is_rf =  bins %>% filter(Predictive_Power != 'None'  & !(class =='character' & pct_missing < 0.05 & unique_vals > 10)) %>% pull(Variable) %>% unique()
          
          gbm_this_df = this_df %>% select(r, Season, Week, player_id, Team, Opp, Date, column_categories_df$Stat, fields_to_use_bins_gbm, fields_to_use_as_is_gbm)
          gbm_this_df_test = this_df_test %>% select(r, Season, Week, player_id, Team, Opp, Date, column_categories_df$Stat, fields_to_use_bins_gbm, fields_to_use_as_is_gbm)
          gbm_this_df_final_test = this_df_final_test %>% select(r, Season, Week, player_id, Team, Opp, Date, column_categories_df$Stat, fields_to_use_bins_gbm, fields_to_use_as_is_gbm)
          rf_this_df = this_df %>% select(r, Season, Week, player_id, Team, Opp, Date, column_categories_df$Stat, fields_to_use_bins_rf, fields_to_use_as_is_rf)
          rf_this_df_test = this_df_test %>% select(r, Season, Week, player_id, Team, Opp, Date, column_categories_df$Stat, fields_to_use_bins_rf, fields_to_use_as_is_rf)
          rf_this_df_final_test = this_df_final_test %>% select(r, Season, Week, player_id, Team, Opp, Date, column_categories_df$Stat, fields_to_use_bins_rf, fields_to_use_as_is_rf)
          
          if(length(fields_to_use_bins_gbm) > 0)
          {
            all_bins_gbm = get_specific_iv_table(df = gbm_this_df,
                                             iv_table = ivs %>% filter(Variable %in% fields_to_use_bins_gbm),
                                             bin_table = bins %>% filter(Variable %in% fields_to_use_bins_gbm),
                                             predictive_power_choice = 'Significant',
                                             column_categories = column_categories,
                                             bin_iv_limit = 0.01) %>% arrange(Variable, Response) %>% select(Variable, Bin, Response, display, Bin_IV, Bin_Count, specific_count, Pos_Prob, overall_mean_response)
            gbm_this_df = iv_to_dummy(df = gbm_this_df, bins = all_bins, r = r)
            gbm_this_df_test = iv_to_dummy(df = gbm_this_df_test, bins = all_bins, r = r)
            gbm_this_df_final_test = iv_to_dummy(df = gbm_this_df_final_test, bins = all_bins, r = r)
          }
          
          if(length(fields_to_use_bins_rf) > 0)
          {
            all_bins_rf = get_specific_iv_table(df = rf_this_df,
                                                 iv_table = ivs %>% filter(Variable %in% fields_to_use_bins_rf),
                                                 bin_table = bins %>% filter(Variable %in% fields_to_use_bins_rf),
                                                 predictive_power_choice = 'Significant',
                                                 column_categories = column_categories,
                                                 bin_iv_limit = 0.01) %>% arrange(Variable, Response) %>% select(Variable, Bin, Response, display, Bin_IV, Bin_Count, specific_count, Pos_Prob, overall_mean_response)
            rf_this_df = iv_to_dummy(df = rf_this_df, bins = all_bins, r = r)
            rf_this_df_test = iv_to_dummy(df = rf_this_df_test, bins = all_bins, r = r)
            rf_this_df_final_test = iv_to_dummy(df = rf_this_df_final_test, bins = all_bins, r = r)
          }
          
          
          for(scope in unique(column_categories_df$scope))
          {
            print(scope)
            for(tf in unique(column_categories_df$Timeframe[column_categories_df$scope == scope]))
            {
              print(tf)
              iv_tables = create_iv_tables(this_df, r = r, var_list = column_categories_df$Stat[column_categories_df$scope == scope & column_categories_df$Timeframe == tf],
                                              column_categories, specific_bins = FALSE)
              
              column_categories_player_stats_with_iv = column_categories_df %>%
                inner_join(iv_tables[[1]] %>% distinct() %>% select(Variable, Total_IV, Predictive_Power, pct_missing), join_by('Stat' == 'Variable')) %>%
                filter(Predictive_Power != 'None')
              
              if(nrow(column_categories_player_stats_with_iv) > 0)
              {
                stats_fields_gbm = trim_columns_by_iv_correlation(columns_df = column_categories_player_stats_with_iv %>% filter(pct_missing < 0.5), train_df = gbm_this_df, num_winners_per_category = 5)
                
                if(!is.null(stats_fields_gbm) && nrow(stats_fields_gbm) > 0)
                {
                  final_stats_fields_df_gbm = rbind(final_stats_fields_df_gbm,
                                                    stats_fields_gbm %>% mutate(Response = r, Scope = scope, Timeframe = tf))
                }
                stats_fields_rf = trim_columns_by_iv_correlation(columns_df = column_categories_player_stats_with_iv %>% filter(pct_missing < 0.1), train_df = rf_this_df, num_winners_per_category = 5)
                
                if(!is.null(stats_fields_rf) && nrow(stats_fields_rf) > 0)
                {
                  final_stats_fields_df_rf = rbind(final_stats_fields_df_rf,
                                                  stats_fields_rf %>% mutate(Response = r, Scope = scope, Timeframe = tf))
                }
              }
            }
          } 
          #handle test data:
          
          
          list_training_data[[paste0(r,'_gbm_',
                                     ifelse(early_season == 1, '_EarlySeason', '_MainSeason'),
                                     ifelse(rookie == 1, '_Rookie', ''))]] = gbm_this_df %>% select(-any_of(c(setdiff(column_categories_df$Stat, final_stats_fields_df_gbm$Stat), setdiff(exclude_from_information_value, c('Season', 'Week', 'player_id', 'Team', 'Opp', 'Date'))))) 
          list_test_data[[paste0(r,'_gbm_',
                                 ifelse(early_season == 1, '_EarlySeason', '_MainSeason'),
                                 ifelse(rookie == 1, '_Rookie', ''))]] = gbm_this_df_test %>% select(-any_of(c(setdiff(column_categories_df$Stat, final_stats_fields_df_gbm$Stat), setdiff(exclude_from_information_value, c('Season', 'Week', 'player_id', 'Team', 'Opp', 'Date'))))) 
          # list_final_test_data[[paste0(r,'_gbm_',
          #                              ifelse(early_season == 1, '_EarlySeason', '_MainSeason'),
          #                              ifelse(rookie == 1, '_Rookie', ''))]] = gbm_this_df_final_test %>% select(-any_of(c(setdiff(column_categories_df$Stat, final_stats_fields_df_gbm$Stat), setdiff(exclude_from_information_value, c('Season', 'Week', 'player_id', 'Team', 'Opp', 'Date'))))) 
          list_training_data[[paste0(r,'_rf_',
                                     ifelse(early_season == 1, '_EarlySeason', '_MainSeason'),
                                     ifelse(rookie == 1, '_Rookie', ''))]] = rf_this_df %>% select(-any_of(c(setdiff(column_categories_df$Stat, final_stats_fields_df_rf$Stat), setdiff(exclude_from_information_value, c('Season', 'Week', 'player_id', 'Team', 'Opp', 'Date'))))) 
          list_test_data[[paste0(r,'_rf_',
                                 ifelse(early_season == 1, '_EarlySeason', '_MainSeason'),
                                 ifelse(rookie == 1, '_Rookie', ''))]] = rf_this_df_test %>% select(-any_of(c(setdiff(column_categories_df$Stat, final_stats_fields_df_rf$Stat), setdiff(exclude_from_information_value, c('Season', 'Week', 'player_id', 'Team', 'Opp', 'Date'))))) 
          # list_final_test_data[[paste0(r,'_rf_',
          #                              ifelse(early_season == 1, '_EarlySeason', '_MainSeason'),
          #                              ifelse(rookie == 1, '_Rookie', ''))]] = rf_this_df_final_test %>% select(-any_of(c(setdiff(column_categories_df$Stat, final_stats_fields_df_rf$Stat), setdiff(exclude_from_information_value, c('Season', 'Week', 'player_id', 'Team', 'Opp', 'Date'))))) 
        }
      }
    } else {
      
      print('creating IVs and bins...')
      ivs_and_bins = create_iv_tables(df = this_response_df, r = r, var_list = setdiff(colnames(this_response_df), c(exclude_from_information_value, column_categories_df$Stat, colnames(this_response_df)[which(str_detect(colnames(this_response_df), gsub('_[0-9]+|','',r)))], unlist(response_var_list))), column_categories = column_categories, specific_bins = TRUE)
      ivs = ivs_and_bins[[1]]
      bins = ivs_and_bins[[2]]
      this_or_that_table = this_or_that_results(ivs = ivs, this_or_that = this_or_that, r = r) %>%
        mutate(NotSelected = ifelse(option1 == decision, option2, option1))
      this_or_that_removals = c(setdiff(this_or_that$option1, this_or_that_table$option1), setdiff(this_or_that$option2, this_or_that_table$option2), this_or_that_table$NotSelected)
      ivs = ivs %>% filter(!(Variable %in% this_or_that_removals))
      bins = bins %>% filter(!(Variable %in% this_or_that_removals))
      
      fields_to_use_bins = bins %>% filter(Predictive_Power  %in% c('Suspicious', 'High', 'Medium') & class =='character' & pct_missing < 0.5 & unique_vals > 10) %>% pull(Variable) %>% unique()
      fields_to_use_as_is =  bins %>% filter(Predictive_Power %in% c('Suspicious', 'High', 'Medium')  & !(class =='character' & pct_missing < 0.5 & unique_vals > 10)) %>% pull(Variable) %>% unique()
  
      all_stats_fields = rbind()
      for(scope in unique(column_categories_df$scope))
      {
        print(scope)
        for(tf in unique(column_categories_df$Timeframe[column_categories_df$scope == scope]))
        {
          print(tf)
          iv_tables = create_iv_tables(this_response_df, r = r, var_list = column_categories_df$Stat[column_categories_df$scope == scope & column_categories_df$Timeframe == tf],
                                       column_categories, specific_bins = FALSE)
          iv_tables = iv_tables[[1]]
          
          column_categories_player_stats_with_iv = column_categories_df %>%
            inner_join(iv_tables %>% distinct() %>% select(Variable, Total_IV, Predictive_Power, pct_missing), join_by('Stat' == 'Variable')) %>%
            filter(Predictive_Power %in% c('Suspicious', 'High', 'Medium'))
          
          if(nrow(column_categories_player_stats_with_iv) > 0)
          {
            all_stats_fields = rbind(all_stats_fields,
                                 trim_columns_by_iv_correlation(columns_df = column_categories_player_stats_with_iv %>% filter(pct_missing < 0.5), train_df = this_response_df, num_winners_per_category = 10))
            
          }
        }
      } 
      
      selected_fields = c(column_categories$identifiers, all_stats_fields$Stat, fields_to_use_as_is, fields_to_use_bins, setdiff(exclude_from_information_value, exclude_from_model), paste0(response_var, '_lag1'), paste0(response_var, '_lag2'), paste0(response_var, '_lag3'))
      
      print(paste('Fields that don\'t exist:', setdiff(selected_fields, colnames(this_response_df))))
      
      training_data = this_response_df %>% select(r, any_of(selected_fields))
      test_data = this_response_df_test %>% select(r, any_of(selected_fields))
      
      training_data = training_data %>% iv_to_dummy(bins = bins %>% filter(Variable %in% fields_to_use_bins), r) %>% select(-any_of(fields_to_use_bins))
      test_data = test_data %>% iv_to_dummy(bins = bins %>% filter(Variable %in% fields_to_use_bins), r) %>% select(-any_of(fields_to_use_bins))
      
      this_response_df_list[[r]] = list(training_data, test_data)
    }
  }
  return(this_response_df_list)
}
  
  
  # all_results = list()
  # 
  # t_per_s = c(1000, 300)
  # i_range = c(2,5,8)
  # s_range = c(0.01,0.05)
  # n_range = 10
  # b_range = c(0.3, 0.5, 0.7)
  # 
  # for(i in 1:length(list_training_data))
  # {
  #   
  #   sample_train = list_training_data[[i]]
  #   sample_test = list_test_data[[i]]
  #   #sample_final_test = list_final_test_data[[i]]
  #   
  #   if(str_detect(names(list_training_data)[i], 'gbm_'))
  #   {
  #     t1 = Sys.time()
  #     res = run_gbm(
  #       df = sample_train %>% select(-Week, -Season, -player_id, -Team, -Opp, -Date),
  #       response = r,
  #       model_name = 'Receiving_Yds',
  #       path = NULL,
  #       t_per_s = t_per_s,
  #       i_range = i_range,
  #       s_range = s_range,
  #       n_range = n_range,
  #       b_range = b_range
  #     )
  #     Sys.time() - t1
  #   
  #     tuning = res[[1]]
  #     best_model = res[[2]]
  #     best_trees = tuning$trees[which.max(tuning$pct_high_medium)]
  #     
  #     test_results = get_prediction_data_metrics(df = sample_test, model = best_model, type = 'gbm', tree = best_trees, response = r)
  #     
  #     all_results[[names(list_training_data)[i]]] = test_results
  #   } else if (str_detect(names(list_training_data)[i], '_rf_'))
  #   {
  #     #RF:
  #     
  #     t1 = Sys.time()
  #     mtry_starting_point = round(sqrt(ncol(sample_train) - 5))
  #     res_rf = run_rf(
  #       df = sample_train %>% filter(Week > 1) %>% drop_na() %>% select(-Week, -Season, -player_id, -Team, -Opp, -Date),
  #       response = r,
  #       model_name = 'Receiving_Yds',
  #       path = NULL,
  #       t = 1000,
  #       mtry = unique(c(round(0.67*mtry_starting_point), mtry_starting_point, round(1.5*mtry_starting_point), 2*mtry_starting_point)),
  #       min_node_size = c(5,10,15),
  #       sample_fraction = c(0.6, 0.8, 1)
  #     )
  #     Sys.time() - t1
  #     
  #     tuning_rf = res_rf[[1]]
  #     best_model_rf = res_rf[[2]]
  #     
  #     test_results_rf = get_prediction_data_metrics(df = sample_test %>% filter(Week > 1) %>% drop_na(), model = best_model_rf, tree = NULL, type = 'rf', response = r)
  #     
  #     all_results[[names(list_training_data)[i]]] = test_results_rf
  #   }
  #   
  # }


#step 0 (prep, cleanup): clean up the fields. like for opp, we have things like Opp_Last_Season_Rank_Defense_Total_Yards_Allowed_Season_median_Allowed which make no sense.
#fix the team and opp fields, and then fix.

#step 1:
#Derive a CV column: sd/mean.

#step 2:  for categorical fields, determine which ones have high cardinality and do IV binning for those, otherwise leave them as is. For low cardinality, run IV on them
#to see if they are predictive at all, and then leave them in as raw values. Continue to use the This-or-That analysis.

#step 3: IV for numeric fields: for all numeric fields, run information value analysis, remove the ones with low IV, and keep the rest as raw scores.
#for fields that are mostly 1s and 0s, turn them into a binary flag.
#for clumpy numeric fields like draft round, keep the binning.

#step 4: Expand the This-or-That concept to work for stat types. For each stat that survived step 3, ctegorize each stat into a timeline, scope, and measurement type.
#For each timeline:
#This season: Avg, Median, Min, Max, SD.
#Last 3: Avg, Median, Min, Max, SD.
#Last season: Avg, Median, Min, Max, SD.
#2 seasons ago: Avg, Median, Min, Max, SD.
#Then scope: Player stats, player rank, team stats, team rank, opp stats, opp rank.
#Measurement type:
#Opportunity targets, rushing attempts, snap, etc.
#Production yards, touchdowns, receptions, 1st downs, etc.
#Efficiency: yards per target, yards per attempt

#step 5: do an IV tournament for each grouping of timeline/scope/measurement type. Determine how many winners should be in each one depending on how much variety there is.
#for example, you might need 2 for production when it comes to yardage/completions and touchdowns, 2 different things. assess when seeing the data.
#Then for each scope/timeline combo, look at the winners for measurement type, adn compare correlations. If the correlations between the production metrics and opportunity
#metrics, for example, are over 0.90, consider cutting. Prioritize the hierarchy opportunity > efficiency > production if the IVs are close, but if production, for example,
#has a much higher IV, then prioritize that. use the hierarchy if IVs are close.

#step 6: try random forest and xgboost. for each model, choose between xgboost and gbm, and then include a rf prediction and a gradient prediction (gbm or xgboost).
#Run these 2 values into a logistic regression model to predict actual probability (to give a final probability to help with overestimating.)
#have 3 datasets: train1, train2, and test. train1 goes into gbm/rf, train2 goes into logistic, and test is the final test.
#use rBayesianOptimization for faster tuning.

#after:
#pull in the betting lines scraped data.
