library(dplyr)
library(ggplot2)
library(tidyr)
library(scorecard)
library(stringr)
source('model/scripts/nfl_model_functions.R')
source('data_collection/scripts/global.R') #to get the response variables and numbers like passing_numbers,passing_response, etc


#manually remove from information value analysis:
manual_remove = c('Team','Opp')
passing_data_this_or_that = data.frame(cbind(option1 = c('draft_round', 'Grass_Type', 'Roof', 'age'),
                                             option2 = c('draft_pick', 'Familiar_Grass_Type', 'Familiar_Roof_Type', 'Years_Since_Drafted')))
rushing_data_this_or_that = data.frame(cbind(option1 = c('draft_round', 'Grass_Type', 'Roof', 'age'),
                                             option2 = c('draft_pick', 'Familiar_Grass_Type', 'Familiar_Roof_Type', 'Years_Since_Drafted')))
receiving_data_this_or_that = data.frame(cbind(option1 = c('draft_round', 'Grass_Type', 'Roof', 'age'),
                                               option2 = c('draft_pick', 'Familiar_Grass_Type', 'Familiar_Roof_Type', 'Years_Since_Drafted')))
touchdown_data_this_or_that = data.frame(cbind(option1 = c('draft_round', 'Grass_Type', 'Roof', 'age'),
                                               option2 = c('draft_pick', 'Familiar_Grass_Type', 'Familiar_Roof_Type', 'Years_Since_Drafted')))





model_prep = function(passing_data, rushing_data, receiving_data, touchdown_data,
                      passing_data_column_categories, rushing_data_column_categories, receiving_data_column_categories, touchdown_data_column_categories,
                      train_test_split = TRUE, train_mode = TRUE, bin_iv_limit = 0.01) #train or prediction
{

  if(train_test_split == TRUE)
  {
    set.seed(123)
    passing_indices_positive_shuffled = sample(which(passing_data$Passing_Yds >= 300), 0.8*sum(passing_data$Passing_Yds >= 300))
    passing_indices_negative_shuffled = sample(which(passing_data$Passing_Yds < 300), 0.8*sum(passing_data$Passing_Yds < 300))
    passing_train_indices = c(passing_indices_positive_shuffled, passing_indices_negative_shuffled)
    passing_data_train = passing_data[passing_train_indices,]
    passing_data_test = passing_data[-passing_train_indices,]
    
    set.seed(123)
    rushing_indices_positive_shuffled = sample(which(rushing_data$Rushing_Yds >= 120), 0.8*sum(rushing_data$Rushing_Yds >= 120))
    rushing_indices_negative_shuffled = sample(which(rushing_data$Rushing_Yds < 120), 0.8*sum(rushing_data$Rushing_Yds < 120))
    rushing_train_indices = c(rushing_indices_positive_shuffled, rushing_indices_negative_shuffled)
    rushing_data_train = rushing_data[rushing_train_indices,]
    rushing_data_test = rushing_data[-rushing_train_indices,]
    
    set.seed(123)
    receiving_indices_positive_shuffled = sample(which(receiving_data$Receiving_Yds >= 120), 0.8*sum(receiving_data$Receiving_Yds >= 120))
    receiving_indices_negative_shuffled = sample(which(receiving_data$Receiving_Yds < 120), 0.8*sum(receiving_data$Receiving_Yds < 120))
    receiving_train_indices = c(receiving_indices_positive_shuffled, receiving_indices_negative_shuffled)
    receiving_data_train = receiving_data[receiving_train_indices,]
    receiving_data_test = receiving_data[-receiving_train_indices,]
    
    set.seed(123)
    touchdown_indices_positive_shuffled = sample(which(touchdown_data$Total_Touchdowns > 0), 0.8*sum(touchdown_data$Total_Touchdowns > 0))
    touchdown_indices_negative_shuffled = sample(which(touchdown_data$Total_Touchdowns == 0), 0.8*sum(touchdown_data$Total_Touchdowns == 0))
    touchdown_train_indices = c(touchdown_indices_positive_shuffled, touchdown_indices_negative_shuffled)
    touchdown_data_train = touchdown_data[touchdown_train_indices,]
    touchdown_data_test = touchdown_data[-touchdown_train_indices,]
  } else {
    passing_data_train = passing_data
    passing_data_test = NULL
    rushing_data_train = rushing_data
    rushing_data_test = NULL
    receiving_data_train = receiving_data
    receiving_data_test = NULL
    touchdown_data_train = touchdown_data
    touchdown_data_test = NULL
  }
   
  
  #Derive response variables in model-ready form:
    
  if(nrow(passing_data) > 0 & train_mode == TRUE)
  {
    for(n in passing_numbers)
    {
      passing_data_train = passing_data_train %>% mutate(!!paste0('Passing_Yds_', n) := ifelse(Passing_Yds > n, 1, 0))
      passing_data_test = passing_data_test %>% mutate(!!paste0('Passing_Yds_', n) := ifelse(Passing_Yds > n, 1, 0))
    }
  }

  if(nrow(rushing_data) > 0 & train_mode == TRUE)
  {
    for(n in rushing_numbers)
    {
      rushing_data_train = rushing_data_train %>% mutate(!!paste0('Rushing_Yds_', n) := ifelse(Rushing_Yds > n, 1, 0))
      rushing_data_test = rushing_data_test %>% mutate(!!paste0('Rushing_Yds_', n) := ifelse(Rushing_Yds > n, 1, 0))
    }
  }
  
  if(nrow(receiving_data) > 0 & train_mode == TRUE)
  { 
    for(n in receiving_numbers)
    {
      receiving_data_train = receiving_data_train %>% mutate(!!paste0('Receiving_Yds_', n) := ifelse(Receiving_Yds > n, 1, 0))
      receiving_data_test = receiving_data_test %>% mutate(!!paste0('Receiving_Yds_', n) := ifelse(Receiving_Yds > n, 1, 0))
    }
  }
    
  if(nrow(touchdown_data) > 0 & train_mode == TRUE)
  {
    touchdown_data_train = touchdown_data_train %>% mutate(!!touchdown_response := ifelse(Total_Touchdowns > 0, 1, 0)) %>% select(-Total_Touchdowns)
    touchdown_data_test = touchdown_data_test %>% mutate(!!touchdown_response := ifelse(Total_Touchdowns > 0, 1, 0)) %>% select(-Total_Touchdowns)
  }
  
    #define which fields will be used for information value:
    
    if(train_mode == TRUE)
    {
      passing_information_value_vars = setdiff(unlist(passing_data_column_categories[names(passing_data_column_categories)[!str_detect(tolower(names(passing_data_column_categories)), 'season')]]),
                                               c('player_id', 'Gtm','Season', 'Date', 'min_year', 'max_year', 'Time', manual_remove))
      passing_information_value_vars = passing_information_value_vars[!(sapply(passing_information_value_vars, function(x) length(unique(na.omit(passing_data_train[,x]))) < 2))]
      
      rushing_information_value_vars = setdiff(unlist(rushing_data_column_categories[names(rushing_data_column_categories)[!str_detect(tolower(names(rushing_data_column_categories)), 'season|rank')]]),
                                               c('player_id', 'Gtm','Season', 'Date', 'min_year', 'max_year', 'Time', manual_remove))
      rushing_information_value_vars = rushing_information_value_vars[!(sapply(rushing_information_value_vars, function(x) length(unique(na.omit(rushing_data_train[,x]))) < 2))]
      
      receiving_information_value_vars = setdiff(unlist(receiving_data_column_categories[names(receiving_data_column_categories)[!str_detect(names(receiving_data_column_categories), 'season|rank')]]),
                                                 c('player_id', 'Gtm','Season', 'Date', 'min_year', 'max_year', 'Time', manual_remove))
      receiving_information_value_vars = receiving_information_value_vars[!(sapply(receiving_information_value_vars, function(x) length(unique(na.omit(receiving_data_train[,x]))) < 2))]
      
      touchdown_information_value_vars = setdiff(unlist(touchdown_data_column_categories[names(touchdown_data_column_categories)[!str_detect(names(touchdown_data_column_categories), 'season|rank')]]),
                                                 c('player_id', 'Gtm','Season', 'Date', 'min_year', 'max_year', 'Time', manual_remove))
      touchdown_information_value_vars = touchdown_information_value_vars[!(sapply(touchdown_information_value_vars, function(x) length(unique(na.omit(touchdown_data_train[,x]))) < 2))]
    
    
    if(nrow(passing_data) > 0)
    {
      ivs_and_bins_passing = create_iv_tables(df = passing_data_train, response_var = passing_response, var_list = passing_information_value_vars, column_categories = passing_data_column_categories)
      ivs_passing = ivs_and_bins_passing[[1]]
      bins_passing = ivs_and_bins_passing[[2]]
      
      all_bins_passing = get_specific_iv_table(df = passing_data_train,
                                               iv_table = ivs_passing,
                                               bin_table = bins_passing,
                                               predictive_power_choice = 'Significant',
                                               column_categories = passing_data_column_categories,
                                               bin_iv_limit = bin_iv_limit) %>% arrange(Variable, Response) %>% select(Variable, Bin, Response, display, Bin_IV, Bin_Count, specific_count, Pos_Prob, overall_mean_response)
      
      columns_0_1 = setdiff(colnames(passing_data_train)[which(sapply(colnames(passing_data_train), function(x) (!str_detect(tolower(x), 'min|max|sum|cumulative|avg|mean|median|sd')) & all(na.omit(passing_data_train[,x]) %in% c(0,1))))], passing_response)
      
    } else {
      ivs_passing = NULL
      all_bins_passing = NULL
    }
    
    if(nrow(rushing_data) > 0)
    {
      ivs_and_bins_rushing = create_iv_tables(df = rushing_data_train, response_var = rushing_response, var_list = rushing_information_value_vars, column_categories = rushing_data_column_categories)
      ivs_rushing = ivs_and_bins_rushing[[1]]
      bins_rushing = ivs_and_bins_rushing[[2]]
      
      all_bins_rushing = get_specific_iv_table(df = rushing_data_train,
                                               iv_table = ivs_rushing,
                                               bin_table = bins_rushing,
                                               predictive_power_choice = 'Significant',
                                               column_categories = rushing_data_column_categories,
                                               bin_iv_limit = bin_iv_limit) %>% arrange(Variable, Response) %>% select(Variable, Bin, Response, display, Bin_IV, Bin_Count, specific_count, Pos_Prob, overall_mean_response)
      
    
      columns_0_1 = setdiff(colnames(rushing_data_train)[which(sapply(colnames(rushing_data_train), function(x) (!str_detect(tolower(x), 'min|max|cumulative|avg|mean|median|sd')) & all(na.omit(rushing_data_train[,x]) %in% c(0,1))))], rushing_response)
    } else {
      ivs_rushing = NULL
      all_bins_rushing = NULL
    }
    if(nrow(receiving_data) > 0)
    {
      ivs_and_bins_receiving = create_iv_tables(df = receiving_data_train, response_var = receiving_response, var_list = receiving_information_value_vars, column_categories = receiving_data_column_categories)
      ivs_receiving = ivs_and_bins_receiving[[1]]
      bins_receiving = ivs_and_bins_receiving[[2]]
      
      all_bins_receiving = get_specific_iv_table(df = receiving_data_train,
                                                 iv_table = ivs_receiving,
                                                 bin_table = bins_receiving,
                                                 predictive_power_choice = 'Significant',
                                                 column_categories = receiving_data_column_categories,
                                                 bin_iv_limit = bin_iv_limit) %>% arrange(Variable, Response) %>% select(Variable, Bin, Response, display, Bin_IV, Bin_Count, specific_count, Pos_Prob, overall_mean_response)
    
      columns_0_1 = setdiff(colnames(receiving_data_train)[which(sapply(colnames(receiving_data_train), function(x) (!str_detect(tolower(x), 'min|max|cumulative|avg|mean|median|sd')) & all(na.omit(receiving_data_train[,x]) %in% c(0,1))))], receiving_response)
      
    } else {
      ivs_receiving = NULL
      all_bins_receiving = NULL
    }
    if(nrow(touchdown_data) > 0)
    {
      ivs_and_bins_touchdown = create_iv_tables(df = touchdown_data_train, response_var = touchdown_response, var_list = touchdown_information_value_vars, column_categories = touchdown_data_column_categories)
      ivs_touchdown = ivs_and_bins_touchdown[[1]]
      bins_touchdown = ivs_and_bins_touchdown[[2]]
    
      
      all_bins_touchdown = get_specific_iv_table(df = touchdown_data_train,
                                                 iv_table = ivs_touchdown,
                                                 bin_table = bins_touchdown,
                                                 predictive_power_choice = 'Significant',
                                                 column_categories = touchdown_data_column_categories,
                                                 bin_iv_limit = bin_iv_limit) %>% arrange(Variable, Response) %>% select(Variable, Bin, Response, display, Bin_IV, Bin_Count, specific_count, Pos_Prob, overall_mean_response)
    columns_0_1 = setdiff(colnames(touchdown_data_train)[which(sapply(colnames(touchdown_data_train), function(x) (!str_detect(tolower(x), 'min|max|cumulative|avg|mean|median|sd')) & all(na.omit(touchdown_data_train[,x]) %in% c(0,1))))], touchdown_response)
    } else {
      ivs_touchdown = NULL
      all_bins_touchdown = NULL
    }
    
    
    saveRDS(ivs_passing, 'model/iv_bins/ivs_passing.rds')
    saveRDS(ivs_rushing, 'model/iv_bins/ivs_rushing.rds')
    saveRDS(ivs_receiving, 'model/iv_bins/ivs_receiving.rds')
    saveRDS(ivs_touchdown, 'model/iv_bins/ivs_touchdown.rds')
    saveRDS(all_bins_passing, 'model/iv_bins/all_bins_passing.rds')
    saveRDS(all_bins_rushing, 'model/iv_bins/all_bins_rushing.rds')
    saveRDS(all_bins_receiving, 'model/iv_bins/all_bins_receiving.rds')
    saveRDS(all_bins_touchdown, 'model/iv_bins/all_bins_touchdown.rds')
  } else{ #prediction mode
    ivs_passing = readRDS('model/iv_bins/ivs_passing.rds')
    ivs_rushing = readRDS('model/iv_bins/ivs_rushing.rds')
    ivs_receiving  = readRDS('model/iv_bins/ivs_receiving.rds')
    ivs_touchdown = readRDS('model/iv_bins/ivs_touchdown.rds')
    all_bins_passing = readRDS('model/iv_bins/all_bins_passing.rds')
    all_bins_rushing = readRDS('model/iv_bins/all_bins_rushing.rds')
    all_bins_receiving = readRDS('model/iv_bins/all_bins_receiving.rds')
    all_bins_touchdown = readRDS('model/iv_bins/all_bins_touchdown.rds')
  }
  
  #THIS OR THAT
  
  #even if neither scored high in IV analysis, still take the highest one and remove the other. 
  if(nrow(passing_data) > 0)
  {
    passing_this_or_that_table = this_or_that_results(ivs = ivs_passing, this_or_that = passing_data_this_or_that, responses = passing_response)
    new_passing_data = iv_to_dummy(df = passing_data_train, bins = all_bins_passing, response_vars = passing_response, this_or_that_table = passing_this_or_that_table)
  } else {
    passing_this_or_that_table = NULL
    new_passing_data = NULL
  }
  if(nrow(rushing_data) > 0)
  {
    rushing_this_or_that_table = this_or_that_results(ivs = ivs_rushing, this_or_that = rushing_data_this_or_that, responses = rushing_response)
    new_rushing_data = iv_to_dummy(df = rushing_data_train, bins = all_bins_rushing, response_vars = rushing_response, this_or_that_table = rushing_this_or_that_table)
  } else {
    rushing_this_or_that_table = NULL
    new_rushing_data = NULL
  }
  if(nrow(receiving_data) > 0)
  {
    receiving_this_or_that_table = this_or_that_results(ivs = ivs_receiving, this_or_that = receiving_data_this_or_that, responses = receiving_response)
    new_receiving_data = iv_to_dummy(df = receiving_data_train, bins = all_bins_receiving, response_vars = receiving_response, this_or_that_table = receiving_this_or_that_table)
  } else {
    receiving_this_or_that_table = NULL
    new_receiving_data = NULL
  }
  if(nrow(touchdown_data) > 0)
  {
    touchdown_this_or_that_table = this_or_that_results(ivs = ivs_touchdown, this_or_that = touchdown_data_this_or_that, responses = touchdown_response)
    new_touchdown_data = iv_to_dummy(df = touchdown_data_train, bins = all_bins_touchdown, response_vars = touchdown_response, this_or_that_table = touchdown_this_or_that_table)
  } else {
    new_touchdown_data = NULL
  }
  if(train_test_split == TRUE)
  {
    if(nrow(passing_data) > 0)
    {
      new_passing_data_test = iv_to_dummy(df = passing_data_test, bins = all_bins_passing, response_vars = passing_response, this_or_that_table = passing_this_or_that_table)
    } else {
      new_passing_data_test = NULL
    }
    if(nrow(rushing_data) > 0)
    {
      new_rushing_data_test = iv_to_dummy(df = rushing_data_test, bins = all_bins_rushing, response_vars = rushing_response, this_or_that_table = rushing_this_or_that_table)
    } else {
      new_rushing_data_test = NULL
    }
    if(nrow(receiving_data) > 0)
    {
      new_receiving_data_test = iv_to_dummy(df = receiving_data_test, bins = all_bins_receiving, response_vars = receiving_response, this_or_that_table = receiving_this_or_that_table)
    } else {
      new_receiving_data_test = NULL
    }
    new_touchdown_data_test = iv_to_dummy(df = touchdown_data_test, bins = all_bins_touchdown, response_vars = touchdown_response, this_or_that_table = touchdown_this_or_that_table)
    
    return(list(new_passing_data, new_rushing_data, new_receiving_data, new_touchdown_data,
                new_passing_data_test, new_rushing_data_test, new_receiving_data_test, new_touchdown_data_test))
  } else {
    return(list(new_passing_data, new_rushing_data, new_receiving_data, new_touchdown_data))
  }
  
 
}













