library(knitr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(scorecard)
library(stringr)
library(furrr)
source('model/scripts/nfl_model_functions.R')
source('data_collection/scripts/global.R')


model_manual_remove = c('min_year', 'max_year')




# Set up parallel plan (you can change multisession to multicore depending on OS)
plan(multisession, workers = 4)  # Use 4 cores; adjust to what your system can handle


# Define a function that runs the model for one response
run_one_model = function(response, model_name, column_categories, data_file_path, manual_remove, response_col_to_remove, path, t_per_s, i_range, s_range, n_range, b_range, df = NULL) {
  
  if(is.null(df))
  {
    df = readRDS(data_file_path)[[response]] %>%
      select(-any_of(c(setdiff(column_categories$basic_cols, 'GS'), model_manual_remove,  response_col_to_remove)))
    if(path== 'reduced')
    {
      string = readLines(paste0('unimportant_vars/', response, "_unimportant_vars_string.txt"))
      fields_to_remove = tryCatch({
        strsplit(gsub("'", "", string), ",")[[1]] %>% trimws()
      }, error = function(e) {
        fields_to_remove = NULL
      })
    } else if (path == 'super_reduced') {
      fields_to_remove = colnames(df)[str_detect(colnames(df), '[M|m]edian_|[M|m]in_|[M|m]ax_|cumulative|sum')]
    }
    df = df %>% select(-any_of(fields_to_remove))
  }
  
  run_gbm(
    df = df,
    response = response,
    model_name = model_name,
    path = path,
    t_per_s = t_per_s,
    i_range = i_range,
    s_range = s_range,
    n_range = n_range,
    b_range = b_range
  )
  
  gc(verbose=FALSE)
}


tune_passing_models = function(t_per_s, i_range, s_range, n_range, b_range, path, override_response_var = NULL)
{
  
  t1 = Sys.time()
  if(!is.null(override_response_var))
  {
    responses = override_response_var
  } else {
    responses = passing_response
  }
  # Run models in parallel
  future_map(.x = responses, 
             .f = run_one_model,
             model_name = 'passing',
             column_categories = passing_data_column_categories,
             data_file_path = 'model/data/model_ready_passing_train_df.rds',
             manual_remove = manual_remove,
             response_col_to_remove = 'Passing_Yds',
             path = path,
             t_per_s= t_per_s,
             i_range = i_range,
             s_range = s_range,
             n_range = n_range,
             b_range = b_range)
  
  Sys.time() - t1
}

tune_rushing_models = function(t_per_s, i_range, s_range, n_range, b_range, path, override_response_var = NULL) {
  t1 = Sys.time()
  if(!is.null(override_response_var))
  {
    responses = override_response_var
  } else {
    responses = rushing_response
  }
  future_map(.x = responses, 
             .f = run_one_model,
             model_name = 'rushing',
             column_categories = rushing_data_column_categories,
             data_file_path = 'model/data/model_ready_rushing_train_df.rds',
             manual_remove = manual_remove,
             response_col_to_remove = 'Rushing_Yds',
             t_per_s= t_per_s,
             i_range = i_range,
             s_range = s_range,
             n_range = n_range,
             b_range = b_range,
             path = path)
  
  Sys.time() - t1
}

tune_receiving_models = function(t_per_s, i_range, s_range, n_range, b_range, path, override_response_var = NULL) {
  t1 = Sys.time()
  if(!is.null(override_response_var))
  {
    responses = override_response_var
  } else {
    responses = receiving_response
  }
  future_map(.x = responses, 
             .f = run_one_model,
             model_name = 'receiving',
             column_categories = receiving_data_column_categories,
             data_file_path = 'model/data/model_ready_receiving_train_df.rds',
             manual_remove = manual_remove,
             response_col_to_remove = 'Receiving_Yds',
             t_per_s= t_per_s,
             i_range = i_range,
             s_range = s_range,
             n_range = n_range,
             b_range = b_range,
             path = path)
  
  Sys.time() - t1
}

tune_touchdown_model = function(t_per_s, i_range, s_range, n_range, b_range, path) {  
  
  t1 = Sys.time()
  response = touchdown_response
  run_one_model(response = response,
                model_name = 'touchdown',
                column_categories = touchdown_data_column_categories,
                data_file_path = 'model/data/model_ready_touchdown_train_df.rds',
                manual_remove = manual_remove,
                response_col_to_remove = 'Total_Touchdowns',
                t_per_s= t_per_s,
                i_range = i_range,
                s_range = s_range,
                n_range = n_range,
                b_range = b_range,
                path = path)
  
  Sys.time() - t1
  
}


tune_old_receiving_model = function(t_per_s, i_range, s_range, n_range, b_ranges, path) {  
  
  old_model_data = readRDS('~/model_data_wk18_2024.rds')
  receiving_numbers = c(25,seq(40,120,10))
  for(n in receiving_numbers)
  {
    print(n)
    old_model_data = old_model_data %>% mutate(!!paste0('Receiving_Yds_', n) := ifelse(Receiving_Yards > n, 1, 0))
  }
  train_indices = sample(1:nrow(old_model_data), 0.75*nrow(old_model_data))
  old_model_data_train = old_model_data[train_indices,]
  old_model_data_test = old_model_data[-train_indices,]
  custom_list = list()
  custom_list$basic_cols = c('name', 'team', 'opp', 'game_date', 'week_num', 'Season')
  t1 = Sys.time()
  
  future_map(.x = receiving_response, 
             .f = run_one_model,
             df = old_model_data_train %>% select(-any_of(c(custom_list$basic_cols, 'Receiving_Yards'))) %>% data.frame(),
             model_name = 'old_receiving',
             column_categories = custom_list,
             data_file_path = '',
             response_col_to_remove = 'Receiving_Yards',
             t_per_s = c(600, 150),
             i_range = c(2,5),
             s_range = c(0.01, 0.05),
             n_range = 10,
             b_range = c(0.5),
             path = path)
  
  Sys.time() - t1
  
  
  saveRDS(old_model_data_train, 'model/data/model_ready_old_receiving_data_train.rds')
  saveRDS(old_model_data_test, 'model/data/model_ready_old_receiving_data_test.rds')
}


