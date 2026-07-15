library(dplyr)
library(ggplot2)
library(tidyr)
library(scorecard)
library(stringr)
library(arrow)
library(furrr)
source('model/data_prep_functions.R')

passing_numbers = c(150, 180, 210, 240, 270, 300, 330, 360)
rushing_numbers = c(25, 40, 60, 80, 100, 120, 140)
receiving_numbers = c(25, 40, 60, 80, 100, 120, 140)
rushing_receiving_numbers = c(40, 70, 100, 130)
reception_numbers = c(4,6,8,10)
spread_numbers = c(-2.5, 2.5, -3.5, 3.5, -6.5, 6.5, -7.5, 7.5)

options(future.globals.maxSize = 5 * 1024^3) 
plan(multisession, workers = availableCores() - 1)

this_or_that = data.frame(cbind(option1 = c('draft_round', 'Grass_Type', 'Roof', 'age'),
                                option2 = c('draft_pick', 'Familiar_Grass_Type', 'Familiar_Roof_Type', 'Years_Since_Drafted')))


prep_lookup_table = data.frame(
  response_var = c('passing_yards', 'rushing_yards', 'receiving_yards', 'anytime_td_scorer', 'rushing_receiving_yards', 'receptions', 'team_win', 'team_differential'),
  numbers = c(paste(passing_numbers, collapse = '|'),
              paste(rushing_numbers, collapse = '|'),
              paste(receiving_numbers, collapse = '|'),
              NA,
              paste(rushing_receiving_numbers, collapse = '|'),
              paste(reception_numbers, collapse = '|'),
              NA,
              paste(spread_numbers, collapse = '|')),
  current_season_column_category = c("passing_current_season_stats", "rushing_current_season_stats", "receiving_current_season_stats", "touchdown_current_season_stats",
                                     "rushing_current_season_stats|receiving_current_season_stats", 'receiving_current_season_stats', NA, NA),
  historical_season_column_category = c("passing_past_season_stats", "rushing_past_season_stats", "receiving_past_season_stats", "touchdown_past_season_stats",
                                        "rushing_past_season_stats|receiving_past_season_stats", 'receiving_past_season_stats', NA, NA)
)
  

data_list = list(passing_pre_prep, rushing_pre_prep, receiving_pre_prep, touchdown_pre_prep, rushing_receiving_pre_prep, reception_model_data, moneyline_pre_prep, spread_pre_prep)

num_iv_winners = 10
acceptable_predictive_power = 'Suspicious,High,Medium,Low'
t1 = Sys.time()

dir.create("model/ml_ready_data/train", showWarnings = FALSE, recursive = TRUE)
dir.create("model/ml_ready_data/test", showWarnings = FALSE, recursive = TRUE)
dir.create("model/ml_ready_data/final_test", showWarnings = FALSE, recursive = TRUE)
for (i in 1:length(data_list))
{
  response_var = prep_lookup_table$response_var[i]
  print(paste('Prepping data for:', response_var))
  prepped_data = model_prep(data_to_prep = data_list[[i]],
                            column_categories = column_categories, 
                            numbers = as.numeric(unlist(strsplit(prep_lookup_table$numbers[[i]], '\\|'))),
                            response_var = response_var,
                            current_season_column_category = prep_lookup_table$current_season_column_category[i],
                            historical_season_column_category = prep_lookup_table$historical_season_column_category[i],
                            acceptable_predictive_power = acceptable_predictive_power,
                            num_winners = num_iv_winners
  )
  for (j in 1:length(prepped_data[[1]]))
  {
    write_parquet(prepped_data[[1]][[j]], paste0('ml_ready_data/train/', names(prepped_data[[1]])[j], '.parquet'))
    write_parquet(prepped_data[[2]][[j]], paste0('ml_ready_data/test/', names(prepped_data[[1]])[j], '.parquet'))
    write_parquet(prepped_data[[3]][[j]], paste0('ml_ready_data/final_test/', names(prepped_data[[1]])[j], '.parquet'))
  }
}
print(paste('Total time to prep:', difftime(Sys.time(), t1, units = 'hours'), 'hours'))


  #column_categories = prepped_data[[4]]
  
  # for (resp in 1:length(training))
  # {
  #   response_var = names(training)[resp]
  #     res = run_xgboost(train = training[[resp]],
  #                       test = test[[resp]],
  #                       response = response_var,
  #                       combos = tuning_combos)
  #     
  #     final_xgb_fit = res[1]
  #     best_params = res[2]
  #     best_results = res[3][[1]]
  #     preds_df = res[4][[1]]
  #     variable_importance = res[5][[1]]
  #     
  #     #run calibration metric
  #     cv_mn_log_loss = best_results$mean
  #     calibration_score_total = calibration_score(model_probability = preds_df$preds, bet_hit = preds_df$actuals)
  #     calibration_score_under_30 = calibration_score(model_probability = preds_df$preds[preds_df$preds < 0.3], bet_hit = preds_df$actuals[preds_df$preds < 0.3])
  #     calibration_score_30_to_60 = calibration_score(model_probability = preds_df$preds[preds_df$preds >= 0.3 & preds_df$preds < 0.6], bet_hit = preds_df$actuals[preds_df$preds >= 0.3 & preds_df$preds < 0.6])
  #     calibration_score_above_60 = calibration_score(model_probability = preds_df$preds[preds_df$preds >= 0.6], bet_hit = preds_df$actuals[preds_df$preds >=  0.6])
  #     
  #     categories_table = preds_df %>% rename(Model_Probability = preds, BetHit = actuals) %>%
  #       calculate_probability_ranges() %>%
  #       assess_probability_ranges(return_categories = TRUE) 
  #     
  #     num_bad = categories_table %>%
  #       filter(Assessment == 'Bad') %>% pull(n) %>% sum()
  #     num_inside_range = categories_table %>%
  #       filter(Assessment == 'Inside Target Range') %>% pull(n) %>% sum()
  #     num_near_range =  categories_table %>%
  #       filter(Assessment == 'Near Target Range') %>% pull(n) %>% sum()
  #     
  #     
  #     categories_tables_list[[response_var]] = categories_table
  #     train_list[[response_var]] = training[[resp]]
  #     test_list[[response_var]] = test[[resp]]
  #     final_test_list[[response_var]] = final_test[[resp]]
  #     
  #     
  #     #save the categories and the models somewhere
  #     #add acceptable predicitve power as a loop
  #     
  #     assessments_df = rbind(assessments_df,
  #                            data.frame(
  #                              response_var = response_var,
  #                              model = model,
  #                              second_step = second_step,
  #                              num_iv_winners = num_iv_winners,
  #                              num_columns = ncol(training[[resp]]),
  #                              tuning_combos = tuning_combos,
  #                              acceptable_predictive_power = acceptable_predictive_power,
  #                              cv_mn_log_loss = cv_mn_log_loss,
  #                              calibration_score = calibration_score_total,
  #                              calibration_score_under_30 = calibration_score_under_30,
  #                              calibration_score_30_to_60 = calibration_score_30_to_60,
  #                              calibration_score_above_60 = calibration_score_above_60,
  #                              num_bad = num_bad,
  #                              num_inside_range = num_inside_range,
  #                              num_near_range = num_near_range,
  #                              top_20_vars_above_0_1 = paste(variable_importance[1:20,] %>% filter(Importance >= 0.01) %>% pull(Variable), collapse = ','),
  #                              pct_bad = num_bad/(num_bad+num_inside_range+num_near_range)
  #                            )
  #                       )
  
  # }
  # print(paste('Time for this variable:', difftime(Sys.time(), t2, units = 'hours'), 'hours'))



