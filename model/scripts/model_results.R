library(dplyr)
source('model/scripts/nfl_model_functions.R')
source('data_collection/scripts/global.R')

passing_unimportant_vars = c()
rushing_unimportant_vars = c()
receiving_unimportant_vars = c()
touchdown_unimportant_vars = c()

assess_model_results = function(test, type, model_category, responses)
{
  all_confidence = data.frame()
  all_tunings = rbind()
  optimals = rbind()
  abnormal_top_20 = rbind()
  for (response in responses)
  {
    print(response)
    test_df = test[[response]]
    model = readRDS(paste0('model/tunings_and_models/',model_category,'/',type,'/model_', tolower(response),'.rds'))
    res = get_prediction_data_metrics(df = test_df, model = model, response = response)
    probability_buckets = res[[1]]
    precision = res[[2]]
    recall = res[[3]]
    eval_buckets_res = evaluate_buckets(probability_buckets)
    avg_buckets_error = eval_buckets_res[[1]]
    median_buckets_error = eval_buckets_res[[2]]
    min_buckets_error = eval_buckets_res[[3]]
    max_buckets_error = eval_buckets_res[[4]]
    confidence = eval_buckets_res[[5]]
    confidence = data.frame(c(confidence, avg_buckets_error,
                              mean(confidence == 'High'),
                              mean(confidence %in% c('High','Medium')),
                              mean(confidence != 'No Data')))
    colnames(confidence) = response
    rownames(confidence) = c(rownames(probability_buckets), 'Avg', 'Pct_High', 'Pct_Not_Low', 'Pct_No_Data')
    all_confidence = rbind(all_confidence, t(confidence))
    
    tunings = readRDS(paste0('model/tunings_and_models/', model_category, '/',type,'/all_tunings_', response, '.rds'))
    all_tunings = bind_rows(all_tunings,tunings)
    
    params = c(response, gbm.perf(model, method = "cv", plot.it = FALSE), model$params$interaction_depth, model$params$min_num_obs_in_node, model$params$shrinkage, model$params$bag_fraction, avg_buckets_error, median_buckets_error, min_buckets_error, max_buckets_error, precision, recall, mean(test_df[[response]], na.rm = TRUE))
    optimals = rbind(optimals, params)
    
    
    top20 = head(summary(model, plot.it = FALSE), 20)
    if(top20$rel_inf[1] > 50)
    {
      abnormal_top_20 = rbind(abnormal_top_20, top20)
    }
    
    if(type == 'full')
    {
      writeLines(paste(summary(model, plot.it = FALSE) %>% filter(rel_inf == 0) %>% select(var) %>% pull(), collapse = ","),
                 paste0('model/unimportant_vars/',response,"_unimportant_vars_string.txt"))
    }
    colnames(optimals) = c('response', 'tree', 'interdepth', 'min obs', 'shrink', 'bag', 'avg_buckets_err', 'med_buckets_err', 'min_buckets_err','max_buckets_err', 'precision', 'recall', 'prevalence')
    
    # print(kable(optimals, caption = paste('Passing: Optimal tunings by response on test dataset:', p), row.names = FALSE))
    # print(kable(t(all_confidence), caption = paste('Passing: Model Probability Confidence by Prediction Bins:', p)))
    # overall_avg_error = c(overall_avg_error, mean(as.numeric(t(all_confidence)["Avg",]), na.rm = TRUE))
    # overall_pct_high = c(overall_pct_high,mean(all_confidence[1:8,] == 'High'))
    # overall_pct_not_low = c(overall_pct_not_low, mean(all_confidence[1:8,]  == 'High' | all_confidence[1:8,] == 'Medium'))
    # overall_pct_non_missing = c(overall_pct_non_missing, mean(all_confidence[1:8,] != 'No Data'))
  }
  
  return(list(all_confidence, optimals, all_tunings, abnormal_top_20))
}





