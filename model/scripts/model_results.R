library(dplyr)
setwd("~/nfl")
# source('data_collection/scripts/global.R')
#source('model/scripts/assess_2025_returns.R')
source('model/scripts/nfl_model_functions.R')

passing_unimportant_vars = c()
rushing_unimportant_vars = c()
receiving_unimportant_vars = c()
touchdown_unimportant_vars = c()

assess_model_results = function(test, type, model_category, responses)
{
  all_confidence = data.frame()
  all_tunings = rbind()
  optimals = rbind()
  abnormal_top_20 = list()
  for (response in responses)
  {
    print(response)
    test_df = test[[response]]
    model = readRDS(paste0('model/tunings_and_models/',model_category,'/',type,'/model_', tolower(response),'.rds'))
    res = get_prediction_data_metrics(df = test_df, model = model, response = response)
    probability_buckets = res[[1]]
    precision = res[[2]]
    recall = res[[3]]
    # eval_buckets_res = evaluate_buckets(probability_buckets)
    # avg_buckets_error = eval_buckets_res[[1]]
    # median_buckets_error = eval_buckets_res[[2]]
    # min_buckets_error = eval_buckets_res[[3]]
    # max_buckets_error = eval_buckets_res[[4]]
    # confidence = eval_buckets_res[[5]]
    pct_high = mean(probability_buckets$Assessment == 'Inside Target Range')
    pct_high_medium = mean(probability_buckets$Assessment %in% c('Inside Target Range','Near Target Range'))
    # confidence_summary = data.frame(rbind(
    #                           cbind('Pct_High', pct_high),
    #                           cbind('Pct_Not_Low', pct_high_medium),
    #                           cbind('Pct_No_Data', mean(probability_buckets != 'Insufficient Data'))))
    # confidence_summary$Response = response
    # colnames(confidence_summary) = c('Metric', 'Value', 'Response')
    all_confidence = rbind(all_confidence, probability_buckets %>% mutate(Response = response))
    # all_confidence_summary = rbind(all_confidence_summary, confidence_summary)
    
    tunings = readRDS(paste0('model/tunings_and_models/', model_category, '/',type,'/all_tunings_', response, '.rds'))
    all_tunings = bind_rows(all_tunings,tunings)
    
    params = data.frame(response, trees = gbm.perf(model, method = "cv", plot.it = FALSE), interdepth = model$params$interaction_depth, minobs = model$params$min_num_obs_in_node, shrink = model$params$shrinkage, bag = model$params$bag_fraction, pct_high, pct_high_medium, precision, recall, prevalence = mean(test_df[[response]], na.rm = TRUE))
    optimals = rbind(optimals, params)
    
    
    top20 = head(summary(model, plot.it = FALSE), 20)
    if(top20$rel_inf[1] > 50)
    {
      abnormal_top_20[[response]] = top20
    }
    
    # if(type == 'full')
    # {
    #   writeLines(paste(summary(model, plot.it = FALSE) %>% filter(rel_inf == 0) %>% select(var) %>% pull(), collapse = ","),
    #              paste0('model/unimportant_vars/',response,"_unimportant_vars_string.txt"))
    # }
 
  }
  
  return(list(all_confidence, optimals, all_tunings, abnormal_top_20))
}


#expected value assessment:




