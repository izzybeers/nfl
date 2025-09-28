library(stats)
setwd("~/nfl")
source('model/scripts/model_prep_script.R')
source('model/scripts/run_models.R')
source('model/scripts/model_results.R')
source('data_collection/scripts/global.R')

passing_pre_prep = readRDS('model/data/passing_preliminary_data.rds')
rushing_pre_prep = readRDS('model/data/rushing_preliminary_data.rds')
receiving_pre_prep = readRDS('model/data/receiving_preliminary_data.rds')
touchdown_pre_prep = readRDS('model/data/touchdown_preliminary_data.rds')

passing_data_column_categories = readRDS('model/data/passing_data_column_categories.rds')

rushing_data_column_categories = readRDS('model/data/rushing_data_column_categories.rds')

receiving_data_column_categories = readRDS('model/data/receiving_data_column_categories.rds')

touchdown_data_column_categories = readRDS('model/data/touchdown_data_column_categories.rds')


model_prep_results = model_prep(passing_pre_prep, rushing_pre_prep, receiving_pre_prep, touchdown_pre_prep,
                                passing_data_column_categories, rushing_data_column_categories,
                                receiving_data_column_categories, touchdown_data_column_categories,
                                train_test_split = TRUE, train_mode = TRUE, bin_iv_limit = 0.01)


new_passing_data = model_prep_results[[1]]
new_rushing_data = model_prep_results[[2]]
new_receiving_data = model_prep_results[[3]]
new_touchdown_data = model_prep_results[[4]]
new_passing_data_test = model_prep_results[[5]]
new_rushing_data_test = model_prep_results[[6]]
new_receiving_data_test = model_prep_results[[7]]
new_touchdown_data_test = model_prep_results[[8]]

saveRDS(new_passing_data, 'model/data/model_ready_passing_train_df.rds')
saveRDS(new_rushing_data, 'model/data/model_ready_rushing_train_df.rds')
saveRDS(new_receiving_data, 'model/data/model_ready_receiving_train_df.rds')
saveRDS(new_touchdown_data, 'model/data/model_ready_touchdown_train_df.rds')

saveRDS(new_passing_data_test, 'model/data/model_ready_passing_test_df.rds')
saveRDS(new_rushing_data_test, 'model/data/model_ready_rushing_test_df.rds')
saveRDS(new_receiving_data_test, 'model/data/model_ready_receiving_test_df.rds')
saveRDS(new_touchdown_data_test, 'model/data/model_ready_touchdown_test_df.rds')

#save memory:
rm(new_passing_data)
rm(new_rushing_data)
rm(new_receiving_data)
rm(new_touchdown_data)
rm(new_passing_data_test)
rm(new_rushing_data_test)
rm(new_receiving_data_test)
rm(new_touchdown_data_test)

# new_passing_data = readRDS('model/data/model_ready_passing_train_df.rds')
# new_rushing_data = readRDS('model/data/model_ready_rushing_train_df.rds')
# new_receiving_data = readRDS('model/data/model_ready_receiving_train_df.rds')
#new_touchdown_data = readRDS('model/data/model_ready_touchdown_train_df.rds')
# 
# new_passing_data_test = readRDS('model/data/model_ready_passing_test_df.rds')
# new_rushing_data_test = readRDS('model/data/model_ready_rushing_test_df.rds')
# new_receiving_data_test = readRDS('model/data/model_ready_receiving_test_df.rds')
#new_touchdown_data_test = readRDS('model/data/model_ready_touchdown_test_df.rds')


type = 'super_reduced' 

model_manual_remove = c('min_year', 'max_year')

tune_passing_models(
  t_per_s = c(750, 150),
  i_range = c(2,5,8),
  s_range = c(0.01,0.05),
  n_range = 10,
  b_range = c(0.3, 0.5, 0.7),
  path = type
)

tune_rushing_models(
  t_per_s = c(750, 150),
  i_range = c(2,5,8),
  s_range = c(0.01,0.05),
  n_range = 10,
  b_range = c(0.3, 0.5, 0.7),
  path = type
)

t1 = Sys.time()
tune_receiving_models(
  t_per_s = c(750, 150),
  i_range = c(2,5,8),
  s_range = c(0.01,0.05),
  n_range = 10,
  b_range = c(0.3, 0.5, 0.7),  
  path = type
)
Sys.time() - t1

t1 = Sys.time()
tune_touchdown_model(
  t_per_s = c(750, 150),
  i_range = c(2,5,8),
  s_range = c(0.01,0.05),
  n_range = 10,
  b_range = c(0.3, 0.5, 0.7),    
  path = type
)
Sys.time() - t1


model_names = c('passing', 'rushing', 'receiving', 'touchdown')
response_list = list(passing_response, rushing_response, receiving_response, touchdown_response)

for(i in 1:length(model_names))
{
  res = assess_model_results(test = readRDS(paste0('model/data/model_ready_',model_names[i],'_test_df.rds')), type = type, model_category = model_names[i],responses = response_list[[i]])
  confidence = t(res[[1]])
  optimals = res[[2]]
  all_tunings = res[[3]]
  abnormal_top20_vars = res[[4]]
  confidence[,names(abnormal_top20_vars)][1:11] = 'Low'
  saveRDS(confidence, paste0('model/tunings_and_models/', model_names[i], '/', type,'/confidence.rds'))
  gc()
}

list_of_top_vars = list()
for(i in 1:length(model_names))
{
  print(i)
  this_mod_list = list()
  for (r in response_list[[i]])
  {
    print(r)
    mod = readRDS(paste0('model/tunings_and_models/', model_names[i],'/',type,'/','model_',tolower(r),'.rds'))
    summary(mod)
    if(summary(mod)$rel_inf[1] < 70)
    {
      this_mod_list[[r]] =  paste(summary(mod)$var[1:20],summary(mod)$rel_inf[1:20])
    }
  }
  list_of_top_vars[[model_names[i]]] = this_mod_list
}

list_of_top_vars
