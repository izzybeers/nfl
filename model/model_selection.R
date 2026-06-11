library(stats)
source('model/new_model_prep.R')
source('model/scripts/run_models.R')
source('model/scripts/model_results.R')
source('data_collection/scripts/global.R')
source('data_collection/new_data_collection.R')

passing_pre_prep = passing_model_data
rushing_pre_prep = rushing_model_data
receiving_pre_prep = receiving_model_data
touchdown_pre_prep = touchdown_model_data
reception_pre_prep = reception_model_data
rushing_receiving_pre_prep = rushing_receiving_model_data
moneyline_pre_prep = moneyline_model_data
spread_pre_prep = spread_model_data

passing_numbers = c(150, 180, 210, 240, 270, 300, 330, 360)
rushing_numbers = c(25, 40, 60, 80, 100, 120, 140)
receiving_numbers = c(25, 40, 60, 80, 100, 120, 140)
rushing_receiving_numbers = c(40, 70, 100, 130)
reception_numbers = c(4,6,8,10)
spread_numbers = c(-2.5, 2.5, -3.5, 3.5, -6.5, 6.5, -7.5, 7.5)

# model_prep_results = model_prep(passing_pre_prep, rushing_pre_prep, receiving_pre_prep, touchdown_pre_prep,
#                                 passing_data_column_categories, rushing_data_column_categories,
#                                 receiving_data_column_categories, touchdown_data_column_categories,
#                                 train_test_split = TRUE, train_mode = TRUE, bin_iv_limit = 0.01)

passing_model_ready = model_prep(data_to_prep = passing_pre_prep, numbers = passing_numbers, response_var = 'passing_yards',
                                 current_season_column_category = "passing_current_season_stats",
                                 historical_season_column_category = "passing_past_season_stats")
rushing_model_ready = model_prep(data_to_prep = rushing_pre_prep, numbers = rushing_numbers, response_var = 'rushing_yards',
                                 current_season_column_category = "rushing_current_season_stats",
                                 historical_season_column_category = "rushing_past_season_stats")
receiving_model_ready = model_prep(data_to_prep = receiving_pre_prep, numbers = receiving_numbers, response_var = 'receiving_yards',
                                 current_season_column_category = "receiving_current_season_stats",
                                 historical_season_column_category = "receiving_past_season_stats")
touchdown_model_ready = model_prep(data_to_prep = touchdown_pre_prep, numbers = NA, response_var = 'anytime_td_scorer',
                                 current_season_column_category = "touchdown_current_season_stats",
                                 historical_season_column_category = "touchdown_past_season_stats")
rushing_receiving_model_ready = model_prep(data_to_prep = rushing_receiving_pre_prep, numbers = rushing_receiving_numbers, response_var = 'rushing_receiving_yards',
                                 current_season_column_category = c("rushing_current_season_stats","receiving_current_season_stats"),
                                 historical_season_column_category = c("rushing_past_season_stats", "receiving_past_season_stats"))
reception_model_ready = model_prep(data_to_prep = reception_pre_prep, numbers = reception_numbers, response_var = 'receptions',
                                   current_season_column_category = "receiving_current_season_stats",
                                   historical_season_column_category = "receiving_past_season_stats")
moneyline_model_ready = model_prep(data_to_prep = moneyline_pre_prep, numbers = NA, response_var = 'team_win',
                                   current_season_column_category = NA,
                                   historical_season_column_category = NA)
spread_model_ready = model_prep(data_to_prep = spread_pre_prep, numbers = spread_numbers, response_var = 'team_differential',
                                   current_season_column_category = NA,
                                   historical_season_column_category = NA)

#keep working on categorizing the production/efficiency/opportunity categories


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

new_passing_data = readRDS('model/data/model_ready_passing_train_df.rds')
new_rushing_data = readRDS('model/data/model_ready_rushing_train_df.rds')
new_receiving_data = readRDS('model/data/model_ready_receiving_train_df.rds')
new_touchdown_data = readRDS('model/data/model_ready_touchdown_train_df.rds')

new_passing_data_test = readRDS('model/data/model_ready_passing_test_df.rds')
new_rushing_data_test = readRDS('model/data/model_ready_rushing_test_df.rds')
new_receiving_data_test = readRDS('model/data/model_ready_receiving_test_df.rds')
new_touchdown_data_test = readRDS('model/data/model_ready_touchdown_test_df.rds')

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
  t_per_s = c(1000, 300, 150),
  i_range = c(1,3,5,7),
  s_range = c(0.01, 0.05, 0.1),
  n_range = 10,
  b_range = c(0.5, 0.7),   
  path = type,
  override_response_var = receiving_response[3:6]
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
response_cols = c('Passing_Yds', 'Rushing_Yds', 'Receiving_Yds', 'Anytime_TD_Scorer')
response_list = list(passing_response, rushing_response, receiving_response, touchdown_response)

for(i in 1:length(model_names))
{
  res = assess_model_results(test = readRDS(paste0('model/data/model_ready_',model_names[i],'_test_df.rds')), type = type, model_category = model_names[i],responses = response_list[[i]])
  confidence = res[[1]]
  optimals = res[[2]]
  all_tunings = res[[3]]
  abnormal_top20_vars = res[[4]]
  confidence$Assessment[which(confidence$Response %in% names(abnormal_top20_vars))] = 'Bad'
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




#to assess again 2025:
#finish the assess 2025 returns functions
#create new training dataset with < 2025
#create new test dataset for 2025
#do the probability range assessments and the return assessments


#expected value analysis for 2025:


colnames(passing_pre_prep) = gsub(' |\\.', '_', colnames(passing_pre_prep))
passing_pre_prep = passing_pre_prep[ , !duplicated(colnames(passing_pre_prep))]
colnames(rushing_pre_prep) = gsub(' |\\.', '_', colnames(rushing_pre_prep))
rushing_pre_prep = rushing_pre_prep[ , !duplicated(colnames(rushing_pre_prep))]
colnames(receiving_pre_prep) = gsub(' |\\.', '_', colnames(receiving_pre_prep))
receiving_pre_prep = receiving_pre_prep[ , !duplicated(colnames(receiving_pre_prep))]
colnames(touchdown_pre_prep) = gsub(' |\\.', '_', colnames(touchdown_pre_prep))
touchdown_pre_prep = touchdown_pre_prep[ , !duplicated(colnames(touchdown_pre_prep))]
model_prep_results_ev_analysis = model_prep(passing_pre_prep, rushing_pre_prep, receiving_pre_prep, touchdown_pre_prep,
                                            passing_data_column_categories, rushing_data_column_categories,
                                            receiving_data_column_categories, touchdown_data_column_categories,
                                            train_test_split = TRUE, split_by_year = 2025, train_mode = TRUE, bin_iv_limit = 0.01)


column_categories = list(passing_data_column_categories = readRDS('model/data/passing_data_column_categories.rds'),
                         rushing_data_column_categories = readRDS('model/data/rushing_data_column_categories.rds'),
                         receiving_data_column_categories = readRDS('model/data/receiving_data_column_categories.rds'),
                         touchdown_data_column_categories = readRDS('model/data/touchdown_data_column_categories.rds'))

train_list = model_prep_results_ev_analysis[1:4]
test_list = model_prep_results_ev_analysis[5:8]
# all_models_data = rbind()
for(i in 1:length(model_names))
{
  model_name = model_names[i]
  print(model_name)
  for (response in names(train_list[[i]]))
  {
    print(response)
    train = train_list[[i]][[response]] %>%
      select(-any_of(c(setdiff(column_categories[[i]]$basic_cols, 'GS'), model_manual_remove,  response_cols[[i]])))
    test_orig = test_list[[i]][[response]] 
    test = test_orig %>%
      select(-any_of(c(setdiff(column_categories[[i]]$basic_cols, 'GS'), model_manual_remove,  response_cols[[i]])))
    colnames(train) = gsub(' |\\.|/|-', '_', colnames(train))
    colnames(test) = gsub(' |\\.|/|-', '_', colnames(test))
    test[] <- lapply(test, function(x) if(is.character(x)) as.factor(x) else x)
    
    res = run_gbm(
      df = train,
      response = response,
      model_name = model_names[i],
      path = NULL,
      t_per_s = c(750, 150),
      i_range = c(2,5,8),
      s_range = c(0.01,0.05),
      n_range = 10,
      b_range = c(0.3, 0.5, 0.7)
    )
    best_model = res[[2]]
    tree = gbm.perf(best_model, method = "cv", plot.it = FALSE)
    preds= predict(best_model, test, n.trees = tree, type = "response")
    all_models_data = rbind(all_models_data, data.frame(Week = test_orig$Week,
                                                        player_id = test_orig$player_id,
                                                        Team = test_orig$Team,
                                                        Season = test_orig$Season,
                                                        response = response,
                                                        Model_Probability = preds))
  }
}

passing_test_data = bind_rows(test_list[[1]])%>% select(player_id, Season, Week, Team)
rushing_test_data = bind_rows(test_list[[2]])%>% select(player_id, Season, Week, Team)
receiving_test_data =bind_rows(test_list[[3]])%>% select(player_id, Season, Week, Team)
td_test_data = bind_rows(test_list[[4]])%>% select(player_id, Season, Week, Team)
all_test_data = bind_rows(passing_test_data, rushing_test_data, receiving_test_data, td_test_data)

player_bios = readRDS('data_collection/saved_data_files/player_bios.rds')
all_models_data_with_bet_info = all_models_data %>% inner_join(player_bios %>% select(player_id, names, positions), 'player_id') %>%
  cbind(all_test_data$Season) %>%
  rename('Season' = 'all_test_data$Season') %>%
  left_join(team_lookup_table %>% select(Team,TV_abbr), join_by('Team')) %>%
  filter(Season == 2025) %>%
  mutate(cleaned_name = clean_names(names)) %>%
  inner_join(playergl,join_by('cleaned_name' == 'cleaned_name', 'Week' == 'week', 'TV_abbr' == 'team')) %>%
  calculate_probability_ranges() %>%
  mutate(Bet_Label = ifelse(response == 'Anytime Touchdown Scorer', NA, str_extract(response,'[0-9]+')),
         Bet_Category = ifelse(response == 'Anytime Touchdown Scorer', 'Anytime Touchdown Scorer', gsub('[0-9]+|_Yds_', '', response)),
         Total_Touchdowns =rushing_tds +receiving_tds,
         BetColumn = case_when(
           Bet_Category == 'Anytime Touchdown Scorer' ~ Total_Touchdowns,
           Bet_Category == 'Passing' ~ passing_yards,
           Bet_Category == 'Rushing'~ rushing_yards,
           Bet_Category == 'Receiving' ~ receiving_yards
         ),
         BetHit = ifelse(BetColumn >= Bet_Label, TRUE, FALSE)) %>%
  mutate(Bet_Category = tolower(Bet_Category)) %>%
  inner_join(bet_recommendations %>%
               mutate(cleaned_name = clean_names(Player)) %>%
               mutate(Bet_Type = tolower(Bet_Type), Label = gsub('\\+','',Label)) %>%
               select(Week, cleaned_name, Bet_Type, Label, Odds), join_by('cleaned_name', 'Week', 'Bet_Category' == 'Bet_Type', 'Bet_Label' == 'Label')) %>%
  mutate(profit_per_100_if_win = ifelse(Odds > 0, Odds, 100^2/(-1*Odds)),
         profit_per_100 = profit_per_100_if_win*Model_Probability - 100*(1-Model_Probability),
         Payout = ifelse(BetHit == TRUE, profit_per_100, -100))

model_assessment = lapply(unique(all_models_data_with_names$response), function(x) all_models_data_with_names %>% filter(response==x) %>% assess_probability_ranges())


model_data_with_ev_ranges = all_models_data_with_bet_info %>% calculate_ev_ranges() %>% calculate_odds_ranges()%>%
  filter(Model_Probability >= 0.6)%>%
  group_by(EV_Range_group, Odds_Range) %>% summarise(TotalBetAmt = 100*n(),
                                         TotalProfit = sum(Payout),
                                         PctWin = mean(BetHit),
                                         Return = TotalProfit/TotalBetAmt) %>%
  select(-TotalBetAmt, -TotalProfit, -PctWin) %>%
  pivot_wider(names_from = Odds_Range, values_from=Return)

#next: join to get the player name (instead of player id) and then join to bet recommendations





