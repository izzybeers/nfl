source('data_collection.R')

#one of predict (data for upcoming week), train (tune hyperparameters for model), full_fit (just run model on full dataset) 

#read in from environment variables:
mode = 'predict'
wk = 1
min_year = 2020
max_year = 2026
num_iv_winners = 10
test_mode = FALSE

data_collection(mode, min_year, max_year, wk, num_iv_winners = 10, test_mode)

#deployment steps:
#create 2 jobs: predict mode start of week and predict mode midweek, and set environment variables accordingly
#wk and season info would have to be defined dynamically
#Keep train/full_fit workflows local and manually initiated; only prediction-mode data collection should be automated.