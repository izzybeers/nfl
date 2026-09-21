print(paste("Beginning job at", Sys.time()))
source('data_collection/data_collection.R')
source('data_collection/scripts/global.R')

#one of predict (data for upcoming week), train (tune hyperparameters for model), full_fit (just run model on full dataset) 

#read in from environment variables:
mode = 'predict'
min_year = 2020
num_iv_winners = 10
test_mode = FALSE

schedules_raw = get_schedules(min_year)
next_game = schedules_raw %>% mutate(game_datetime = as.POSIXct(paste(gameday, gametime), tz = "America/New_York")) %>%
  filter(game_datetime > Sys.time()) %>% arrange(game_datetime, week) %>% slice(1) %>% data.frame()
max_year = next_game %>% pull(season)
wk = next_game %>% pull(week)

data_collection(mode, min_year, max_year, wk, num_iv_winners = 10, test_mode)

print(paste('Completed job at', Sys.time()))
