library(rvest)
library(RSelenium)
library(tidyr)
library(wdman)
library(httr)
library(chromote)
library(dplyr)
library(stringr)
library(gbm)
source('nfl_functions.R')
source('nfl_data_sources.R')

this_season = 2024
this_week = 18


b = ChromoteSession$new()

#model dataset from last week:
stats_full = readRDS('stats_full.rds')


#get full list of WR names:
player_df = readRDS("player_df.rds")

start = 1
new_gamelogs = rbind()
players_this_season = unique(stats_full$name[stats_full$Season == this_season])
for(p in start:length(players_this_season))
{
  print(p)
  weight = stats_full %>% filter(name == players_this_season[p] & Season == this_season) %>% select(weight) %>% pull() %>% max()
  height = stats_full %>% filter(name == players_this_season[p] & Season == this_season) %>% select(height) %>% pull() %>% max()
  college = stats_full %>% filter(name == players_this_season[p] & Season == this_season) %>% select(college) %>% pull() %>% max()
  gamelog = get_game_log(player_row = player_df[which(player_df$names == players_this_season[p]),], yr = this_season)
  if(!is.null(gamelog))
  {
    gamelog_with_stats = get_per_game_stats(log = gamelog)
    recent_week_games = gamelog_with_stats %>% filter(week_num == (this_week - 1))
    
    if(nrow(recent_week_games) > 0)
    {
      Median_Receiving_Yards_Recent = get_last_3_games(week_num = (this_week-1), field_name = 'Receiving_Yards', df = gamelog_with_stats) #3 most recent active games. If there aren't 3 active games within the last 5, take the most possible in the last 5 games.
      Median_Rec_Recent = get_last_3_games(week_num = (this_week-1), field_name = 'rec', df = gamelog_with_stats)
      Median_TD_Recent  = get_last_3_games(week_num = (this_week-1), field_name = 'rec_td', df = gamelog_with_stats)
      Median_Targets_Recent = get_last_3_games(week_num = (this_week-1), field_name = 'targets', df = gamelog_with_stats)
      Median_Snaps_Played_Recent = get_last_3_games(week_num = (this_week-1), field_name = 'snaps_played', df = gamelog_with_stats)
      
      Median_Receiving_Yards = median(gamelog_with_stats$Receiving_Yards, na.rm = TRUE)
      Median_Rec = median(gamelog_with_stats$rec, na.rm = TRUE)
      Median_Targets = median(gamelog_with_stats$targets, na.rm = TRUE)
      Median_TD = median(gamelog_with_stats$rec_td, na.rm = TRUE)
      
      new_gamelogs = rbind(new_gamelogs, cbind(name = players_this_season[p], weight, height,college,recent_week_games,
                                               Median_Receiving_Yards_Recent,
                                               Median_Rec_Recent,
                                               Median_Targets_Recent,
                                               Median_TD_Recent,
                                               Median_Snaps_Played_Recent,
                                               Median_Receiving_Yards,
                                               Median_Rec,
                                               Median_Targets,
                                               Median_TD))

    }
  }
}
hold = new_gamelogs


player_recent_years = readRDS('player_recent_years_stats.rds')

new_gamelogs = new_gamelogs %>% left_join(player_recent_years, join_by('name' == 'name', 'Season' == 'Season'))




new_gamelogs =  new_gamelogs %>% left_join(divisional_table, join_by('team' == 'Team')) %>% left_join(divisional_table, join_by('opp' == 'Team')) %>%
  rename('Team_Conf' = 'Conference.x',
         'Team_Div' = 'Division.x',
         'Opp_Conf' = 'Conference.y',
         'Opp_Div' = 'Division.y',
         'Team_FullName' = 'Team_FullName.x') %>% select(-Team_FullName.y)

new_gamelogs  = new_gamelogs %>% mutate(Inter_Conference_Game = ifelse(Team_Conf != Opp_Conf, 1, 0),
                         Divisional_Game = ifelse(Team_Conf == Opp_Conf & Team_Div == Opp_Div, 1, 0),
                         Team_Div = paste(Team_Conf, Team_Div))



b = ChromoteSession$new()

team_stats_by_game = readRDS('team_stats_by_game.rds')

team_stats_by_game_this_week = rbind()
y = this_season
for (t in setdiff(tolower(unique(stats_full$team)),''))
{
  print(t)
  team_stats_by_game_this_week = rbind(team_stats_by_game_this_week, get_team_stats_per_game(t, y))
}
team_stats_by_game = rbind(team_stats_by_game, team_stats_by_game_this_week)
team_stats_by_game = team_stats_by_game %>% unique()
team_stats_by_game = team_stats_by_game %>% filter(((Season < this_season) | (Season == this_season & week < this_week)) & !is.na(opp_points))
saveRDS(team_stats_by_game, 'team_stats_by_game.rds')


stats_by_year = readRDS('stats_by_year.rds')

#add offensive stats and overall team stats
game_schedule_by_year = team_stats_by_game %>% select(team, Season, last_season, seasons_ago_2, week, time_of_day, win_pct, point_diff_per_game, point_diff_per_winning_games,
                                                      passing_yards_per_game, passing_yards_allowed_per_game,
                                                      yard_diff_per_game, yard_diff_per_winning_games, team_turnovers_allowed_per_game, team_turnovers_forced_per_game) %>%
  left_join(stats_by_year, join_by('team' == 'team', 'last_season' == 'Season')) %>%
  left_join(stats_by_year, join_by('team' == 'team', 'seasons_ago_2' == 'Season')) %>%
  rename('team_passing_yards_per_game' = 'passing_yards_per_game',
         'team_passing_yards_allowed_per_game' = 'passing_yards_allowed_per_game',
         'team_win_pct_last_season' = 'win_pct_season.x',
         'team_point_diff_last_season' = 'point_diff_season.x',
         'team_point_diff_per_game_last_season' = 'point_diff_per_game_season.x',
         'team_point_diff_per_win_last_season' = 'point_diff_per_win_season.x',
         'team_yard_diff_last_season' = 'yards_diff_season.x',
         'team_yard_diff_per_game_last_season' = 'yard_diff_per_game_season.x',
         'team_yard_diff_per_win_last_season' = 'yard_diff_per_win_season.x',
         'team_turnovers_allowed_per_game_last_season' = 'turnovers_allowed_per_game.x',
         'team_turnovers_forced_per_game_last_season' = 'turnovers_forced_per_game.x',
         'team_win_pct_2_seasons_ago' = 'win_pct_season.y',
         'team_point_diff_2_seasons_ago' = 'point_diff_season.y',
         'team_point_diff_per_game_2_seasons_ago' = 'point_diff_per_game_season.y',
         'team_point_diff_per_win_2_seasons_ago' = 'point_diff_per_win_season.y',
         'team_yard_diff_2_seasons_ago' = 'yards_diff_season.y',
         'team_yard_diff_per_game_2_seasons_ago' = 'yard_diff_per_game_season.y',
         'team_yard_diff_per_win_2_seasons_ago' = 'yard_diff_per_win_season.y',
         'team_turnovers_allowed_per_game_2_seasons_ago' = 'turnovers_allowed_per_game.y',
         'team_turnovers_forced_per_game_2_seasons_ago' = 'turnovers_forced_per_game.y',
         'team_passing_yards_last_season' = 'passing_yards_season.x',
         'team_passing_yards_2_seasons_ago' = 'passing_yards_season.y',
         'team_passing_yards_allowed_last_season' = 'passing_yards_allowed_season.x',
         'team_passing_yards_allowed_2_seasons_ago' = 'passing_yards_allowed_season.y'
  ) %>% filter(Season >= 2022)


saveRDS(game_schedule_by_year, 'game_schedule_by_year.rds')

game_schedule_by_year = readRDS('game_schedule_by_year.rds')

#merge with winners:
new_gamelogs = new_gamelogs %>% 
  left_join(game_schedule_by_year %>% select(-last_season, -seasons_ago_2), join_by('team' == 'team', 'Season' == 'Season', 'week_num' == 'week'))%>%
  filter(week_num <= 18) %>% 
  mutate(week18 = ifelse(week_num == 18, 1, 0),
         week17 = ifelse(week_num == 17, 1, 0))

#defensive stats for opponent:
new_gamelogs = new_gamelogs %>% left_join(game_schedule_by_year %>% select('team','Season','week','team_passing_yards_allowed_per_game','team_turnovers_forced_per_game') %>%
                              rename('opp_defense_passing_yards_allowed_per_game' = 'team_passing_yards_allowed_per_game',
                                     'opp_defense_turnovers_forced_per_game' = 'team_turnovers_forced_per_game'), join_by('opp' == 'team', 'Season' == 'Season', 'week_num' == 'week'))



new_gamelogs = new_gamelogs %>% mutate(stadium = ifelse(time_of_day != 'Morning',ifelse(game_location == 'Home', team, opp), NA)) %>% left_join(stadiums, join_by('stadium' == 'Team')) %>%
  left_join(stadiums %>% select(Team, Grass_Type, Roof) %>% rename('Home_Grass_Type' = 'Grass_Type', 'Home_Roof_Type' = 'Roof'), join_by('team' == 'Team')) %>%
  mutate(Familiar_Grass_Type = ifelse(Grass_Type == Home_Grass_Type, 1, 0),
         has_roof = ifelse(Roof %in% c('Dome', 'Retractable'), 1, 0),
         Familiar_Roof_Type = ifelse(Roof %in% c('Dome', 'Retractable'), 1, 0) == ifelse(Home_Roof_Type %in% c('Dome','Retractable'),1,0)) %>%
  select(-Roof, -Home_Grass_Type, -Home_Roof_Type)


new_gamelogs = new_gamelogs %>% 
  mutate(playoffs_at_stake = ifelse(week_num < 17, NA,
                                    ifelse(week_num == 17 & paste(Season, team) %in% (clinching_data %>% filter(Week == 17) %>% select(year_team) %>% pull()), 1,
                                           ifelse(week_num == 18 & paste(Season, team) %in% (clinching_data %>% filter(Week == 18) %>% select(year_team) %>% pull()), 1, 0))))




injuries_df = get_injuries_data(y = this_season, w = (this_week-1))



new_gamelogs = new_gamelogs %>% left_join(injuries_df %>% select(-Game_Status), join_by('name' == 'Player', 'Season' == 'year', 'week_num' == 'week')) %>%
  mutate(Less_Practice = replace(Less_Practice, is.na(Less_Practice), 0),
         On_Injury_List = replace(On_Injury_List, is.na(On_Injury_List), 0))

#depth chart estimates:
all_target_ranks = rbind()

for (t in unique(stats_full$team))
{
  target_ranks = get_target_rankings(df = new_gamelogs, y = this_season, t, injuries = injuries_df)
  all_target_ranks = rbind(all_target_ranks, target_ranks)
}
all_target_ranks = all_target_ranks %>% unique()

new_gamelogs = new_gamelogs %>% left_join(all_target_ranks, join_by('name' == 'name', 'team' == 't',
                                                                    'Season' == 'y', 'week_num' == 'week'))


#familiar climate:


new_gamelogs = new_gamelogs %>% left_join(climates, join_by('team' == 'Team')) %>% select(-Used_To_Rain, -Used_To_Snow) #precip data not reliable at this time
#for teams that play/practice in dome, they're not used to any extreme weather:
new_gamelogs = new_gamelogs %>% mutate(
  # Used_To_Rain = ifelse(is.na(Used_To_Rain),0,Used_To_Rain),
  # Used_To_Snow = ifelse(is.na(Used_To_Snow),0,Used_To_Snow),
  Used_To_Hot = ifelse(is.na(Used_To_Hot),0,Used_To_Hot),
  Used_To_Cold = ifelse(is.na(Used_To_Cold),0,Used_To_Cold)
)

weather_data_previous = readRDS("weather_data.rds")

unique_games = new_gamelogs %>% filter(has_roof == 0 & !is.na(stadium) & week_num == (this_week-1) & Season == this_season) %>% distinct(game_date, time_of_day, stadium) %>% select(game_date, time_of_day, stadium)
weather_data = rbind()
start = 1
for (i in start:nrow(unique_games))
{
  print(i)
  weather_data = rbind(weather_data, get_weather(date = unique_games$game_date[i],
                                                 stadium = unique_games$stadium[i],
                                                 time_of_day = unique_games$time_of_day[i]))
}

weather_data = rbind(weather_data_previous, weather_data)
saveRDS(weather_data, "weather_data.rds")

weather_data = readRDS("weather_data.rds")

new_gamelogs = new_gamelogs %>% left_join(weather_data, join_by('game_date' == 'date', 'stadium' == 'stadium')) %>%
  mutate(unfamiliar_temperature = ifelse(is.na(stadium), NA,
                                         ifelse(has_roof == 1, 0,
                                                ifelse((Used_To_Hot == 0 & approx_temperature >= 80) | (Used_To_Cold == 0 & approx_temperature < 40), 1, 0))))


new_gamelogs =  new_gamelogs %>% left_join(long_travel, join_by('team' == 'Team')) %>% left_join(long_travel, join_by('opp' == 'Team'))
new_gamelogs = new_gamelogs %>% mutate(cross_country_travel = case_when(
  game_location == 'Home' ~ 0,
  is.na(Coast.x) ~ 0,
  is.na(Coast.y) ~ 0,
  Coast.x == Coast.y ~ 0,
  .default = 1
)) %>% select(-Coast.x, -Coast.y)

#QB ROSTERS

start = 1
qb_names = readRDS('qb_names.rds')
# saveRDS(qb_names, 'qb_names.rds')
qb_gamelog_this_week = rbind()
for (p in start:nrow(qb_names))
{
  print(p)
  qb_gamelog_this_week = rbind(qb_gamelog_this_week, get_qb_gamelog(qb_list = qb_names[p,], y = this_season))
}
qb_gamelog_this_week = qb_gamelog_this_week %>% filter(week_num == (this_week-1))



qb_gamelog_this_week = qb_gamelog_this_week %>% left_join(starting_qbs, join_by('Season' == 'Season',
                                                                                'Team' == 'Team')) %>%
  mutate(starting_qb = ifelse(name == Qb1,1,0), week_num = as.numeric(week_num)) %>% select(-Qb1)


qb_gamelog_this_week = qb_gamelog_this_week %>% group_by(Team, Season, week_num) %>% summarise(starting_qb = max(starting_qb))

new_gamelogs = new_gamelogs %>% left_join(qb_gamelog_this_week, join_by('team' == 'Team', 'Season' == 'Season', 'week_num' == 'week_num'))


qb_games_played = readRDS('qb_games_played.rds')
qb_games_played = rbind(qb_games_played, qb_gamelog_this_week) %>% unique()
qb_games_played = qb_games_played %>% arrange(Team, Season, week_num) %>% data.frame()
saveRDS(qb_games_played,'qb_games_played.rds')

new_gamelogs = new_gamelogs %>% left_join(qb_games_played, join_by('team' == 'Team', 'Season' == 'Season', 'week_num' == 'week_num'))




# new_gamelogs = new_gamelogs %>% left_join(stats_full %>% select(name,college) %>% unique(), join_by('name' == 'name'))

stats_full = stats_full %>% select(-last_season, -years_ago_2, -y)

stats_full = rbind(stats_full, new_gamelogs)



#rectify missing age and targets rank with approximations:

missing_ages = which(is.na(stats_full$age))
missing_target_rank = which(is.na(stats_full$targets_rank) &
                              !is.na(stats_full$Cumulative_Games_Active) &
                              stats_full$Cumulative_Games_Active > 0)

for (i in sort(unique(c(missing_ages, missing_target_rank))))
{
  nm = stats_full$name[i]
  replace_age =  stats_full %>% filter(name == nm & Season == stats_full$Season[i]) %>% select(age) %>% pull()
  replace_age[is.na(replace_age)] = 0
  stats_full$age[i] = ifelse(length(replace_age) == 0, NA, max(replace_age))
  
  recent_weeks_with_target_rank = stats_full %>% filter(name == nm & Season == stats_full$Season[i] &
                                                       week_num < stats_full$week_num[i] & !is.na(targets_rank)) %>%
    select(week_num) %>% pull()
  
  if(length(recent_weeks_with_target_rank) > 0) 
  {
    #take the targets rank for the last time that they had an active game:
  stats_full$targets_rank[i] = stats_full %>% filter(name == nm & Season == stats_full$Season[i] &
                                                       week_num == max(recent_weeks_with_target_rank)) %>%
    select(targets_rank) %>% pull()
  }
}

saveRDS(stats_full, 'stats_full.rds')



model_data_with_labels = stats_full %>% select(name, team, opp, game_date, week_num, Season, #labels
                                          Receiving_Yards, #response variable
                                          #predictors about game:
                                          game_location, Month, day_of_week, time_of_day, Inter_Conference_Game, Divisional_Game, cross_country_travel,
                                          week17, week18, playoffs_at_stake, starting_qb,
                                          #predictors about stadium:
                                          Grass_Type, Altitude, has_roof, Familiar_Grass_Type, Familiar_Roof_Type,
                                          #player biography
                                          age, height, weight,
                                          #general player info:
                                          targets_rank, #depth chart approximation using avg targets per game
                                          Less_Practice, On_Injury_List,
                                          #player stats from recent games:
                                          Receiving_Yards_Lag1, Receiving_Yards_Lag2, Receiving_Yards_Lag3,
                                          Median_Receiving_Yards_Recent, Median_Rec_Recent, Median_TD_Recent,
                                          Median_Targets_Recent, Median_Snaps_Played_Recent,
                                          #player stats from the whole current season:
                                          Percent_Games_Started_Season, Avg_Targets_Per_Game, Avg_Rec_Per_Game,
                                          Avg_Rec_Per_Target, Avg_Receiving_Yards_Per_Game, Avg_Receiving_Yards_Per_Target,
                                          Avg_Receiving_TDs_Per_Game, Avg_Percent_Snaps_Played,
                                          Median_Receiving_Yards, Median_Rec, Median_Targets, Median_TD,
                                          #stats from recent years:
                                          num_active_games_last_year, num_games_start_last_year, rec_yd_per_game_last_year, rec_per_game_last_year,
                                          targets_per_game_last_year, rec_per_target_last_year, snaps_per_game_last_year,
                                          num_active_games_2_years_ago, num_games_start_2_years_ago, rec_yd_per_game_2_years_ago, rec_per_game_2_years_ago,
                                          targets_per_game_2_years_ago, rec_per_target_2_years_ago, snaps_per_game_2_years_ago,
                                          #predictors about team:
                                          Team_Conf, Team_Div, win_pct, point_diff_per_game, point_diff_per_winning_games,
                                          team_passing_yards_per_game, team_passing_yards_allowed_per_game, yard_diff_per_game,
                                          yard_diff_per_winning_games, team_turnovers_allowed_per_game, team_turnovers_forced_per_game,
                                          #team recent years data:
                                          team_win_pct_last_season, team_point_diff_last_season, team_point_diff_per_game_last_season,
                                          team_yard_diff_last_season, team_yard_diff_per_game_last_season, team_turnovers_allowed_per_game_last_season,
                                          team_turnovers_forced_per_game_last_season, team_point_diff_per_win_last_season, team_yard_diff_per_win_last_season,
                                          team_passing_yards_last_season, team_passing_yards_allowed_last_season,
                                          team_win_pct_2_seasons_ago, team_point_diff_2_seasons_ago, team_point_diff_per_game_2_seasons_ago, team_yard_diff_2_seasons_ago,
                                          team_yard_diff_per_game_2_seasons_ago, team_turnovers_allowed_per_game_2_seasons_ago, team_turnovers_forced_per_game_2_seasons_ago,
                                          team_point_diff_per_win_2_seasons_ago, team_yard_diff_per_win_2_seasons_ago, team_passing_yards_2_seasons_ago, team_passing_yards_allowed_2_seasons_ago,
                                          #stats about opponent's defense:
                                          opp_defense_passing_yards_allowed_per_game, opp_defense_turnovers_forced_per_game,
                                          #weather:
                                          approx_temperature, approx_visibility, Approx_Wind_Speed, unfamiliar_temperature
) %>% unique()


model_data_with_labels = model_data_with_labels %>% mutate(
  game_location = as.factor(game_location),
  Month = as.factor(Month),
  day_of_week = as.factor(day_of_week),
  time_of_day = as.factor(time_of_day),
  Grass_Type = as.factor(Grass_Type),
  weight = as.numeric(weight),
  Team_Conf = as.factor(Team_Conf),
  Team_Div = as.factor(Team_Div),
  Familiar_Roof_Type = as.numeric(Familiar_Roof_Type)
  
)




model_data_with_labels = model_data_with_labels %>% filter(!is.na(Receiving_Yards))


saveRDS(model_data_with_labels,paste0('model_data_wk',this_week,'_',this_season,'.rds'))

model_data_with_labels = readRDS(paste0('model_data_wk',this_week,'_',this_season,'.rds'))
set.seed(123)
train_indices = sample(1:nrow(model_data_with_labels), 0.7*nrow(model_data_with_labels), replace = FALSE)
train_data_with_labels = model_data_with_labels[train_indices,] 
test_data_with_labels = model_data_with_labels[-train_indices,] 

train_data= train_data_with_labels %>% select(-name, -team, -opp, -game_date, -week_num, -Season)
test_data = test_data_with_labels %>% select(-name, -team, -opp, -game_date, -week_num, -Season)
#tuning:


library(gbm)
interaction_depth = c(1,3,5,7,9)
shrinkage = c(0.001, 0.005, 0.01, 0.1)
n.minobsinnode = 10
tuning = rbind()
for (i in interaction_depth)
{
  print(i)
  for (s in shrinkage)
  {
    print(s)
    for (n in n.minobsinnode)
    {
      model = gbm(formula = Receiving_Yards ~ .,
                  data = train_data,
                  distribution = 'gaussian',
                  n.trees = 10000,
                  interaction.depth = i,
                  shrinkage = s,
                  n.minobsinnode = 10,
                  cv.folds = 5,
                  train.fraction = 0.8)
      best_trees = gbm.perf(model, method = "cv")
      e = model$valid.error[best_trees]
      
      tuning = rbind(tuning, c(trees = best_trees, idepth = i, shrink = s, nmino = n, error = e))
    }
  }
}

#setting specific values:
t = 993
i = 1
s = 0.01
n = 10


model = gbm(formula = Receiving_Yards  ~ .,
            data = train_data,
            distribution = 'gaussian',
            n.trees = t,
            interaction.depth = i,
            shrinkage = s,
            n.minobsinnode = n,
            cv.folds = 5)

saveRDS(model, paste0('model_wk',this_week,'_',this_season,'.rds'))

model = readRDS(paste0('model_wk',this_week,'_',this_season,'.rds'))

test_preds = predict(model, newdata = test_data, n.trees = t)
median(abs(test_data$Receiving_Yards - test_preds))



#weather
#future fixes: fix inactive games problem, information value for stadium, 


#PREDICTION DF


schedule_html = get_html("https://www.pro-football-reference.com/years/2024/games.htm")
week_num = schedule_html %>% html_nodes('th[scope="row"][data-stat = "week_num"]') %>% html_text(trim = TRUE)
day_of_week = schedule_html %>% html_nodes('td[data-stat = "game_day_of_week"]') %>% html_text(trim = TRUE)
game_date = schedule_html %>% html_nodes('td[data-stat = "game_date"]') %>% html_text(trim = TRUE)
away_team = schedule_html %>% html_nodes('td[data-stat = "winner"]') %>% html_text(trim = TRUE)
home_team = schedule_html %>% html_nodes('td[data-stat = "loser"]') %>% html_text(trim = TRUE)
time = schedule_html %>% html_nodes('td[data-stat = "gametime"]') %>% html_text(trim = TRUE)

schedule_df = data.frame(week_num, day_of_week, game_date, away_team, home_team, time) %>% filter(week_num == this_week)
schedule_df = schedule_df %>% filter(game_date >= Sys.Date())
future_weather = read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vT9_LcNO2d8L5kzbJQZZti9kxfAZRFRAl2oJz5WlpusfvL1txbkc8OU6BSlB54TA9HCBHRlIxi9MpuT/pub?gid=966365422&single=true&output=csv")

#FIX THIS EACH WEEK:
starting_qbs_this_week = data.frame('DAL' = 0,
                                    'CAR' = 1,
                                    'KAN' = 0,
                                    'CLE' = 0,
                                    'MIA' = 0,
                                    'HOU' = 1,
                                    'NYJ' = 1,
                                    'JAX' = 0,
                                    'WAS' = 1,
                                    'NOR' = 0,
                                    'BAL' = 1,
                                    'NYG' = 0,
                                    'CIN' = 1,
                                    'TEN' = 0,
                                    'NWE' = 1,
                                    'ARI' = 1,
                                    'IND' = 0,
                                    'DEN' = 1,
                                    'BUF' = 1,
                                    'DET' = 1,
                                    'TAM' = 1,
                                    'LAC' = 1,
                                    'PIT' = 1,
                                    'PHI' = 0,
                                    'GNB' = 1,
                                    'SEA' = 1,
                                    'CHI' = 1,
                                    'MIN' = 1,
                                    'ATL' = 0,
                                    'LVR' = 0,
                                    'SFO' = 0,
                                    'LAR' = 0)

teams_to_run = c('LAC','NWE','DEN','CIN','ARI','LAR','')
all_names_this_year = na.omit(unique(model_data_with_labels$name[model_data_with_labels$Season == this_season &
                                                                   model_data_with_labels$team %in% teams_to_run]))
prediction_df_saturday = prediction_df
#if you want to run on all teams:
all_names_this_year = na.omit(unique(model_data_with_labels$name[model_data_with_labels$Season == this_season]))

prediction_df = rbind()
start = 1
#fix some issuethat's happeningwith #107 since he has a period in his name (Jr.)
for (n in start:length(all_names_this_year))
{
  print(n)
  prediction_df = rbind(prediction_df, create_prediction_df(wk_num = this_week,
                                                            player_df=player_df,
                                                            season = this_season,
                                                            df = stats_full %>% filter(name == all_names_this_year[n] & Season == this_season),
                                                            full_df = stats_full,
                                                            schedule_df = schedule_df,
                                                            team_stats_by_game = team_stats_by_game,
                                                            injuries = injuries_df,
                                                            stadiums = stadiums,
                                                            future_weather = future_weather,
                                                            starting_qbs = starting_qbs_this_week,
                                                            long_travel = long_travel))
}
prediction_df = readRDS(paste0('predictiondf_wk',this_week,'_',this_season,'.rds'))



b = ChromoteSession$new()

saveRDS(model, paste0('model_wk',this_week,'_',this_season,'.rds'))
saveRDS(prediction_df, paste0('predictiondf_wk',this_week,'_',this_season,'.rds'))



#calculate residuals
residuals = test_data$Receiving_Yards - test_preds
residual_mean = mean(residuals)
residual_sd = sd(residuals)


library(httr)
library(jsonlite)

best_trees = t


future_df_predictions = predict(model, newdata = prediction_df %>% select(-Name), n.trees = best_trees)

receiving_props = get_receiving_props()
thresholds = sort(as.numeric(gsub('\\+','',unique(receiving_props$bet_type))))


thresholds_df = rbind()
for (i in 1:length(future_df_predictions))
{
  Name = prediction_df$Name[i]
  Game_Time = prediction_df$time_of_day[i]
  simulations = replicate(1000, future_df_predictions[i] + sample(residuals, size = 1, replace = TRUE))
  probabilities = sapply(thresholds, function(x) mean(simulations > x))
  thresholds_df = rbind(thresholds_df, data.frame(Name = Name,
                                                  time_of_day= Game_Time,                       
                                                  Threshold = thresholds,
                                                  Model_Estimated_Yards = future_df_predictions[i],
                                                  Probability = probabilities))
}

thresholds_df$Threshold = paste0(thresholds_df$Threshold,'+')

#needs to be run once above before the for loop, but for all subsequent 
#refreshes, can just be run from here
receiving_props = get_receiving_props() 
receiving_props_with_probabilities = receiving_props %>% inner_join(thresholds_df,
                                                                    join_by('bet_type' == 'Threshold',
                                                                            'Participant' == 'Name')) %>%
  mutate(expected_profit_per_100 = (payout_per_100-100)*Probability - 100*(1-Probability)) %>% arrange(desc(expected_profit_per_100)) %>%
  left_join(stats_full %>% filter(Season == 2024 & week_num >= 11) %>% distinct(name, team),
            join_by('Participant' == 'name'))

receiving_props_with_probabilities




top_20_vars = summary.gbm(model)[1:20,"var"]

(prediction_df %>% filter(Name == 'Darius Slayton'))[,c(top_20_vars,"targets_rank")]
