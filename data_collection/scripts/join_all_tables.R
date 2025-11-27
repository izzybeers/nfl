
library(stringr)
source('data_collection/scripts/global.R')


join_all_tables = function(player_bios, player_gamelogs, player_seasonal_stats,
                           team_gamelogs, team_seasonal_stats,
                           player_rankings,
                           weather_and_stadium_data,
                           playoff_clinching_data,
                           injuries_data,
                           missing_cutoff,
                           season_data_cutoff,
                           qb1_by_year,
                           predict_mode = FALSE)
{
  
  player_receiving_rankings = player_rankings[[1]]
  player_rushing_rankings = player_rankings[[2]]
  player_receiving_seasonal_rankings = player_rankings[[3]]
  player_rushing_seasonal_rankings = player_rankings[[4]]
  qb_starters = player_rankings[[5]]
  
  team_abbreviations = team_lookup_table %>% select(Team, FullName, TV_abbr)

  player_data = player_bios %>%
    inner_join(player_gamelogs %>%
                select(-any_of(c("Position", "Name", "Playoffs", "Opp", "Date", "Game_Location", "Month"))), #duplicate names with other tables
              join_by('player_id' == 'player_id')) %>%
    left_join(player_seasonal_stats %>%
                rename_with(.cols = matches("sum|mean|median|max|min|sd|Pct|Per"),
                            .fn = ~paste0('Last_Season_', .x)) %>%
                mutate(Year_To_Match = Season + 1)  %>%
                select(-Name, -Season),
              join_by('player_id' == 'player_id', 'Season' == 'Year_To_Match')) %>%
     left_join(player_seasonal_stats %>%
                rename_with(.cols = matches("sum|mean|median|max|min|sd|Pct"),
                            .fn = ~paste0('Two_Seasons_Ago_', .x)) %>%
                mutate(Year_To_Match = Season + 2) %>%
                select(-Name, -Season),
              join_by('player_id' == 'player_id', 'Season' == 'Year_To_Match')) %>%
    left_join(team_gamelogs %>%
                select(Season, Team, Opp, Week, Date, Month, Day, Time, Time_of_Day, Game_Location, Playoffs, Short_Week, Long_Week, matches('(Win)|(Differential)|(Offense)')) %>%
                select(Season, Team, Opp, Week, Date, Month, Day, Time, Time_of_Day, Game_Location, Playoffs, Short_Week, Long_Week,  matches('Cumulative|Pct|Avg|Min|Max|Median|SD|Last3')) %>%
              rename_with(
                  .cols = matches('Offense|Win|Differential|Short|Long'),
                  .fn = ~paste0('Team_',.x)),
              join_by('Team' == 'Team', 'Season' == 'Season', 'Week' == 'Week')) %>%
    left_join(team_gamelogs %>%
                select(Season, Team, Opp, Week, Short_Week, Long_Week, matches('(Win)|(Differential)|(Defense)')) %>%
                select(Season, Team, Opp, Week, Short_Week, Long_Week, matches('Cumulative|Pct|Avg|Min|Max|Median|SD|Last3')) %>%
                rename_with(
                  .cols = matches('Defense'),
                  .fn = ~paste0('Opp_',.x, '_Allowed')) %>%
                rename_with(
                  .cols = matches('Win|Differential|Short|Long'),
                  .fn = ~paste0('Opp_',.x)),
              join_by('Opp' == 'Team', 'Season' == 'Season', 'Week' == 'Week')) %>%
    #season-end stats for team:
    left_join(team_seasonal_stats %>%
                select(Season, Team, Won_Superbowl, matches('(Win)|(Differential)|(Offense)')) %>%
                select(Season, Team, Won_Superbowl, matches('Sum|Cumulative|Pct|Avg|Min|Max|Median|SD|Per')) %>%
                mutate(Year_To_Match = Season + 1) %>% select(-Season) %>%
                rename_with(
                  .cols = matches('(Win)|(Differential)|(Offense)|Superbowl'),
                  .fn = ~paste0('Team_Last_Season_', .x)
                ),
              join_by('Team' == 'Team', 'Season' == 'Year_To_Match')) %>%
     left_join(team_seasonal_stats %>%
                select(Season, Team, Won_Superbowl, matches('(Win)|(Differential)|(Offense)')) %>%
                select(Season, Team, Won_Superbowl, matches('Sum|Cumulative|Pct|Avg|Min|Max|Median|SD|Per')) %>%
                mutate(Year_To_Match = Season + 2) %>% select(-Season) %>%
                rename_with(
                  .cols = matches('(Win)|(Differential)|(Offense)|Superbowl'),
                  .fn = ~paste0('Team_Two_Seasons_Ago_', .x)
                ),
              join_by('Team' == 'Team', 'Season' == 'Year_To_Match')) %>%
     #season-end stats for opponent:
     left_join(team_seasonal_stats %>%
                select(Season, Team, Won_Superbowl, matches('(Win)|(Differential)|(Defense)')) %>%
                select(Season, Team, Won_Superbowl, matches('Sum|Cumulative|Pct|Avg|Min|Max|Median|SD|Per')) %>%
                mutate(Year_To_Match = Season + 1) %>% select(-Season) %>%
                rename_with(
                  .cols = matches('Defense'),
                  .fn = ~paste0('Opp_Last_Season_',.x, '_Allowed')) %>%
                rename_with(
                  .cols = matches('Win|Differential|Superbowl'),
                  .fn = ~paste0('Opp_Last_Season_',.x)),
              join_by('Opp' == 'Team', 'Season' == 'Year_To_Match')) %>%
     left_join(team_seasonal_stats %>%
                select(Season, Team, Won_Superbowl, matches('(Win)|(Differential)|(Defense)')) %>%
                select(Season, Team, Won_Superbowl, matches('Sum|Cumulative|Pct|Avg|Min|Max|Median|SD|Per')) %>%
                mutate(Year_To_Match = Season + 2) %>% select(-Season) %>%
               rename_with(
                .cols = matches('Defense'),
                .fn = ~paste0('Opp_Two_Seasons_Ago_',.x, '_Allowed')) %>%
                rename_with(
                  .cols = matches('Win|Differential|Superbowl'),
                  .fn = ~paste0('Opp_Two_Seasons_Ago_',.x)),
              join_by('Opp' == 'Team', 'Season' == 'Year_To_Match')) %>%
    left_join(player_receiving_rankings %>% select(-Name, -Position), join_by('player_id' == 'player_id', 'Season' == 'Season', 'Week' == 'Week', 'Team' == 'Team')) %>%
    left_join(player_rushing_rankings %>% select(-Name, -Position), join_by('player_id' == 'player_id', 'Season' == 'Season', 'Week' == 'Week', 'Team' == 'Team')) %>%
    left_join(player_receiving_seasonal_rankings %>%
                mutate(Year_To_Match = Season + 1, player_id = as.character(player_id)) %>% select(-Season) %>%
                rename("Last_Season_Pct_of_All_Receiving_Yds" = "Pct_Team_Receiving_Yds_Season"),
              join_by('player_id' == 'player_id', 'Season' == 'Year_To_Match')) %>%
    left_join(player_receiving_seasonal_rankings %>%
                mutate(Year_To_Match = Season + 2, player_id = as.character(player_id)) %>% select(-Season) %>%
                rename("Two_Seasons_Ago_Pct_of_All_Receiving_Yds" = "Pct_Team_Receiving_Yds_Season"),
              join_by('player_id' == 'player_id', 'Season' == 'Year_To_Match')) %>%
    left_join(qb_starters, join_by('Season' == 'Season', 'Week' == 'Week', 'Team' == 'Team')) %>%
    left_join(player_rushing_seasonal_rankings %>%
                mutate(Year_To_Match = Season + 1, player_id = as.character(player_id)) %>% select(-Season) %>%
                rename("Last_Season_Pct_of_All_Rushing_Yds" = "Pct_Team_Rushing_Yds_Season"),
              join_by('player_id' == 'player_id', 'Season' == 'Year_To_Match')) %>%
    left_join(player_rushing_seasonal_rankings %>%
                mutate(Year_To_Match = Season + 2, player_id = as.character(player_id)) %>% select(-Season) %>%
                rename("Two_Seasons_Ago_Pct_of_All_Rushing_Yds" = "Pct_Team_Rushing_Yds_Season"),
              join_by('player_id' == 'player_id', 'Season' == 'Year_To_Match')) %>%
    filter(Season >= season_data_cutoff) %>%
    left_join(weather_and_stadium_data %>% select(-Opp) %>% mutate(Week = as.numeric(Week)), join_by('Team' == 'Team', 'Week' == 'Week', 'Season' == 'Season')) %>%
    left_join(playoff_clinching_data %>% mutate(Team = as.character(Team)) %>%
                rename('TV_team' = 'Team') %>%
                inner_join(team_abbreviations %>% select(Team, TV_abbr), join_by('TV_team' == 'TV_abbr')) %>%
                select(-TV_team),
              join_by('Season' == 'Season', 'Week' == 'Week', 'Team' == 'Team')) %>%
    left_join(injuries_data, join_by('names' == 'Player', 'Week' == 'Week', 'Season' == 'Season')) %>%
    filter(!is.na(Date)) %>% #completed games come up as NA date
    left_join(qb1_by_year %>% mutate(Temp = 1), join_by('Season' == 'Season', 'Team' == 'Team', 'names' == 'Qb1')) %>%
    #Qb1_starting doesn't make sense for the QB model, so deriving new field that says whether this QB is the QB1 is better:
    mutate(Is_Qb1 = ifelse(str_detect(positions, 'QB'), ifelse(!is.na(Temp), 1, 0), NA)) %>% select(-Temp) %>%
    mutate(On_Injury_List = ifelse(is.na(On_Injury_List), 0, On_Injury_List),
           Less_Practice = ifelse(is.na(Less_Practice), 0, Less_Practice),
           Injury_Out = ifelse(Game_Status == 'Out', 1, 0),
           Injury_Questionable = ifelse(Game_Status %in% c('Doubtful', 'Questionable'), 1, 0),
           Unfamiliar_Temperature = ifelse((Used_To_Cold == 0 & Roof == 'Open' & weather_approx_temperature < 30) |
                                           (Used_To_Hot == 1 & Roof == 'Open' & weather_approx_temperature > 80),
                                           1, 0),
           Opp_Unfamiliar_Temperature = ifelse((Opp_Used_To_Cold == 0 & Roof == 'Open' & weather_approx_temperature < 30) |
                                           (Opp_Used_To_Hot == 1 & Roof == 'Open' & weather_approx_temperature > 80),
                                           1, 0),
           Years_Since_Drafted = Season - year_drafted,
           Stadium_Capacity = as.numeric(gsub(',', '', Stadium_Capacity)),
           weight = as.numeric(weight),
           player_id = as.character(player_id),
           age = floor(as.numeric(difftime(paste0(ifelse(Month %in% c('01','02'), Season + 1, Season),'-',Month,'-', str_pad(str_extract(Date,'[0-9]+'), 2, 'left','0')), birthday, units = 'weeks')/52)),
           game_on_birthday = ifelse(substring(birthday, 6, nchar(birthday)) == paste0(as.numeric(Month),'-', str_pad(str_extract(Date,'[0-9]+'), 2, 'left','0')), 1, 0),
           Home_Stadium = ifelse(Stadium == Team, 1, 0),
           Home = ifelse(Game_Location == 'Home', 1, 0),
           Qb1_starting = ifelse(str_detect(positions, 'QB'), NA, Qb1_starting)
           ) %>% select(-Game_Status)
    
    column_names = colnames(player_data) #to be called later
  
    print(nrow(player_data))
  
  
  
  
  #get data for each model:
  passing_data = player_data %>% filter(str_detect(positions, 'QB')) %>% select(-Qb1_starting)
  
  rushing_data = player_data %>% select(-Is_Qb1)
  receiving_data = player_data %>% filter((!str_detect(positions, 'QB'))) %>% select(-Is_Qb1)
  
  touchdown_data = player_data
  
  rm(player_data)
  gc()
  
  if(predict_mode == FALSE)
  {
    passing_data = passing_data %>% filter(!is.na(Passing_Yds))
    rushing_data = rushing_data %>% filter(!is.na(Rushing_Yds))
    receiving_data = receiving_data %>% filter(!is.na(Receiving_Yds))
    touchdown_data = touchdown_data %>% filter(!is.na(Total_Touchdowns))
  }
  
  
  
  remove_sparse = function(df, threshold = missing_cutoff)
  {
    missing_pct = apply(df, 2, function(x) mean(is.na(x)|x==Inf|x==(-Inf)))
    missing_pct = missing_pct[which(missing_pct > threshold)]
    
    df = df %>% select(-all_of(names(missing_pct)))
    
    return(df)
  }
  
  remove_present_stats = function(df, stats_regex, historical_regex, additional_manual_removal, override)
  {
    present_stats = setdiff(c(colnames(df)[str_detect(tolower(colnames(df)), stats_regex) & #identify column names with stats words like passing, receiving, etc
                                             !str_detect(tolower(colnames(df)), historical_regex)], #identify historical stats words like median, mean, last3, etc, we want to keep stuff like this
                              additional_manual_removal), #any columns that don't fit the above pattern can be manually entered
                            override) #column we want to keep -- typically just the response variable
    
    return(df %>% select(-any_of(present_stats)))
  }
  
  remove_unwanted_columns = function(df, regex = NULL, colnames = NULL, verbose = TRUE)
  {
    if(!is.null(regex))
    {
      unwanted_columns = colnames(df[str_detect(tolower(colnames(df)), regex)])
    } else {
      unwanted_columns = NULL
    }
    df = df %>% select(-any_of(c(unwanted_columns, colnames)))
    if(verbose == TRUE)
    {
      removed_columns = unique(c(unwanted_columns, colnames))
      print(paste('Removed:', length(removed_columns), 'columns:', paste(removed_columns, collapse = ",")))
    }
    return(df)
  }
  
  
  pull_all_stats_columns = function(df)
  {
      player_historical_stats_columns = colnames(df)[(str_detect(tolower(colnames(df)), '(sum_)|(pct_)|(avg_)|(median_)|(sd_)|(max_)|(min_)|(cumulative_)|(per_)|(seasons_ago)|(last_season)|(last3)|(lag)[0-9]') | str_detect(tolower(colnames(df)), '(_sum)|(_avg)|(_pct)|(_median)|(_sd)|(_max)|(_min)|(_cumulative)')) & (!str_detect(colnames(df), '(Opp)|(Team)|(Rank)|(Pct_Team)'))]
    player_historical_stats_current_season = player_historical_stats_columns[-which(str_detect(player_historical_stats_columns, '(Last_Season)|(Two_Seasons_Ago)'))]
    player_historical_stats_recent_seasons = player_historical_stats_columns[which(str_detect(player_historical_stats_columns, '(Last_Season)|(Two_Seasons_Ago)'))]
    
    team_historical_stats_columns = colnames(df)[str_detect(tolower(colnames(df)), 'team') & !str_detect(tolower(colnames(df)), 'pct_team') & (str_detect(tolower(colnames(df)), '(sum_)|(pct_)|(rank_)|(avg_)|(median_)|(sd_)|(max_)|(min_)|(cumulative_)|(per_)|(seasons_ago)|(last_season)|(last3)|(lag)[0-9]') | str_detect(tolower(colnames(df)), '(_sum)|(_avg)|(_pct)|(rank_)|(_median)|(_sd)|(_max)|(_min)|(_cumulative)'))]
    team_historical_stats_current_season = team_historical_stats_columns[-which(str_detect(team_historical_stats_columns, '(Last_Season)|(Two_Seasons_Ago)'))]
    team_historical_stats_recent_seasons = team_historical_stats_columns[which(str_detect(team_historical_stats_columns, '(Last_Season)|(Two_Seasons_Ago)'))]
    
    opp_historical_stats_columns = colnames(df)[str_detect(tolower(colnames(df)), 'opp') & (str_detect(tolower(colnames(df)), '(sum_)|(pct_)|(rank_)|(avg_)|(median_)|(sd_)|(max_)|(min_)|(cumulative_)|(per_)|(seasons_ago)|(last_season)|(last3)|(lag)[0-9]') | str_detect(tolower(colnames(df)), '(_sum)|(_avg)|(rank_)|(_pct)|(_median)|(_sd)|(_max)|(_min)|(_cumulative)'))]
    opp_historical_stats_current_season = opp_historical_stats_columns[-which(str_detect(opp_historical_stats_columns, '(Last_Season)|(Two_Seasons_Ago)'))]
    opp_historical_stats_recent_seasons = opp_historical_stats_columns[which(str_detect(opp_historical_stats_columns, '(Last_Season)|(Two_Seasons_Ago)'))]
    
    return(list(player_historical_stats_current_season,
                player_historical_stats_recent_seasons,
                team_historical_stats_current_season,
                team_historical_stats_recent_seasons,
                opp_historical_stats_current_season,
                opp_historical_stats_recent_seasons))
  }
  
  
  
  
  
  #remove sparse columns:
  if(predict_mode == FALSE)
  {
    passing_data = remove_sparse(df = passing_data, threshold = missing_cutoff)
    rushing_data = remove_sparse(df = rushing_data, threshold = missing_cutoff)
    receiving_data = remove_sparse(df = receiving_data, threshold = missing_cutoff)
    touchdown_data = remove_sparse(df = touchdown_data, threshold = missing_cutoff)
  }
  
  
  #remove present stats (target leakage)
  
  stats_regex = 'passing|receiving|rushing|fumbles|snap|accuracy|pressure|tackles|sfty|yds|yards|touchdown|kick'
  historical_regex  = '(sum_)|(pct_)|(avg_)|(median_)|(sd_)|(max_)|(min_)|(cumulative_)|(seasons_ago)|(last_season)|(last3)|(lag)[0-9]|(_sum)|(_avg)|(_pct)|(_median)|(_sd)|(_max)|(_min)|(_cumulative)'
  additional_manual_removal = c('Sk', 'Active', 'links')
  
  passing_data = remove_present_stats(df = passing_data,
                                      stats_regex = stats_regex,
                                      historical_regex  =  historical_regex,
                                      additional_manual_removal = additional_manual_removal,
                                      override = 'Passing_Yds')
  
  rushing_data = remove_present_stats(df = rushing_data,
                                      stats_regex = stats_regex,
                                      historical_regex  = historical_regex,
                                      additional_manual_removal = additional_manual_removal,
                                      override = 'Rushing_Yds')
  
  receiving_data = remove_present_stats(df = receiving_data,
                                      stats_regex = stats_regex,
                                      historical_regex  = historical_regex,
                                      additional_manual_removal = additional_manual_removal,
                                      override = 'Receiving_Yds')
  
  touchdown_data = remove_present_stats(df = touchdown_data,
                                      stats_regex = stats_regex,
                                      historical_regex  = historical_regex,
                                      additional_manual_removal = additional_manual_removal,
                                      override = 'Total_Touchdowns')
  
  
  #any other columns to remove:
  
  
  passing_data = remove_unwanted_columns(df = passing_data,
                                         regex = 'receiving|(team.*snaps)|((?<!opp_)rank_)|(pct_team)', colnames = NULL)
  
  rushing_data = remove_unwanted_columns(df = rushing_data,
                                         regex = 'passing|accuracy|pressure|(team.*snaps)', colnames = NULL)
  
  receiving_data = remove_unwanted_columns(df = receiving_data,
                                         regex = '((?<!opp)passing)|accuracy|pressure|(team.*snaps)', colnames = NULL)
  
  
  
  
  
  
  
  #universal columns for all models:
  basic_cols = c('player_id', 'names', 'positions', 'Gtm', 'Week', 'Team', 'Opp', 'Season', 'Date', 'Time', 'GS')
  player_bio_data = c('min_year', 'max_year', 'height', 'weight', 'college', 'Years_Since_Drafted', 'draft_round', 'draft_pick', 'original_draft_team', 'age')
  game_info = c('Month', 'Day', 'Home', 'Home_Stadium', 'Time_of_Day', 'Playoffs', 'International', 'Same_Conference', 'Same_Division', 'game_on_birthday', 'Team_Long_Week', 'Team_Short_Week', 'Opp_Long_Week', 'Opp_Short_Week', 'Long_Travel')
  misc_team_opp_info = c('Team_Last_Season_Won_Superbowl', 'Team_Two_Seasons_Ago_Won_Superbowl', 'Opp_Last_Season_Won_Superbowl', 'Opp_Two_Seasons_Ago_Won_Superbowl')
  stadium_info = c('Stadium', 'Grass_Type', 'Roof', 'Familiar_Roof_Type', 'Familiar_Grass_Type', 'Altitude', 'Stadium_Capacity', 'Loudest_Stadiums')
  injury_data = c('On_Injury_List', 'Less_Practice', 'Injury_Out', 'Injury_Questionable')
  weather_data = c(column_names[str_detect(tolower(column_names), 'weather')], 'Unfamiliar_Temperature', 'Opp_Unfamiliar_Temperature')
  playoff_clinching_data = c('Div_Ranking', 'Div_Pct_Wins', 'Already_Clinched_Playoff', 'Already_Clinched_Division', 'Already_Clinched_Seed1', 'Already_Eliminated', 'Already_Eliminated_Division', 'playoffs_at_stake', 'elimination_at_stake')
  qb_columns = c('Qb1_starting', 'Is_Qb1') # former for rushing/rec/td models, is_qb1 for passing model
  rank_columns = column_names[str_detect(tolower(column_names), '((?<!opp_)rank_)|pct_team')]
  
  accounted_for_columns = c(basic_cols,
                            player_bio_data,
                            game_info,
                            misc_team_opp_info,
                            stadium_info,
                            injury_data,
                            weather_data,
                            playoff_clinching_data,
                            qb_columns,
                            rank_columns)
  
  if(predict_mode == FALSE)
  {
    accounted_for_columns = c(accounted_for_columns,'Passing_Yds', 'Rushing_Yds', 'Receiving_Yds', 'Total_Touchdowns') #response variables for each model
  }
  passing_data_all_stats_columns = pull_all_stats_columns(df = passing_data %>% select(-any_of(accounted_for_columns)))
  
  passing_data_player_current_season = passing_data_all_stats_columns[[1]]
  passing_data_player_recent_seasons = passing_data_all_stats_columns[[2]]
  passing_data_team_current_season = passing_data_all_stats_columns[[3]]
  passing_data_team_recent_seasons = passing_data_all_stats_columns[[4]]
  passing_data_opp_current_season = passing_data_all_stats_columns[[5]]
  passing_data_opp_recent_seasons = passing_data_all_stats_columns[[6]]
  
  
  
  rushing_data_all_stats_columns = pull_all_stats_columns(df = rushing_data %>% select(-any_of(accounted_for_columns)))
  
  rushing_data_player_current_season = rushing_data_all_stats_columns[[1]]
  rushing_data_player_recent_seasons = rushing_data_all_stats_columns[[2]]
  rushing_data_team_current_season = rushing_data_all_stats_columns[[3]]
  rushing_data_team_recent_seasons = rushing_data_all_stats_columns[[4]]
  rushing_data_opp_current_season = rushing_data_all_stats_columns[[5]]
  rushing_data_opp_recent_seasons = rushing_data_all_stats_columns[[6]]
  
  
  receiving_data_all_stats_columns = pull_all_stats_columns(df = receiving_data %>% select(-any_of(accounted_for_columns)))
  
  receiving_data_player_current_season = receiving_data_all_stats_columns[[1]]
  receiving_data_player_recent_seasons = receiving_data_all_stats_columns[[2]]
  receiving_data_team_current_season = receiving_data_all_stats_columns[[3]]
  receiving_data_team_recent_seasons = receiving_data_all_stats_columns[[4]]
  receiving_data_opp_current_season = receiving_data_all_stats_columns[[5]]
  receiving_data_opp_recent_seasons = receiving_data_all_stats_columns[[6]]
  
  
  touchdown_data_all_stats_columns =   pull_all_stats_columns(df = touchdown_data %>% select(-any_of(accounted_for_columns)))
  
  touchdown_data_player_current_season = touchdown_data_all_stats_columns[[1]]
  touchdown_data_player_recent_seasons = touchdown_data_all_stats_columns[[2]]
  touchdown_data_team_current_season = touchdown_data_all_stats_columns[[3]]
  touchdown_data_team_recent_seasons = touchdown_data_all_stats_columns[[4]]
  touchdown_data_opp_current_season = touchdown_data_all_stats_columns[[5]]
  touchdown_data_opp_recent_seasons = touchdown_data_all_stats_columns[[6]]
  
  
  
  
  
  
  
  
  #leftover fields are any fields that were used to derive other fields but are no longer needed:
  leftover_passing = setdiff(colnames(passing_data), c(accounted_for_columns,
                                               passing_data_player_current_season,
                                               passing_data_player_recent_seasons,
                                               passing_data_team_current_season,
                                               passing_data_team_recent_seasons,
                                               passing_data_opp_current_season,
                                               passing_data_opp_recent_seasons))
  
  #if you are ok with the leftover fields being removed:
  print(leftover_passing)
  passing_data = passing_data %>% select(-any_of(leftover_passing))
  
  #leftover fields are any fields that were used to derive other fields but are no longer needed:
  leftover_rushing = setdiff(colnames(rushing_data), c(accounted_for_columns,
                                               rushing_data_player_current_season,
                                               rushing_data_player_recent_seasons ,
                                               rushing_data_team_current_season,
                                               rushing_data_team_recent_seasons,
                                               rushing_data_opp_current_season,
                                               rushing_data_opp_recent_seasons))
  
  #if you are ok with the leftover fields being removed:
  print(leftover_rushing)
  rushing_data = rushing_data %>% select(-any_of(leftover_rushing))
  
  #leftover fields are any fields that were used to derive other fields but are no longer needed:
  leftover_receiving = setdiff(colnames(receiving_data), c(accounted_for_columns,
                                               receiving_data_player_current_season,
                                               receiving_data_player_recent_seasons ,
                                               receiving_data_team_current_season,
                                               receiving_data_team_recent_seasons,
                                               receiving_data_opp_current_season,
                                               receiving_data_opp_recent_seasons))
  
  #if you are ok with the leftover fields being removed:
  print(leftover_receiving)
  receiving_data = receiving_data %>% select(-any_of(leftover_receiving))
  
  #leftover fields are any fields that were used to derive other fields but are no longer needed:
  leftover_touchdown = setdiff(colnames(touchdown_data), c(accounted_for_columns,
                                               touchdown_data_player_current_season,
                                               touchdown_data_player_recent_seasons ,
                                               touchdown_data_team_current_season,
                                               touchdown_data_team_recent_seasons,
                                               touchdown_data_opp_current_season,
                                               touchdown_data_opp_recent_seasons))
  
  #if you are ok with the leftover fields being removed:
  print(leftover_touchdown)
  touchdown_data = touchdown_data %>% select(-any_of(leftover_touchdown))
  
  
  
  
  
  
  
  
  cols_to_force_numeric = c(passing_data_player_current_season,
                            passing_data_player_recent_seasons,
                            passing_data_team_current_season,
                            passing_data_team_recent_seasons,
                            passing_data_opp_current_season,
                            passing_data_opp_recent_seasons,
                            rushing_data_player_current_season,
                            rushing_data_player_recent_seasons,
                            rushing_data_team_current_season,
                            rushing_data_team_recent_seasons,
                            rushing_data_opp_current_season,
                            rushing_data_opp_recent_seasons,
                            receiving_data_player_current_season,
                            receiving_data_player_recent_seasons,
                            receiving_data_team_current_season,
                            receiving_data_team_recent_seasons,
                            receiving_data_opp_current_season,
                            receiving_data_opp_recent_seasons,
                            touchdown_data_player_current_season,
                            touchdown_data_player_recent_seasons,
                            touchdown_data_team_current_season,
                            touchdown_data_team_recent_seasons,
                            touchdown_data_opp_current_season,
                            touchdown_data_opp_recent_seasons)
  
  if(nrow(passing_data) > 0)
  {
    passing_data[,which(colnames(passing_data) %in% cols_to_force_numeric)] = apply(passing_data[,which(colnames(passing_data) %in% cols_to_force_numeric)], 2, as.numeric)
  }
  if(nrow(rushing_data) > 0)
  {
    rushing_data[,which(colnames(rushing_data) %in% cols_to_force_numeric)] =  apply(rushing_data[,which(colnames(rushing_data) %in% cols_to_force_numeric)], 2, as.numeric)
  }
  if(nrow(receiving_data) > 0)
  {
    receiving_data[,which(colnames(receiving_data) %in% cols_to_force_numeric)] = apply(receiving_data[,which(colnames(receiving_data) %in% cols_to_force_numeric)], 2, as.numeric)
  }
  if(nrow(touchdown_data) > 0)
  {
    touchdown_data[,which(colnames(touchdown_data) %in% cols_to_force_numeric)] = apply(touchdown_data[,which(colnames(touchdown_data) %in% cols_to_force_numeric)], 2, as.numeric)
  }
  
  
  
  
  
  return(list(passing_data,
              rushing_data,
              receiving_data,
              touchdown_data,
              list(basic_cols = intersect(basic_cols, colnames(passing_data)),
                   player_bio_data = intersect(player_bio_data, colnames(passing_data)),
                   game_info = intersect(game_info, colnames(passing_data)),
                   stadium_info = intersect(stadium_info, colnames(passing_data)),
                   injury_data = intersect(injury_data, colnames(passing_data)),
                   weather_data = intersect(weather_data, colnames(passing_data)),
                   playoff_clinching_data = intersect(playoff_clinching_data, colnames(passing_data)),
                   qb_columns = intersect(qb_columns, colnames(passing_data)),
                   rank_columns = intersect(rank_columns, colnames(passing_data)),
                   passing_data_player_current_season = passing_data_player_current_season,
                   passing_data_player_recent_seasons= passing_data_player_recent_seasons,
                   passing_data_team_current_season = passing_data_team_current_season,
                   passing_data_team_recent_seasons = passing_data_team_recent_seasons,
                   passing_data_opp_current_season = passing_data_opp_current_season,
                   passing_data_opp_recent_seasons= passing_data_opp_recent_seasons),
              list(basic_cols = intersect(basic_cols, colnames(rushing_data)),
                   player_bio_data = intersect(player_bio_data, colnames(rushing_data)),
                   game_info = intersect(game_info, colnames(rushing_data)),
                   stadium_info = intersect(stadium_info, colnames(rushing_data)),
                   injury_data = intersect(injury_data, colnames(rushing_data)),
                   weather_data = intersect(weather_data, colnames(rushing_data)),
                   playoff_clinching_data = intersect(playoff_clinching_data, colnames(rushing_data)),
                   qb_columns = intersect(qb_columns, colnames(rushing_data)),
                   rank_columns = intersect(rank_columns, colnames(rushing_data)),
                   rushing_data_player_current_season = rushing_data_player_current_season,
                   rushing_data_player_recent_seasons= rushing_data_player_recent_seasons,
                   rushing_data_team_current_season = rushing_data_team_current_season,
                   rushing_data_team_recent_seasons = rushing_data_team_recent_seasons,
                   rushing_data_opp_current_season = rushing_data_opp_current_season,
                   rushing_data_opp_recent_seasons= rushing_data_opp_recent_seasons),
              list(basic_cols = intersect(basic_cols, colnames(receiving_data)),
                   player_bio_data = intersect(player_bio_data, colnames(receiving_data)),
                   game_info = intersect(game_info, colnames(receiving_data)),
                   stadium_info = intersect(stadium_info, colnames(receiving_data)),
                   injury_data = intersect(injury_data, colnames(receiving_data)),
                   weather_data = intersect(weather_data, colnames(receiving_data)),
                   playoff_clinching_data = intersect(playoff_clinching_data, colnames(receiving_data)),
                   qb_columns = intersect(qb_columns, colnames(receiving_data)),
                   rank_columns = intersect(rank_columns, colnames(receiving_data)),
                   receiving_data_player_current_season = receiving_data_player_current_season,
                   receiving_data_player_recent_seasons= receiving_data_player_recent_seasons,
                   receiving_data_team_current_season = receiving_data_team_current_season,
                   receiving_data_team_recent_seasons = receiving_data_team_recent_seasons,
                   receiving_data_opp_current_season = receiving_data_opp_current_season,
                   receiving_data_opp_recent_seasons= receiving_data_opp_recent_seasons),
              list(basic_cols = intersect(basic_cols, colnames(touchdown_data)),
                   player_bio_data = intersect(player_bio_data, colnames(touchdown_data)),
                   game_info = intersect(game_info, colnames(touchdown_data)),
                   stadium_info = intersect(stadium_info, colnames(touchdown_data)),
                   injury_data = intersect(injury_data, colnames(touchdown_data)),
                   weather_data = intersect(weather_data, colnames(touchdown_data)),
                   playoff_clinching_data = intersect(playoff_clinching_data, colnames(touchdown_data)),
                   qb_columns = intersect(qb_columns, colnames(touchdown_data)),
                   rank_columns = intersect(rank_columns, colnames(touchdown_data)),
                   touchdown_data_player_current_season = touchdown_data_player_current_season,
                   touchdown_data_player_recent_seasons= touchdown_data_player_recent_seasons,
                   touchdown_data_team_current_season = touchdown_data_team_current_season,
                   touchdown_data_team_recent_seasons = touchdown_data_team_recent_seasons,
                   touchdown_data_opp_current_season = touchdown_data_opp_current_season,
                   touchdown_data_opp_recent_seasons= touchdown_data_opp_recent_seasons)
              
              ))
}




