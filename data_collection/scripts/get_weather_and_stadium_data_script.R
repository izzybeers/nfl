
source('data_collection/scripts/global.R')
library(tidyr)
library(purrr)
library(furrr)
library(dplyr)

plan(multisession, workers = 4)



get_weather_and_stadium_data = function(games, forecast = FALSE)
{
  t1 = Sys.time()
  games = games %>%
    mutate(Stadium = ifelse(International == 1, NA, ifelse(Game_Location == 'Away', Opp, Team)),
           Date = paste0(Season, '-', str_pad(match(word(Date, 1), month.name), 2, 'left', '0'), '-', str_pad(word(Date, 2), 2, 'left', '0'))) %>%
    select(Season, Week, Date, Time_of_Day, Stadium, Team, Opp, International) %>% distinct() %>%
    left_join(team_lookup_table %>%
                select(Team, Grass_Type, Roof, Altitude, Stadium_Capacity, Loudest_Stadiums, Coast, weather_station_link, Latitude, Longitude) %>% rename('Stadium_Coast' = 'Coast'), join_by('Stadium' == 'Team')) %>%
    left_join(team_lookup_table %>%
                select(Team, Coast, Conference, Division, Used_To_Cold, Used_To_Hot, Roof, Grass_Type) %>%
                rename('Home_Roof' = 'Roof', 'Home_Grass_Type' = 'Grass_Type'),
              join_by('Team' == 'Team')) %>%
    left_join(team_lookup_table %>%
                select(Team, Coast, Conference, Division, Used_To_Cold, Used_To_Hot) %>%
                rename_with(~ paste0("Opp_",.), .cols = -Team),
              join_by('Opp' == 'Team')
    ) %>%
    mutate(Same_Division = ifelse(Division == Opp_Division, 1, 0),
           Same_Conference = ifelse(Conference == Opp_Conference, 1, 0),
           Long_Travel = ifelse((Stadium_Coast != '' & Coast != '' & Stadium_Coast != Coast) | International == 1, 1, 0),
           Familiar_Grass_Type = ifelse(Grass_Type == Home_Grass_Type, 1, 0),
           Familiar_Roof_Type = ifelse(Roof == Home_Roof, 1, 0)) %>%
    select(-Home_Grass_Type, -Home_Roof)
  
  if(forecast == FALSE)
  {
    nondome_games_with_weather = games %>% filter(Roof == 'Open') %>%
      select(Season, Week, Date, Time_of_Day, Stadium, weather_station_link) %>% distinct() %>%
      mutate(weather = pmap(list(Date, Time_of_Day, weather_station_link, Stadium),
                            ~ get_historical_weather (..1, ..2, station_link = ..3, stadium = ..4))) %>%
      unnest_wider(weather, names_sep = "_")
  } else {
      
    api_key = 'e4042a3d60cf937c4064161a86429bc2'
    
    games = games %>%
      mutate(weather_forecast_time = case_when(
        Time_of_Day == 'Early Window' ~ '15:00:00', #don't need to worry about tz because it will assign NY timezone in the next step
        Time_of_Day == 'Late Window' ~'18:00:00',
        Time_of_Day == 'Night' ~ '22:00:00',
        Time_of_Day == 'Morning' ~ '11:00:00' #probably would never happen since these are international
    ),
    posix_timestamp_game =  as.POSIXct(paste(Date, weather_forecast_time), tz = "America/New_York"))
    
    games = games %>% filter(posix_timestamp_game > Sys.time())
    
    nondome_games_with_weather = games %>% filter(Roof == 'Open') %>%
      select(Season, Week, Date, posix_timestamp_game, Stadium, Latitude, Longitude) %>% distinct() %>%
      mutate(weather = future_pmap(list(posix_timestamp_game, Latitude, Longitude, api_key),
                                   ~ get_forecasted_weather(timestamp = ..1, lat = ..2, long = ..3, api_key = ..4))) %>%
      unnest_wider(weather, names_sep = "_")
  }
    games_with_weather = games %>%
      left_join(nondome_games_with_weather %>% select(Stadium, Week, Season, weather_approx_temperature, weather_approx_visibility, weather_approx_wind_speed), join_by('Stadium' == 'Stadium', 'Week' == 'Week', 'Season' == 'Season')) %>%
      select(-weather_station_link, -Date, -Time_of_Day, -Opp_Coast, -Opp_Conference, -Opp_Division) %>%
      arrange(Season, Week)
  
  Sys.time() - t1
  
  return(games_with_weather)
 
}








