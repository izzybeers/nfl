library(shiny)
library(shinyWidgets)
library(httr)
library(googlesheets4)
library(dplyr)
library(jsonlite)
library(shinycssloaders)
library(furrr)
library(googlesheets4)
library(DT)
library(shinyjs)
library(lubridate)
library(quadprog)
library(stringr)
library(Matrix)
gs4_auth(cache = ".secrets", email = "izzyb961@gmail.com")

# setwd("~/nfl")
# source('data_collection/scripts/global.R')


sheet_id = '19sWOOPFI37WaR5lmlYS6UUrV-0dmTn0iTFqUp26sfGI'
link_prefix =  'https://docs.google.com/spreadsheets/d/e/2PACX-1vTyIaWWovW2YUP1-JxYpg9ZHpF7a2i_7AEVan5ptaBBiwj6gwYp0STpE8HvYILR190HTrOFt2GMyUqn/pub?gid='
link_suffix = "&single=true&output=csv"
passing_gid = '0'
rushing_gid = '1041188467'
receiving_gid = '1971939369'
touchdown_gid = '1979813660'
gid_bets_placed = '95780958'
gid_bet_results = '1472501972'
team_lookup_table = read.csv('https://docs.google.com/spreadsheets/d/1DSSz4X-3LLAarRlBRtuMsGJ1hh2FDdVeHJZFdpZGW0A/export?format=csv&gid=0')

min_return_portfolio_optimization = 0.5

correlations = read.csv('https://docs.google.com/spreadsheets/d/e/2PACX-1vT9_LcNO2d8L5kzbJQZZti9kxfAZRFRAl2oJz5WlpusfvL1txbkc8OU6BSlB54TA9HCBHRlIxi9MpuT/pub?gid=956130726&single=true&output=csv')
previous_recs = read.csv('https://docs.google.com/spreadsheets/d/e/2PACX-1vTyIaWWovW2YUP1-JxYpg9ZHpF7a2i_7AEVan5ptaBBiwj6gwYp0STpE8HvYILR190HTrOFt2GMyUqn/pub?gid=277208139&single=true&output=csv')
depth_charts = read.csv('https://docs.google.com/spreadsheets/d/e/2PACX-1vTyIaWWovW2YUP1-JxYpg9ZHpF7a2i_7AEVan5ptaBBiwj6gwYp0STpE8HvYILR190HTrOFt2GMyUqn/pub?gid=594515538&single=true&output=csv') %>% select(player_id, Depth)

if(!is.null(previous_recs) && nrow(previous_recs) > 0)
{
  most_recent_save = max(as.POSIXct(previous_recs$run_time,format = "%Y-%m-%d %I:%M %p"))
} else {
  most_recent_save = NULL
}

extra_passing_info = read.csv('https://docs.google.com/spreadsheets/d/e/2PACX-1vTyIaWWovW2YUP1-JxYpg9ZHpF7a2i_7AEVan5ptaBBiwj6gwYp0STpE8HvYILR190HTrOFt2GMyUqn/pub?gid=1528317693&single=true&output=csv')
extra_rushing_info = read.csv('https://docs.google.com/spreadsheets/d/e/2PACX-1vTyIaWWovW2YUP1-JxYpg9ZHpF7a2i_7AEVan5ptaBBiwj6gwYp0STpE8HvYILR190HTrOFt2GMyUqn/pub?gid=1396923583&single=true&output=csv')
extra_receiving_info = read.csv('https://docs.google.com/spreadsheets/d/e/2PACX-1vTyIaWWovW2YUP1-JxYpg9ZHpF7a2i_7AEVan5ptaBBiwj6gwYp0STpE8HvYILR190HTrOFt2GMyUqn/pub?gid=942194055&single=true&output=csv')
extra_touchdown_info = read.csv('https://docs.google.com/spreadsheets/d/e/2PACX-1vTyIaWWovW2YUP1-JxYpg9ZHpF7a2i_7AEVan5ptaBBiwj6gwYp0STpE8HvYILR190HTrOFt2GMyUqn/pub?gid=864263040&single=true&output=csv')


#need this if the app.R file can't access global.R in a different directory:
passing_numbers = seq(150,360,30)
rushing_numbers = c(25,seq(40,140,20))
receiving_numbers = c(25,seq(40,140,20))

passing_response = c()
for(n in passing_numbers)
{
  passing_response = c(passing_response, paste0('Passing_Yds_', n))
}
rushing_response = c()
for(n in rushing_numbers)
{
  rushing_response = c(rushing_response, paste0('Rushing_Yds_', n))
}
receiving_response = c()
for(n in rushing_numbers)
{
  receiving_response = c(receiving_response, paste0('Receiving_Yds_', n))
}
touchdown_response = 'Anytime_Touchdown'


pull_prediction_data = function(index, gids, responses)
{
  gid = gids[index]
  response_list = responses[[index]]
  td = any(response_list == 'Anytime_Touchdown')
  link =  paste0(link_prefix, gid, link_suffix)
  res = read.csv(link)
  if(nrow(res) > 0)
  {
    if(td == TRUE)
    {
      res$label = 'Anytime TD Scorer'
    } else {
      res$label = paste0(str_extract(res$Response, '[0-9]+'), '+')
    }
  }
  
  #get most recent update:
  res = res %>% filter(Response %in% response_list) %>%
    group_by(Season, Week, Response, player_id)  %>%
    mutate(updateTime = as.POSIXct(
      as.character(updateTime),
      format = "%Y-%m-%d %I:%M %p",
      tz = "America/New_York"
    )) %>%
    slice_max(order_by = updateTime, n = 1, with_ties = FALSE, na_rm = TRUE) %>%
    ungroup()
  return(res)
}



get_props <- function(bet_category) {
  # URL for NFL event group (replace "88808" if the event group ID changes)
  bet_id = case_when(
    bet_category == 'Receiving' ~ '16570',
    bet_category == 'Rushing' ~ '16571',
    bet_category == 'Touchdown' ~ '12438',
    bet_category == 'Passing' ~ '16569'
  )
  base_url = paste0("https://sportsbook-nash.draftkings.com/sites/US-NJ-SB/api/sportscontent/controldata/league/leagueSubcategory/v1/markets?isBatchable=false&templateVars=%2C",bet_id,"&eventsQuery=%24filter%3DleagueId%20eq%20%2788808%27%20AND%20clientMetadata%2FSubcategories%2Fany%28s%3A%20s%2FId%20eq%20%27", bet_id, "%27%29&marketsQuery=%24filter%3DclientMetadata%2FsubCategoryId%20eq%20%27",bet_id,"%27%20AND%20tags%2Fall%28t%3A%20t%20ne%20%27SportcastBetBuilder%27%29&include=Events&entity=events")
  lines = tryCatch({
    fromJSON(content(GET(base_url), as = "text", encoding = "UTF-8"), flatten = TRUE)$selections %>% select(marketId, label, `displayOdds.american`) %>%
    left_join(fromJSON(content(GET(base_url), as = "text", encoding = "UTF-8"))$markets %>% select(id,name), join_by(marketId == id)) %>%
    rename('Odds' = `displayOdds.american`)
  }, error = function(e) {
    print(e$message)
    return(NULL)
  })
  if(!is.null(lines))
  {
    lines$Odds <- gsub("\u2212", "-", lines$Odds)
    if(bet_category == 'Touchdown')
    {
      lines = lines %>% filter(name == 'Anytime TD Scorer') %>% rename('Type' = 'name', 'name' = 'label') %>% mutate(label = 'Anytime TD Scorer')
      lines = lines %>% mutate(name = gsub(paste(bet_category, ifelse(bet_category == 'Touchdown', 'Anytime TD Scorer', 'Yards')), '', name) %>% trimws())
    } else {
      lines = lines %>% mutate(Type = bet_category)
      lines = lines %>% mutate(name = gsub(paste(bet_category, 'Yards'), '', name) %>% trimws())
    }
    
    lines = lines %>% mutate(profit_per_100 = ifelse(as.numeric(Odds) < 0, (100*100/abs(as.numeric(Odds))), as.numeric(Odds)))
  
    return(lines)
  } else{
    return(NULL)
  }
}

clean_names = function(name)
{
  return(tolower(name) %>% str_remove_all("[[:punct:]]+") %>% str_remove("\\b(jr|sr|i{1,3}|iv|v|vi{1,3}|ix|x|xi{1,3})\\b") %>% str_squish() %>% trimws())
}

join_preds_and_props = function(preds, props)
{
  preds$Type = ifelse(preds$Response == 'Anytime_Touchdown', 'Anytime TD Scorer', sapply(strsplit(preds$Response, '_'), function(x) x[1]))
  joined = preds %>% mutate(cleaned_names = clean_names(names)) %>% left_join(props %>% mutate(cleaned_name = clean_names(name)), join_by('cleaned_names' == 'cleaned_name', 'label' == 'label', 'Type' == 'Type')) %>% filter(!is.na(marketId)) %>%
    mutate(Betting_Line_Implied_Prob = ifelse(as.numeric(Odds) < 0, (-1)*as.numeric(Odds) / ((-1)*as.numeric(Odds) + 100), 100 / (as.numeric(Odds) + 100)),
           Timeslot = paste(Day, Time_of_Day)) %>%
    rename('Player' = 'names') %>%
    filter(as.POSIXct(paste0(Date, ", ", Season, " ", Time),format = "%B %d, %Y %I:%M %p",tz = "America/New_York") > Sys.time() + 3600) %>%
    mutate(posix_timestamp = as.POSIXct(paste0(Date, ", ", Season, " ", Time),format = "%B %d, %Y %I:%M %p",tz = "America/New_York")) %>%
    select(Player, Position, Starting, Type, label, Team, Opp, Date, Time, posix_timestamp, Timeslot, Odds, Model_Probability, Betting_Line_Implied_Prob, Expected_Accuracy, profit_per_100)
  return(joined)
}


display_extra_info = function(df, bet_type, player_name, week, season)
{
  print(week)
  print(season)
  print(extra_rushing_info)
  print(bet_type)
  print(player_name)
  if(bet_type == 'Passing')
  {
    print('entered passing')
    extra_info = df %>% filter(Name == player_name)
    extra_info_min_year = paste0('In NFL since: ', extra_info$min_year)
    extra_info_draft = ifelse(!is.na(extra_info$draft_round),
                              paste0('Drafted Round ', extra_info$draft_round, ' (Pick ', extra_info$draft_pick, ') to team ', team_lookup_table$FullName[team_lookup_table$Team == extra_info$original_draft_team]),
                              'Undrafted, or no draft info available')
    extra_info_home = ifelse(extra_info$International == 1, 'International Game',
                             ifelse(extra_info$Home == 1, 'Home Game', 'Away Game'))
    extra_info_depth = paste('Depth:', extra_info$Depth)
    if(week > 1)
    {
      if(!is.na(extra_info$Pct_Active) && extra_info$Pct_Active > 0)
      {
        extra_info_pct_active_gs = paste0('This season, Active for ', round(100*extra_info$Pct_Active),'% of games, and Starter for ', round(100*extra_info$Pct_GS), '% of games')
        opp_defense_passyd  = round(extra_info$Opp_Avg_Defense_PassY_Allowed)
        opp_defense_passyd_score = ifelse(opp_defense_passyd <= 130, 'Very Good Pass Defense',
                                          ifelse(opp_defense_passyd <= 202, 'Pretty Good Pass Defense',
                                                 ifelse(opp_defense_passyd <= 241, 'Okay Pass Defense',
                                                        ifelse(opp_defense_passyd <= 320, 'Not Good Pass Defense',
                                                               'Terrible Pass Defense'))))
        extra_info_stats_this_season = paste0('Is the team\'s QB1: ', ifelse(extra_info$Is_Qb1 == 1, 'Yes', 'No'), '<br>',
                                              'Passing Yds Previous Game: ', extra_info$Passing_Yds_Lag1, '<br>',
                                              'This Season, Average Passing Yds Per Game: ', round(extra_info$Avg_Passing_Yds), '<br>',
                                              'This Season, Average Passing Attempts Per Game: ', round(extra_info$Avg_Passing_Att, 1), '<br>',
                                              'This Season, Average Passing 1st Downs Per Game: ', round(extra_info$Avg_Passing_1D, 1), '<br>',
                                              'This Season, Average Passing Completions Per Game: ', round(extra_info$Avg_Passing_Cmp,1), '<br>',
                                              'This Season, Average Passing TD Per Game: ', round(extra_info$Avg_Passing_TD,1), '<br>',
                                              'This Season, Opponent\'s Average Passing Yards Allowed Per Game: ', opp_defense_passyd, ' (', opp_defense_passyd_score,')', '<br>')
      } else {
        extra_info_stats_this_season = 'Player had no active games this season, so no current stats to show.'
      }
    } else {
      extra_info_stats_this_season = 'Since it is only week 1, there are no current season stats to show.'
    }
    if(extra_info$min_year < season) {
      if(!is.na(extra_info$Last_Season_Pct_Active) && extra_info$Last_Season_Pct_Active > 0)
      {
        extra_info_stats_last_season = paste0('Last Season, Percent of Games Active: ', round(100*extra_info$Last_Season_Pct_Active), '%<br>',
                                              'Last Season, Median Passing Yds Per Game: ', round(extra_info$Last_Season_Passing_Yds_median), '<br>',
                                              'Last Season, Passing Completion Percent: ', round(100*extra_info$Last_Season_Passing_Comp_Pct), '%<br>',
                                              'Last Season, Average Passing TD Per Game: ', round(extra_info$Last_Season_Passing_TD_mean,1), '<br>')
      } else {
        extra_info_stats_last_season = 'Player had no active games last season, or last season stats unavailable.'
      }
    } else {
      extra_info_stats_last_season = 'This is the player\'s first season in the NFL, so no previous season stats to show.'
    }
    
  } else if(bet_type == 'Rushing')
  {
    print('entered rushing')
    extra_info = df %>% filter(Name == player_name)
    print(extra_info)
    extra_info_min_year = paste0('In NFL since: ', extra_info$min_year)
    extra_info_draft = ifelse(!is.na(extra_info$draft_round),
                              paste0('Drafted Round ', extra_info$draft_round, ' (Pick ', extra_info$draft_pick, ') to team ', team_lookup_table$FullName[team_lookup_table$Team == extra_info$original_draft_team]),
                              'Undrafted, or no draft info available')
    extra_info_home = ifelse(extra_info$International == 1, 'International Game',
                             ifelse(extra_info$Home == 1, 'Home Game', 'Away Game'))
    extra_info_depth = paste('Depth:', extra_info$Depth)
    if(week > 1)
    {
      extra_info_pct_active_gs = paste0('This season, Active for ', round(100*extra_info$Pct_Active),'% of games, and Starter for ', round(100*extra_info$Pct_GS), '% of games')
      opp_defense_rushyd  = round(extra_info$Opp_Avg_Defense_RushY_Allowed)
      opp_defense_rushyd_score = ifelse(opp_defense_rushyd <= 58, 'Very Good Rush Defense',
                                        ifelse(opp_defense_rushyd <= 99, 'Pretty Good Rush Defense',
                                               ifelse(opp_defense_rushyd <= 124, 'Okay Rush Defense',
                                                      ifelse(opp_defense_rushyd <= 185, 'Not Good Rush Defense',
                                                             'Terrible Rush Defense'))))
      if(!is.na(extra_info$Pct_Active) && extra_info$Pct_Active > 0)
      {
        extra_info_stats_this_season = paste0('This Season, Average Rushing Yds Per Game: ',  round(extra_info$Avg_Rushing_Yds,1),'<br>',
                                              'This Season, Average Rushing Attempts Per Game: ', round(extra_info$Avg_Rushing_Att,1),'<br>',
                                              'This Season, Average Rushing 1st Downs Per Game: ', round(extra_info$Avg_Rushing_1D, 1), '<br>',
                                              'Rushing Yds Previous Game: ', extra_info$Rushing_Yds_Lag1, '<br>',
                                              'This Season, Opponent\'s Defense Avg Rush Yards Allowed Per Game: ', opp_defense_rushyd, ' (', opp_defense_rushyd_score, ')')
      } else {
        extra_info_stats_this_season = 'Player had no active games this year, so there are no current season stats to show.'
      }
    } else {
      extra_info_stats_this_season = 'Since it is only week 1, there are no current season stats to show.'
    }
    if(extra_info$min_year < season) {
      if(!is.na(extra_info$Last_Season_Pct_Active) && extra_info$Last_Season_Pct_Active > 0)
      {
        extra_info_stats_last_season = paste0('Last Season, Percent of Games Active: ', round(100*extra_info$Last_Season_Pct_Active), '%<br>',
                                              'Last Season, Average Rushing Yds Per Game: ', round(extra_info$Last_Season_Rushing_Yds_mean, 1), '<br>',
                                              'Last Season, Highest Rushing Yds in a Game: ', round(extra_info$Last_Season_Rushing_Yds_max), '<br>',
                                              'Last Season, Average Rushing TD Per Game: ', round(extra_info$Last_Season_Rushing_TD_mean,1), '<br>',
                                              'Last Season, Average Rushing Attempts Per Game: ', round(extra_info$Last_Season_Rushing_Att_mean,1), '<br>')
        
      } else {
        extra_info_stats_last_season = 'Player had no active games last year, so no previous season stats to show.'
      }
    } else {
      extra_info_stats_last_season = 'This is the player\'s first season in the NFL, so no previous season stats to show.'
    }
    
    print(extra_info_min_year)
    print(extra_info_draft)
    print(extra_info_home)
    print(extra_info_stats_this_season)
    print(extra_info_stats_last_season)
  } else if(bet_type == 'Receiving')
  {
    print('entered receiving')
    extra_info = df  %>% filter(Name == player_name)
    extra_info_min_year = paste0('In NFL since: ', extra_info$min_year)
    extra_info_draft = ifelse(!is.na(extra_info$draft_round),
                              paste0('Drafted Round ', extra_info$draft_round, ' (Pick ', extra_info$draft_pick, ') to team ', team_lookup_table$FullName[team_lookup_table$Team == extra_info$original_draft_team]),
                              'Undrafted, or no draft info available')
    extra_info_home = ifelse(extra_info$International == 1, 'International Game',
                             ifelse(extra_info$Home == 1, 'Home Game', 'Away Game'))
    extra_info_depth = paste('Depth:', extra_info$Depth)
    if(week > 1)
    {
      extra_info_pct_active_gs = paste0('This season, Active for ', round(100*extra_info$Pct_Active),'% of games, and Starter for ', round(100*extra_info$Pct_GS), '% of games')
      if(!is.na(extra_info$Pct_Active) && extra_info$Pct_Active > 0)
      {
        extra_info_stats_this_season = paste0('This Season, Average Receiving Yds Per Game: ',  round(extra_info$Avg_Receiving_Yds,1), '<br>',
                                              'This Season, Average Targets Per Game: ',  round(extra_info$Avg_Receiving_Tgt,1), '<br>',
                                              'This Season, Average Receiving 1st Downs Per Game: ', round(extra_info$Avg_Receiving_1D, 1), '<br>',
                                              'This Season, Average Receptions Per Game: ', round(extra_info$Avg_Receiving_Rec,1))
      } else {
        extra_info_stats_this_season = 'Player had no active games this year, so there are no current season stats to show.'
      }
    } else {
      extra_info_stats_this_season = 'Since it is only week 1, there are no current season stats to show.'
    }
    if(extra_info$min_year < season) {
      if(!is.na(extra_info$Last_Season_Pct_Active) && extra_info$Last_Season_Pct_Active > 0)
      {
        extra_info_stats_last_season = paste0('Last Season, Percent of Games Active: ', round(100*extra_info$Last_Season_Pct_Active), '%<br>',
                                              'Last Season, Average Receiving Yds Per Game: ', round(extra_info$Last_Season_Receiving_Yds_mean, 1), '<br>',
                                              'Last Season, Average Targets Per Game: ', round(extra_info$Last_Season_Receiving_Tgt_mean, 1), '<br>',
                                              'Last Season, Average Receiving 1st Downs Per Game: ', round(extra_info$Last_Season_Receiving_1D_mean, 2), '%<br>',
                                              'Last Season, Average Receiving Yards Before Catch Per Game: ', round(extra_info$Last_Season_Receiving_YBC_mean,1), '<br>',
                                              'Last Season, Highest Receiving Yards Before Catch Per Game: ', round(extra_info$Last_Season_Receiving_YBC_max), '<br>')
        
      } else {
        extra_info_stats_last_season = 'Player had no active games last year, so no previous season stats to show.'
      }
    } else {
      extra_info_stats_last_season = 'This is the player\'s first season in the NFL, so no previous season stats to show.'
    }
  } else {
    print('entered touchdown')
    extra_info = df %>% filter(Name == player_name)
    extra_info_min_year = paste0('In NFL since: ', extra_info$min_year)
    extra_info_draft = ifelse(!is.na(extra_info$draft_round),
                              paste0('Drafted Round ', extra_info$draft_round, ' (Pick ', extra_info$draft_pick, ') to team ', team_lookup_table$FullName[team_lookup_table$Team == extra_info$original_draft_team]),
                              'Undrafted, or no draft info available')
    extra_info_home = ifelse(extra_info$International == 1, 'International Game',
                             ifelse(extra_info$Home == 1, 'Home Game', 'Away Game'))
    extra_info_depth = paste('Depth:', extra_info$Depth)
    if(week > 1)
    {
      extra_info_pct_active_gs = paste0('This season, Active for ', round(100*extra_info$Pct_Active),'% of games, and Starter for ', round(100*extra_info$Pct_GS), '% of games')
      if(!is.na(extra_info$Pct_Active) && extra_info$Pct_Active > 0)
      {
        extra_info_stats_this_season = paste0('This Season, Average Touchdowns (Rushing/Receiving) Per Game: ',  round(extra_info$Avg_Total_Touchdowns,1), '<br>',
                                              'This Season, Average Targets Per Game: ',  round(extra_info$Avg_Receiving_Tgt,1), '<br>',
                                              'This Season, Average Receptions Per Game: ', round(extra_info$Avg_Receiving_Rec, 1), '<br>',
                                              'This Season, Average Receiving Yards After Catch Per Game: ', round(extra_info$Avg_Receiving_YAC, 1), '<br>',
                                              'This Season, Average Rushing Yards After Catch Per Game: ', round(extra_info$Avg_Rushing_YAC, 1), '<br>')
      } else {
        extra_info_stats_this_season = 'Player had no active games this year, so there are no current season stats to show.'
      }
    } else {
      extra_info_stats_this_season = 'Since it is only week 1, there are no current season stats to show.'
    }
    if(extra_info$min_year < season) {
      if(!is.na(extra_info$Last_Season_Pct_Active) && extra_info$Last_Season_Pct_Active > 0)
      {
        extra_info_stats_last_season = paste0('Last Season, Percent of Games Active: ', round(100*extra_info$Last_Season_Pct_Active), '%<br>',
                                              'Last Season, Average Touchdowns (Rushing/Receiving) Per Game: ', round(extra_info$Last_Season_Total_Touchdowns_mean, 1), '<br>',
                                              'Last Season, Standard Deviation of Touchdowns (Rushing/Receiving) Per Game: ', round(extra_info$Last_Season_Total_Touchdowns_sd, 2), '<br>',
                                              'Last Season, Average Receiving 1st Downs Per Target: ', round(extra_info$Last_Season_Receiving_1D_Per_Tgt, 2), '%<br>',
                                              'Last Season, Average Receiving Yards After Catch Per Game : ', extra_info$Last_Season_Receiving_YAC_max, '<br>')
        
      } else {
        extra_info_stats_last_season = 'Player had no active games last year, so no previous season stats to show.'
      }
    } else {
      extra_info_stats_last_season = 'This is the player\'s first season in the NFL, so no previous season stats to show.'
    }
  }
  print(c(extra_info_min_year,
          extra_info_draft,
          extra_info_home,
          extra_info_pct_active_gs,
          extra_info_stats_this_season,
          extra_info_stats_last_season,
          extra_info_depth))
  return(c(extra_info_min_year,
           extra_info_draft,
           extra_info_home,
           extra_info_pct_active_gs,
           extra_info_stats_this_season,
           extra_info_stats_last_season,
           extra_info_depth))
}




#read.csv to get prediction df
#get current week and year from there

ui <- fluidPage(
  titlePanel('NFL Prop Bet Recommender'),
  useShinyjs(),
  tags$script(HTML("
     document.addEventListener('DOMContentLoaded', function() {
                    var tab = document.querySelector('#results')
                     if(tab)
                     {
                        tab.addEventListener('click', function(e) {
                        console.log('you clicked table')
                        row = e.target.closest('tr')
                        console.log(row)
                        cells = row.querySelectorAll('td')
                        console.log(cells)
                        headers = tab.querySelectorAll('thead th') //column names
                        headerNames = Array.from(headers).map(h=>h.innerText)//go through each header item and get the inner text of the header
                        console.log(headerNames)
                        var nameIndex = headerNames.findIndex(h => h == 'Player')
                        var betIndex = headerNames.findIndex(h => h == 'Type')
                        var betLabelIndex = headerNames.findIndex(h => h == 'label')
                        var betOddsIndex = headerNames.findIndex(h => h == 'Odds')
                        nameValue = cells[nameIndex].innerText
                        betValue = cells[betIndex].innerText
                        betLabelValue = cells[betLabelIndex].innerText
                        betOddsValue = cells[betOddsIndex].innerText
                        console.log(nameValue)
                        console.log(betValue)
                        console.log(betLabelValue)
                        console.log(betOddsValue)
                        Shiny.setInputValue('click', {
                          name: nameValue,
                          bet: betValue,
                          label: betLabelValue,
                          odds: betOddsValue
                        })
                       })
                     }
     })
                     ")),
  
  tags$script(HTML("
     document.addEventListener('DOMContentLoaded', function() {
                    var tab = document.querySelector('#portfolio_optimization_output')
                     if(tab)
                     {
                        tab.addEventListener('click', function(e) {
                        console.log('you clicked portfolio optimization table')
                        row = e.target.closest('tr')
                        console.log(row)
                        cells = row.querySelectorAll('td')
                        console.log(cells)
                        headers = tab.querySelectorAll('thead th') //column names
                        headerNames = Array.from(headers).map(h=>h.innerText)//go through each header item and get the inner text of the header
                        console.log(headerNames)
                        rownameCell = row.querySelector('td').textContent
                        var betAmountIndex = headerNames.findIndex(h => h == 'BetAmount')
                        var toPayIndex = headerNames.findIndex(h => h == 'ToPay')
                        var betAmountValue = cells[betAmountIndex].innerText
                        var toPayValue = cells[toPayIndex].innerText
                        Shiny.setInputValue('click_portfolio_row', {
                          name: rownameCell,
                          amount: betAmountValue,
                          topay: toPayValue
                        })
                       })
                     }
     })
                     ")),
  
  tabsetPanel(
    tabPanel('Bet Recommendations',
             sidebarLayout(
               sidebarPanel(
                 width = 4,
                 uiOutput("portfolio_optimization_heading"),
                 uiOutput("max_bets_slider_ui"),
                 uiOutput("odds_range_slider_ui"),
                 uiOutput("portfolio_optimization_bet_amt_ui"),
                 uiOutput("remove_players_ui"),
                 uiOutput("portfolio_optimization_button_ui"),
                 textOutput("no_bets"),
                 uiOutput("riskier_alternative_ui"),
                 dataTableOutput("portfolio_optimization_output"),
                 tags$br(),
                 uiOutput("portfolio_return"),
                 uiOutput("portfolio_risk"),
                 tags$br(),
                 uiOutput("optimization_instructions"),
                 tags$br(),
                 uiOutput("log_portfolio_optimization_bet") %>% withSpinner()
               ),
               mainPanel(
                   width = 8,
                   uiOutput("error_message"),
                   div(style = 'font-weight: bold; font-size: 16px;', textOutput("header")),
                   textOutput("header2"),
                   tags$br(),
                   # uiOutput("bet_size_ui"),
                   fluidRow(
                     column(3, uiOutput("type_filter_ui")),
                     column(3, uiOutput("timeslot_filter_ui")),
                     column(3, uiOutput("team_filter_ui")),
                     column(3, uiOutput("model_accuracy_ui")),
                     ),
                   uiOutput("model_probability_slider_ui"),
                   tags$br(),
                   uiOutput("refresh"),
                   div(id = 'refresh_message', textOutput("refreshing_message")),
                   tags$br(),
                   uiOutput("instructions"),
                   textOutput("weather_warning"),
                   tags$br(),
                   dataTableOutput('results') %>% withSpinner()
                 )
             )
             
    ),
    tabPanel('Update Bet Results',
             tags$br(),
             uiOutput("refresh_bet_updates"),
             div(id = 'refresh_bet_updates_message', textOutput("refreshing_bet_updates_message")),
             uiOutput("bettor_selection_ui"),
             tags$br(),
             uiOutput("bet_radio_options") %>% withSpinner(),
             tags$br(),
             uiOutput("save_button")
    )
  )
)

server <- function(input, output, session) {
  shinyjs::hide("refresh_message")
  shinyjs::hide("refresh_bet_updates_message")
  gids = c(passing_gid, rushing_gid, receiving_gid, touchdown_gid)
  responses = list(passing_response, rushing_response, receiving_response, touchdown_response)
  
  predictions = future_map(.x = 1:length(gids), #index, for parallel processing
                           .f = pull_prediction_data,
                           gids = gids, responses = responses) %>%
    bind_rows()
  
  print(predictions)
  
  
  latest_season = max(predictions$Season)
  latest_week = max(predictions$Week)
  latest_update_time = max(predictions$updateTime) %>% format("%Y-%m-%d %I:%M %p", tz = "America/New_York")

  extra_passing_info = extra_passing_info %>% filter(Week == latest_week) %>% left_join(depth_charts, join_by('player_id'))
  extra_rushing_info = extra_rushing_info %>% filter(Week == latest_week) %>% left_join(depth_charts, join_by('player_id'))
  extra_receiving_info = extra_receiving_info %>% filter(Week == latest_week) %>% left_join(depth_charts, join_by('player_id'))
  extra_touchdown_info = extra_touchdown_info %>% filter(Week == latest_week) %>% left_join(depth_charts, join_by('player_id'))
  
  predictions = predictions %>% filter(Week == latest_week)
  
  props_initial = future_map(.x = c('Passing', 'Rushing', 'Receiving', 'Touchdown'),
                     .f = get_props) %>%
    bind_rows()
  props_initial$name = gsub('\\(.*\\)', '', props_initial$name) %>% trimws()
  
  props_reactive_val = reactiveVal(NULL) #initialize
  
  if (is.null(props_initial) || nrow(props_initial) == 0) {
    output$error_message <- renderUI(HTML(
      '<div style="color:red; font-size:24px; font-weight:bold;">
       Betting lines could not be pulled. If you are on public WiFi, try a hotspot.
     </div>'
    ))
  } else {
    output$error_message <- renderUI(NULL)
    props_reactive_val(props_initial) 
    output$header  <- renderText(paste(latest_season, 'Week', latest_week))
    output$header2 <- renderText(paste('Last updated:', latest_update_time))
  }
  
  results <- reactive({
    req(!is.null(props_reactive_val()))             
    join_preds_and_props(preds = predictions,
                         props = props_reactive_val())
  })
 
  output$header = renderText({
    req(results())
    paste(latest_season, 'Week', latest_week)
  })
  output$header2 = renderText({
    req(results())
    paste('Last updated:', latest_update_time)
  })
  
  
  output$refresh = renderUI({
    req(results())
    actionButton('refresh', 'Refresh Betting Lines')
    })
  output$refreshing_message = renderText("Refreshing...")
  output$refresh_bet_updates = renderUI(actionButton('refresh_bet_updates', 'Refresh'))
  output$refreshing_bet_updates_message = renderText("Refreshing...")
  
  observeEvent(input$refresh, {
    shinyjs::show("refresh_message")
    shinyjs::disable("refresh")
    new_props = future_map(.x = c('Passing', 'Rushing', 'Receiving', 'Touchdown'),
                           .f = get_props) %>%
      bind_rows()
    new_props$name = gsub('\\(.*\\)', '', new_props$name) %>% trimws()
    props_reactive_val(new_props)
    #after render:
    session$onFlushed(function() {
      shinyjs::hide("refresh_message")
      shinyjs::enable("refresh")
    }, once = TRUE)
  })
  
  output$type_filter_ui = renderUI({
    req(results())
    pickerInput(inputId = 'bet_type_filter', label = "Filter on bet type", choices = unique(results()$Type), multiple = TRUE)
  })
  output$timeslot_filter_ui = renderUI({
    req(results())
    pickerInput(inputId = 'timeslot_filter', label = "Filter on timeslot", choices = unique(results()$Timeslot), multiple = TRUE)
  })
  output$team_filter_ui = renderUI({
    req(results())
    res = results() %>% left_join(team_lookup_table, join_by('Team')) %>% select(Team, FullName) %>% distinct() %>% arrange(FullName)
    choices = res$Team
    names(choices) = res$FullName
    print(choices)
    pickerInput(inputId = 'team_filter', label = 'Filter on team', choices = choices, multiple = TRUE)
  })
  output$model_accuracy_ui = renderUI({
    req(results())
    pickerInput(inputId = 'model_accuracy_filter', label = 'Filter on Expected Model Accuracy', choices = unique(results()$Expected_Accuracy), multiple = TRUE)
  })
  
  output$model_probability_slider_ui = renderUI({
    req(results())
    sliderInput(inputId = 'model_probability_slider', label = 'Range of model probabilities to consider', min = 0, max = 100, value = c(0,100), step = 2, round = TRUE, animate = TRUE, width = '100%')
  })
  
  output$weather_warning = renderText({
    req(results())
    ifelse(any(difftime(as.POSIXct(paste0(predictions$Date, ", ", predictions$Season, " ", predictions$Time),format = "%B %d, %Y %I:%M %p",tz = "America/New_York"), predictions$updateTime, units = 'hours') > 47),
       'Note: Games more than 48 hours after the model latest update time do not take into account weather forecast information.',
       '')
  })
  
  results_filtered = reactive({
    req(results())
    req(nrow(results()) > 0)
    
    
    res = results()
    
    if(!is.null(input$bet_type_filter))
    {
      res = res %>% filter(Type %in% input$bet_type_filter)
    }
    if(!is.null(input$timeslot_filter))
    {
      res = res %>% filter(Timeslot %in% input$timeslot_filter)
    }
    if(!is.null(input$team_filter))
    {
      res = res %>% filter(Team %in% input$team_filter)
    }
    if(!is.null(input$model_accuracy_filter))
    {
      res = res %>% filter(Expected_Accuracy %in% input$model_accuracy_filter)
    }
    if(!is.null(input$model_probability_slider))
    {
      res = res %>% filter(Model_Probability >= input$model_probability_slider[1]/100 & Model_Probability <= input$model_probability_slider[2]/100)
    }
    res %>% mutate(Return = ((Model_Probability*profit_per_100 - 100*(1-Model_Probability)))/100,
                   Risk_Raw = Model_Probability*(1 - Model_Probability)*(profit_per_100/100 + 1)^2,
                   ratio_risk = case_when(Expected_Accuracy == 'High' ~ 1,
                                          Expected_Accuracy == 'Medium' ~ 1.5,
                                          Expected_Accuracy == 'Low' ~ 2,
                                          Expected_Accuracy == 'No Data' ~ 1.7,
                                          TRUE ~ 1),
                   Risk_Score = Risk_Raw * ratio_risk)
  })
  
  output$instructions = renderUI({
    req(results())
    HTML('Below are all the recommended bets based on your above filters, sorted by highest expected return. You can choose individual bets below, or run the optimizer to the left to find an optimal portfolio of bets. If you end up placing an individual bet from the recommendations below, <b>click on the row of the table to log your bet<b>, so we can use this information to improve the model in the future.')
  })
  output$results = renderDataTable({
    req(results_filtered())
    req(nrow(results_filtered()) > 0)
    results = results_filtered() %>% 
      mutate(Return = round(Return*100,2), Risk_Score = round(Risk_Score, 2)) %>%
      rename('expected_return_profit_per_100' = 'Return') %>% 
      arrange(desc(expected_return_profit_per_100)) %>%
      select(-profit_per_100,-ratio_risk, -Risk_Raw) %>%
      mutate(run_time = format(force_tz(Sys.time(), "America/New_York"), "%Y-%m-%d %I:%M %p"))
      
    
    if(difftime(Sys.time(), most_recent_save, units = 'hours') > 1)
    {
      sheet_append(ss = sheet_id, data = results %>% mutate(Week = latest_week, Season = latest_season) %>% select(Season, Week, everything()), sheet = 'bet_recommendations')
    }
    
    shinyjs::hide("refresh_message")

    results %>% select(-run_time, -Time, -Date, -posix_timestamp)  %>%
        datatable() %>%
      formatPercentage(c('Model_Probability', 'Betting_Line_Implied_Prob'), digits = 1) #run time was just for writing to the csv 
  })
  
  observeEvent(input$click, {
    print(input$click$name)
    print(input$click$bet)
    output$name_text = renderText(paste('Name:', input$click$name))
    output$bet_text = renderText(paste('Bet:', ifelse(input$click$bet == 'Anytime TD Scorer', input$click$bet, paste(input$click$bet, input$click$label))))

    if(input$click$bet == 'Passing')
    {
      df = extra_passing_info
    } else if(input$click$bet == 'Rushing')
    {
      df = extra_rushing_info
    } else if(input$click$bet == 'Receiving')
    {
      df = extra_receiving_info
    } else {
      df = extra_touchdown_info
    }
    strings = display_extra_info(df = df, bet_type = input$click$bet, player_name = input$click$name, week = latest_week, season = latest_season)
    extra_info_min_year = strings[1]
    extra_info_draft = strings[2]
    extra_info_home = strings[3]
    extra_info_pct_active_gs = strings[4]
    extra_info_stats_this_season = strings[5]
    extra_info_stats_last_season = strings[6]
    extra_info_depth = strings[7]
    subset = results() %>% filter(Player == input$click$name & Type == input$click$bet & label == input$click$label)

    output$detailed_player_info =  renderUI(tagList(
      h2(input$click$name),
      h4(paste0(subset$Position, ' (', ifelse(subset$Starting == 1, 'Starter', 'Backup'), ')')),
      h4(paste0('Team: ', team_lookup_table$FullName[team_lookup_table$Team == subset$Team])),
      h4(paste0('Opp: ', team_lookup_table$FullName[team_lookup_table$Team == subset$Opp])),
      h4(extra_info_home),
      h4(subset$Timeslot),
      h4(extra_info_depth),
      p(paste0('Model Expected Accuracy: ', subset$Expected_Accuracy)),
      p(paste0('Model Probability: ', round(100*subset$Model_Probability,1),'%')),
      p(paste0('Odds: ', subset$Odds, ' (', round(100*subset$Betting_Line_Implied_Prob,1), '%)')),
     p(extra_info_min_year),
      p(extra_info_draft),
      p(extra_info_pct_active_gs),
      p(HTML(extra_info_stats_this_season)),
      p(HTML(extra_info_stats_last_season))
    ))
    
    showModal(modalDialog(
      tabsetPanel(
        tabPanel('Log Bet',
                  tags$h2('Import bet info here. After the game, come back to the app and go to the Update Bet Results tab to log the results (win/loss)'),
                  tags$br(),
                  textOutput("name_text"),
                  textOutput("bet_text"),
                  textInput(inputId = 'bettor_name', label = 'Put your name here', value = ''),
                  numericInput(inputId = "bet_amt", label = "How much did you bet, in dollars?", value = 10),
                  textInput(inputId = 'bet_odds', label = "What odds did you get the bet at? Put a + or - and then the number", value = input$click$odds),
                  actionButton('submit_bet', 'Submit') 
        ),
        tabPanel('Detailed Player Info',
                 uiOutput("detailed_player_info")
                 )
    )))
  })
  
  observeEvent(input$submit_bet, {
    row_to_write = data.frame(
      id = sample(1:1000000000, 1) %>% as.character(),
      Season = latest_season,
      Week = latest_week,
      Bettor = input$bettor_name,
      Player = input$click$name,
      Bet_Type = input$click$bet,
      Label = input$click$label,
      Odds = input$bet_odds %>% as.character(),
      Amount = input$bet_amt,
      Time_Submitted = format(force_tz(Sys.time(), "America/New_York"), "%Y-%m-%d %I:%M %p"),
      Type = 'Individual Bet',
      Team = results() %>% filter(clean_names(Player) == clean_names(input$click$name)) %>% select(Team) %>% distinct() %>% pull(),
      Opp = results() %>% filter(clean_names(Player) == clean_names(input$click$name)) %>% select(Opp) %>% distinct() %>% pull(),
      Gametime = results() %>% filter(clean_names(Player) == clean_names(input$click$name)) %>% select(posix_timestamp) %>% distinct() %>% pull(),
      Timeslot =  results() %>% filter(clean_names(Player) == clean_names(input$click$name)) %>% select(Timeslot) %>% distinct() %>% pull()
      )
    
    print(row_to_write)
    tryCatch({
        sheet_append(ss = sheet_id, data = row_to_write, sheet = 'bets_placed')
      showNotification("✅  Successfully Updated", type = "message", duration = 5)
    }, error = function(e) {
      showNotification(paste0("❌ Failed to write: ", e$message), type = "error", duration = 7)
    }, finally = {
      shinyjs::enable("write_row")
    })
    
  })
  
  
  placed_bets = read.csv(paste0(link_prefix, gid_bets_placed, link_suffix))
  existing_results = read.csv(paste0(link_prefix, gid_bet_results, link_suffix))
  bets_with_existing_results = existing_results$id
  unspecified_bets = placed_bets %>% filter(!(id %in% bets_with_existing_results)) %>% mutate(bet_descriptions = paste(Season, 'Week', Week, Bettor, '-', Player, Bet_Type, ifelse(Bet_Type == 'Anytime TD Scorer', '', Label), paste0('$', Amount, '  (', Time_Submitted, ')')))
  
  unspecified_bets_reactive_val = reactiveVal(unspecified_bets)
  output$bettor_selection_ui = renderUI({
    pickerInput(inputId = 'bettor_selection', label = 'Filter on bettor name', choices = unique(unspecified_bets_reactive_val()$Bettor), multiple = TRUE, selected = NULL)
  })
  unspecified_bets_filtered = reactive({
    if(!is.null(input$bettor_selection))
    {
      unspecified_bets_reactive_val() %>% filter(Bettor %in% input$bettor_selection)
    } else {
      unspecified_bets_reactive_val()
    }
  })
  
  output$save_button = renderUI({
    req(nrow(unspecified_bets_filtered()) > 0)
    actionButton('save', 'Save')
  })
  
  observeEvent(input$refresh_bet_updates, {
    shinyjs::show("refresh_bet_updates_message")
    shinyjs::disable("refresh_bet_updates")
    placed_bets = read.csv(paste0(link_prefix, gid_bets_placed, link_suffix))
    existing_results = read.csv(paste0(link_prefix, gid_bet_results, link_suffix))
    bets_with_existing_results = existing_results$id
    unspecified_bets = placed_bets %>% filter(!(id %in% bets_with_existing_results)) %>% mutate(bet_descriptions = paste(Season, 'Week', Week, Bettor, '-', Player, Bet_Type, ifelse(Bet_Type == 'Anytime TD Scorer', '', Label), paste0('$', Amount, '  (', Time_Submitted, ')')))
    
    new_unspecified_bets = placed_bets %>% filter(!(id %in% bets_with_existing_results)) %>% mutate(bet_descriptions = paste(Season, 'Week', Week, Bettor, '-', Player, Bet_Type, ifelse(Bet_Type == 'Anytime TD Scorer', '', Label), paste0('$', Amount, '  (', Time_Submitted, ')')))
    
    unspecified_bets_reactive_val(new_unspecified_bets)
    #after render:
    session$onFlushed(function() {
      shinyjs::hide("refresh_bet_updates_message")
      shinyjs::enable("refresh_bet_updates")
    }, once = TRUE)
  })
  
  
  output$bet_radio_options = renderUI({
    if(nrow(unspecified_bets_filtered()) > 0)
    {
      radio_list = lapply(1:nrow(unspecified_bets_filtered()), function(i) {
        description = unspecified_bets_filtered()$bet_descriptions[i]
        bet_id      = unspecified_bets_filtered()$id[i]
        
        radioButtons(
          inputId  = paste0("result_", bet_id),   # safer, ensures unique input IDs
          label    = description,
          choices  = c("Win", "Loss", "Unfinished"),
          selected = "Unfinished",
          inline   = TRUE
        )
      })
    tagList(radio_list)
  } else {
    tags$p("No outstanding bets available to update.")
  }
  })
  
  observeEvent(input$save, {
    temp = unspecified_bets_filtered() %>% mutate(Result = 'Unfinished') %>% select(id, Result)
    for (i in 1:nrow(temp))
    {
      bet_id = temp$id[i]
      selection = input[[paste0('result_',bet_id)]]
      temp$Result[temp$id == bet_id] = selection
    }
    table_to_write = temp %>% filter(Result != 'Unfinished')
    tryCatch({
      sheet_append(ss = sheet_id, data = table_to_write, sheet = 'bet_results')
      showNotification("✅  Successfully Updated", type = "message", duration = 5)
    }, error = function(e) {
      showNotification(paste0("❌ Failed to write: ", e$message), type = "error", duration = 7)
    }, finally = {
      shinyjs::enable("write_row")
    })
  })
 
  output$portfolio_optimization_heading = renderUI({
    req(results())
    tagList(h1('Optimize Portfolio of Bets'),
              p('Be sure the bet recommendation table to the right has all your desired filters applied.'),
            p('The optimizer takes into account expected returns, risk (based on how long-shot the odds are), model expected accuracy, and correlations between bets. It only considers bets from High and Medium accuracy models.'),
            p('Due to DraftKings minimum bet requirement, bet recommendations for less than $0.10 will automatically be removed, and the remaining recommendations re-calculated. This would give you less bets than you requested.'),
            p('When the bet portfolio list populates, click on a bet for more information.')
    )
  })
  output$max_bets_slider_ui = renderUI({
    req(results())
    sliderInput(inputId = 'max_bets', label = "Max # of Bets", value = 5, min = 1, max = 10)
  })
  
  output$odds_range_slider_ui = renderUI({
    req(results())
    sliderInput(inputId = 'odds_range', label = "Range of odds to consider. If you want the optimizer to choose, leave this as is.",
                value = c(min(results_filtered()$Odds %>% as.numeric()),max(results_filtered()$Odds %>% as.numeric())),
                min = min(results_filtered()$Odds %>% as.numeric()), max = max(results_filtered()$Odds %>% as.numeric()))
  })
  
  
  output$portfolio_optimization_bet_amt_ui = renderUI({
    req(results())
    numericInput(inputId = 'optimization_bet_amt', label = "Total Amount to Bet", value = 100)
  })
  
  output$remove_players_ui = renderUI({
    req(nrow(results_filtered()) > 0)
    pickerInput(inputId = 'remove_players', label = "Players to remove from consideration", choices = sort(unique(results_filtered()$Player)), multiple = TRUE, options = list(`live-search` = TRUE))
  })
  
  output$portfolio_optimization_button_ui = renderUI({
    req(results())
    actionButton(inputId = 'portfolio_optimization_button', 'Run')
  })
  #PORTFOLIO OPTIMIZATION
  
  portfolio_res_ready_to_run = reactiveVal(FALSE)
  portfolio_res_ready_to_show = reactiveVal(FALSE) 
  
  observeEvent(input$portfolio_optimization_button, {
    portfolio_res_ready_to_run(TRUE)  
    portfolio_res_ready_to_show(FALSE)
  })
  
  observeEvent(input$remove_players, {
    portfolio_res_ready_to_run(FALSE)  
    portfolio_res_ready_to_show(FALSE)
  })
  
  observeEvent(input$max_bets, {
    portfolio_res_ready_to_run(FALSE)  
    portfolio_res_ready_to_show(FALSE)
  })
  
  observeEvent(input$odds_range, {
    portfolio_res_ready_to_run(FALSE)  
    portfolio_res_ready_to_show(FALSE)
  })
  
  observeEvent(input$optimization_bet_amt, {
    portfolio_res_ready_to_run(FALSE)  
    portfolio_res_ready_to_show(FALSE)
  })
  
  observeEvent(input$bet_type_filter, {
    portfolio_res_ready_to_run(FALSE)  
    portfolio_res_ready_to_show(FALSE)
  })
  
  observeEvent(input$timeslot_filter, {
    portfolio_res_ready_to_run(FALSE)  
    portfolio_res_ready_to_show(FALSE)
  })
  
  observeEvent(input$team_filter, {
    portfolio_res_ready_to_run(FALSE)  
    portfolio_res_ready_to_show(FALSE)
  })
  
  observeEvent(input$model_accuracy_filter, {
    portfolio_res_ready_to_run(FALSE)  
    portfolio_res_ready_to_show(FALSE)
  })
  
  output$riskier_alternative_ui = renderUI({
    req(results())
    req(portfolio_res_ready_to_show())
    radioButtons(inputId = 'riskier', label = '', choices = c('Default Portfolio' = 0, 'Riskier Alternative (if available)' = 1), selected = 0, inline = TRUE)
  })
  
  both_portfolios = reactive({
    req(portfolio_res_ready_to_run())
    req(!is.null(input$optimization_bet_amt) && input$optimization_bet_amt > 0)
    positive_returns = results_filtered() %>%
      filter(!(Player %in% input$remove_players)) %>%
      filter(Expected_Accuracy %in% c('Medium', 'High')) %>%
      filter(as.numeric(Odds) >= input$odds_range[1] & as.numeric(Odds) <= input$odds_range[2])  %>% 
      filter(Return > min_return_portfolio_optimization) 
    if(nrow(positive_returns) > 0)
    {
      cov_matrix = matrix(NA, ncol = nrow(positive_returns), nrow = nrow(positive_returns))
      colnames(cov_matrix) = paste0(positive_returns$Player, ' ', positive_returns$Type, ifelse(positive_returns$label == 'Anytime TD Scorer', '', positive_returns$label))
      rownames(cov_matrix) = paste0(positive_returns$Player, ' ', positive_returns$Type, ifelse(positive_returns$label == 'Anytime TD Scorer', '', positive_returns$label))
      
      for (i in 1:nrow(cov_matrix))
      {
        for(j in i:nrow(cov_matrix))
        {
          if(i == j)
          {
            cov_matrix[i,j] = positive_returns$Risk_Score[i]
          } else {
            #if player is the same: correlation = 1
            #if player fits in one of the correlation categories, assign the correct correlation based on the correlations spreadsheet
            #otherwise, correlation = 0
            if(positive_returns$Player[i] == positive_returns$Player[j] & positive_returns$Type[i] == positive_returns$Type[j])
            {
              cor = 1
            } else if (positive_returns$Player[i] == positive_returns$Player[j] & positive_returns$Type[i] != positive_returns$Type[j]) 
            {
              bet_type_1 = ifelse(positive_returns$Type[i] == 'Anytime TD Scorer', 'Anytime_TD', paste0(positive_returns$Type[i], '_Yds'))
              bet_type_2 = ifelse(positive_returns$Type[j] == 'Anytime TD Scorer', 'Anytime_TD', paste0(positive_returns$Type[j], '_Yds'))
              cor = correlations %>% filter(Correlation_Type == 'same_player' & Var1 ==  bet_type_1 & Var2 == bet_type_2) %>% select(Correlation) %>% distinct() %>% pull()
              cor = ifelse(length(cor) == 0, 0, cor)
            } else if (positive_returns$Team[i] == positive_returns$Team[j])
            {
              bet_type_1 = ifelse(positive_returns$Type[i] == 'Anytime TD Scorer', 'Anytime_TD', paste0(positive_returns$Type[i], '_Yds'))
              bet_type_2 = ifelse(positive_returns$Type[j] == 'Anytime TD Scorer', 'Anytime_TD', paste0(positive_returns$Type[j], '_Yds'))
              cor = correlations %>% filter(Correlation_Type == 'same_team' & Var1 ==  bet_type_1 & Var2 == bet_type_2 & str_detect(positive_returns$Position[i], Position1) & str_detect(positive_returns$Position[j], Position2)) %>%
                select(Correlation) %>% distinct() %>% pull()
              cor = ifelse(length(cor) == 0, 0, cor)
            } else if (positive_returns$Team[i] == positive_returns$Opp[j]) {
              bet_type_1 = ifelse(positive_returns$Type[i] == 'Anytime TD Scorer', 'Anytime_TD', paste0(positive_returns$Type[i], '_Yds'))
              bet_type_2 = ifelse(positive_returns$Type[j] == 'Anytime TD Scorer', 'Anytime_TD', paste0(positive_returns$Type[j], '_Yds'))
              cor = correlations %>% filter(Correlation_Type == 'opp_team' & Var1 ==  bet_type_1 & Var2 == bet_type_2 & str_detect(positive_returns$Position[i], Position1) & str_detect(positive_returns$Position[j], Position2)) %>%
                select(Correlation) %>% distinct() %>% pull()
              cor = ifelse(length(cor) == 0, 0, cor)
            } else{
              cor = 0
            }
            cov_matrix[i, j] = cor*sqrt(positive_returns$Risk_Score[i])*sqrt(positive_returns$Risk_Score[j])
            cov_matrix[j, i] = cor*sqrt(positive_returns$Risk_Score[i])*sqrt(positive_returns$Risk_Score[j])
          }
        }
      }
      
      mu = positive_returns$Return
      names(mu) = rownames(cov_matrix)
      Sigma = as.matrix(cov_matrix)
      Sigma <- (cov_matrix + t(cov_matrix)) / 2
      Sigma <- as.matrix(Matrix::nearPD(Sigma, corr = FALSE)$mat)
    
      get_optimized_by_gamma = function(mu, Sigma, gamma = 1, max_bets) {
        print(gamma)
        n <- length(mu)
        Dmat <- 2 * gamma * Sigma + 1e-8 * diag(n)
        dvec <- mu
        Amat <- cbind(rep(1, n),     
                      diag(n)) 
        bvec <- c(1, rep(0, n))
        meq  <- 1
        
        sol <- tryCatch({
          solve.QP(Dmat, dvec, Amat, bvec, meq = meq)
        }, error = function(e) {
          return(NA)
        })
        new_w = NA
        if(all(!is.na(sol)))
        {
          w <- sol$solution
          names(w) = names(mu)
          num_bets = min(max_bets, length(w))
          new_w = w[order(w, decreasing = TRUE)][1:num_bets]
          new_w = new_w/sum(new_w)
        }
      
        return(new_w)
      }
      
      gammas = 10^seq(-3, 3, length.out = 31)
      n = length(mu)
      if (n == 1) {
        w = 1
        names(w) = names(mu)
        mu_p = mu
        sd_p = sqrt(Sigma[1,1])
        return(list(w = w, mu = mu_p, sd = sd_p, sharpe = ifelse(sd_p > 0, (mu_p - rf)/sd_p, NA)))
      }
      
      weights = lapply(gammas, function(g) get_optimized_by_gamma(mu, Sigma, gamma = g, max_bets = input$max_bets))
      mu_vec     <- rep(NA, length(gammas))
      sd_vec     <- rep(NA, length(gammas))
      sharpe_vec <- rep(NA, length(gammas))
      
      # sharpe = 0 #initialize
      # second_best_sharpe = -1
      # best_weights = NA
      for(w in 1:length(weights))
      {
        these_weights = unlist(weights[[w]])
        if(!is.na(these_weights))
        {
          mu_portfolio <- sum(these_weights * mu[names(these_weights)])
          sd_portfolio <- sqrt(as.numeric(t(these_weights) %*% Sigma[names(these_weights), names(these_weights)] %*% these_weights))
          
          # new_sharpe <- ifelse(sd_portfolio > 0, mu_portfolio / sd_portfolio, NA)
          sharpe_val    = ifelse(sd_portfolio > 0, mu_portfolio / sd_portfolio, NA)
          mu_vec[w]     = mu_portfolio
          sd_vec[w]     = sd_portfolio
          sharpe_vec[w] = sharpe_val
        }
      }
      best_indx = which.max(sharpe_vec)
      best_weights = unlist(weights[[best_indx]])
      sel = names(best_weights)
      # sel = names(best_weights)
      mu_port  = sum(best_weights * mu[sel])
      
      var_port  = as.numeric(t(best_weights) %*% Sigma[sel, sel] %*% best_weights)
      risk_port = sqrt(var_port)
      
      if(risk_port < mean(sd_vec[is.finite(sd_vec)], na.rm = TRUE)) #the best portfolio has below-average risk
      {
        finite = which(is.finite(sd_vec))  #finite is non-NA values
        avg_risk_indx = which.min(abs(sd_vec[finite] - mean(sd_vec)))
        avg_risk_weights = unlist(weights[[avg_risk_indx]])
        avg_risk_sel = names(avg_risk_weights)
        avg_risk_mu_port  = sum(avg_risk_weights * mu[avg_risk_sel])
        avg_risk_var_port  = as.numeric(t(avg_risk_weights) %*% Sigma[avg_risk_sel, avg_risk_sel] %*% avg_risk_weights)
        avg_risk_risk_port = sqrt(avg_risk_var_port)
      } else {#if the portfolio has above average risk, then don't recommend a riskier alternative
        avg_risk_indx = best_indx
        avg_risk_weights = best_weights
        avg_risk_sel = sel
        avg_risk_mu_port = mu_port
        avg_risk_risk_port = risk_port
      }
      df = best_weights %>% data.frame()
      df_alt = avg_risk_weights %>% data.frame()
      indx = which(colnames(cov_matrix) %in% rownames(df))
      indx_alt = which(colnames(cov_matrix) %in% rownames(df_alt))
      
      bet_rows = positive_returns[indx,]
      bet_rows_alt = positive_returns[indx_alt,]
      
      players = gsub('Anytime TD Scorer|Rushing[0-9]+\\+|Receiving[0-9]+\\+|Passing[0-9]+\\+', '', rownames(df)) %>% trimws()
      players_alt = gsub('Anytime TD Scorer|Rushing[0-9]+\\+|Receiving[0-9]+\\+|Passing[0-9]+\\+', '', rownames(df_alt)) %>% trimws()
      
      types = sapply(rownames(df), function(x) str_extract(x, 'Passing|Rushing|Receiving|Anytime TD Scorer')) %>% as.character()
      types_alt = sapply(rownames(df_alt), function(x) str_extract(x, 'Passing|Rushing|Receiving|Anytime TD Scorer')) %>% as.character()
      
      labels = sapply(rownames(df), function(x) str_extract(x, '[0-9]+\\+|Anytime TD Scorer')) %>% as.character()
      labels_alt = sapply(rownames(df_alt), function(x) str_extract(x, '[0-9]+\\+|Anytime TD Scorer')) %>% as.character()
      
      labels_df = data.frame(players, types, labels)
      labels_df_alt = data.frame(players_alt, types_alt, labels_alt)
      
      colnames(labels_df) = c('Player', 'Type', 'label')
      colnames(labels_df_alt) = c('Player', 'Type', 'label')
      
      from_bets_table = labels_df %>% inner_join(bet_rows, join_by(Player, Type, label))
      from_bets_table_alt = labels_df_alt %>% inner_join(bet_rows_alt, join_by(Player, Type, label))
      
      df = cbind(df, from_bets_table$Odds)
      df_alt = cbind(df_alt, from_bets_table_alt$Odds)
      
      colnames(df) = c('BetWeight', 'Odds')
      colnames(df_alt) = c('BetWeight', 'Odds')
      portfolio_res_ready_to_show(TRUE) #ready to show, no longer waiting on update
      
      
      return(list('default' = list(df, mu_port, risk_port),
                  'riskier' = list(df_alt, avg_risk_mu_port, avg_risk_risk_port)))
    } else {
      portfolio_res_ready_to_show(FALSE)
      return(NULL)
    }
  })
  
  output$no_bets = renderText({
    req(portfolio_res_ready_to_run())
    if(is.null(both_portfolios()))
    {
      "No recommended bets available. Check your selections and try again."
    }
  })
    
  optimal_portfolio = reactive({
    req(both_portfolios())
    req(!is.na(both_portfolios()))
                                 
    if(is.null(input$riskier) || input$riskier == 0)
    {
      selected_portfolio = both_portfolios()[['default']]
    } else{
      selected_portfolio = both_portfolios()[['riskier']]
      
    }
    return(list(selected_portfolio[[1]], selected_portfolio[[2]], selected_portfolio[[3]])) #df, mu_port, risk_port
    
  })
  
  output$portfolio_optimization_output = renderDataTable({
    req(optimal_portfolio())
    req(portfolio_res_ready_to_show())
   res = optimal_portfolio()[[1]] %>%
     mutate(BetAmount = BetWeight*input$optimization_bet_amt,
            ToPay = ifelse(as.numeric(Odds) < 0, BetAmount + BetAmount*(100/abs(as.numeric(Odds))), BetAmount + BetAmount*(as.numeric(Odds)/100))) %>%
     select(-BetWeight) %>%
     filter(BetAmount > 0.1)
   total_bet_amount = sum(res$BetAmount) #in case it got smaller when we tookout BetAmount < 0.1
   res$BetAmount = 50*(res$BetAmount/total_bet_amount)
   
   
   
   tryCatch({
     sheet_append(ss = sheet_id, data = res %>%
                    mutate(name = rownames(res),
                           max_bets = input$max_bets,
                           time = format(lubridate::with_tz(Sys.time(), "America/New_York"),
                                         "%Y-%m-%d %I:%M %p")) %>%
                    select(max_bets, name, Odds, BetAmount, ToPay, time),
                  sheet = 'portfolio_bet_recommendations')
     showNotification("✅  Successfully Updated", type = "message", duration = 5)
   }, error = function(e) {
     showNotification(paste0("❌ Failed to write: ", e$message), type = "error", duration = 7)
   }, finally = {
     shinyjs::enable("write_row")
   })
   
   res %>%
     datatable(options = list(dom = 't')) %>% formatCurrency(c('BetAmount', 'ToPay'), digits = 2)
  })
  
  output$portfolio_return = renderUI({
    req(optimal_portfolio())
    req(portfolio_res_ready_to_show())
    ev_portfolio = optimal_portfolio()[[2]]*input$optimization_bet_amt
    p(paste0('Portfolio Expected Profit for a $', input$optimization_bet_amt, ' bet: $', round(ev_portfolio)))
  })
  
  output$portfolio_risk = renderUI({
    req(optimal_portfolio())
    req(portfolio_res_ready_to_show())
    risk_portfolio = optimal_portfolio()[[3]]^2 #variance risk score
    p(paste0('Portfolio Risk Score: ', round(risk_portfolio,2)))
  })
  
  output$optimization_instructions = renderUI({
    req(optimal_portfolio())
    req(portfolio_res_ready_to_show())
    p('If you end up placing the above recommended bet portfolio, click the below button to log your bets.')
  })
  
  output$log_portfolio_optimization_bet = renderUI({
    req(optimal_portfolio())
    req(portfolio_res_ready_to_show())
    actionButton('log_portfolio_optimization_bet', 'Log My Bets')
  })
  
  observeEvent(input$click_portfolio_row, {
    full_name = input$click_portfolio_row$name
    bet_amount = gsub('\\$', '', input$click_portfolio_row$amount) %>% as.numeric()
    topay = gsub('\\$', '', input$click_portfolio_row$topay) %>% as.numeric()
    bet_type = str_extract(full_name, 'Passing|Rushing|Receiving|(Anytime TD Scorer)')
    if(bet_type != 'Anytime TD Scorer')
    {
      label_extracted = str_extract(full_name, '[0-9]+\\+')
    } else {
      label_extracted = 'Anytime TD Scorer'
    }
    player_name = gsub(bet_type, '', full_name)
    player_name = gsub(label_extracted, '', player_name) 
    player_name = gsub('\\+', '', player_name)  %>% trimws()
    subset = results_filtered() %>% filter(Player == player_name & label == label_extracted & Type == bet_type)

    if( bet_type == 'Passing')
    {
      df = extra_passing_info
    } else if( bet_type == 'Rushing')
    {
      df = extra_rushing_info
    } else if(bet_type== 'Receiving')
    {
      df = extra_receiving_info
    } else {
      df = extra_touchdown_info
    }
    strings = display_extra_info(df = df, bet_type = bet_type, player_name = player_name, week = latest_week, season = latest_season)
    extra_info_min_year = strings[1]
    extra_info_draft = strings[2]
    extra_info_home = strings[3]
    extra_info_pct_active_gs = strings[4]
    extra_info_stats_this_season = strings[5]
    extra_info_stats_last_season = strings[6]
    extra_info_depth = strings[7]
    
    output$portfolio_optimization_more_info = renderUI(tagList(
      h2(full_name),
      h4(paste0(subset$Position, ' (', ifelse(subset$Starting == 1, 'Starter', 'Backup'), ')')),
      h4(paste0('Team: ', team_lookup_table$FullName[team_lookup_table$Team == subset$Team])),
      h4(paste0('Opp: ', team_lookup_table$FullName[team_lookup_table$Team == subset$Opp])),
      h4(extra_info_home),
      h4(subset$Timeslot),
      h4(extra_info_depth),
      p(paste0('Model Expected Accuracy: ', subset$Expected_Accuracy)),
      p(paste0('Model Probability: ', round(100*subset$Model_Probability,1),'%')),
      p(paste0('Odds: ', subset$Odds, ' (', round(100*subset$Betting_Line_Implied_Prob,1), '%)')),
      p(paste0('Explanation: You have ', round(subset$Model_Probability*100), '% chance of profiting $', topay - bet_amount, ' and ', round((1-subset$Model_Probability)*100), '% chance of losing $', bet_amount)),
      p(paste0('Overall expected return for this bet: $', round(subset$Return*bet_amount,2), ' (', round(subset$Return*100,1),'%)')),
      p(extra_info_min_year),
      p(extra_info_draft),
      p(extra_info_pct_active_gs),
      p(HTML(extra_info_stats_this_season)),
      p(HTML(extra_info_stats_last_season))
    ))
    
    showModal(modalDialog(
      uiOutput('portfolio_optimization_more_info')
    ))
  })
  

  
  observeEvent(input$log_portfolio_optimization_bet, {
  
    output$optimization_bets <- renderUI({
      req(optimal_portfolio())
      df <- optimal_portfolio()[[1]]  # data.frame with BetWeight, Odds; rownames are bet labels
      ids <- seq_len(nrow(df))
      
      tagList(
        textInput(inputId = 'optimization_bettor_name', label = 'Your Name'),
        lapply(ids, function(i) {
          bet_label <- rownames(df)[i]
          
          tagList(
            numericInput(
              inputId = paste0("bet_amt_", i),
              label   = paste("Bet Amount:", bet_label),
              value   = round(df$BetWeight[i]*input$optimization_bet_amt,2),
              min     = 0
            ),
            textInput(
              inputId = paste0("bet_odds_", i),
              label   = paste("Bet Odds (+/- then number):", bet_label),
              value   = df$Odds[i]
            ),
            tags$hr()
          )
        })
      )
    })
    
    showModal(modalDialog(
      tags$h2('Import Bets Info'),
      tags$br(),
      uiOutput('optimization_bets'),
      actionButton('submit_optimization_bets', 'Submit')
    ))
    
    observeEvent(input$submit_optimization_bets, {
      df <- req(optimal_portfolio()[[1]])
      ids <- seq_len(nrow(df))
      
      bet_amounts <- sapply(ids, function(i) input[[paste0("bet_amt_", i)]])
      bet_odds    <- sapply(ids, function(i) input[[paste0("bet_odds_", i)]])
      
      updated <- cbind(
        id = sample(1:1000000, nrow(df)),
        Season = latest_season,
        Week = latest_week,
        Bettor = input$optimization_bettor_name,
        Player = gsub('(Anytime TD Scorer)|(Rushing[0-9]+\\+)|(Receiving[0-9]+\\+)|(Receiving[0-9]+\\+)', '', rownames(df)) %>% trimws(),
        Bet_Type = ifelse(str_detect(rownames(df), 'Anytime TD Scorer'), 'Anytime TD Scorer', str_extract(rownames(df), 'Rushing|Passing|Receiving')),
        Label = ifelse(str_detect(rownames(df), 'Anytime TD Scorer'), 'Anytime TD Scorer', str_extract(rownames(df), '[0-9]+\\+')),
        Odds = bet_odds,
        Amount = bet_amounts,
        Time_Submitted = Sys.time() %>% format('%Y-%m-%d %I:%M %p'),
        Type = 'Optimization Recommender') %>%
        data.frame() %>%
        mutate(clean_name = clean_names(Player)) %>%
        inner_join(results() %>% mutate(clean_name = clean_names(Player)) %>% select(clean_name, Team, Opp, posix_timestamp, Timeslot) %>% distinct(), join_by ('clean_name')) %>%
        distinct()%>%
        select(-clean_name)
      
      tryCatch({
        sheet_append(ss = sheet_id, data = updated, sheet = 'bets_placed')
        showNotification("✅  Successfully Updated", type = "message", duration = 5)
      }, error = function(e) {
        showNotification(paste0("❌ Failed to write: ", e$message), type = "error", duration = 7)
      }, finally = {
        shinyjs::enable("write_row")
      })
      
    })
  })
  


}



# Run the application 
shinyApp(ui = ui, server = server)
