#weekly script should write the selected models somewhere
#build front end
#prediction df should write the prediction df to csv file, including the probabilities
#model pulls from there
#Update the get props function with the correct ids for the bets


library(shiny)
library(shinyWidgets)
library(googlesheets4)
library(dplyr)
library(jsonlite)

link_prefix =  'https://docs.google.com/spreadsheets/d/e/2PACX-1vTyIaWWovW2YUP1-JxYpg9ZHpF7a2i_7AEVan5ptaBBiwj6gwYp0STpE8HvYILR190HTrOFt2GMyUqn/pub?gid='
link_suffix = "&single=true&output=csv"
passing_gid = '0'
rushing_gid = '1041188467'
receiving_gid = '1971939369'
touchdown_gid = '1979813660'

pull_prediction_data = function(gid, td = FALSE)
{
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
  lines = fromJSON(content(GET(base_url), as = "text", encoding = "UTF-8"), flatten = TRUE)$selections %>% select(marketId, label, `displayOdds.american`) %>%
    left_join(fromJSON(content(GET(base_url), as = "text", encoding = "UTF-8"))$markets %>% select(id,name), join_by(marketId == id)) %>%
    rename('Odds' = `displayOdds.american`)
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
}

join_preds_and_props = function(preds, props)
{
  joined = preds %>% left_join(props, join_by('names' == 'name', 'label' == 'label')) %>% filter(!is.na(marketId)) %>%
    mutate(expected_profit_per_100 = Probability*profit_per_100 - 100*(1-Probability),
           current_line_prob = ifelse(as.numeric(Odds) < 0, (-1)*as.numeric(Odds) / ((-1)*as.numeric(Odds) + 100), 100 / (as.numeric(Odds) + 100)),
           Timeslot = paste(Day, Time_of_Day)) %>%
    rename('Player' = 'names')  %>%
    filter(as.POSIXct(paste0(Date, ", ", Season, " ", Time),format = "%B %d, %Y %I:%M %p",tz = "America/New_York") > Sys.time()) %>% 
    select(Player, Type, label, Team, Opp, Timeslot, Odds, Probability, current_line_prob, expected_profit_per_100) %>% arrange(desc(expected_profit_per_100))
  return(joined)
}


#read.csv to get prediction df
#get current week and year from there

ui <- fluidPage(
  titlePanel('NFL Prop Bet Recommender'),
  
  div(style = 'font-weight: bold; font-size: 16px;', textOutput("header")),
  textOutput("header2"),
  fluidRow(
    column(4, uiOutput("type_filter_ui")),
    column(4, uiOutput("timeslot_filter_ui")),
    column(4, uiOutput("team_filter_ui"))
  ),
  textOutput("weather_warning"),
  
  dataTableOutput('results')
  
  #checkbox to choose Passing, Rushing, Receiving, Touchdown
  #show high confidence models only
  #filter on a date/window of games
  #read in props
  #calculate expected value
  #display table of recommendations
  #modal to see more information about player
  #portfolio optimization??
  #refresh button
)

server <- function(input, output) {
  
  passing_predictions = pull_prediction_data(passing_gid)
  rushing_predictions = pull_prediction_data(rushing_gid)
  receiving_predictions = pull_prediction_data(receiving_gid)
  touchdown_predictions = pull_prediction_data(touchdown_gid, td = TRUE)
  
  latest_season = max(c(passing_predictions$Season, rushing_predictions$Season, receiving_predictions$Season, touchdown_predictions$Season))
  latest_week = max(c(passing_predictions$Week, rushing_predictions$Week, receiving_predictions$Week, touchdown_predictions$Week))
  latest_update_time = max(c(passing_predictions$updateTime, rushing_predictions$updateTime, receiving_predictions$updateTime, touchdown_predictions$updateTime))
  
  passing_props = get_props(bet_category = 'Passing')
  rushing_props =  get_props(bet_category = 'Rushing')
  receiving_props = get_props(bet_category = 'Receiving')
  touchdown_props =  get_props(bet_category = 'Touchdown')
  
  passing_results = join_preds_and_props(preds = passing_predictions,
                                       props = passing_props)
  rushing_results = join_preds_and_props(preds = rushing_predictions,
                                         props = rushing_props)
  receiving_results = join_preds_and_props(preds = receiving_predictions,
                                           props = receiving_props)
  touchdown_results = join_preds_and_props(preds = touchdown_predictions,
                                           props = touchdown_props)
  
  results = bind_rows(passing_results, rushing_results, receiving_results, touchdown_results)
 
  output$header = renderText(paste(latest_season, 'Week', latest_week))
  output$header2 = renderText(paste('Last updated:', format(as.POSIXct(latest_update_time, format = '%Y-%m-%d %H:%M', tz = "America/New_York"),"%Y-%m-%d %I:%M %p"), 'ET'))
  
  output$type_filter_ui = renderUI(
    pickerInput(inputId = 'bet_type_filter', label = "Filter on bet type", choices = unique(results$Type), multiple = TRUE)
  )
  output$timeslot_filter_ui = renderUI(
    pickerInput(inputId = 'timeslot_filter', label = "Filter on timeslot", choices = unique(results$Timeslot), multiple = TRUE)
  )
  output$team_filter_ui = renderUI({
    pickerInput(inputId = 'team_filter', label = 'Filter on team', choices = unique(results$Team), multiple = TRUE)
  })
  
  output$weather_warning = renderText({
    ifelse(any(difftime(as.POSIXct(paste0(results$ate, ", ", results$Season, " ", results$Time),format = "%B %d, %Y %I:%M %p",tz = "America/New_York"), results$updateTime, units = 'hours') > 47),
       'Note: Games more than 48 hours after the model latest update time will not take into account weather forecast information.',
       '')
  })
  
  
  output$results = renderDataTable({
    if(!is.null(input$bet_type_filter))
    {
      passing_results = passing_results %>% filter(Type %in% input$bet_type_filter)
    }
    if(!is.null(input$timeslot_filter))
    {
      passing_results = passing_results %>% filter(Type %in% input$timeslot_filter)
    }
    if(!is.null(input$team_filter))
    {
      passing_results = passing_results %>% filter(Type %in% input$team_filter)
    }
    passing_results
  })
  
  #show the current week and season number (dates maybe?)
  #show the last updated time
  #showmodal to put bet in, an write to google sheet
  #refresh model based on new info?
  
  # tryCatch({
  #   sheet_append(ss = sheet_id, data = new_passing_data, sheet = 'passing_predictions')
  #   showNotification("✅ Row successfully written", type = "message", duration = 5)
  # }, error = function(e) {
  #   showNotification(paste0("❌ Failed to write row: ", e$message), type = "error", duration = 7)
  # }, finally = {
  #   shinyjs::enable("write_row")
  # })
  
}

# thresholds_df$Threshold = paste0(thresholds_df$Threshold,'+')
# 
# #needs to be run once above before the for loop, but for all subsequent 
# #refreshes, can just be run from here
#  
# receiving_props_with_probabilities = receiving_props %>% inner_join(thresholds_df,
#                                                                     join_by('bet_type' == 'Threshold',
#                                                                             'Participant' == 'Name')) %>%
#   mutate(expected_profit_per_100 = (payout_per_100-100)*Probability - 100*(1-Probability)) %>% arrange(desc(expected_profit_per_100)) %>%
#   left_join(stats_full %>% filter(Season == 2024 & week_num >= 11) %>% distinct(name, team),
#             join_by('Participant' == 'name'))
# 
# receiving_props_with_probabilities
# 
# 
# 
# 
# top_20_vars = summary.gbm(model)[1:20,"var"]
# 
# (prediction_df %>% filter(Name == 'Darius Slayton'))[,c(top_20_vars,"targets_rank")]


# Run the application 
shinyApp(ui = ui, server = server)
