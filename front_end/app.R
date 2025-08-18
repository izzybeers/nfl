#weekly script should write the selected models somewhere


library(shiny)
library(shinyWidgets)
library(googlesheets4)
library(dplyr)
library(jsonlite)
library(shinycssloaders)
library(furrr)
library(googlesheets4)
library(DT)
gs4_auth(cache = ".secrets", email = "izzyb961@gmail.com")

setwd("~/nfl")
source('data_collection/scripts/global.R')


sheet_id = '19sWOOPFI37WaR5lmlYS6UUrV-0dmTn0iTFqUp26sfGI'
link_prefix =  'https://docs.google.com/spreadsheets/d/e/2PACX-1vTyIaWWovW2YUP1-JxYpg9ZHpF7a2i_7AEVan5ptaBBiwj6gwYp0STpE8HvYILR190HTrOFt2GMyUqn/pub?gid='
link_suffix = "&single=true&output=csv"
passing_gid = '0'
rushing_gid = '1041188467'
receiving_gid = '1971939369'
touchdown_gid = '1979813660'
gid_bets_placed = '95780958'
gid_bet_results = '1472501972'

# qb1_starting = read.csv('https://docs.google.com/spreadsheets/d/e/2PACX-1vT9_LcNO2d8L5kzbJQZZti9kxfAZRFRAl2oJz5WlpusfvL1txbkc8OU6BSlB54TA9HCBHRlIxi9MpuT/pub?gid=2014202336&single=true&output=csv') %>%
#   filter(Season == this_season) %>% select(Team, !!sym(paste0('Week', this_week)))
# colnames(qb1_starting) = c('Team', 'qb1_start')
# qb1_by_year = read.csv('https://docs.google.com/spreadsheets/d/e/2PACX-1vT9_LcNO2d8L5kzbJQZZti9kxfAZRFRAl2oJz5WlpusfvL1txbkc8OU6BSlB54TA9HCBHRlIxi9MpuT/pub?gid=1914165552&single=true&output=csv') %>% mutate(Temp = 1)


#need this if the app.R file can't access global.R in a different directory:
# passing_numbers = seq(150,360,30)
# rushing_numbers = c(25,seq(40,160,20))
# receiving_numbers = c(25,seq(40,160,20))
# 
# passing_response = c()
# for(n in passing_numbers)
# {
#   passing_response = c(passing_response, paste0('Passing_Yds_', n))
# }
# rushing_response = c()
# for(n in rushing_numbers)
# {
#   rushing_response = c(rushing_response, paste0('Rushing_Yds_', n))
# }
# receiving_response = c()
# for(n in rushing_numbers)
# {
#   receiving_response = c(receiving_response, paste0('Receiving_Yds_', n))
# }
# touchdown_response = 'Anytime_Touchdown'


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
    mutate(Betting_Line_Implied_Prob = ifelse(as.numeric(Odds) < 0, (-1)*as.numeric(Odds) / ((-1)*as.numeric(Odds) + 100), 100 / (as.numeric(Odds) + 100)),
           Timeslot = paste(Day, Time_of_Day)) %>%
    rename('Player' = 'names') %>%
    filter(as.POSIXct(paste0(Date, ", ", Season, " ", Time),format = "%B %d, %Y %I:%M %p",tz = "America/New_York") > Sys.time()) %>% 
    select(Player, Position, Type, label, Team, Opp, Timeslot, Odds, Model_Probability, Betting_Line_Implied_Prob, Expected_Accuracy, profit_per_100)
  return(joined)
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
  
  tabsetPanel(
    tabPanel('Bet Recommendations',
             div(style = 'font-weight: bold; font-size: 16px;', textOutput("header")),
             textOutput("header2"),
             uiOutput("bet_size_ui"),
             fluidRow(
               column(3, uiOutput("type_filter_ui")),
               column(3, uiOutput("timeslot_filter_ui")),
               column(3, uiOutput("team_filter_ui")),
               column(3, uiOutput("checkbox_high_accuracy_ui"))
             ),
             textOutput("weather_warning"),
             tags$br(),
             uiOutput("refresh"),
             div(id = 'refresh_message', textOutput("refreshing_message")),
             dataTableOutput('results') %>% withSpinner()
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
  
  


  #modal to see more information about player
  #portfolio optimization??
  #refresh button
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
  
  
  latest_season = max(predictions$Season)
  latest_week = max(predictions$Week)
  latest_update_time = max(predictions$updateTime)
  
  props_initial = future_map(.x = c('Passing', 'Rushing', 'Receiving', 'Touchdown'),
                     .f = get_props) %>%
    bind_rows()
  
  props_reactive_val = reactiveVal(props_initial)
  
  results = reactive({join_preds_and_props(preds = predictions,
                                 props = props_reactive_val())})
 
  output$header = renderText(paste(latest_season, 'Week', latest_week))
  output$header2 = renderText(paste('Last updated:', format(as.POSIXct(latest_update_time, format = '%Y-%m-%d %H:%M', tz = "America/New_York"),"%Y-%m-%d %I:%M %p"), 'ET'))
  
  output$refresh = renderUI(actionButton('refresh', 'Refresh Bettling Lines'))
  output$refreshing_message = renderText("Refreshing...")
  output$refresh_bet_updates = renderUI(actionButton('refresh_bet_updates', 'Refresh'))
  output$refreshing_bet_updates_message = renderText("Refreshing...")
  
  observeEvent(input$refresh, {
    shinyjs::show("refresh_message")
    shinyjs::disable("refresh")
    new_props = future_map(.x = c('Passing', 'Rushing', 'Receiving', 'Touchdown'),
                           .f = get_props) %>%
      bind_rows()
    props_reactive_val(new_props)
    #after render:
    session$onFlushed(function() {
      shinyjs::hide("refresh_message")
      shinyjs::enable("refresh")
    }, once = TRUE)
  })
  
  output$bet_size_ui = renderUI(numericInput(inputId = 'bet_size', label = 'Calculate expected winnings based on this bet amount:', value = 100))
  output$type_filter_ui = renderUI(
    pickerInput(inputId = 'bet_type_filter', label = "Filter on bet type", choices = unique(results()$Type), multiple = TRUE)
  )
  output$timeslot_filter_ui = renderUI(
    pickerInput(inputId = 'timeslot_filter', label = "Filter on timeslot", choices = unique(results()$Timeslot), multiple = TRUE)
  )
  output$team_filter_ui = renderUI({
    pickerInput(inputId = 'team_filter', label = 'Filter on team', choices = unique(results()$Team), multiple = TRUE)
  })
  output$checkbox_high_accuracy_ui = renderUI({
    checkboxInput(inputId = 'high_only', label = 'Show high accuracy recommendations only', value = FALSE)
  })
  
  output$weather_warning = renderText({
    ifelse(any(difftime(as.POSIXct(paste0(results()$Date, ", ", results()$Season, " ", results()$Time),format = "%B %d, %Y %I:%M %p",tz = "America/New_York"), results()$updateTime, units = 'hours') > 47),
       'Note: Games more than 48 hours after the model latest update time do not take into account weather forecast information.',
       '')
  })
  
  
  output$results = renderDataTable({
    req(nrow(results()) > 0)
    results = results() %>% mutate(expected_profit = (Model_Probability*profit_per_100 - 100*(1-Model_Probability))*(input$bet_size/100)) %>% #calculate for 100 and then adjust based on user's specified bet amount 
      arrange(desc(expected_profit)) %>% mutate(expected_profit = round(expected_profit, 2),
                                                Model_Probability = paste0(100*round(Model_Probability,2), '%'),
                                                Betting_Line_Implied_Prob = paste0(100*round(Betting_Line_Implied_Prob, 2), '%')) %>%
      rename_with(~ paste0('expected_profit_per_', input$bet_size), 'expected_profit')  %>% select(-profit_per_100)
    
    if(!is.null(input$bet_type_filter))
    {
      results = results %>% filter(Type %in% input$bet_type_filter)
    }
    if(!is.null(input$timeslot_filter))
    {
      results = results %>% filter(Timeslot %in% input$timeslot_filter)
    }
    if(!is.null(input$team_filter))
    {
      results = results %>% filter(Team %in% input$team_filter)
    }
    if(!is.null(input$high_only) && input$high_only == TRUE)
    {
      results = results %>% filter(Expected_Accuracy == 'High')
    }
    
    sheet_append(ss = sheet_id, data = results, sheet = 'bet_recommendations')
    
    shinyjs::hide("refresh_message")
    results 
  })
  
  observeEvent(input$click, {
    print(input$click$name)
    print(input$click$bet)
    output$name_text = renderText(paste('Name:', input$click$name))
    output$bet_text = renderText(paste('Bet:', ifelse(input$click$bet == 'Anytime TD Scorer', input$click$bet, paste(input$click$bet, input$click$label))))
    showModal(modalDialog(
      tags$h2('Import Bet Info'),
      tags$br(),
      textOutput("name_text"),
      textOutput("bet_text"),
      textInput(inputId = 'bettor_name', label = 'Put your name here', value = ''),
      numericInput(inputId = "bet_amt", label = "How much did you bet, in dollars?", value = 10),
      textInput(inputId = 'bet_odds', label = "What odds did you get the bet at? Put a + or - and then the number", value = input$click$odds),
      actionButton('submit_bet', 'Submit')
    ))
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
      Time_Submitted = Sys.time() %>% format('%Y-%m-%d %I:%M %p')
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
    pickerInput(inputId = 'bettor_selection', label = 'Filter on bettor name', choices = unique(unspecified_bets$Bettor), multiple = TRUE, selected = NULL)
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
 
  
}



# Run the application 
shinyApp(ui = ui, server = server)
