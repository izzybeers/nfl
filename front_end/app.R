#weekly script should write the selected models somewhere


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

join_preds_and_props = function(preds, props)
{
  preds$Type = ifelse(preds$Response == 'Anytime_Touchdown', 'Anytime TD Scorer', sapply(strsplit(preds$Response, '_'), function(x) x[1]))
  joined = preds %>% left_join(props, join_by('names' == 'name', 'label' == 'label', 'Type' == 'Type')) %>% filter(!is.na(marketId)) %>%
    mutate(Betting_Line_Implied_Prob = ifelse(as.numeric(Odds) < 0, (-1)*as.numeric(Odds) / ((-1)*as.numeric(Odds) + 100), 100 / (as.numeric(Odds) + 100)),
           Timeslot = paste(Day, Time_of_Day)) %>%
    rename('Player' = 'names') %>%
    filter(as.POSIXct(paste0(Date, ", ", Season, " ", Time),format = "%B %d, %Y %I:%M %p",tz = "America/New_York") > Sys.time()) %>% 
    select(Player, Position, Starting, Type, label, Team, Opp, Timeslot, Odds, Model_Probability, Betting_Line_Implied_Prob, Expected_Accuracy, profit_per_100)
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
             sidebarLayout(
               sidebarPanel(
                 width = 4,
                 uiOutput("portfolio_optimization_heading"),
                 uiOutput("max_bets_slider_ui"),
                 uiOutput("portfolio_optimization_bet_amt_ui"),
                 uiOutput("remove_players_ui"),
                 uiOutput("portfolio_optimization_button_ui"),
                 dataTableOutput("portfolio_optimization_output"),
                 tags$br(),
                 uiOutput("portfolio_return"),
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
                   column(3, uiOutput("model_accuracy_ui"))
                   ),
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
  latest_update_time = max(predictions$updateTime)
  
  props_initial = future_map(.x = c('Passing', 'Rushing', 'Receiving', 'Touchdown'),
                     .f = get_props) %>%
    bind_rows()
  
  props_reactive_val = reactiveVal(NULL) #initialize
  
  if (is.null(props_initial) || nrow(props_initial) == 0) {
    output$error_message <- renderUI(HTML(
      '<div style="color:red; font-size:24px; font-weight:bold;">
       Betting lines could not be pulled. If you are on public WiFi, try a hotspot.
     </div>'
    ))
  } else {
    output$error_message <- renderUI(NULL)
    props_reactive_val(props_initial)  # unlocks results()
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
    props_reactive_val(new_props)
    #after render:
    session$onFlushed(function() {
      shinyjs::hide("refresh_message")
      shinyjs::enable("refresh")
    }, once = TRUE)
  })
  
  # output$bet_size_ui = renderUI({
  #   req(results())
  #   numericInput(inputId = 'bet_size', label = 'Calculate expected winnings based on this bet amount:', value = 100)
  # })
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
    res
  })
  
  output$instructions = renderUI({
    req(results())
    HTML('Below are all the recommended bets based on your above filters, sorted by highest expected return. You can choose individual bets below, or run the optimizer to the left to find an optimal portfolio of bets. If you end up placing an individual bet from the recommendations below, <b>click on the row of the table to log your bet<b>, so we can use this information to improve the model in the future.')
  })
  output$results = renderDataTable({
    req(results_filtered())
    req(nrow(results_filtered()) > 0)
    results = results_filtered() %>% mutate(expected_profit_per_100 = round((Model_Probability*profit_per_100 - 100*(1-Model_Probability)),2)) %>% #calculate for 100 and then adjust based on user's specified bet amount 
      arrange(desc(expected_profit_per_100)) %>% mutate(Model_Probability = paste0(100*round(Model_Probability,2), '%'),
                                                        Betting_Line_Implied_Prob = paste0(100*round(Betting_Line_Implied_Prob, 2), '%')) %>%
      # rename_with(~ paste0('expected_profit_per_100'), 'expected_profit')  %>%
      select(-profit_per_100) %>%
      mutate(run_time = format(force_tz(Sys.time(), "America/New_York"), "%Y-%m-%d %I:%M %p"))
    
    
    
    sheet_append(ss = sheet_id, data = results, sheet = 'bet_recommendations')
    
    shinyjs::hide("refresh_message")
    results %>% select(-run_time) #run time was just for writing to the csv 
  })
  
  observeEvent(input$click, {
    print(input$click$name)
    print(input$click$bet)
    output$name_text = renderText(paste('Name:', input$click$name))
    output$bet_text = renderText(paste('Bet:', ifelse(input$click$bet == 'Anytime TD Scorer', input$click$bet, paste(input$click$bet, input$click$label))))
    showModal(modalDialog(
      tags$h2('Import bet info here. After the game, come back to the app and go to the Update Bet Results tab to log the results (win/loss)'),
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
      Time_Submitted = format(force_tz(Sys.time(), "America/New_York"), "%Y-%m-%d %I:%M %p"),
      Type = 'Individual Bet'
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
            p('The optimizer takes into account expected returns, risk (based on how long-shot the odds are), model expected accuracy, and correlations between bets.')
    )
  })
  output$max_bets_slider_ui = renderUI({
    req(results())
    sliderInput(inputId = 'max_bets', label = "Max # of Bets", value = 5, min = 1, max = 10)
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
  
  optimal_portfolio = reactive({
    req(portfolio_res_ready_to_run())
    req(!is.null(input$optimization_bet_amt) && input$optimization_bet_amt > 0)
    positive_returns = results_filtered() %>%
      filter(!(Player %in% input$remove_players)) %>%
      mutate(Return = ((Model_Probability*profit_per_100 - 100*(1-Model_Probability)))/100,
             Risk= Model_Probability*(1 - Model_Probability)*(profit_per_100/100 + 1)^2) %>% 
      filter(Return > min_return_portfolio_optimization) 
    
    cov_matrix = matrix(NA, ncol = nrow(positive_returns), nrow = nrow(positive_returns))
    colnames(cov_matrix) = paste0(positive_returns$Player, ' ', positive_returns$Type, ifelse(positive_returns$label == 'Anytime TD Scorer', '', positive_returns$label))
    rownames(cov_matrix) = paste0(positive_returns$Player, ' ', positive_returns$Type, ifelse(positive_returns$label == 'Anytime TD Scorer', '', positive_returns$label))
    
    for (i in 1:nrow(cov_matrix))
    {
      for(j in i:nrow(cov_matrix))
      {
        if(i == j)
        {
          ratio_risk = case_when(positive_returns$Expected_Accuracy[i] == 'High' ~ 1,
                                 positive_returns$Expected_Accuracy[i] == 'Medium' ~ 1.5,
                                 positive_returns$Expected_Accuracy[i] == 'Low' ~ 2,
                                 positive_returns$Expected_Accuracy[i] == 'No Data' ~ 1.7,
                                 TRUE ~ 1
                                 )
          cov_matrix[i,j] = positive_returns$Risk[i]*ratio_risk
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
          cov_matrix[i, j] = cor*sqrt(positive_returns$Risk[i])*sqrt(positive_returns$Risk[j])
          cov_matrix[j, i] = cor*sqrt(positive_returns$Risk[i])*sqrt(positive_returns$Risk[j])
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
    sharpe = 0 #initialize
    best_weights = NA
    for(w in 1:length(weights))
    {
      these_weights = unlist(weights[[w]])
      if(!is.na(these_weights))
      {
        mu_portfolio <- sum(these_weights * mu[names(these_weights)])
        sd_portfolio <- sqrt(as.numeric(t(these_weights) %*% Sigma[names(these_weights), names(these_weights)] %*% these_weights))
        new_sharpe <- ifelse(sd_portfolio > 0, mu_portfolio / sd_portfolio, NA)
        if(new_sharpe > sharpe)
        {
          sharpe = new_sharpe
          best_weights = these_weights
          best_gamma = gammas[w]
        }
      }
    }
    sel = names(best_weights)
    mu_port  = sum(best_weights * mu[sel])
    
    df = best_weights %>% data.frame()
    
    indx = which(colnames(cov_matrix) %in% rownames(df))
    bet_rows = positive_returns[indx,]
    players = gsub('Anytime TD Scorer|Rushing[0-9]+\\+|Receiving[0-9]+\\+|Passing[0-9]+\\+', '', rownames(df)) %>% trimws()
    types = sapply(rownames(df), function(x) str_extract(x, 'Passing|Rushing|Receiving|Anytime TD Scorer')) %>% as.character()
    labels = sapply(rownames(df), function(x) str_extract(x, '[0-9]+\\+|Anytime TD Scorer')) %>% as.character()
    labels_df = data.frame(players, types, labels)
    colnames(labels_df) = c('Player', 'Type', 'label')
    from_bets_table = labels_df %>% inner_join(bet_rows, join_by(Player, Type, label))
    df = cbind(df, from_bets_table$Odds)
    colnames(df) = c('BetWeight', 'Odds')
    portfolio_res_ready_to_show(TRUE) #ready to show, no longer waiting on update
    list(df, mu_port)
  })
  
  output$portfolio_optimization_output = renderDataTable({
    req(optimal_portfolio())
    req(portfolio_res_ready_to_show())
   optimal_portfolio()[[1]] %>% mutate(BetAmount = BetWeight*input$optimization_bet_amt) %>% select(-BetWeight) %>% datatable(options = list(dom = 't')) %>% formatCurrency('BetAmount')
  })
  
  output$portfolio_return = renderUI({
    req(optimal_portfolio())
    req(portfolio_res_ready_to_show())
    ev_portfolio = optimal_portfolio()[[2]]*input$optimization_bet_amt
    p(paste0('Portfolio Expected Profit for a $', input$optimization_bet_amt, ' bet: $', round(ev_portfolio)))
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
        Type = 'Optimization Recommender'
      ) %>% data.frame()
      
      
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
