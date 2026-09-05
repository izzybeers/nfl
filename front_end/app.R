source('app_helper_funs.R')

ui <- fluidPage(
  titlePanel('NFL Prop Bet Recommender'),
  tags$br(),
  useShinyjs(),
  tags$head(
    tags$style(HTML("
      #shiny-modal .modal-dialog.modal-lg {
        width: 95vw !important;
        max-width: none !important;
        margin: 30px auto !important;
      }
    details > summary {
      display: list-item !important;
      list-style-position: inside;
      cursor: pointer;
    }
    "))
  ),
  tags$script(HTML("
     document.addEventListener('DOMContentLoaded', function() {
                    var tab = document.querySelector('#results_df')
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
                        var betIndex = headerNames.findIndex(h => h == 'bet_display_name')
                        var betOddsIndex = headerNames.findIndex(h => h == 'Odds')
                        betValue = cells[betIndex].innerText
                        betOddsValue = cells[betOddsIndex].innerText
                        console.log(betValue)
                        console.log(betOddsValue)
                        Shiny.setInputValue('click', {
                          bet: betValue,
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
             uiOutput("error_message"),
             tags$br(),
             div(style = 'font-weight: bold; font-size: 16px;', textOutput("header")),
             textOutput("header2"),
             tags$br(),
             # uiOutput("bet_size_ui"),
             fluidRow(
               column(4, uiOutput("type_filter_ui")),
               column(4, uiOutput("timeslot_filter_ui")),
               column(4, uiOutput("team_filter_ui"))
               ),
             uiOutput("odds_range_ui"),
             tags$br(),
             tags$br(),
             uiOutput("instructions"),
             tags$br(),
             textOutput("weather_warning"),
             tags$br(),
             fluidRow(
               column(6, uiOutput("popup_portfolio_button_ui")),
               column(6,
                       div(style = 'text-align:right',
                           uiOutput("refresh"),
                           div(id = 'refresh_message', textOutput("refreshing_message"))))
             ),
             tags$br(),
             dataTableOutput('results_df') %>% withSpinner(),
             tags$br(),
             tags$br()
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
    ),
    tabPanel('Upcoming Cheat Sheets',
             uiOutput("bettor_choice_ui"),
             uiOutput("survivor_teams_ui"),
             uiOutput("cheat_sheet_button_ui")
             ),
    tabPanel('Bettor Summaries',
             uiOutput("choose_bettor_summary_ui"),
             uiOutput("summary_only_finished"),
             tags$br(),
             uiOutput("bettor_summary_header"),
             tags$br(),
             uiOutput("weekly_plot_ui"),
             uiOutput("bet_type_plot_ui"),
             uiOutput("portfolio_type_plot_ui"),
             uiOutput("odds_range_plot_ui"),
             tags$br(),
             tags$br()
    ),
    tabPanel('Survivor Dashboard',
             tags$br(),
             uiOutput("survivor_filters"),
             tags$br(),
             tags$br(),
             uiOutput("survivor_display")
    )
  )
)

server = function(input, output, session) {
  
  shinyjs::hide("refresh_message")
  shinyjs::hide("refresh_bet_updates_message")
  # gids = c(passing_gid, rushing_gid, receiving_gid, touchdown_gid)
  # responses = list(passing_response, rushing_response, receiving_response, touchdown_response)
  
  predictions_res = pull_prediction_data()
  player_preds = predictions_res[[1]]
  team_preds = predictions_res[[2]]
  
  latest_season = max(player_preds$season)
  latest_week = max(player_preds$Week[player_preds$season == latest_season])
  
  latest_player_update_time = max(player_preds$updated_at)
  latest_team_update_time = max(team_preds$updated_at)
  latest_update_time = max(latest_player_update_time, latest_team_update_time)
  
  player_preds = player_preds %>% filter(season == latest_season, Week == latest_week, updated_at == latest_player_update_time)
  team_preds = team_preds %>% filter(season == latest_season, Week == latest_week, updated_at == latest_team_update_time)
  
  player_info = get_supabase_data('MainData', 'PlayerBios', additional_sql = list(season = paste0('eq.', latest_season), week = paste0('eq.',latest_week)))
  player_historical_stats = get_supabase_data('MainData', 'OffensePlayerStats') %>% filter(gsis_id %in% player_preds$gsis_id)
  #player_historical_stats = get_supabase_data('MainData', 'OffensePlayerStats', additional_sql = list(gsis_id = paste0("in.(", paste(unique(na.omit(player_preds$gsis_id)), collapse = ","), ")")))
  team_historical_stats = get_supabase_data('MainData', 'TeamStats')
  opp_historical_stats = get_supabase_data('MainData', 'OppStats')
  
  previous_recs = get_supabase_data('betting', 'BetRecommendations',additional_sql = list(Season = paste0('eq.',latest_season)))
  if(!is.null(previous_recs) && nrow(previous_recs) > 0)
  {
    most_recent_save = reactiveVal(
      max(as.POSIXct(previous_recs$run_time, format = "%Y-%m-%d %I:%M %p"))
    )
  } else {
    most_recent_save = reactiveVal(NULL)
  }
  
  # extra_passing_info = extra_passing_info %>% filter(Week == latest_week) %>% left_join(depth_charts %>% select(player_id, Depth), join_by('player_id'))
  # extra_rushing_info = extra_rushing_info %>% filter(Week == latest_week) %>% left_join(depth_charts %>% select(player_id, Depth), join_by('player_id'))
  # extra_receiving_info = extra_receiving_info %>% filter(Week == latest_week) %>% left_join(depth_charts %>% select(player_id, Depth), join_by('player_id'))
  # extra_touchdown_info = extra_touchdown_info %>% filter(Week == latest_week) %>% left_join(depth_charts %>% select(player_id, Depth), join_by('player_id'))
  # 
  
  results = reactiveVal(NULL) #initialize
  combined_data = join_preds_and_props(player_preds, team_preds)

  if (is.null(combined_data) || nrow(combined_data) == 0) {
    output$error_message <- renderUI(HTML(
      '<div style="color:red; font-size:24px; font-weight:bold;">
       Betting lines could not be pulled. If you are on public WiFi, try a hotspot.
     </div>'
    ))
  } else {
    output$error_message <- renderUI(NULL)
    results(combined_data) 
  }
  results(combined_data)
  
  #bet_details = pull_details(results(), player_info, player_historical_stats, team_historical_stats, opp_historical_stats)
  bet_details = reactive(pull_details(results(), player_info, player_historical_stats, team_historical_stats, opp_historical_stats, stats_season = latest_season))

  output$header = renderText({
    req(results())
    paste(latest_season, 'Week', latest_week)
  })
  output$header2 = renderText({
    req(results())
    paste('Last updated:', format(lubridate::ymd_hms(latest_update_time), "%Y-%m-%d %I:%M%p"))
  })
  
  output$refresh = renderUI({
    req(results())
    actionButton('refresh', NULL, icon = icon("rotate"), title = 'Refresh Betting Lines')
    })
  output$refreshing_message = renderText("Refreshing...")
  output$refresh_bet_updates = renderUI(actionButton('refresh_bet_updates', 'Refresh'))
  output$refreshing_bet_updates_message = renderText("Refreshing...")
  
  observeEvent(input$refresh, {
    shinyjs::show("refresh_message")
    shinyjs::disable("refresh")
    new_props = join_preds_and_props(player_preds, team_preds)
    results(new_props)
    #after render:
    session$onFlushed(function() {
      shinyjs::hide("refresh_message")
      shinyjs::enable("refresh")
    }, once = TRUE)
  })
  
  output$type_filter_ui = renderUI({
    req(results())
    pickerInput(inputId = 'bet_type_filter', label = "Filter on bet type", choices = unique(results()$bet_type), multiple = TRUE)
  })
  output$timeslot_filter_ui = renderUI({
    req(results())
    pickerInput(inputId = 'timeslot_filter', label = "Filter on timeslot", choices = unique(results()$timeslot), multiple = TRUE)
  })
  output$team_filter_ui = renderUI({
    req(results())
    res = results() %>% inner_join(team_lookup %>% select(TV_abbr, FullName), join_by('team' == 'TV_abbr')) %>% select(team, FullName) %>% distinct() %>% arrange(FullName)
    choices = res$team
    names(choices) = res$FullName
    pickerInput(inputId = 'team_filter', label = 'Filter on team', choices = choices, multiple = TRUE)
  })
  
  output$odds_range_ui = renderUI({
    req(results())
    max_odds = max(as.numeric(results() %>% filter() %>% pull(Odds)))
    sliderInput("odds_cap", "Odds Cap", min = 0, max = max_odds, value = max_odds, width = '100%')
  })
  
  output$weather_warning = renderText({
    req(results())
    ifelse(any(difftime(results()$posix_timestamp, min(c(player_preds$updated_at, team_preds$updated_at)), units = 'hours') > 47),
       'Note: Games more than 48 hours after the model latest update time do not take into account weather forecast information.',
       '')
  })
  
  results_filtered = reactive({
    req(results())
    req(nrow(results()) > 0)
    
    res = results()
    
    if(!is.null(input$bet_type_filter))
    {
      res = res %>% filter(bet_type %in% input$bet_type_filter)
    }
    if(!is.null(input$timeslot_filter))
    {
      res = res %>% filter(timeslot %in% input$timeslot_filter)
    }
    if(!is.null(input$team_filter))
    {
      res = res %>% filter(team %in% input$team_filter)
    }
    if(!is.null(input$odds_cap))
    {
      res = res %>% filter(as.numeric(Odds)  <= input$odds_cap)
    }
    
    bets_with_evs = request(paste0(api_base_url, 'get_bets_with_ev')) %>%
      req_body_json(list(season = latest_season, data = as.list(res %>% select(response_var, Week, label, Odds, Model_Probability, Position, team, opponent_team, bet_display_name, Betting_Line_Implied_Prob, bet_type, timeslot, game_location, posix_timestamp)))) %>%
      req_perform() %>%
      resp_body_json(simplifyVector = TRUE) %>%
      as.data.frame()

  })
  
  output$instructions = renderUI({
    req(results())
    HTML('If you end up placing an individual bet from the recommendations below, <b>click on the row of the table to log your bet<b>, so we can use this information to improve the model in the future.')
  })
  
  output$results_df = renderDataTable({
    req(results_filtered())
    req(nrow(results_filtered()) > 0)
    results = results_filtered() %>% 
      mutate(Return = round(EVProfitPer100,2), Risk_Score = round(Risk_Score, 2)) %>%
      rename('expected_return_profit_per_100' = 'Return') %>% 
      arrange(desc(expected_return_profit_per_100)) %>%
      select(-ProfitPer100) %>%
      mutate(run_time = format(force_tz(Sys.time(), "America/New_York"), "%Y-%m-%d %I:%M %p"))

    if(is.null(most_recent_save()) || difftime(Sys.time(), most_recent_save(), units = 'hours') > 1)
    {
      tryCatch({
        write_to_supabase('betting', 'BetRecommendations',
                          results %>%
                            mutate(Week = latest_week, Season = latest_season) %>%
                            rename('BettingOn' = 'label',
                                   'Bet_Type' = 'bet_type',
                                   'Label' = 'bet_display_name',
                                   'Team' = 'team',
                                   'Opp' = 'opponent_team',
                                   'Gametime' = 'posix_timestamp',
                                   'Timeslot' = 'timeslot',
                                   'profit_per_100' = 'expected_return_profit_per_100'
                                   ) %>%
                            mutate(Starter = NA,
                                   'Expected_Accuracy' = NA,
                                   'Time' = NA,
                                   Date = as.Date(Gametime)) %>%
                            select(Season, Week, BettingOn, Position, Starter, Bet_Type, Label, Team, Opp, Date, Time, Gametime, Timeslot, Odds, Model_Probability, Betting_Line_Implied_Prob, Expected_Accuracy, profit_per_100, Risk_Score, run_time)
          )
        most_recent_save(Sys.time())
        }, error = function(e) {
          print(e$message)
        })
    }
    
    shinyjs::hide("refresh_message")

    
    results %>% select(bet_display_name, Position, team, opponent_team, game_location, timeslot, Odds, Model_Probability, Betting_Line_Implied_Prob, expected_return_profit_per_100, Risk_Score)  %>%
        datatable(options = list(dom = 'ftp'), rownames = FALSE) %>%
      formatPercentage(c('Model_Probability', 'Betting_Line_Implied_Prob'), digits = 1)
  })
  
  output$popup_portfolio_button_ui = renderUI({
    req(results_filtered())
    req(nrow(results_filtered()) > 0)
    actionButton('popup_portfolio_button', 'Create a Diversified Bet Portfolio For Me')
    })
  
  observeEvent(input$popup_portfolio_button, {
    showModal(modalDialog(
      title = 'Bet Portfolio',
      size = 'l',
      easyClose = TRUE,
      tags$style(HTML("
        .modal-dialog.modal-lg {
          --bs-modal-width: 95vw;
        }
      ")),
      fluidRow(
        column(4, sliderInput("portfolio_num_bets", "Max # of Bets", min = 1, max = 20, step = 1, value = 10)),
        column(4, numericInput("portfolio_bet_amount", "Total Bet Amount ($)", min = 1, max = NA, step = 5, value = 50)),
        column(4, numericInput("min_stake", "Minimum Stake ($) Per Bet", min = 0, max = NA, step = 0.1, value = 0.5))
      ),
      pickerInput("sd_cap", "Risk Tolerance", choices = c('Less Aggressive' = 1.5, 'Moderate' = 2, 'More Aggressive' = 3)),
      actionButton('generate_portfolio', 'Generate Portfolio'),
      tags$br(),
      uiOutput("portfolio_stats"),
      uiOutput("portfolio_view_radio"),
      textOutput("table_sorting_text"),
      dataTableOutput("portfolio_df"),
      uiOutput("portfolio_bet_details"),
      uiOutput("button_log_portfolio_ui"),
      tags$br(),
      uiOutput("optimization_bets"),
      uiOutput("write_logged_portfolio_bets_button_ui")
    ))
})
  
  portfolios_reactive = reactiveVal(NULL)
  
  observeEvent(input$portfolio_num_bets, {
    portfolios_reactive(NULL)
  })
  observeEvent(input$portfolio_bet_amount, {
    portfolios_reactive(NULL)
  })
  observeEvent(input$min_stake, {
    portfolios_reactive(NULL)
  })
  observeEvent(input$sd_cap,{
    portfolios_reactive(NULL)
  })
  
observeEvent(input$generate_portfolio, {
  req(!is.null(input$portfolio_num_bets))
  req(!is.null(input$portfolio_bet_amount))
  req(!is.null(input$sd_cap))
  req(!is.null(input$min_stake))
  table_to_pass = results_filtered() %>%
    select(bet_display_name, response_var, Week, label, Odds, Model_Probability, Position, team, opponent_team, EVProfitPer100, Risk_Score, Type)
  portfolios = request(paste0(api_base_url, 'get_portfolio')) %>%
    req_body_json(list(season = latest_season, data = as.list(table_to_pass),
                       max_bets = input$portfolio_num_bets,
                       total_amount = input$portfolio_bet_amount,
                       sd_cap = as.numeric(input$sd_cap),
                       min_stake = input$min_stake)) %>%
    req_perform() %>%
    resp_body_json(simplifyVector = TRUE) %>%
    as.data.frame()
  portfolios_reactive(portfolios)
  write_to_supabase('betting', 'BetPortfolioRecommendations', 
                    portfolios_reactive() %>%
                      rename('Bet' = 'bet_display_name',
                             'PortfolioID' = 'portfolio_id',
                             'Weight' = 'Portfolio_Weight',
                             'Mu' = 'Portfolio_Mu',
                             'Var' = 'Portfolio_Var',
                             'SD' = 'Portfolio_SD',
                             'Sharpe' = 'Portfolio_Sharpe',
                             ) %>%
                      mutate(Season = latest_season,
                             NumBets = nrow(portfolios_reactive()),
                             Gamma = NA, BetID = NA,
                             updateTime = Sys.time()) %>%
                      select(Season, Week, PortfolioID, BetID, NumBets, Bet, Weight, Odds, Gamma, Mu, Var, SD, Sharpe, updateTime))
  
  output$portfolio_stats = renderUI(tagList(
    p(paste0("Portfolio Expected Value Return: ", 100*round(max(portfolios_reactive()$Portfolio_Mu),3),'%')),
    p(paste('Portfolio Variance:', round(max(portfolios_reactive()$Portfolio_Var),2))),
    p(paste('Portfolio SD:', round(max(portfolios_reactive()$Portfolio_SD),2)))
  ))
  
  output$portfolio_view_radio = renderUI({
    req(portfolios_reactive())
    radioButtons('portfolio_view', '', choices = c('Portfolio View' = TRUE, 'Sportsbook View' = FALSE), inline = TRUE)
    })
  
  portfolio_table_to_show = reactive({
    req(portfolios_reactive())
    req(!is.null(input$portfolio_view))
    table_to_show = portfolios_reactive() %>% left_join(results_filtered() %>% select(bet_display_name, timeslot, game_location, Betting_Line_Implied_Prob, posix_timestamp), join_by('bet_display_name')) %>%
      select(bet_display_name, Position, bet_amount, team, opponent_team, game_location, timeslot, Odds, Model_Probability, Betting_Line_Implied_Prob, posix_timestamp, Type) %>%
      arrange(desc(bet_amount))
    
    if(input$portfolio_view)
    {
      table_to_show = table_to_show %>% select(-posix_timestamp, -Type) %>% arrange(desc(bet_amount))
      output$table_sorting_text = renderText({
        req(portfolios_reactive())
        'This table is sorted in order of descending bet amount.\n\nClick on a bet to view more details below.'
      })
    } else {
      table_to_show = table_to_show %>% mutate(home_team = ifelse(game_location == 'Home', team, ifelse(game_location == 'Away', opponent_team, 'Neutral'))) %>%
        arrange(Type, posix_timestamp, home_team) %>% select(-home_team, -posix_timestamp, -Type)
      output$table_sorting_text = renderText({
        req(portfolios_reactive())
        'This table is sorted as the bets appear in a sportsbook: by bet type and game time.\n\nClick on a bet to view more details below.'
      })
    }
    return(table_to_show)
  })
  
  output$portfolio_df = renderDataTable({
    req(portfolio_table_to_show())
    
    portfolio_table_to_show() %>% datatable(options = list(dom = 'tp'), rownames = FALSE, selection = 'single') %>%
      formatCurrency('bet_amount', digits = 2) %>%
      formatPercentage(c('Model_Probability', 'Betting_Line_Implied_Prob'), digits = 1)
  })
  
  output$button_log_portfolio_ui = renderUI({
    req(portfolios_reactive())
    actionButton('log_portfolio_bets', 'Log Portfolio Bets')
  })
  
  output$portfolio_bet_details = renderUI({
    req(portfolio_table_to_show())
    req(input$portfolio_df_rows_selected)
    
    row_selected = portfolio_table_to_show()[input$portfolio_df_rows_selected,]
    details_this_bet = bet_details() %>% filter(bet_display_name == row_selected$bet_display_name)
    
    req(nrow(details_this_bet) > 0)
    
    tags$details(
      open = TRUE,
      tags$div(style = "padding: 10px 5px 20px 5px;", HTML(details_this_bet$extra_info))
      )
    
  })
})

observeEvent(input$log_portfolio_bets, {
  output$optimization_bets = renderUI({
   req(portfolios_reactive())
    df = portfolios_reactive()
    ids = seq_len(nrow(df))
    
    tagList(
      textInput(inputId = 'optimization_bettor_name', label = 'Your Name'),
      lapply(ids, function(i) {
        bet_label = df$bet_display_name[i]

        tagList(
          fluidRow(
            column(6, numericInput(
              inputId = paste0("portfolio_bet_amt_", i),
              label   = paste("Bet Amount:", bet_label),
              value   = round(df$bet_amount[i],2),
              min     = 0,
              width = '100%')),
            column(6, textInput(
              inputId = paste0("portfolio_bet_odds_", i),
              label   = paste("Bet Odds (+/- then number):", bet_label),
              value   = df$Odds[i],
              width = '100%'))
          ),
          tags$hr()
        )
      })
    )
  })
})
  
  output$write_logged_portfolio_bets_button_ui = renderUI({
    req(input$portfolio_bet_amt_1)
    actionButton('write_logged_portfolio_bets', 'Write')
  })
    
observeEvent(input$write_logged_portfolio_bets, {
portfolio_bet_amounts = c()
portfolio_bet_odds = c()
for (i in 1:nrow(portfolios_reactive()))
{
  portfolio_bet_amounts = c(portfolio_bet_amounts, input[[paste0('portfolio_bet_amt_',i)]])
  portfolio_bet_odds = c(portfolio_bet_odds, input[[paste0('portfolio_bet_odds_',i)]])
}
bets_to_write = portfolios_reactive() %>% left_join(results_filtered() %>% select(bet_display_name, bet_type, timeslot, posix_timestamp), join_by('bet_display_name'))
  bets_to_write$Amount = portfolio_bet_amounts
  bets_to_write$Odds = portfolio_bet_odds
  bets_to_write = bets_to_write %>% 
    filter(Amount > 0) %>%
    mutate(
      id = sample(1:1000000000, nrow(bets_to_write)) %>% as.character(),
      Season = latest_season,
      Week = latest_week,
      Bettor = input$optimization_bettor_name,
      Time_Submitted = format(force_tz(Sys.time(), "America/New_York"), "%Y-%m-%d %I:%M %p"),
      updated_at = Time_Submitted,
      Notes = NA) %>%
    rename(
      'BettingOn' = 'label',
      'Bet_Type' = 'bet_type',
      'Label' = 'bet_display_name',
      'Team' = 'team',
      'Opp' = 'opponent_team',
      'Gametime' = 'posix_timestamp',
      'Timeslot' = 'timeslot',
      'Mu_Port' = 'Portfolio_Mu',
      'SD_Port' = 'Portfolio_SD',
      'Sharpe_Port' = 'Portfolio_Sharpe',
      'PortfolioID' = 'portfolio_id'
    ) %>% mutate(Type = 'Optimization Recommender') %>% select(id, Season, Week, Bettor, Bet_Type, Label, Odds, Amount, Time_Submitted, Type, Mu_Port, SD_Port, Sharpe_Port, Team, Opp, Gametime, Timeslot, BettingOn, PortfolioID)
  
  write_to_supabase('betting', 'BetsPlaced', bets_to_write)
})
  
  observeEvent(input$click, {
    print(input$click$name)
    print(input$click$bet)
  
    output$name_text = renderText(paste('Name:', input$click$name))
    output$bet_text = renderText(paste('Bet:', ifelse(input$click$bet == 'Anytime TD Scorer', input$click$bet, paste(input$click$bet, input$click$label))))
    
    details_this_bet = bet_details() %>% filter(bet_display_name == input$click$bet)
    output$detailed_player_info = renderUI(HTML(details_this_bet$extra_info))
    
    showModal(modalDialog(
      tags$h1('Log Bet'),
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
      )
    ))
  })
  
  observeEvent(input$submit_bet, {
    info_this_row = results_filtered() %>% filter(bet_display_name == input$click$bet)
    row_to_write = info_this_row %>%
      mutate(
        id = sample(1:1000000000, 1) %>% as.character(),
        Season = latest_season,
        Week = latest_week,
        Bettor = input$bettor_name,
        BettingOn = max(info_this_row$label),
        Bet_Type = max(info_this_row$bet_type),
        Label = input$click$bet,
        Odds = input$bet_odds %>% as.character(),
        Amount = input$bet_amt,
        Time_Submitted = format(force_tz(Sys.time(), "America/New_York"), "%Y-%m-%d %I:%M %p"),
        Type = 'Individual Bet',
        Team = max(info_this_row$team),
        Opp = max(info_this_row$opponent_team),
        Gametime = max(info_this_row$posix_timestamp),
        Timeslot =  max(info_this_row$timeslot)
      ) %>% select(id, Season, Week, Bettor, Bet_Type, Label, Odds, Amount, Time_Submitted, Type, Team, Opp, Gametime, Timeslot, BettingOn)
    
    write_to_supabase('betting', 'BetsPlaced', row_to_write)
    
  })
  
  
  placed_bets = reactiveVal(get_supabase_data('betting', 'BetsPlaced') %>% filter(Season == latest_season))
  unsettled_bets = reactive({ placed_bets() %>% filter(is.na(Result)) %>% mutate(bet_descriptions = paste(Bettor, 'Week', Week, '-', Label)) })
  
  output$bettor_selection_ui = renderUI({
    pickerInput(inputId = 'bettor_selection', label = 'Filter on bettor name', choices = unique(unsettled_bets()$Bettor), multiple = TRUE, selected = NULL)
  })
  
  unspecified_bets_filtered = reactive({
    req(unsettled_bets())
    if(!is.null(input$bettor_selection))
    {
      unsettled_bets() %>% filter(Bettor %in% input$bettor_selection)
    } else {
      unsettled_bets()
    }
  })
  
  observeEvent(input$refresh_bet_updates, {
    shinyjs::show("refresh_bet_updates_message")
    shinyjs::disable("refresh_bet_updates")
    placed_bets(get_supabase_data('betting', 'BetsPlaced') %>% filter(Season == latest_season))
    
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
          choices  = c("Win", "Loss", "Refund", "Unfinished"),
          selected = "Unfinished",
          inline   = TRUE
        )
      })
    tagList(radio_list)
  } else {
    tags$p("No outstanding bets available to update.")
  }
  })
  
  output$save_button = renderUI({
    req(nrow(unspecified_bets_filtered()) > 0)
    actionButton('save', 'Save')
  })

  observeEvent(input$save, {
    temp = unspecified_bets_filtered() %>% mutate(Result = 'Unfinished') %>% select(id, Season, Week, Bettor, Result)
    for (i in 1:nrow(temp))
    {
      bet_id = temp$id[i]
      selection = input[[paste0('result_',bet_id)]]
      temp$Result[temp$id == bet_id] = selection
    }
    table_to_write = temp %>% filter(!(Result %in% c('Unfinished'))) %>% mutate(updated_at = Sys.time())
    tryCatch({
      upsert_to_supabase('betting', 'BetsPlaced', table_to_write, c('id','Season','Week','Bettor'))
    }, error = function(e) {
      showNotification(paste0("❌ Failed to write: ", e$message), type = "error", duration = 7)
    }, finally = {
      shinyjs::enable("write_row")
      placed_bets(get_supabase_data('betting', 'BetsPlaced') %>% filter(Season == latest_season))
    })
  })


   
   #CHEAT SHEETS
  current_week_bets = reactive(placed_bets() %>% filter(Week == latest_week & Season == latest_season)  %>% select(id, Bettor, Season, Week, Bet_Type, Label, Team, Opp, Gametime, Timeslot))

  output$bettor_choice_ui = renderUI({
    req(current_week_bets())
    pickerInput(inputId = 'cheat_sheet_bettors', label = 'Choose bettors to view on cheat sheet', choices = sort(unique(current_week_bets()$Bettor)), multiple = TRUE)
  })
  
  bets_for_rmd = reactive({
    req(current_week_bets())
    req(input$cheat_sheet_bettors)
    current_week_bets() %>% filter(Bettor %in% input$cheat_sheet_bettors)
  })

  output$survivor_teams_ui = renderUI({
    team_choices = team_lookup$TV_abbr
    names(team_choices) = team_lookup$FullName
    pickerInput(inputId = 'survivor_teams', label = 'Survivor pool teams for this week', choices = sort(team_choices), multiple = TRUE)
  })
  output$cheat_sheet_button_ui = renderUI({
    req(input$cheat_sheet_bettors)
    downloadButton('cheat_sheet_button', "Generate This Week's Cheat Sheet")
  })
  output$cheat_sheet_button = downloadHandler(
    filename = function() {
      paste0('Week', latest_week, 'CheatSheet.html')
    },
    content = function(file) {
      params = list(
        bet_table =  bets_for_rmd(),
        survivor_teams = input$survivor_teams,
        bet_details = bet_details() %>% filter(bet_display_name %in% bets_for_rmd()$Label),
        team_lookup_table = team_lookup,
        season = latest_season,
        week = latest_week
      )
      out_dir   = tempdir()
      out_name  = paste0('Week', latest_week, '-CheatSheet.html')
      out_path  = file.path(out_dir, out_name)

      rmarkdown::render(
        input         = 'BetWeeklySummary.Rmd',
        output_format = 'html_document',
        output_file   = out_name,
        output_dir    = out_dir,
        params        = params,
        envir         = new.env(parent = globalenv())
      )

      file.copy(from = out_path, to = file, overwrite = TRUE)
    }
  )
 
  output$choose_bettor_summary_ui = renderUI({
    pickerInput(inputId = 'summary_bettors', label = 'Choose bettors to view summaries', choices = sort(unique(placed_bets()$Bettor)), multiple = TRUE)
  })

  output$summary_only_finished = renderUI({
    checkboxInput(inputId = 'summary_finished', label = "Do not include unfinished bets", value = FALSE)
  })

  bet_summary_filtered = reactive({
    req(placed_bets())
    req(length(input$summary_bettors) > 0)
    bet_subset = placed_bets() %>% filter(Bettor %in% input$summary_bettors)
    if(input$summary_finished)
    {
      bet_subset = bet_subset %>% filter(!is.na(Result))
    }
    bet_subset %>% mutate(Payout = ifelse(Result == 'Win', ifelse(Odds > 0, Amount+(Amount*Odds)/100, Amount + (100*Amount/(-1*Odds))),
                                          ifelse(Result == 'Refund', Amount, 0)))
  })

  output$bettor_summary_header = renderUI({
    req(bet_summary_filtered())
    df = bet_summary_filtered() %>% filter(!is.na(Result))
    req(nrow(df) > 0)
    
    total_bet = sum(df$Amount, na.rm = TRUE)
    total_payout = sum(df$Payout, na.rm = TRUE)
    total_return = total_payout - total_bet
    total_return_pct = ifelse(total_bet == 0, NA, total_return / total_bet)
    win_rate = mean(df$Result == "Win", na.rm = TRUE)
    
    tags$div(
      style = "border:1px solid #d9d9d9; border-radius:10px; padding:16px 18px; margin-bottom:18px; background-color:#fafafa;",
      tags$h3(style = "margin-top:0; margin-bottom:10px;", "Bettor Summary"),
      HTML(paste0(
        "<b>Bettors:</b> ", paste(sort(unique(df$Bettor)), collapse = ", "), "<br>",
        "<b>Completed Bets:</b> ", nrow(df), "<br>",
        "<b>Win Rate:</b> ", round(100 * win_rate, 1), "%<br>",
        "<b>Total Bet:</b> $", formatC(total_bet, format = "f", digits = 2, big.mark = ","), "<br>",
        "<b>Total Payout:</b> $", formatC(total_payout, format = "f", digits = 2, big.mark = ","), "<br>",
        "<b>Total Return:</b> $", formatC(total_return, format = "f", digits = 2, big.mark = ","), 
        " (", round(100 * total_return_pct, 1), "%)"
      ))
    )
  })

  weekly_bet_summaries = reactive({
    req(bet_summary_filtered())
    
    bet_summary_filtered() %>%
      group_by(Bettor, Season, Week) %>%
      summarise(
        Number_Bets_Placed = n(),
        Pct_Win = ifelse(sum(!is.na(Result)) == 0, NA_real_, mean(Result == 'Win', na.rm = TRUE)),
        Total_Bet = sum(Amount, na.rm = TRUE),
        Total_Payout = sum(Payout, na.rm = TRUE),
        Total_Return = Total_Payout - Total_Bet,
        Total_Return_Percent = Total_Return / Total_Bet,
        .groups = 'drop'
      ) %>%
      arrange(Bettor, Season, Week)
  })
  
  remove_dup_bets = reactive({
    req(bet_summary_filtered())
    
    bet_summary_filtered() %>%
      arrange(Week, Label) %>%
      group_by(Season, Week, Label) %>%
      slice(1) %>%
      ungroup()
  })
  
  return_summary = function(df, group_col) {
    df %>%
      filter(!is.na(Result)) %>%
      group_by(.data[[group_col]]) %>%
      summarise(
        Number_Bets_Placed = n(),
        Pct_Win = mean(Result == 'Win'),
        Total_Bet = sum(Amount, na.rm = TRUE),
        Total_Payout = sum(Payout, na.rm = TRUE),
        Total_Return = Total_Payout - Total_Bet,
        Total_Return_Percent = Total_Return / Total_Bet,
        .groups = 'drop'
      )
  }
  
  return_bar_plot = function(df, category_col, title) {
    df$Category = as.character(df[[category_col]])
    
    df = df %>%
      mutate(
        Return_Label = paste0(round(100 * Total_Return_Percent, 1), "%"),
        Tooltip = paste0(
          "<b>", Category, "</b><br>",
          "Return: ", Return_Label, "<br>",
          "# of bets: ", Number_Bets_Placed, "<br>",
          "Win rate: ", round(100 * Pct_Win, 1), "%<br>",
          "Total bet: $", formatC(Total_Bet, format = "f", digits = 2, big.mark = ","), "<br>",
          "Total payout: $", formatC(Total_Payout, format = "f", digits = 2, big.mark = ","), "<br>",
          "Total return: $", formatC(Total_Return, format = "f", digits = 2, big.mark = ","),
          "<extra></extra>"
        )
      )
    
    neg_max = max(abs(df$Total_Return_Percent[df$Total_Return_Percent < 0]), na.rm = TRUE)
    pos_max = max(df$Total_Return_Percent[df$Total_Return_Percent > 0], na.rm = TRUE)
    
    if(!is.finite(neg_max) || neg_max == 0) neg_max = 1
    if(!is.finite(pos_max) || pos_max == 0) pos_max = 1
    
    df = df %>%
      mutate(
        Color_Score = pmax(-1, pmin(1, Total_Return_Percent))
      )
    
    palette = grDevices::colorRampPalette(
      c("#c0392b", "#f7f7f7", "#1e8449")
    )(201)
    
    df$Bar_Color = palette[
      round((df$Color_Score + 1) / 2 * 200) + 1
    ]
    
    y_min = min(c(0, df$Total_Return_Percent), na.rm = TRUE)
    y_max = max(c(0, df$Total_Return_Percent), na.rm = TRUE)
    y_range = y_max - y_min
    pad = max(.10, .12 * y_range)
    
    plot_ly(
      df,
      x = ~Category,
      y = ~Total_Return_Percent,
      type = "bar",
      text = ~Return_Label,
      textposition = "outside",
      cliponaxis = FALSE,
      hovertemplate = ~Tooltip,
      marker = list(
        color = df$Bar_Color,
        line = list(color = "rgba(0,0,0,.2)", width = 1)
      ),
      showlegend = FALSE
    ) %>%
      layout(
        title = list(text = title),
        xaxis = list(title = "", automargin = TRUE),
        yaxis = list(
          title = "Return",
          tickformat = ".1%",
          zeroline = TRUE,
          range = c(y_min - pad, y_max + pad),
          automargin = TRUE
        ),
        font = list(color = "#222222"),
        paper_bgcolor = "white",
        plot_bgcolor = "white",
        margin = list(b = 110, t = 60, l = 70, r = 20)
      )
  }
  
  output$bet_summary_by_week = renderPlotly({
    df = weekly_bet_summaries()
    req(nrow(df) > 0)
    
    df = df %>%
      mutate(
        Return_Label = paste0(round(100 * Total_Return_Percent, 1), "%"),
        Tooltip = paste0(
          "<b>", Bettor, " - ", Season, " Week ", Week, "</b><br>",
          "Return: ", Return_Label, "<br>",
          "# of bets: ", Number_Bets_Placed, "<br>",
          "Win rate: ", ifelse(is.na(Pct_Win), "N/A", paste0(round(100 * Pct_Win, 1), "%")), "<br>",
          "Total bet: $", formatC(Total_Bet, format = "f", digits = 2, big.mark = ","), "<br>",
          "Total payout: $", formatC(Total_Payout, format = "f", digits = 2, big.mark = ","), "<br>",
          "Total return: $", formatC(Total_Return, format = "f", digits = 2, big.mark = ","),
          "<extra></extra>"
        )
      )
    
    plot_ly(
      df,
      x = ~Week,
      y = ~Total_Return_Percent,
      color = ~Bettor,
      type = "scatter",
      mode = "lines+markers+text",
      text = ~Return_Label,
      textposition = "top center",
      hovertemplate = ~Tooltip
    ) %>%
      layout(
        title = list(text = "Weekly Bet Return"),
        xaxis = list(
          title = "Week",
          dtick = 1,
          range = if(length(unique(df$Week)) == 1) c(df$Week[1] - .5, df$Week[1] + .5) else NULL
        ),
        yaxis = list(title = "Return", tickformat = ".1%", zeroline = TRUE, automargin = TRUE),
        legend = list(title = list(text = "Bettor")),
        font = list(color = "#222222"),
        paper_bgcolor = "white",
        plot_bgcolor = "white",
        margin = list(b = 70, t = 60, l = 70, r = 20)
      )
  })
  
  output$bet_summary_by_type = renderPlotly({
    df = return_summary(remove_dup_bets(), "Bet_Type")
    req(nrow(df) > 0)
    
    return_bar_plot(df, "Bet_Type", "Return by Bet Type")
  })
  
  output$bet_summary_by_portfolio_type = renderPlotly({
    df = return_summary(remove_dup_bets(), "Type")
    req(nrow(df) > 0)
    
    return_bar_plot(df, "Type", "Return by Optimizer vs Individual Bet")
  })
  
  output$bet_summary_by_odds_range = renderPlotly({
    levels_odds = c(
      'Negative Odds',
      'Positive Odds Up To +250',
      'Odds +251 to +450',
      'Odds +451 to +650',
      'Odds +651 to +1000',
      'Odds +1000 to +2000',
      'Odds above +2000'
    )
    
    df = remove_dup_bets() %>%
      mutate(
        Odds_Range = case_when(
          Odds < 0 ~ 'Negative Odds',
          Odds <= 250 ~ 'Positive Odds Up To +250',
          Odds <= 450 ~ 'Odds +251 to +450',
          Odds <= 650 ~ 'Odds +451 to +650',
          Odds <= 1000 ~ 'Odds +651 to +1000',
          Odds <= 2000 ~ 'Odds +1000 to +2000',
          TRUE ~ 'Odds above +2000'
        ),
        Odds_Range = factor(Odds_Range, levels = levels_odds, ordered = TRUE)
      ) %>%
      return_summary("Odds_Range") %>%
      arrange(Odds_Range)
    
    req(nrow(df) > 0)
    
    return_bar_plot(df, "Odds_Range", "Return by Odds Range")
  })
  
  plot_box = function(plot_output_id, height = "400px") {
    div(
      style = paste0(
        "border:1px solid #d9d9d9;",
        "border-radius:10px;",
        "padding:12px;",
        "margin-bottom:18px;",
        "background-color:white;"
      ),
      plotlyOutput(plot_output_id, height = height)
    )
  }
  
  output$weekly_plot_ui = renderUI({
    req(nrow(weekly_bet_summaries()) > 0)
    plot_box("bet_summary_by_week", "400px")
  })
  
  output$bet_type_plot_ui = renderUI({
    df = return_summary(remove_dup_bets(), "Bet_Type")
    req(nrow(df) > 0)
    plot_box("bet_summary_by_type", "400px")
  })
  
  output$portfolio_type_plot_ui = renderUI({
    df = return_summary(remove_dup_bets(), "Type")
    req(nrow(df) > 0)
    plot_box("bet_summary_by_portfolio_type", "400px")
  })
  
  output$odds_range_plot_ui = renderUI({
    df = remove_dup_bets() %>% filter(!is.na(Result))
    req(nrow(df) > 0)
    plot_box("bet_summary_by_odds_range", "450px")
  })
  
  
  
  #SURVIVOR DASHBOARD
  
  current_spread_lines = reactive({
    spreads = get_spreads()
    spreads_with_bets = spreads %>% left_join(team_preds %>% filter(response_var == 'team_win') %>% mutate(team = ifelse(team == 'LA','LAR',team)),
                                              join_by('team')) %>%
      mutate(match_name = paste(label, 'Moneyline')) %>%
      left_join(bet_details() %>% filter(str_detect(bet_display_name, 'Moneyline')), join_by('match_name' == 'bet_display_name')) %>%
      left_join(team_lookup %>% select(TV_abbr, FullName) %>% rename('OppFullName' = 'FullName'), join_by('opponent_team' == 'TV_abbr')) %>%
      select(label, points, team, opponent_team, OppFullName, Model_Probability, timeslot, game_location, extra_info) %>% arrange(points)
  })
  
  output$survivor_filters = renderUI({
    choices = team_lookup$TV_abbr
    names(choices) = team_lookup$FullName
    pickerInput('survivor_removal', 'Teams to Exclude', choices, multiple = TRUE)
  })
  
  output$survivor_display = renderUI({
    req(current_spread_lines())
    
    if(!is.null(input$survivor_removal))
    {
      lines = current_spread_lines() %>% filter(!(team %in% input$survivor_removal))
    } else {
      lines = current_spread_lines()
    }
    schedules = load_schedules(latest_season:latest_season) %>% clean_homeaway()
    
    spread_html = lapply(seq_len(nrow(lines)), function(b)
    {
      bet = lines[b,]
      upcoming_opponents = schedules %>% mutate(team = ifelse(team == 'LA', 'LAR', team)) %>% filter(team == bet$team, week > latest_week) %>% select(week, opponent, location)
      
      HTML(paste0(
        '<h2>', bet$label, ' (', ifelse(bet$points > 0, '+', ''), bet$points, ' against ', bet$OppFullName, ' - ', ifelse(bet$game_location == 'Neutral', 'Neutral Field', bet$game_location), ')', '</h2>',
        '<details><summary>View Game Details</summary>', gsub('Odds', 'Moneyline Odds',
             gsub('<h2>.* Moneyline</h2>', '', bet$extra_info)),
        '</details>',
        '<details><summary>View upcoming opponents (<b>home games bold</b>)</summary>',
        '<p>',paste(sapply(seq_len(nrow(upcoming_opponents)), function(x) ifelse(upcoming_opponents$location[x] == 'home', paste0('<b>',upcoming_opponents$opponent[x],'</b>'), upcoming_opponents$opponent[x])), collapse = ', '), '</p>',
        '</details><br>'
      ))
    })
    #locally:
    #htmltools::html_print(htmltools::tagList(spread_html))
    
    htmltools::tagList(spread_html)
    
    
  })

}


#next steps:
#Deploy betting lines script to gcp
#update the update-midweek-data branch to update depth charts and player bios (changed team)
#cheat sheet Rmd

# Run the application 
shinyApp(ui = ui, server = server)
