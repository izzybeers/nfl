#Betting lines scraper
library(googlesheets4)
library(httr)
library(jsonlite)
library(dplyr)
library(furrr)

gs4_auth(cache = ".secrets", email = "izzyb961@gmail.com")

get_props = function(bet_category) {
  bet_id = case_when(
    bet_category == 'Receiving' ~ '16570',
    bet_category == 'Receptions' ~ '16821',
    bet_category == 'Rushing' ~ '16571',
    bet_category == 'RushingAttempts' ~ '16820',
    bet_category == 'RushRec' ~ '16572',
    bet_category == 'Touchdown' ~ '12438',
    bet_category == 'Passing' ~ '16569',
    bet_category == 'Team' ~ '4518',
    bet_category == 'SpreadAlternate' ~ '13195',
    bet_category == 'TotalAlternate' ~ '13196'
  )

  base_url = paste0("https://sportsbook-nash.draftkings.com/sites/US-NJ-SB/api/sportscontent/controldata/league/leagueSubcategory/v1/markets?isBatchable=false&templateVars=%2C",bet_id,"&eventsQuery=%24filter%3DleagueId%20eq%20%2788808%27%20AND%20clientMetadata%2FSubcategories%2Fany%28s%3A%20s%2FId%20eq%20%27", bet_id, "%27%29&marketsQuery=%24filter%3DclientMetadata%2FsubCategoryId%20eq%20%27",bet_id,"%27%20AND%20tags%2Fall%28t%3A%20t%20ne%20%27SportcastBetBuilder%27%29&include=Events&entity=events")
  
  events = fromJSON(content(GET(base_url), as = "text", encoding = "UTF-8"))$events 
  markets = fromJSON(content(GET(base_url), as = "text", encoding = "UTF-8"))$markets
  selections = fromJSON(content(GET(base_url), as = "text", encoding = "UTF-8"), flatten = TRUE)$selections
  
  if(length(events) > 0)
  {
    events = events %>% select(id, leagueId, sportId, name, startEventDate, status, subscriptionKey) %>% rename('EventName' = 'name', 'eventId' = 'id', 'eventSubscriptionKey' = 'subscriptionKey')
  } else{
    events = NULL
  }
  if(length(markets) > 0)
  {
    markets = markets  %>% select(id, eventId, name, subcategoryId, subscriptionKey) %>% rename('marketId' = 'id', 'marketSubscriptionKey' = 'subscriptionKey')
  } else {
    markets = NULL
  }
  if(length(selections) > 0)
  {
    selections = selections  %>% select(any_of(c('id', 'marketId', 'label', 'outcomeType', 'points', 'displayOdds.american', 'displayOdds.decimal',  'trueOdds'))) %>% rename('selectionId' = 'id', 'americanOdds' = `displayOdds.american`, 'decimalOdds' = `displayOdds.decimal`)
  } else {
    selections = NULL
  }
  
  if(!is.null(events) & !is.null(markets) & !is.null(selections))
  {
    lines = events %>% inner_join(markets, join_by('eventId')) %>%
      inner_join(selections, join_by('marketId')) %>% mutate(runTime = Sys.time())
  } else {
    lines = NULL
  }
  
  if(!is.null(lines))
  {
    lines$americanOdds <- gsub("\u2212", "-", lines$americanOdds)
    return(lines)
  } else{
    return(NULL)
  }
}


props = future_map(.x = c('Passing', 'Rushing', 'Receiving', 'Receptions', 'RushRec', 'RushingAttempts', 'Touchdown', 'Team', 'SpreadAlternate', 'TotalAlternate'),
                           .f = get_props) %>%
  bind_rows()
props$name = gsub('\\(.*\\)', '', props$name) %>% trimws()

# sheet_id = '1SxAtw9aKAJ7hs4_ktmdcU-PBOPzgTJkNHLMANav-OoY'
sheet_id= '1wRpBMnZEkAcRJUwMxvJlUDvFYlQ2go5l_ihMpYdCL34'

sheet_append(ss = sheet_id, data = props, sheet = 'BettingLines')
