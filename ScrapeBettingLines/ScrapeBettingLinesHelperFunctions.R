library(httr)
library(jsonlite)
library(stringr)
suppressPackageStartupMessages(library(dplyr))

print(paste('Running at', Sys.time()))
readRenviron(".Renviron")
SUPABASE_URL = Sys.getenv('SUPABASE_URL')
SUPABASE_KEY = Sys.getenv('SUPABASE_KEY')
base_url_root = Sys.getenv('DRAFTKINGS_BASE_URL_ROOT')

write_to_supabase = function(schema, table_name, df, batch_size = 500)
{
  url = paste0(SUPABASE_URL, "/rest/v1/", table_name)
  
  for (start_row in seq(1, nrow(df), by = batch_size))
  {
    end_row = min(start_row + batch_size - 1, nrow(df))
    
    batch = df[start_row:end_row, ]
    
    body_data = toJSON(
      batch,
      dataframe = "rows",
      auto_unbox = TRUE,
      na = "null",
      null = "null",
      digits = NA
    )
    
    response = POST(
      url,
      add_headers(
        "apikey" = SUPABASE_KEY,
        "Authorization" = paste("Bearer", SUPABASE_KEY),
        "Content-Type" = "application/json",
        "Content-Profile" = schema,
        "Prefer" = "return=minimal"
      ),
      body = body_data
    )
    
    if (http_error(response))
    {
      stop(
        paste(
          "Failed on rows", start_row, "to", end_row, ":",
          content(response, "text", encoding = "UTF-8")
        )
      )
    }
    
    print(paste("Wrote rows", start_row, "to", end_row))
  }
  
  print(paste("Successfully wrote", nrow(df), "rows to", table_name))
  return(TRUE)
}

get_bet_ids_by_category = function()
{
  url = paste0(base_url_root, "api/sportscontent/dkusnj/v1/leagues/88808")
  response = tryCatch({
    GET(url, user_agent("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/150.0.0.0 Safari/537.36"))
  }, error = function(e) {
    print(e$message)
    return(NULL)
  })
  if(is.null(response))
  {
    return (NULL)
  }
  if (status_code(response) == 200)
  {
    dk_meta = fromJSON(
      content(response, as = "text", encoding = "UTF-8"),
      flatten = TRUE
    )
    categories = dk_meta$categories %>% select(id, name) %>% rename(category_name = name, categoryId = id)
    subcategories = dk_meta$subcategories %>% select(id, categoryId, name)
    all_categories = categories %>%
      left_join(subcategories, join_by('categoryId')) %>%
      filter(str_detect(category_name, 'Props|TD Scorers|Game Lines')) %>%
      filter((str_detect(name, 'Yards|TD Scorer|Receptions') | name %in% c('Alternate Spread', 'Alternate Total', 'Game')) & !str_detect(name, 'O/U'))
    return(all_categories)
  } else {
    return(NULL)
  }
}

get_props = function() {
  
  props_lookup = get_bet_ids_by_category()
  if (is.null(props_lookup) || nrow(props_lookup) == 0) {
    return(NULL)
  }
  lines_df = data.frame()
  for (i in 1:nrow(props_lookup))
  {
    bet_id = props_lookup$id[i]
    bet_name = props_lookup$name[i]
    response_var_root = props_lookup$response_var_root[i]
    
    base_url = paste0(base_url_root, "sites/US-NJ-SB/api/sportscontent/controldata/league/leagueSubcategory/v1/markets?isBatchable=false&templateVars=%2C",bet_id,"&eventsQuery=%24filter%3DleagueId%20eq%20%2788808%27%20AND%20clientMetadata%2FSubcategories%2Fany%28s%3A%20s%2FId%20eq%20%27", bet_id, "%27%29&marketsQuery=%24filter%3DclientMetadata%2FsubCategoryId%20eq%20%27",bet_id,"%27%20AND%20tags%2Fall%28t%3A%20t%20ne%20%27SportcastBetBuilder%27%29&include=Events&entity=events")
    
    base_url = paste0("https://sportsbook-nash.draftkings.com/sites/US-NJ-SB/api/sportscontent/controldata/league/leagueSubcategory/v1/markets?isBatchable=false&templateVars=%2C",bet_id,"&eventsQuery=%24filter%3DleagueId%20eq%20%2788808%27%20AND%20clientMetadata%2FSubcategories%2Fany%28s%3A%20s%2FId%20eq%20%27", bet_id, "%27%29&marketsQuery=%24filter%3DclientMetadata%2FsubCategoryId%20eq%20%27",bet_id,"%27%20AND%20tags%2Fall%28t%3A%20t%20ne%20%27SportcastBetBuilder%27%29&include=Events&entity=events")
    response = GET(
      base_url, user_agent("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/150.0.0.0 Safari/537.36"),
      add_headers(
        "Accept" = "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.7",
        "Accept-Language" = "en-US,en;q=0.9",
        "Cache-Control" = "no-cache",
        "Pragma" = "no-cache",
        "Upgrade-Insecure-Requests" = "1"
      )
    )
    data = fromJSON(content(response, as = "text", encoding = "UTF-8"), flatten = TRUE)
    
    events = data$events 
    markets = data$markets
    selections = data$selections
    
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
        inner_join(selections, join_by('marketId'))
    } else {
      lines = NULL
    }
    
    if(!is.null(lines))
    {
      lines$americanOdds <- gsub("\u2212", "-", lines$americanOdds)
      lines_df = bind_rows(lines_df, lines%>% mutate(runTimeUTC = lubridate::with_tz(Sys.time(), "UTC") %>% as.character(),
                                                     startEventDateUTC = lubridate::with_tz(startEventDate, "UTC")) %>% select(-startEventDate))
    }
  }
  return(lines_df)
}
