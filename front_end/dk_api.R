library(plumber)
library(httr)
library(jsonlite)
library(dplyr)
library(stringr)

ua = paste0(
  "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) ",
  "AppleWebKit/537.36 (KHTML, like Gecko) ",
  "Chrome/150.0.0.0 Safari/537.36"
)


#* Is the local API alive?
#* @get /health
function()
{
  list(
    status = "ok",
    machine = Sys.info()[["nodename"]],
    time = as.character(Sys.time())
  )
}

base_url_root = 'https://sportsbook-nash.draftkings.com/'

#* @get /get_bet_ids_by_category
function()
{
  url = paste0(base_url_root, "api/sportscontent/dkusnj/v1/leagues/88808")
  response = tryCatch({
    GET(url, user_agent("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/150.0.0.0 Safari/537.36"),
        httr::config(followlocation = FALSE))
  }, error = function(e) {
    print(e$message)
    message("DK category request failed. Status: ", status_code(response))
    message("Response: ", substr(content(response, as = "text", encoding = "UTF-8"), 1, 500))
    return(NULL)
  })
  if(is.null(response))
  {
    message("DK category request failed, second block. Status: ", status_code(response))
    message("Response: ", substr(content(response, as = "text", encoding = "UTF-8"), 1, 500))
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
      filter((str_detect(name, 'Yards|TD Scorer|Receptions') | name %in% c('Alternate Spread','Game')) & !str_detect(name, 'O/U')) %>%
      mutate(response_var_root = case_when(
        str_detect(tolower(name), 'pass') ~ 'passing_yards',
        str_detect(tolower(name), 'rush') ~ 'rushing_yards',
        str_detect(tolower(name), 'receiving') ~ 'receiving_yards',
        str_detect(tolower(name), 'td') ~ 'anytime_td_scorer',
        str_detect(tolower(name), 'receptions') ~ 'receptions',
        str_detect(tolower(name), 'game') ~ 'team_win',
        str_detect(tolower(name), 'spread') ~ 'team_differential',
        str_detect(tolower(name), 'rec') & str_detect(name,tolower('rush')) ~ 'rushing_receiving_yards',
        .default = NA
      ),
      category = ifelse(category_name == 'Game Lines', 'Team', 'Player'))
    
    return(all_categories)
  } else {
    message("DK subcategory request failed. Status: ", status_code(response))
    message("Response: ", substr(content(response, as = "text", encoding = "UTF-8"), 1, 500))
    return(NULL)
  }
}

#* @get /lines_by_bet_id
function(bet_id)
{
  base_url = paste0(base_url_root, "sites/US-NJ-SB/api/sportscontent/controldata/league/leagueSubcategory/v1/markets?isBatchable=false&templateVars=%2C",bet_id,"&eventsQuery=%24filter%3DleagueId%20eq%20%2788808%27%20AND%20clientMetadata%2FSubcategories%2Fany%28s%3A%20s%2FId%20eq%20%27", bet_id, "%27%29&marketsQuery=%24filter%3DclientMetadata%2FsubCategoryId%20eq%20%27",bet_id,"%27%20AND%20tags%2Fall%28t%3A%20t%20ne%20%27SportcastBetBuilder%27%29&include=Events&entity=events")
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
  return(jsonlite::fromJSON(
    httr::content(response, as = "text", encoding = "UTF-8")
  ))
}