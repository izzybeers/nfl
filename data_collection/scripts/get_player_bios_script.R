library(stringr)
library(dplyr)
library(purrr)
source('data_collection/scripts/global.R')


##First, pull the master list of players and extract their basic info like name, position, and active years
get_player_bios = function(year_cutoff, max_year_cutoff, draft_year = year(Sys.Date()))
{
  get_players_by_letter = function(let)
  {
    t1 = Sys.time()
    print(let)
    player_list = get_html_content(url = paste0("https://www.pro-football-reference.com/players/",let))
    if (any(str_detect(player_list %>% html_nodes("p") %>% as.character(), 'block traffic')))
    {
      print("Site detected scraping. Try again in one hour.")
      Sys.sleep(65*60)
      player_list = get_html_content(url = paste0("https://www.pro-football-reference.com/players/",let))
    }
    
    player_list_content = player_list %>% html_nodes(".section_content#div_players")
    total_info = player_list_content %>% html_nodes("p") %>% html_text(trim = TRUE)
    years = str_extract(total_info, '[0-9]+-[0-9]+')
    min_year = sapply(strsplit(years, "-"), function(x) x[1])
    max_year = sapply(strsplit(years, "-"), function(x) x[2])
    positions = gsub('\\(|\\)','',str_extract(total_info, "\\(.*\\)"))
    names = gsub(' \\(', '', str_extract(total_info, '.+\\('))
    links = player_list_content %>% html_nodes("a") %>% html_attr("href")
    player_id = gsub('.htm|/[A-Z]/', '', links %>% str_extract('/[A-Z]/.*\\.htm'))
    
    df = data.frame(player_id, names, positions, min_year, max_year, links) %>% mutate(min_year = as.numeric(min_year), max_year = as.numeric(max_year))
    
    df_filtered = df %>% filter(max_year >= year_cutoff & min_year <= max_year_cutoff) #must have been active at some point during the year cutoffs. not including IR years, just based on career years.
    Sys.time() - t1
    wait = runif(1,5,10)
    Sys.sleep(wait)
    
    return(df_filtered)
  }
  
  player_df = bind_rows(map(.x = LETTERS, .f = get_players_by_letter))

  #draft players missing from main df:
  draft_player_list = get_html_content(url = paste0('https://www.pro-football-reference.com/years/',draft_year,'/draft.htm'))
  names = draft_player_list %>% html_nodes('td[data-stat="player"] a') %>% html_text(trim = TRUE)
  positions = draft_player_list %>% html_nodes('td[data-stat="pos"]') %>% html_text(trim = TRUE)
  links = draft_player_list %>% html_nodes('td[data-stat="player"] a') %>% html_attr("href")
  player_id =  gsub('.htm|/[A-Z]/', '', links %>% str_extract('/[A-Z]/.*\\.htm'))
  min_year = draft_year
  max_year = year(Sys.Date())
  draft_df = data.frame(player_id, names, positions, min_year = min_year, max_year = max_year, links = links) %>% filter(!(player_id %in% player_df$player_id))
  
  player_df = bind_rows(player_df, draft_df)

  players_with_positions = player_df %>% filter(positions != '')
  
  receiving_positions = c("WR", "SE", "FL", "TE", "WB", "RB", "HB", "FB", "TB")
  
  rushing_positions = c("RB", "HB", "TB", "FB", "QB", "WR", "WB", "TE")
  
  passing_positions = c("QB")
  
  players_with_positions$receiving_model = sapply(strsplit(players_with_positions$positions, '-|/|,'), function(x) ifelse(any(x[[1]] %in% receiving_positions), 1, 0))
  players_with_positions$rushing_model = sapply(strsplit(players_with_positions$positions, '-|/|,'), function(x) ifelse(any(x[[1]] %in% rushing_positions), 1, 0))
  players_with_positions$passing_model = sapply(strsplit(players_with_positions$positions, '-|/|,'), function(x) ifelse(any(x[[1]] %in% passing_positions), 1, 0))
  
  model_players = players_with_positions %>% filter(receiving_model == 1 | rushing_model == 1 | passing_model == 1)
  
  
  
  ##Next, pull the bio for each player by following the links for each player
  
  get_bio_by_player = function(i)
  {
    print(i)
    df = get_player_bio(player_row = model_players[i,])
    wait = runif(1,2,4) #too many requests too quickly will cause site to block request. Staggering the waiting time also helps make us look human.
    Sys.sleep(wait)
    return(cbind(model_players[i,], df %>% select(-name)))
  }
  print(paste('Number of model players:', nrow(model_players)))
  player_bios = bind_rows(map(.x = 1:nrow(model_players), .f = get_bio_by_player))
  
  return(player_bios)
}
                                         




