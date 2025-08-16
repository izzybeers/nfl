library(dplyr)
source('data_collection/scripts/global.R')

plan(multisession, workers = 4)

get_injuries_data = function(min_year, max_year, wk = NULL)
{

  #For some reason this workflow is very draining, so I typically had to run one year at a time, save that year in an RDS, restart the computer, and try again. Otherwise, my computer would start getting overworked and slow and crash. After finishing the different pieces, I combine all the pieces together. If the whole loop can be done together, then when that's done, the next chunk can be skipped and go immediately to saveRDS.
  
  injuries_by_year = function(y)
  {
    injuries_report = data.frame(rbind())
    print(y)
    if(is.null(wk))
    {
      num_weeks = ifelse(y <= 2020, 17, 18)
      weeks = 1:num_weeks
    } else {
      weeks = wk
    }
    for (w in weeks)
    {
      print(w)
      injuries_html = get_html_content(url = paste0('https://www.nfl.com/injuries/league/', y, '/reg', w), skip_to_chromote = TRUE, extra_wait = 2)
      
      tables = injuries_html %>% html_nodes("table.d3-o-table--detailed")
      if(length(tables) == 0)
      {
        injuries_table = data.frame(Season = y, Week = w, Team = NA, Player = NA, Position = NA, Less_Practice = NA, Game_Status = NA, On_Injury_List = NA)
      } else {
        j = 0
        while((length(xml_children(injuries_html)) <= 1 | length(tables) == 0) & (j < 5))
        {
          j = j + 1
          print(paste('missing data for', y, w, '- trying again'))
          injuries_html = get_html_content(url = paste0('https://www.nfl.com/injuries/league/', y, '/reg', w), skip_to_chromote = TRUE, extra_wait = 10*j)
          tables = injuries_html %>% html_nodes("table.d3-o-table--detailed")
        }
        
        if(j == 5)
        {
          stop("try again later")
        }
        
        
        get_injuries_table <- function(table_node) {
          tab = (table_node %>% html_table(fill = TRUE))[[1]] %>% data.frame()
          prev_team = table_node %>%
            html_node(xpath = "preceding::div[contains(@class, 'd3-o-section-sub-title')][1]/span") %>%
            html_text(trim = TRUE)
          tab$Team = prev_team
          return(tab)
        }
        
        injuries_table = data.frame(rbind())
        for(i in 1:length(tables))
        {
          injuries_table = rbind(injuries_table, get_injuries_table(table_node = tables[i]))
        }
        
        injuries_table = injuries_table %>%
          mutate(Season = y,
                 Week = w,
                 Less_Practice = ifelse(Practice.Status %in% c('Limited Participation in Practice', 'Did Not Participate In Practice'), 0, 1),
                 On_Injury_List = 1) %>%
          rename(Game_Status = Game.Status) %>%
          select(Season, Week, Team, Player, Position, Less_Practice, Game_Status, On_Injury_List)
        
        # Combine the data into a data frame
        
      }
      injuries_report = rbind(injuries_report, injuries_table) %>% unique()
      
      wait = runif(1,2,4)
      Sys.sleep(wait)
    }
    
    
    
    
    return(injuries_report)
    
    
    
  }
  injuries_report = bind_rows(future_map(.x = min_year:max_year,
                                         .f = injuries_by_year))
  
  
  
  #optional piece if the above loop had to be done in multiple parts.
  
  # injuries_data = rbind()
  # 
  # for(i in min_year:max_year)
  # {
  #   injuries_data = rbind(injuries_data, readRDS(paste0('backup/injuries_report_',i,'.rds')))
  # }
  
  #occasionally, a fraction of a % of the time, a player is listed on the injury list for 2 teams. Roll up by player and just take max.
  #Game Status is character so max would just be alphabetical max, but since this is so infrequent, the logic is sufficient.
  #The purpose is to avoid dupes when joining back to main table.
  injuries_report = injuries_report %>% group_by(Season, Week, Player) %>%
    summarise(Less_Practice = max(Less_Practice),
              Game_Status = max(Game_Status),
              On_Injury_List = max(On_Injury_List))
  
  return(injuries_report)
}



