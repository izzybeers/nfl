
library(purrr)
library(dplyr)
library(rvest)
library(furrr)


source('data_collection/scripts/global.R')
team_abbr = team_lookup_table %>% select(TV_abbr, Conference)
plan(multisession, workers = 4)

get_playoff_clinching_data = function(min_year, max_year, wk = NULL, predict_mode = FALSE)
{
  
  playoff_data_by_year = function(y)
  {
    print(y)
    if(is.null(wk))
    {
      num_weeks = ifelse(y <= 2020, 17, 18)
      weeks = 1:num_weeks
    } else {
      weeks = wk
    }
    playoff_clinching_all_weeks = rbind()
    for (w in weeks)
    {
      print(w)
      playoff_clinching_html = tryCatch(get_html_content(url = paste0("https://nflplayoffscenarios.com/",y,"/week/",w,"/week-",w,"-playoff-picture/")),
                                        error = function(e) {
                                          message('')
                                          return (NULL)
                                        }
      )
      
      print(playoff_clinching_html)
      if(class(playoff_clinching_html) != 'try-error' && !is.null(playoff_clinching_html) && !any(playoff_clinching_html %>% html_nodes('p') %>% html_text(trim = TRUE) == 'Page not found :('))
      {
        print('entered here')
        
        #divisional table:
        
        playoff_clinching_table = (playoff_clinching_html %>%
                                     html_nodes('#Division-standings') %>%
                                     html_table(fill = TRUE))
        
        
        if(length(playoff_clinching_table) == 0)
        { 
          playoff_clinching_table_initial = (playoff_clinching_html %>%
                                               html_nodes('.table-responsive') %>%
                                               html_table(fill = TRUE))[[1]]
          
          colnames(playoff_clinching_table_initial)  = c('NFCRank', 'NFC', 'AFCRank', 'AFC')
          
          playoff_clinching_table = data.frame(Team = c(playoff_clinching_table_initial$NFC, playoff_clinching_table_initial$AFC)) %>%
            mutate(Already_Clinched_Playoff = as.numeric(ifelse(str_detect(Team, 'x|y|z'),1,0)), # x means clinched playoff, y means clinched division, z means clinched #1 seed -- all cases fall under "clinched playoffs"
                   Already_Clinched_Division = as.numeric(ifelse(str_detect(Team, 'y|z'),1,0)), #y means clinched division and z means clinched #1 seed -- both cases fall under "clinched division"
                   Already_Clinched_Seed1 = as.numeric(ifelse(str_detect(Team, 'z'),1,0)), #z means clinched #1 seed
                   Already_Eliminated = as.numeric(ifelse(str_detect(Team, 'e'),1,0)), #e means eliminated from playoffs
                   Already_Eliminated_Division = as.numeric(ifelse(str_detect(Team, 'k|e'),1,0)), #k means eliminated from division and e means eliminated from playoffs -- both cases would fall under being eliminated from division
                   Div_Ranking = as.numeric(NA), #this is the scenario where the division table does not exist yet for this week so we don't have this information yet 
                   Div_Pct_Wins = as.numeric(NA), #this is the scenario where the division table does not exist yet for this week so we don't have this information yet
                   Team = str_extract(Team, '[A-Z]+'),
                   Week = w,
                   Season = y) %>%
            select(Season, Week, Team, Div_Ranking, Div_Pct_Wins, Already_Clinched_Playoff, Already_Clinched_Division, Already_Clinched_Seed1, Already_Eliminated, Already_Eliminated_Division)
          
          
        } else {
          playoff_clinching_table = playoff_clinching_table[[1]]
          
          colnames(playoff_clinching_table) = c('Div_Ranking', 'Team', 'Record', 'Div_Record', 'Explanation')
          
          playoff_clinching_table = playoff_clinching_table %>% filter(Div_Ranking != '' & !(Div_Ranking %in% c('NFC','AFC'))) %>%
            mutate(Already_Clinched_Playoff = as.numeric(ifelse(str_detect(Team, 'x'),1,0)), # x means clinched playoff, y means clinched division, z means clinched #1 seed -- all cases fall under "clinched playoffs"
                   Already_Clinched_Division = as.numeric(ifelse(str_detect(Team, 'y'),1,0)),  #y means clinched division and z means clinched #1 seed -- both cases fall under "clinched division"
                   Already_Clinched_Seed1 = as.numeric(ifelse(str_detect(Team, 'z'),1,0)), #z means clinched #1 seed
                   Already_Eliminated = as.numeric(ifelse(str_detect(Team, 'e'),1,0)), #e means eliminated from playoffs
                   Already_Eliminated_Division = as.numeric(ifelse(str_detect(Team, 'k'),1,0)), #k means eliminated from division and e means eliminated from playoffs -- both cases would fall under being eliminated from division
                   Team = str_extract(Team, '[A-Z]+'),
                   Week = w,
                   Season = y,
                   Div_Pct_Wins = as.numeric(sapply(strsplit(Div_Record, '-'), function(x) as.numeric(x[1])/(as.numeric(x[1])+as.numeric(x[2]))))) %>%
            select(Season, Week, Team, Div_Ranking, Div_Pct_Wins, Already_Clinched_Playoff, Already_Clinched_Division, Already_Clinched_Seed1, Already_Eliminated, Already_Eliminated_Division)
          
          
        }
        
        #playoff clinching scenarios:
        
        scenarios_node = playoff_clinching_html %>% html_elements("div")%>% as.character()
        
        if(length(scenarios_node) > 0)
        {
          #unfortunately the labels for the clinching/eliminating scenarios (ex: "KC can clinch a playoff birth with:")   and the actual scenario requirements itself (ex: "1) KC win/tie, OR 2) DEN loss"), are not written in the same spot, they are listed separately, so we have to match them up.
          #first the site lays out the clinching scenarios (if they exist), then the NFC seed scenarios (if they exist), then the NFC elimination scenarios (if they exist), then AFC seeds and AFC elimination. There are other tables in between but we can filter those out.
          #the idea is to try to count how many of each there are, so that we can try to match them up.
          
          #using the labels for the clinching scenarios, first we extract the list of teams with clinching scenarios:
          teams_with_clinching_scenarios = str_extract((scenarios_node %>% str_extract_all('.*clinches.*with'))[2][[1]], '[A-Z]{2,3}')
          
          #we won't actually use the seed scenarios but we still need to know how many they are in order to make sure everything lines up
          teams_with_seed_scenarios = str_extract((scenarios_node %>% str_extract_all('.*obtains'))[2][[1]], '[A-Z]{2,3}')
          nfc_teams_with_seed_scenarios = teams_with_seed_scenarios[which(teams_with_seed_scenarios %in% team_abbr$TV_abbr[team_abbr$Conference == 'NFC'])]
          afc_teams_with_seed_scenarios = teams_with_seed_scenarios[which(teams_with_seed_scenarios %in% team_abbr$TV_abbr[team_abbr$Conference == 'AFC'])]
          
          #using the labels for the elimination scenarios, first we extract the list of teams with elimination scenarios:
          teams_with_elimination_scenarios = str_extract((scenarios_node %>% str_extract_all('.*is ((knocked out)|eliminate).*'))[[2]], '[A-Z]{2,3}')
          #we have to separate out nfc and afc because the nfc elimination scenarios are listed separately as afc.
          nfc_teams_with_elimination_scenarios = teams_with_elimination_scenarios[which(teams_with_elimination_scenarios %in% team_abbr$TV_abbr[team_abbr$Conference == 'NFC'])]
          afc_teams_with_elimination_scenarios = teams_with_elimination_scenarios[which(teams_with_elimination_scenarios %in% team_abbr$TV_abbr[team_abbr$Conference == 'AFC'])]
          
          #the above was the labels. Below are the tables corresponding to the actual clinching/elimination requirements.
          playoff_scenarios_tables = playoff_clinching_html %>% html_nodes('.table-responsive') %>% html_table(fill = TRUE)
          
          #filter to only the scenario tables, not the other tables on the page:
          playoff_scenarios_tables =  playoff_scenarios_tables[which(unlist(lapply(playoff_scenarios_tables, function(x) 'X1' %in% colnames(x))))]
          
          #initialize an empty list of teams that can clinch, and then pull the scenario.
          teams_can_clinch = c()
          if(length(teams_with_clinching_scenarios) > 0)
          {
            
            for (i in 1:(length(teams_with_clinching_scenarios)))
            {
              team = teams_with_clinching_scenarios[i]
              clinching_scenarios = playoff_scenarios_tables[[i]] %>% select(X3) %>% pull()
              #we want to find a clinching scenario that has the team's name in it, so this means the team can control their fate (either guaranteed clinch or keep them alive
              #if the only clinching scenario is that another team must lose, and they have no stake in their own fate, this will not count.
              if(any(str_detect(clinching_scenarios, team)))
              {
                teams_can_clinch = c(teams_can_clinch, team)
              }
            }
          }
          
          #same with the elimination scenarios:
          nfc_teams_can_be_eliminated = c()
          afc_teams_can_be_eliminated = c()
          #there's the clinching tables, a seeding table for each conference, and then the next one would be the start of the elimination scenarios.
          if(length(nfc_teams_with_elimination_scenarios) > 0)
          {
            nfc_elimination_index_start = length(teams_with_clinching_scenarios)+length(nfc_teams_with_seed_scenarios) + 1
            end = nfc_elimination_index_start + (length(nfc_teams_with_elimination_scenarios)-1)
            for (i in nfc_elimination_index_start:end)
            {
              team = nfc_teams_with_elimination_scenarios[i-nfc_elimination_index_start+1]
              elimination_scenarios = playoff_scenarios_tables[[i]] %>% select(X3) %>% pull()
              #look to see if the team has any stake in their own elimination:
              if(any(str_detect(elimination_scenarios, team)))
              {
                nfc_teams_can_be_eliminated = c(nfc_teams_can_be_eliminated, team)
              }
            }
          }
          
          if(length(afc_teams_with_elimination_scenarios) > 0)
          {
            afc_elimination_index_start = length(teams_with_clinching_scenarios)+length(nfc_teams_with_seed_scenarios) + length(nfc_teams_with_elimination_scenarios)+length(afc_teams_with_seed_scenarios)+1
            end = afc_elimination_index_start + (length(afc_teams_with_elimination_scenarios)-1)
            for (i in afc_elimination_index_start:end)
            {
              team = afc_teams_with_elimination_scenarios[i-afc_elimination_index_start+1]
              elimination_scenarios = playoff_scenarios_tables[[i]] %>% select(X3) %>% pull()
              if(any(str_detect(elimination_scenarios, team)))
              {
                afc_teams_can_be_eliminated = c(afc_teams_can_be_eliminated, team)
              }
            }     
          }
          
          teams_can_be_eliminated = c(nfc_teams_can_be_eliminated, afc_teams_can_be_eliminated)
          
          playoff_clinching_table$playoffs_at_stake = ifelse(playoff_clinching_table$Team %in% teams_can_clinch, 1, 0)
          playoff_clinching_table$elimination_at_stake = ifelse(playoff_clinching_table$Team %in% teams_can_be_eliminated, 1, 0)
          
        } else {
          
          playoff_scenarios_html = tryCatch(get_html_content(paste0('https://nflplayoffscenarios.com/',y,'/week/',w,'/week-',w,'-path-to-the-playoffs/')),
                                            error = function(e) {return(NULL)})
          
          if(class(playoff_scenarios_html) != 'try-error' && !is.null(playoff_scenarios_html) && !(any(playoff_scenarios_html %>% html_nodes('p') %>% html_text(trim = TRUE) == 'Page not found :(')))
          {
            
            #if the playoff picture table doesn't exist, we can try path to the playoffs page instead and see if that exists, and then apply similar logic, but this page is formatted differently.
            #each playoff scenario has an h2 heading and then a few scenarios underneath. so we find each h2 and extract all the "sibling" xml objects, which are the ones listed below, until we reach another h2.
            
            
            playoff_scenarios_names = playoff_scenarios_html %>% html_nodes('.post-content a') %>% html_attr('href') %>% str_extract_all('#[a-z]{2,3}-+.*') %>% unlist()
            
            
            can_clinch = c()
            for (p in playoff_scenarios_names)
            {
              print(p)
              start_node <- playoff_scenarios_html %>% html_element(p)
              
              # Get all sibling elements after that node
              sibs = start_node %>% xml2::xml_find_all("following-sibling::*")
              
              # Stop at the next <h2>
              if(any(xml_name(sibs) == "h2"))
              {
                section_nodes = sibs[1:(which(xml_name(sibs) == "h2")[1] - 1)]
              }
              
              # Combine their text (or extract html if needed)
              section_text = section_nodes %>% html_text(trim = TRUE)
              
              section_text = section_text[-which(str_detect(section_text, 'Option|Notes'))]
              trimmed_section_text = tolower(unlist(str_split(section_text, '\n')))
              can_clinch_playoffs = any(str_detect(trimmed_section_text, paste0('(week', w,')|a game')) & str_detect(trimmed_section_text, gsub('#','',str_extract(p, '#[a-z]+'))))
              
              if(can_clinch_playoffs)
              {
                can_clinch = c(can_clinch, toupper(gsub('#','',str_extract(p, '#[a-z]+'))))
              }
            }
            
            
            
            playoff_clinching_table$playoffs_at_stake = ifelse(playoff_clinching_table$Team %in% can_clinch, 1, 0)
            playoff_clinching_table$elimination_at_stake = 0
          } else {
            
            playoff_clinching_table$playoffs_at_stake = 0
            playoff_clinching_table$elimination_at_stake = 0
          }
        }
      } else {
        playoff_clinching_table = rbind()
      }
     
        
        playoff_clinching_all_weeks = rbind(playoff_clinching_all_weeks, playoff_clinching_table)
      
      
      wait = runif(1,2,4)
      Sys.sleep(wait)
      
     
      
    }
    
    if((is.null(playoff_clinching_all_weeks) || nrow(playoff_clinching_all_weeks) == 0) & predict_mode == TRUE)
    {
      playoff_clinching_all_weeks = data.frame(Season = y, Week = wk, Team = NA, Div_Ranking = as.numeric(NA), Div_Pct_Wins = as.numeric(NA), Already_Clinched_Playoff = as.numeric(NA), Already_Clinched_Division = as.numeric(NA), Already_Clinched_Seed1 = as.numeric(NA), Already_Eliminated = as.numeric(NA), Already_Eliminated_Division = as.numeric(NA), playoffs_at_stake = as.numeric(NA), elimination_at_stake = as.numeric(NA))
    }
    
    return(playoff_clinching_all_weeks)
  }
  
  
  full_playoff_clinching_table_all_years = bind_rows(future_map(.x = (min_year:max_year),
                                                                .f = playoff_data_by_year))
  
  
  
  return(full_playoff_clinching_table_all_years)
  
  
}



