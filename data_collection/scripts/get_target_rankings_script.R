library(dplyr)
library(purrr)
library(furrr)
source('data_collection/scripts/global.R')

plan(multisession, workers = 4)


get_players_target_rankings = function(min_year, max_year, player_gamelogs, player_seasonal, team_gamelogs, qb1_by_year, wk = NULL, predict_mode = FALSE, manual_qb_starters = NULL)
{

  #STARTING QBS:
  df =  player_gamelogs %>% filter(Season >=  min_year & Season <= max_year)
  
  if(predict_mode == FALSE)
  {
    qb_starters = df %>% filter(str_detect('QB', Position) & GS == 1) %>%
      select(Season, Week, Team, player_id, Name)  %>%
      left_join(qb1_by_year, by = c('Name' = 'Qb1', 'Season' = 'Season', 'Team' ='Team')) %>%
      mutate(Qb1_starting = ifelse(!is.na(Temp), 1, 0)) %>%
      group_by(Season, Week, Team) %>% summarise(Qb1_starting = max(Qb1_starting)) %>%
      arrange(Season, Team, Week)
    
    df = df %>% filter(!is.na(Receiving_Yds) | !is.na(Rushing_Yds) | !is.na(Passing_Yds))

  } else {
    qb_starters = manual_qb_starters %>% mutate(Season = max_year, Week = wk) %>% select(Season, Week, Team, Qb1_starting)
  }
  
  if(!is.null(wk) & predict_mode == FALSE)
  {
    qb_starters = qb_starters %>% filter(Week == wk)
  }

  get_rankings_for_team = function(t)
  {
    rankings_receiving_all_years = data.frame(rbind())
    rankings_rushing_all_years = data.frame(rbind())
    seasonal_rushing_table_all_years = data.frame(rbind())
    seasonal_receiving_table_all_years = data.frame(rbind())
    for (y in sort(unique(df$Season)))
    {
      this_team_gamelog = team_gamelogs %>% filter(Team == t & Season == y)
      
      #weekly gamelogs:
      
      if(sum(!is.na(df$Receiving_Yds)) > 0) #games with receiving data this year
      {
      receiving_subtable = df %>%
        filter(Team == t & Season == y) %>%
        select(player_id, Name, Season, Team, Position, Week, contains('Receiving')) %>%
        group_by(Week) %>%
        mutate(Target_Rank_Median_Season = dense_rank(desc(Median_Receiving_Tgt)),
               Receiving_Yards_Rank_Median_Season = dense_rank(desc(Median_Receiving_Yds)),
               Target_Rank_Median_Last3 = dense_rank((desc(Last3_Median_Receiving_Tgt))),
               Receiving_Yards_Rank_Median_Last3 = dense_rank(desc(Last3_Median_Receiving_Yds))) %>%
        ungroup() %>%
        left_join(this_team_gamelog %>% mutate(Week = as.numeric(Week)) %>% select(Week, Median_Offense_PassY, Last3_Median_Offense_PassY),join_by('Week')) %>%
        mutate(Pct_Team_Receiving_Yds_Median_Season = ifelse(is.na(Median_Offense_PassY) | is.na(Median_Receiving_Yds) | Median_Offense_PassY == 0, NA, Median_Receiving_Yds/Median_Offense_PassY),
               Pct_Team_Receiving_Yds_Median_Last3 = ifelse(is.na(Last3_Median_Offense_PassY) | is.na(Last3_Median_Receiving_Yds) | Last3_Median_Offense_PassY == 0, NA, Last3_Median_Receiving_Yds/Last3_Median_Offense_PassY)) %>%
        select(player_id, Name, Position, Season, Team, Week, Target_Rank_Median_Season, Receiving_Yards_Rank_Median_Season, Target_Rank_Median_Last3, Receiving_Yards_Rank_Median_Last3, Pct_Team_Receiving_Yds_Median_Season, Pct_Team_Receiving_Yds_Median_Last3)
      } else {
        receiving_subtable = df %>%
          filter(Team == t & Season == y) %>%
          select(player_id, Name, Season, Team, Position, Week, contains('Receiving')) %>%
          group_by(Week) %>%
          mutate(Target_Rank_Median_Season = NA,
                 Receiving_Yards_Rank_Median_Season = NA,
                 Target_Rank_Median_Last3 = NA,
                 Receiving_Yards_Rank_Median_Last3 = NA) %>%
          ungroup() %>%
          left_join(this_team_gamelog %>% mutate(Week = as.numeric(Week)) %>% select(Week, Median_Offense_PassY, Last3_Median_Offense_PassY),join_by('Week')) %>%
         mutate(
            Pct_Team_Receiving_Yds_Median_Season = ifelse(is.na(Median_Receiving_Yds) | is.na(Median_Offense_PassY) | Median_Offense_PassY == 0, NA, Median_Receiving_Yds / Median_Offense_PassY),
            Pct_Team_Receiving_Yds_Median_Last3 = ifelse(is.na(Last3_Median_Receiving_Yds) | is.na(Last3_Median_Offense_PassY) | Last3_Median_Offense_PassY == 0, NA, Last3_Median_Receiving_Yds / Last3_Median_Offense_PassY)) %>%
          select(player_id, Name, Position, Season, Team, Week, Target_Rank_Median_Season, Receiving_Yards_Rank_Median_Season, Target_Rank_Median_Last3, Receiving_Yards_Rank_Median_Last3, Pct_Team_Receiving_Yds_Median_Season, Pct_Team_Receiving_Yds_Median_Last3)
        
      }
      rankings_receiving_all_years = rbind(rankings_receiving_all_years, receiving_subtable)
      
      if(sum(!is.na(df$Rushing_Yds)) > 0)
      {
        rushing_subtable_nonqb = df %>%
          filter(Team == t & Season == y & !str_detect('QB', Position)) %>%
          select(player_id, Name, Position, Week, contains('Rushing')) %>%
          group_by(Week) %>%
          #Rushing attempts rankings shouldn't count qbs because qbs can rush whenever they want. The purpose of this is to show how often the play goes to them, so that only makes sense for RBs.
          mutate(Rushing_Att_Rank_Median_Season = ifelse(is.na(Median_Rushing_Att), NA, dense_rank(desc(Median_Rushing_Att))),
                 Rushing_Att_Rank_Median_Last3 = ifelse(is.na(Last3_Median_Rushing_Att), NA, dense_rank(desc(Last3_Median_Rushing_Att)))) %>%
          ungroup() %>%
          select(player_id, Name, Position, Week, Rushing_Att, Last3_Median_Rushing_Att, Median_Rushing_Att, Rushing_Att_Rank_Median_Season, Rushing_Att_Rank_Median_Last3)
        
        rushing_subtable_all = df %>%
          filter(Team == t & Season == y) %>%
          select(player_id, Name, Season, Team, Position, Week, contains('Rushing')) %>%
          group_by(Week) %>%
          #qbs can be included in rankings for rushing yards:
          mutate(Rushing_Yards_Rank_Median_Season = ifelse(is.na(Median_Rushing_Yds), NA, dense_rank(desc(Median_Rushing_Yds))),
                 Rushing_Yards_Rank_Median_Last3 = ifelse(is.na(Last3_Median_Rushing_Yds), NA, dense_rank(desc(Last3_Median_Rushing_Yds))))%>%
          ungroup() %>%
          select(player_id, Name, Position, Season, Team, Week, Rushing_Yds, Median_Rushing_Yds, Last3_Median_Rushing_Yds, Rushing_Yards_Rank_Median_Season, Rushing_Yards_Rank_Median_Last3)
        
        rushing_subtable = rushing_subtable_all %>%
          left_join(rushing_subtable_nonqb %>% select(player_id, Week, Rushing_Att_Rank_Median_Season, Rushing_Att_Rank_Median_Last3), join_by('player_id', 'Week')) %>%
          left_join(this_team_gamelog %>% mutate(Week = as.numeric(Week)) %>% select(Week, Median_Offense_RushY, Last3_Median_Offense_RushY),join_by('Week')) %>%
          mutate(Pct_Team_Rushing_Yds_Median_Season = ifelse(is.na(Median_Rushing_Yds) | is.na(Median_Offense_RushY) | Median_Offense_RushY == 0, NA, Median_Rushing_Yds/Median_Offense_RushY),
                 Pct_Team_Rushing_Yds_Median_Last3 = ifelse(is.na(Last3_Median_Rushing_Yds) | is.na(Last3_Median_Offense_RushY) | Last3_Median_Offense_RushY == 0, NA, Last3_Median_Rushing_Yds/Last3_Median_Offense_RushY)) %>%
          select(player_id, Name, Position, Season, Team, Week, Rushing_Yards_Rank_Median_Season, Rushing_Yards_Rank_Median_Last3, Rushing_Att_Rank_Median_Season,
                 Rushing_Att_Rank_Median_Last3, Pct_Team_Rushing_Yds_Median_Season, Pct_Team_Rushing_Yds_Median_Last3)
      } else {
        rushing_subtable = df %>%
          filter(Team == t & Season == y) %>%
          select(player_id, Name, Season, Team, Position, Week, contains('Rushing')) %>%
          group_by(Week) %>%
          mutate(Rushing_Att_Rank_Median_Season = NA,
                 Rushing_Yards_Rank_Median_Season = NA,
                 Rushing_Att_Rank_Median_Last3 = NA,
                 Rushing_Yards_Rank_Median_Last3 = NA) %>%
          ungroup() %>%
          left_join(this_team_gamelog %>% mutate(Week = as.numeric(Week)) %>% select(Week, Median_Offense_RushY, Last3_Median_Offense_RushY),join_by('Week')) %>%
          mutate(
            Pct_Team_Rushing_Yds_Median_Season = ifelse(is.na(Median_Rushing_Yds) | is.na(Median_Offense_RushY) | Median_Offense_RushY == 0, NA, Median_Rushing_Yds / Median_Offense_RushY),
            Pct_Team_Rushing_Yds_Median_Last3 = ifelse(is.na(Last3_Median_Rushing_Yds) | is.na(Last3_Median_Offense_RushY) | Last3_Median_Offense_RushY == 0, NA, Last3_Median_Rushing_Yds / Last3_Median_Offense_RushY)) %>%
          select(player_id, Name, Position, Season, Team, Week, Rushing_Att_Rank_Median_Season, Rushing_Yards_Rank_Median_Season, Rushing_Att_Rank_Median_Last3, Rushing_Yards_Rank_Median_Last3, Pct_Team_Rushing_Yds_Median_Season, Pct_Team_Rushing_Yds_Median_Last3)
        
      }
      rankings_rushing_all_years = rbind(rankings_rushing_all_years, rushing_subtable)
      
      
      #seasonal table:
      
      team_stats_this_year = this_team_gamelog %>% group_by(Season) %>%
        summarise(Total_Team_Pass_Yards = sum(Offense_PassY, na.rm = TRUE),
                  Total_Team_Rush_Yards = sum(Offense_RushY, na.rm = TRUE))
      
      seasonal_receiving_subtable = player_seasonal %>%
        filter(Team == t & Season == y & !is.na(Receiving_Yds_median))
      
      active_all_weeks_receiving = seasonal_receiving_subtable %>% filter(Weeks_Active == 'All')
      active_all_weeks_receiving$total_pass_yards = team_stats_this_year$Total_Team_Pass_Yards
      
      not_active_all_weeks_receiving = data.frame(rbind())
      for (p in seasonal_receiving_subtable %>% filter(Weeks_Active != 'All') %>% select(player_id) %>% distinct() %>% pull())
      {
        weeks_active = as.numeric(unlist(strsplit(seasonal_receiving_subtable$Weeks_Active[which(seasonal_receiving_subtable$player_id == p)], ',')))
        team_gamelogs_subset = this_team_gamelog %>% filter(Week %in% weeks_active)
        total_pass_yards = team_stats_this_year$Total_Team_Pass_Yards
        not_active_all_weeks_receiving = rbind(not_active_all_weeks_receiving,
                                               seasonal_receiving_subtable[seasonal_receiving_subtable$player_id == p,] %>% mutate(total_pass_yards = total_pass_yards))
      }
      
      seasonal_receiving_table_all_years = rbind(seasonal_receiving_table_all_years,
                                       rbind(active_all_weeks_receiving, not_active_all_weeks_receiving) %>% select(player_id, Name, Season, Team, Receiving_Yds_sum, total_pass_yards))
      
      
      
      seasonal_rushing_subtable = player_seasonal %>%
        filter(Team == t & Season == y & !is.na(Rushing_Yds_median))
      
      active_all_weeks_rushing = seasonal_rushing_subtable %>% filter(Weeks_Active == 'All')
      active_all_weeks_rushing$total_rush_yards = team_stats_this_year$Total_Team_Rush_Yards
      
      not_active_all_weeks_rushing = data.frame(rbind())
      
      for (p in seasonal_rushing_subtable %>% filter(Weeks_Active != 'All') %>% select(player_id) %>% distinct() %>% pull())
      {
        weeks_active = as.numeric(unlist(strsplit(seasonal_rushing_subtable$Weeks_Active[which(seasonal_rushing_subtable$player_id == p)], ',')))
        team_gamelogs_subset = this_team_gamelog %>% filter(Week %in% weeks_active)
        total_rush_yards = team_stats_this_year$Total_Team_Rush_Yards
        not_active_all_weeks_rushing = rbind(not_active_all_weeks_rushing,
                                             seasonal_rushing_subtable[seasonal_rushing_subtable$player_id == p,] %>% mutate(total_rush_yards = total_rush_yards))
      }
      
      seasonal_rushing_table_all_years = rbind(seasonal_rushing_table_all_years,
                                     rbind(active_all_weeks_rushing, not_active_all_weeks_rushing) %>% select(player_id, Name, Season, Team, Rushing_Yds_sum, total_rush_yards))
      
    }
    return(list(rankings_receiving_all_years, rankings_rushing_all_years, seasonal_rushing_table_all_years, seasonal_receiving_table_all_years))
  }
  
  all_stats_tables = future_map(.x = sort(unique(df$Team)),
                                  .f = get_rankings_for_team)
  rankings_by_slot = transpose(all_stats_tables)
  
  rankings_receiving = bind_rows(rankings_by_slot[[1]])
  rankings_rushing = bind_rows(rankings_by_slot[[2]])
  seasonal_rushing_table = bind_rows(rankings_by_slot[[3]]) 
  seasonal_receiving_table = bind_rows(rankings_by_slot[[4]])
 
  
  #roll up by player/season in case player played on multiple teams:
  seasonal_receiving_table = seasonal_receiving_table %>%
    group_by(player_id, Season) %>%
    summarise(Pct_Team_Receiving_Yds_Season = sum(Receiving_Yds_sum)/sum(total_pass_yards))
  
  seasonal_rushing_table = seasonal_rushing_table %>%
    group_by(player_id, Season) %>%
    summarise(Pct_Team_Rushing_Yds_Season = sum(Rushing_Yds_sum)/sum(total_rush_yards))
  
  if(!is.null(wk))
  {
    print(paste('filter on week:', wk))
    rankings_receiving = rankings_receiving %>% filter(Week == wk)
    rankings_rushing = rankings_rushing %>% filter(Week == wk)
  }
  
  
  return(list(rankings_receiving, rankings_rushing, seasonal_receiving_table, seasonal_rushing_table, qb_starters))
  
}
