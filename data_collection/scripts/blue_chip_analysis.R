#blue chip analysis

library(tidyr)
library(zoo)

get_blue_chip_analysis = function(player_df, team_df, direction_play_vs_not_play)
{
  
  #Games played vs not played analysis, look for low p value
  team_data_with_player_participation = team_df %>% inner_join(player_df %>% select(season, week, gsis_id, team) %>% distinct(), join_by('season', 'week', 'team')) %>% #first get list of all players who played in a season for the team
    left_join(player_df, join_by('team', 'season', 'week', 'gsis_id')) %>% #then get players for a specific week
    group_by(season, team, gsis_id, week) %>% mutate(Played = !is.na(avg_player_metric)) %>%
    group_by(gsis_id) %>% arrange(season, week) %>% mutate(pvalue = 
      map_dbl(1:n(), function(i) {
        games_prior = cur_data()[1:(i-1),]
        num_games_not_played = sum(games_prior$Played == 0)
        num_games_played = sum(games_prior$Played == 1)
        if(num_games_not_played > 1 & num_games_played > 1 &length(unique(games_prior$Played)) == 2)
        {
          return(wilcox.test(games_prior$team_metric ~ factor(games_prior$Played), alternative = direction_play_vs_not_play)$p.value)
        } else {
          return(NA %>% as.numeric())
        }
      })) %>%
    group_by(season, week, team, gsis_id) %>% summarise(pvalue_play_vs_not_play = max(pvalue))
  
  #Changing teams analysis
  
  changing_teams_df = player_df %>% arrange(gsis_id, season, week) %>% mutate(Played = 1) %>%
    # mutate(total_yards_this_team = cumsum(yards) - yards, total_games_played_this_team = (cumsum(Played)-1),
    #        avg_yards_this_team = total_yards_this_team/total_games_played_this_team,
    #        sd_including_today = rollapplyr(yards, seq_along(yards), sd, fill = NA),
    #        sd_yards_prior = lag(sd_including_today)) %>% select(gsis_id, season, team, week, avg_yards_this_team, total_games_played_this_team, sd_yards_prior) %>%
    group_by(gsis_id) %>% mutate(num_teams= cummax(match(team, unique(team)))) %>%
    mutate(team_factor = as.numeric(as.factor(team))) %>%
    reframe(map_df(1:n(), function(i) {
      games_prior = cur_data()[1:(i-1),]
      unique_teams = unique(games_prior$team_factor)
      if(length(unique_teams) > 1)
      {
        pairs = combn(unique_teams, 2)
        all_pairs = data.frame()
        for(j in 1:ncol(pairs))
        {
          subset = games_prior %>% filter(team_factor %in% pairs[,j])
          team1 = subset$team[subset$team_factor == pairs[1,j]][1]
          team2 = subset$team[subset$team_factor == pairs[2,j]][1]
          num_games_team1 = sum(subset$team == team1)
          num_games_team2 = sum(subset$team == team2)
          if(num_games_team1 > 1 & num_games_team2 > 1) 
          {
          all_pairs = rbind(all_pairs, data.frame(season = cur_data()$season[i], week = cur_data()$week[i], team1 = team1, team2 = team2, pvalue_worse = as.numeric(wilcox.test(subset$player_metric ~ subset$team_factor, alternative = 'greater')$p.value)))
          }
        }
        return(all_pairs)
      }
      })) %>% group_by(gsis_id, season, week) %>% summarise(changing_teams_pvalue_worse = min(pvalue_worse))
  
  blue_chip_analysis_df = player_df %>% group_by(gsis_id, season) %>% filter(!is.na(Last_Season_weeks_active) & !is.na(Two_Seasons_Ago_weeks_active) & Last_Season_weeks_active > 0 & Two_Seasons_Ago_weeks_active > 0) %>%
    arrange(gsis_id, season, week) %>%
    mutate(played = 1, games_active_this_season = cumsum(played)-1,
           avg_metric_this_season_two_previous_seasons = (ifelse(games_active_this_season > 0, avg_player_metric*games_active_this_season, 0) + last_season_avg_player_metric*Last_Season_weeks_active + two_seasons_ago_avg_player_metric*Two_Seasons_Ago_weeks_active)/(games_active_this_season + Last_Season_weeks_active + Two_Seasons_Ago_weeks_active)) %>%
    select(gsis_id, season, week, team, avg_metric_this_season_two_previous_seasons) %>% arrange(desc(avg_metric_this_season_two_previous_seasons)) %>%
    full_join(team_data_with_player_participation, join_by('season', 'week', 'gsis_id', 'team')) %>%
    left_join(changing_teams_df, join_by('gsis_id', 'season', 'week')) %>% arrange(gsis_id, season, week) %>% fill(avg_metric_this_season_two_previous_seasons, .direction = "down")
  
  
  #Target/carry share (rushing and receiving)
  
  if('pct_share' %in% colnames(player_df))
  {
    blue_chip_analysis_df = blue_chip_analysis_df %>% left_join(player_df %>% select(gsis_id, season, week, team, pct_share), join_by('gsis_id', 'season', 'week', 'team'))
  }
  return(blue_chip_analysis_df) 
}
#for passing models, it will be important that a star receiver is out
#for receiving models, it will be important that a star qb is out, or that a star WR is out since maybe they'll get more targets
#for rushing models, it will be important that a star RB is out, because they might get more snaps. But maybe we only use # of carries, not the above stuff. (other than for team model)
#maybe if the star RB is out based on the above analysis, the team will pass more? look into this.
#finally, create a table for each team, week, month, that defines the blue chips at this point. can use multiple definitions, like for rushing it seems that we haven't finalized a definition yet.

#step 2: identify a blue chip presence game
# using the blue chips defined above, add a dummy to each game to indicate whether blue chip is playing.
# maybe add a dummy for whether there is a blue chip on the team. this might show if they're going to be playign worse than usual because their blue chip is out.

#step 3: 
# adjust teh seasonal table, or make a new seasonal table, adding blue chip as an aggregation
# then when adding historical seasonal stats, use the blue chip or no blue chip value accordingly
# if more than one blue chip on a team, might get messy, will have to see
