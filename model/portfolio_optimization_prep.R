

library(stringr)
library(dplyr)
library(purrr)
setwd("~/Documents/nfl")

min_year = 2020
max_year = 2025

gamelogs = load_player_stats(seasons = min_year:max_year) %>%
  mutate(anytime_td_scorer = rushing_tds + receiving_tds > 0,
         rushing_receiving_yards = rushing_yards + receiving_yards) %>%
  select(season, week, player_id, team, opponent_team, passing_yards, rushing_yards, receiving_yards, receptions, anytime_td_scorer, rushing_receiving_yards) %>%
  rename(opponent = opponent_team, label = player_id) %>%
  inner_join(schedules_raw %>% mutate(team_diff = team_score - opponent_score,
                                      team_win = as.numeric(team_score > opponent_score)) %>% select(season, week, team, team_win, team_diff), join_by('team','season','week')) %>%
  inner_join(load_players() %>% filter(position_group %in% c('QB', 'RB', 'WR', 'TE')) %>% select(gsis_id, position_group) %>% distinct(), join_by('label' == 'gsis_id'))


#quarterbacks:
qbs_passing = gamelogs %>% filter(!is.na(passing_yards) & position_group == "QB") %>% select(label, team, opponent, season, week, passing_yards)

qbs_rushing = gamelogs %>% filter(!is.na(rushing_yards) & str_detect(position_group, "QB")) %>% select(label, team, opponent, season, week, rushing_yards)

qb_td = gamelogs %>% filter(!is.na(anytime_td_scorer) & str_detect(position_group, "QB")) %>% select(label, team, opponent, season, week, anytime_td_scorer)

# non-RB non-QB (WR, TE, etc.)
wr_rush = gamelogs %>% filter(!is.na(rushing_yards) & !str_detect(position_group, "QB|RB")) %>% select(label, team, opponent, season, week, rushing_yards)

wr_rec = gamelogs %>% filter(!is.na(receiving_yards) & !str_detect(position_group, "RB|QB")) %>% select(label, team, opponent, season, week, receiving_yards)

wr_receptions = gamelogs %>% filter(!is.na(receptions) & !str_detect(position_group, "RB|QB")) %>% select(label, team, opponent, season, week, receptions)

wr_rush_rec = gamelogs %>% filter(!is.na(receiving_yards) & !is.na(rushing_yards) & !str_detect(position_group, "RB|QB")) %>% select(label, team, opponent, season, week, rushing_receiving_yards)

wr_td = gamelogs %>% filter(!is.na(anytime_td_scorer) & !str_detect(position_group, "QB|RB")) %>% select(label, team, opponent, season, week, anytime_td_scorer)

# Running Backs
rb_rec = gamelogs %>% filter(!is.na(receiving_yards) & str_detect(position_group, "RB")) %>% select(label, team, opponent, season, week, receiving_yards)

rb_rush = gamelogs %>% filter(!is.na(rushing_yards) & str_detect(position_group, "RB")) %>% select(label, team, opponent, season, week, rushing_yards)

rb_receptions = gamelogs %>% filter(!is.na(receptions) & str_detect(position_group, "RB")) %>% select(label, team, opponent, season, week, receptions)

rb_rush_rec = gamelogs %>% filter(!is.na(rushing_yards) & !is.na(receiving_yards) & str_detect(position_group, "RB")) %>% select(label, team, opponent, season, week, rushing_receiving_yards)

rb_td = gamelogs %>% filter(!is.na(anytime_td_scorer) & str_detect(position_group, "RB")) %>% select(label, team, opponent, season, week, anytime_td_scorer)

#team:

team_win = gamelogs %>% select(season, week, team, opponent, team_win) %>% distinct() %>% mutate(label = team)
team_diff = gamelogs %>% select(season, week, team, opponent, team_diff) %>% distinct() %>% mutate(label = team)

get_corr = function(df1, df2, var1, var2, scope1, scope2, same_team = TRUE, same_player = FALSE)
{
  if(var1==var2) 
  {
    var1 = paste0(var1, '.x')
    var2 = paste0(var2, '.y')
  }
  if(same_team == TRUE & same_player == FALSE)
  {
    cor = df1 %>% inner_join(df2, join_by(season, week, team, opponent)) %>% filter(label.x != label.y) %>% select(-label.x, -label.y) %>% summarise(cor = stats::cor(.data[[var1]], .data[[var2]], use = 'pairwise.complete.obs')) %>% pull()
  } else if (same_player == TRUE) {
    cor = df1 %>% inner_join(df2, join_by(season, week, label)) %>% summarise(cor = stats::cor(.data[[var1]], .data[[var2]], use = 'pairwise.complete.obs')) %>% pull()
  } else {
    cor = df1 %>% inner_join(df2, join_by(season, week, team == opponent))  %>% select(-label.x, -label.y) %>% summarise(cor = stats::cor(.data[[var1]], .data[[var2]], use = 'pairwise.complete.obs'))  %>% pull()
  }  
  return(cor)
}

# assumes these data frames already exist: 
# qbs_passing, qbs_rushing, qb_td, nonqbnonrb_rush, nonrb_rec, nonqbnonrb_td, rb_rush, rb_rec, rb_td

scenarios = list(
  qb_passing = list(df = qbs_passing, var = "passing_yards",   group = "QB", scope = 'Player'),
  qb_rushing = list(df = qbs_rushing, var = "rushing_yards",   group = "QB", scope = 'Player'),
  qb_td      = list(df = qb_td, var = "anytime_td_scorer",    group = "QB", scope = 'Player'),
  
  wr_rush    = list(df = wr_rush, var = "rushing_yards",   group = "WR|TE", scope = 'Player'),
  wr_rec     = list(df = wr_rec, var = "receiving_yards", group = "WR|TE", scope = 'Player'),
  wr_receptions = list(df = wr_receptions, var = "receptions", group = "WR|TE", scope = 'Player'),
  wr_rush_rec = list(df = wr_rush_rec, var = "rushing_receiving_yards", group = "WR|TE", scope = 'Player'),
  wr_td      = list(df = wr_td, var = "anytime_td_scorer",    group = "WR|TE", scope = 'Player'),
  
  rb_rush    = list(df = rb_rush, var = "rushing_yards",   group = "RB", scope = 'Player'),
  rb_rec     = list(df = rb_rec, var = "receiving_yards", group = "RB", scope = 'Player'),
  rb_receptions = list(df = rb_receptions, var = "receptions", group = "RB", scope = 'Player'),
  rb_rush_rec = list(df = rb_rush_rec, var = "rushing_receiving_yards", group = "RB", scope = 'Player'),
  rb_td      = list(df = rb_td, var = "anytime_td_scorer", group = "RB", scope = 'Player'),
  
  team_win = list(df = team_win, var = 'team_win', group = 'Team', scope = 'Team'),
  team_diff = list(df = team_diff, var = 'team_diff', group = 'Team', scope = 'Team')
)

# build a named list of correlations (same-team and opp-team), skipping QB–QB
corrs = data.frame()
for (i in names(scenarios)) {
  print(i)
  index_i = which(names(scenarios) == i)
  remaining_choices = scenarios[index_i:length(scenarios)]
  for (j in names(remaining_choices)) {
    print(j)
    var1 = scenarios[[i]]$var
    var2 = scenarios[[j]]$var
    df1 = scenarios[[i]]$df
    df2 = scenarios[[j]]$df
    group1 = scenarios[[i]]$group
    group2 = scenarios[[j]]$group
    scope1 = scenarios[[i]]$scope
    scope2 = scenarios[[j]]$scope
    symmetrical =  var1 == var2 & group1 == group2
    
    # for QB-QB combos, skip the scenario where they're on the same team because 2 QBs wouldn't be playing in the same game on the same team unless injury. opposing team correlations are fine.
    if (!(group1 == "QB" && group2 == "QB") && !(scope1 == 'Team' && scope2 == 'Team'))
    {
      r_same = get_corr(df1  = df1, df2  = df2, var1 = var1, var2 = var2, scope1 = scope1, scope2 = scope2, same_team = TRUE)
      
      if(abs(r_same) >= 0.05)
      {
        corrs = rbind(corrs,
                      c(group1, var1, group2, var2, 'same_team', r_same)
                      )
        
        if(!symmetrical)
        {
          corrs = rbind(corrs,
                        c(group2, var2, group1, var1, 'same_team', r_same)
          )
                      
        }

      }
    }
    
    r_opp = get_corr(df1  = df1, df2  = df2, var1 = var1, var2 = var2, same_team = FALSE)
    
    if(abs(r_opp) >= 0.05)
    {
      corrs = rbind(corrs,
                    c(group1, var1, group2, var2, 'opp_team', r_opp)
                    )
      
      if(!symmetrical)
      {
          corrs = rbind(corrs,
                        c(group2, var2, group1, var1, 'opp_team', r_opp)
                    )
      }
    }
    if(var1 != var2 && group1 == group2 && !(scope1 == 'Team' & scope2 == 'Team'))
    {
      r_same_player = get_corr(df1  = df1, df2  = df2, var1 = var1, var2 = var2, same_team = TRUE, same_player = TRUE)
      
      if(abs(r_same_player) >= 0.05)
      {
        corrs = rbind(corrs,
                      c(group1, var1, group2, var2, 'same_player', r_same_player)
                      )
        
        if(!symmetrical)
        {
          corrs = rbind(corrs,
                        c(group2, var2, group1, var1, 'same_player', r_same_player)
                        )
        }
      }
    }
  }
}
colnames(corrs) = c('Position1', 'Var1', 'Position2', 'Var2', 'Correlation_Type', 'Cor')
corrs$Cor = as.numeric(corrs$Cor)

# write.csv(corrs, 'correlations_up_to_2024.csv')

write_to_supabase('MainData', 'Correlations', corrs %>% mutate(min_year = min_year, max_year = max_year, created_at = Sys.time()))
