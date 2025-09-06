

library(stringr)
library(dplyr)
library(purrr)
library(googlesheets4)
gs4_auth(cache = ".secrets", email = "izzyb961@gmail.com")
setwd('~/nfl')
gamelogs = readRDS('data_collection/saved_data_files/player_gamelogs.rds')  %>%
  mutate(Anytime_TD = ifelse(Total_Touchdowns >= 1, 1, 0)) %>%
  left_join(readRDS('data_collection/saved_data_files/team_gamelogs.rds') %>% select(Season, Week, Team, Opp), join_by(Season, Week, Team)) %>%
  select(player_id, Name, Season, Week, Team, Opp, Position, Passing_Yds, Rushing_Yds, Receiving_Yds, Anytime_TD) %>%
  mutate(Passing_Yds = ifelse(Position == 'QB', Passing_Yds, NA),
         Receiving_Yds = ifelse(Position == 'QB', NA, Receiving_Yds))
set_min_obs = 5


sheet_id = '1DSSz4X-3LLAarRlBRtuMsGJ1hh2FDdVeHJZFdpZGW0A'


#quarterbacks:
qbs_passing = gamelogs %>% filter(!is.na(Passing_Yds)) %>% select(player_id, Team, Opp, Season, Week, Passing_Yds)

qbs_rushing = gamelogs %>% filter(!is.na(Rushing_Yds) & str_detect(Position, "QB")) %>% select(player_id, Team, Opp, Season, Week, Rushing_Yds)

qb_td = gamelogs %>% filter(!is.na(Anytime_TD) & str_detect(Position, "QB")) %>% select(player_id, Team, Opp, Season, Week, Anytime_TD)

# non-RB non-QB (WR, TE, etc.)
wr_rush = gamelogs %>% filter(!is.na(Rushing_Yds) & !str_detect(Position, "QB|RB")) %>% select(player_id, Team, Opp, Season, Week, Rushing_Yds)

wr_rec = gamelogs %>% filter(!is.na(Receiving_Yds) & !str_detect(Position, "RB")) %>% select(player_id, Team, Opp, Season, Week, Receiving_Yds)

wr_td = gamelogs %>% filter(!is.na(Anytime_TD) & !str_detect(Position, "QB|RB")) %>% select(player_id, Team, Opp, Season, Week, Anytime_TD)

# Running Backs
rb_rec = gamelogs %>% filter(!is.na(Receiving_Yds) & str_detect(Position, "RB")) %>% select(player_id, Team, Opp, Season, Week, Receiving_Yds)

rb_rush = gamelogs %>% filter(!is.na(Rushing_Yds) & str_detect(Position, "RB")) %>% select(player_id, Team, Opp, Season, Week, Rushing_Yds)

rb_td = gamelogs %>% filter(!is.na(Anytime_TD) & str_detect(Position, "RB")) %>% select(player_id, Team, Opp, Season, Week, Anytime_TD)


get_corr = function(df1, df2, var1, var2, same_team = TRUE, same_player = FALSE)
{
  if(var1==var2)
  {
    var1 = paste0(var1, '.x')
    var2 = paste0(var2, '.y')
  }
  if(same_team == TRUE & same_player == FALSE)
  {
    cor = df1 %>% inner_join(df2, join_by(Season, Week, Team, Opp)) %>% filter(player_id.x != player_id.y) %>% select(-player_id.x, -player_id.y) %>% summarise(cor = stats::cor(.data[[var1]], .data[[var2]], use = 'pairwise.complete.obs')) %>% pull()
  } else if (same_player == TRUE) {
    cor = df1 %>% inner_join(df2, join_by(Season, Week, player_id)) %>% summarise(cor = stats::cor(.data[[var1]], .data[[var2]], use = 'pairwise.complete.obs')) %>% pull()
  } else {
    cor = df1 %>% inner_join(df2, join_by(Season, Week, Team == Opp))  %>% select(-player_id.x, -player_id.y) %>% summarise(cor = stats::cor(.data[[var1]], .data[[var2]], use = 'pairwise.complete.obs'))  %>% pull()
  }  
  return(cor)
}

# assumes these data frames already exist: 
# qbs_passing, qbs_rushing, qb_td, nonqbnonrb_rush, nonrb_rec, nonqbnonrb_td, rb_rush, rb_rec, rb_td

scenarios = list(
  qb_passing = list(df = qbs_passing,      var = "Passing_Yds",   group = "QB"),
  qb_rushing = list(df = qbs_rushing,      var = "Rushing_Yds",   group = "QB"),
  qb_td      = list(df = qb_td,            var = "Anytime_TD",    group = "QB"),
  
  wr_rush    = list(df = wr_rush,  var = "Rushing_Yds",   group = "WR|TE"),
  wr_rec     = list(df = wr_rec,        var = "Receiving_Yds", group = "WR|TE"),
  wr_td      = list(df = wr_td,    var = "Anytime_TD",    group = "WR|TE"),
  
  rb_rush    = list(df = rb_rush,          var = "Rushing_Yds",   group = "RB"),
  rb_rec     = list(df = rb_rec,           var = "Receiving_Yds", group = "RB"),
  rb_td      = list(df = rb_td,            var = "Anytime_TD",    group = "RB")
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
    symmetrical =  var1 == var2 & group1 == group2
    
    # for QB-QB combos, skip the scenario where they're on the same team because 2 QBs wouldn't be playing in the same game on the same team unless injury. opposing team correlations are fine.
    if (!(scenarios[[i]]$group == "QB" && scenarios[[j]]$group == "QB"))
    {
      r_same = get_corr(df1  = df1, df2  = df2, var1 = var1, var2 = var2, same_team = TRUE)
      
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
    if(var1 != var2 && group1 == group2)
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

sheet_append(ss = sheet_id, data = corrs, sheet = 'Correlations')

#next steps:
#store this somewhere
#call this in the app
#app must identify which of these correlation categories it belongs to
#create a cov matrix in the app: if it fits one of these categories, use that correlation. otherwise, if it's 2 bets for the same player, use correlation = 1. if the 2 bets are from different games, use correlation = 0.
#figure out how to apply the portfolio optimization formulas based on this.

