

library(stringr)
library(dplyr)
library(purrr)

setwd('~/nfl')
gamelogs = readRDS('data_collection/saved_data_files/player_gamelogs.rds')  %>%
  mutate(Anytime_TD = ifelse(Total_Touchdowns >= 1, 1, 0)) %>%
  left_join(readRDS('data_collection/saved_data_files/team_gamelogs.rds') %>% select(Season, Week, Team, Opp), join_by(Season, Week, Team)) %>%
  select(player_id, Name, Season, Week, Team, Opp, Position, Passing_Yds, Rushing_Yds, Receiving_Yds, Anytime_TD) %>%
  mutate(Passing_Yds = ifelse(Position == 'QB', Passing_Yds, NA),
         Receiving_Yds = ifelse(Position == 'QB', NA, Receiving_Yds))
set_min_obs = 5


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


get_corr = function(df1, df2, var1, var2, same_team = TRUE)
{
  if(var1==var2)
  {
    var1 = paste0(var1, '.x')
    var2 = paste0(var2, '.y')
  }
  if(same_team == TRUE)
  {
    cor = df1 %>% inner_join(df2, join_by(Season, Week, Team, Opp)) %>% filter(player_id.x != player_id.y) %>% select(-player_id.x, -player_id.y) %>% summarise(cor = stats::cor(.data[[var1]], .data[[var2]], use = 'pairwise.complete.obs')) %>% pull()
  } else {
    
    cor = df1 %>% inner_join(df2, join_by(Season, Week, Team == Opp))  %>% select(-player_id.x, -player_id.y) %>% summarise(cor = stats::cor(.data[[var1]], .data[[var2]], use = 'pairwise.complete.obs'))  %>% pull()
  }  
}

# assumes these data frames already exist: 
# qbs_passing, qbs_rushing, qb_td, nonqbnonrb_rush, nonrb_rec, nonqbnonrb_td, rb_rush, rb_rec, rb_td

scenarios = list(
  qb_passing = list(df = qbs_passing,      var = "Passing_Yds",   group = "QB"),
  qb_rushing = list(df = qbs_rushing,      var = "Rushing_Yds",   group = "QB"),
  qb_td      = list(df = qb_td,            var = "Anytime_TD",    group = "QB"),
  
  wr_rush    = list(df = wr_rush,  var = "Rushing_Yds",   group = "WRTE"),
  wr_rec     = list(df = wr_rec,        var = "Receiving_Yds", group = "WRTE"),
  wr_td      = list(df = wr_td,    var = "Anytime_TD",    group = "WRTE"),
  
  rb_rush    = list(df = rb_rush,          var = "Rushing_Yds",   group = "RB"),
  rb_rec     = list(df = rb_rec,           var = "Receiving_Yds", group = "RB"),
  rb_td      = list(df = rb_td,            var = "Anytime_TD",    group = "RB")
)


# build a named list of correlations (same-team and opp-team), skipping QB–QB
corrs = list()
for (i in names(scenarios)) {
  index_i = which(names(scenarios) == i)
  remaining_choices = scenarios[index_i:length(scenarios)]
  for (j in names(remaining_choices)) {
    # for QB-QB combos, skip the scenario where they're on the same team because 2 QBs wouldn't be playing in the same game on the same team unless injury. opposing team correlations are fine.
    if (!(scenarios[[i]]$group == "QB" && scenarios[[j]]$group == "QB"))
    {
      r_same = get_corr(
        df1  = scenarios[[i]]$df,
        df2  = scenarios[[j]]$df,
        var1 = scenarios[[i]]$var,
        var2 = scenarios[[j]]$var,
        same_team = TRUE
      )
      if(abs(r_same) >= 0.05)
      {
        corrs[[paste0(i, "_", j, "_same")]] = r_same
      }
    }
    
    r_opp = get_corr(
        df1  = scenarios[[i]]$df,
        df2  = scenarios[[j]]$df,
        var1 = scenarios[[i]]$var,
        var2 = scenarios[[j]]$var,
        same_team = FALSE
      )
    if(abs(r_opp) >= 0.05)
    {
      corrs[[paste0(i, "_", j, "_opp")]]  = r_opp
    }
  }
}


#next steps:
#store this somewhere
#call this in the app
#app must identify which of these correlation categories it belongs to
#create a cov matrix in the app: if it fits one of these categories, use that correlation. otherwise, if it's 2 bets for the same player, use correlation = 1. if the 2 bets are from different games, use correlation = 0.
#figure out how to apply the portfolio optimization formulas based on this.

