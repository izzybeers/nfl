
get_injuries_data = function(predict_mode, min_year, max_year, wk, testmode = FALSE)
{
  if (!predict_mode)
  {
    injuries = load_injuries(seasons = min_year:max_year)
  } else {
    injuries = load_injuries(seasons = max_year:max_year) %>% filter(week == wk)
  }
  
  injuries = injuries %>% group_by(season, week, gsis_id, team) %>% slice(1) %>% ungroup() %>%
    mutate(illness = str_detect(tolower(practice_primary_injury), 'illness|covid|appendix|appendicitis|headache'),
           out_not_injury_related = str_detect(tolower(practice_primary_injury), 'not injury related|personal|team decision|travel|suspension|coach|jury duty|rest')) %>%
    mutate(less_practice = !(practice_status == 'Full Participation in Practice')) %>%
    select(season, week, gsis_id,  team, report_status, less_practice, illness, out_not_injury_related) %>%
    full_join(load_rosters_weekly((min_year):(max_year)) %>% filter(status != 'ACT') %>% select(season, week, status, gsis_id, team) %>% mutate(report_status = 'Out'),join_by('gsis_id','season','week','team')) %>%
    mutate(out = report_status.x == 'Out' |report_status.y == 'Out') %>% select(-report_status.x, -report_status.y) %>% filter(!is.na(gsis_id)) %>%
    group_by(season,week,gsis_id) %>% slice(n()) %>% ungroup() %>% mutate(out = ifelse(is.na(out), FALSE, out)) %>% select(-status)
  if (predict_mode & !testmode)
  {
    upsert_to_supabase('MainData', 'Injuries', injuries %>% filter(week == wk) %>% mutate(updated_at = Sys.time()), c('gsis_id','season','week'))
  }
  return(injuries)
}

#when joining on blue chip data, join on the player but also how it affects other players:
#get blue chips listed as out and add field for team members called blue_chip_team_member_out
combine_injuries_with_blue_chip = function(blue_chip, injuries)
{
  return(blue_chip %>%
    left_join(injuries %>% select(gsis_id,  season, week, team, out),
              join_by('season', 'week', 'gsis_id', 'team')) %>%
    mutate(out = ifelse(is.na(out), FALSE, out)) %>%
    group_by(team,season,week) %>% summarise(has_passing_blue_chip_out = any(out & passing_blue_chip),
                                             has_rushing_blue_chip_out = any(out & rushing_blue_chip),
                                             has_receiving_blue_chip_out = any(out & receiving_blue_chip),
                                             has_pass_rushers_blue_chip_out = any(out & pass_rushers_blue_chip),
                                             has_rush_tackles_blue_chip_out = any(out & rush_tackles_blue_chip),
                                             has_secondary_blue_chip_out = any(out & secondary_blue_chip)
    ) %>% arrange(team,season,week))
}