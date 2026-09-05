
source('ScrapeBettingLinesHelperFunctions.R')

props = get_props()

write_to_supabase('betting', 'BettingLines', props)
