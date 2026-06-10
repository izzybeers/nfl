library(rvest)
library(httr)
library(dplyr)
library(stringr)
source('data_collection/scripts/global.R')
team_lookup  = team_lookup_table
date_cutoff = '2022-08-01'

Sys.setenv(RETICULATE_PYTHON = "/Users/izzybeers/.virtualenvs/nfl_model_env/bin/python")
library(reticulate)

gemini_api_key = 'AIzaSyBhJgDJGFSgMQ17jgedw1KTGkyXPdtnjbA'

virtualenv_create("nfl_model_env")
virtualenv_install("nfl_model_env", packages = c("scrapetube", "youtube-transcript-api", "yt_dlp"))
use_virtualenv("nfl_model_env", required = TRUE)

py_require(c("scrapetube", "youtube-transcript-api", 'yt_dlp'))
youtube_api <- import("youtube_transcript_api")
scrapetube = import("scrapetube")
youtube_dlp = import('yt_dlp')

#come back to i=7 TEN
#come back to i=10 PHI
#come back to i=11 SEA


get_transcripts = function(team_lookup, date_cutoff)
{
  consecutive_errors <<- 0
  #video_data_list = rbind()
  video_data_list = readRDS('video_data_checkpoint.rds')
  t1 = Sys.time()
  for (i in 2:nrow(team_lookup))
  {
    team_start = Sys.time()
    this_team = team_lookup$NFLReadr_Team_Abbr[i]
    print(this_team)
    channel_id = team_lookup$YoutubeChannel[i]
    playlist_id = team_lookup$YoutubePlaylistId[i]
    if(!is.na(playlist_id))
    {
      playlist_url = paste0("https://www.youtube.com/playlist?list=", playlist_id)
      ydl = youtube_dlp$YoutubeDL(list(extract_flat = "in_playlist", quiet = TRUE))
      playlist_info = ydl$extract_info(playlist_url, download = FALSE)
      video_ids = sapply(playlist_info$entries, function(x) x$id)
    } else {
      channel_streams = scrapetube$get_channel(channel_id, content_type = 'streams')
      channel_videos = scrapetube$get_channel(channel_id)
      streams = iterate(channel_streams)
      if(length(streams) < 100)
      {
        videos = iterate(channel_videos)
      } else {
        videos = c()
      }
      video_ids = sapply(c(streams,videos), function(x) x$videoId)
    }
    
    print(paste('Number of videos:', length(video_ids)))
    videos_this_team = video_data_list %>% filter(team == this_team)
    if(nrow(videos_this_team) == 0)
    {
      counter = 0
    } else {
      counter = max(which(video_ids == videos_this_team$video_id[length(videos_this_team$video_id)]))
    }
    for(v in video_ids[(counter+1):length(video_ids)])
    {
      counter = counter + 1
      cat(counter, ' ')
      tryCatch({
        video_url = paste0('https://www.youtube.com/watch?v=', v)
        dlp_res = youtube_dlp$YoutubeDL(list(quiet = TRUE,
                                             no_warnings = TRUE, # This kills the ffmpeg warning
                                             extractor_args = list(youtube = list(player_client = list("default")))))$extract_info(video_url, download = FALSE)
        title = dlp_res$title
        timestamp_posted = as.POSIXct(dlp_res$timestamp, origin = '1970-01-01', tz = 'America/New_York')
        if(length(timestamp_posted) == 0)
        {
          timestamp_posted = dlp_res$upload_date
        }
        if(timestamp_posted >= date_cutoff)
        {
          description = dlp_res$description
          video_duration_mins =  dlp_res$duration/60
          was_live = dlp_res$was_live
          view_count = dlp_res$view_count
          tags = paste(unique(tolower(dlp_res$tags)), collapse = '|')
          Sys.sleep(runif(1,15,45))
          text = youtube_api$YouTubeTranscriptApi()$fetch(video_id = v)
          full_text = paste(sapply(text$snippets,function(x) trimws(gsub('(text=(\"|\'))|(\"|\', start)|>>', '', str_extract(as.character(x), "text=.*start")))), collapse = ' ')
          video_data_list = rbind(video_data_list,
                                  data.frame(timestamp_posted, title, video_duration_mins, was_live, view_count, tags, description, full_text,  video_id = v) %>% mutate(team = this_team))
          
          Sys.sleep(runif(1,15,45))
          if (counter %% 5 == 0) {
            saveRDS(video_data_list, 'video_data_checkpoint.rds')
            cat("Taking a long break to avoid IP ban...")
            Sys.sleep(runif(1, 180, 300)) 
          }
        } else{
          break
        }
        
        consecutive_errors <<- 0
      }, error = function(e) {
        message(paste('Error on video', v))
        message(e$message)
        consecutive_errors <<- consecutive_errors + 1
        if(grepl("blocked|429|Too Many Requests|Forbidden|403", e) | consecutive_errors == 10)
        {
          stop('You have reached 10 consecutive errors or too many requests')
        }
      }, finally = {
        consecutive_errors <<- 0
      })
    }
    Sys.time() - team_start
  }
    print(paste('Total time for all teams'))
    Sys.time() - t1
    video_data_df = video_data_list %>%
      mutate(season = case_when(
        month(timestamp_posted) %in% c(3,4,5,6,7,8,9,10,11,12) ~ year(timestamp_posted),
        month(timestamp_posted) %in% c(1,2) ~ year(timestamp_posted) - 1,
        .default = NA)) %>%
      left_join(week_dates, join_by('season', 'timestamp_posted' <= 'week_end', 'timestamp_posted' >= 'week_start', 'team')) %>% select(-week_start, -week_end) %>%
      mutate(week = ifelse(is.na(week), 0, week),
             season = ifelse(week == 0 & month(timestamp_posted) %in% c(1,2), season + 1, season),
             press_conf = str_detect(tolower(title), 'press') | (str_detect(tolower(title), 'address') & str_detect(tolower(title), 'media')),
             coach_press_conf = press_conf & str_detect(tolower(title), 'coach')) %>%
      select(timestamp_posted, season, week, team, title, press_conf, coach_press_conf, video_duration_mins, view_count, description, full_text)
    
    saveRDS(video_data_df, 'video_data_checkpoint.rds')
  video_data_by_week = video_data_df %>% filter(press_conf) %>%
    group_by(season, week, team) %>% summarise(press_conf_text_this_week = paste(title, description, full_text, collapse = '\n\n')) 
  
  return(video_data_by_week)
}


get_video_llm_results = function(video_data, player_lookup)
{
  video_data = video_data %>%
    mutate(llm_prompt = paste0("### INSTRUCTIONS ###
Analyze the provided transcripts for ", team, ", Week ", week, ", ", season, ".
Return a JSON object with TWO main keys: 'team_intent' and 'player_features'.
1. 'team_intent' should be a single object.
2. 'player_features' MUST be an ARRAY of objects (not a nested list).
### JSON SCHEMA RULES ###
- pass_volume_shift: (-1 to 1)
- rush_volume_shift: (-1 to 1)
- gsis_id: The ID from the lookup table.
- health_concerns: (0 to 2)
- sentiment_score: (-1 to 1)
- role_clarity: (0 or 1, strict binary only.)
### ID LOOKUP TABLE ###", paste(player_lookup$gsis_id, ':', player_lookup$display_name, collapse = ', '),
                               "
### OUTPUT FORMAT ###
{
  \"team_intent\": { \"pass_volume_shift\": 0, \"rush_volume_shift\": 0 },
  \"player_features\": [
    { \"gsis_id\": \"00-00000\", \"health_concerns\": 0, \"sentiment_score\": 0, \"role_clarity\": 0 }
  ]
}", "### TRANSCRIPT DATA ###\n", press_conf_text_this_week))
  
  setAPI(gemini_api_key)
  teams_llm_results = data.frame()
  players_llm_results = data.frame()
  for(i in 1:nrow(video_data_by_week))
  {
    team = video_data_by_week$team[i]
    season = video_data_by_week$season[i]
    week = video_data_by_week$week[i]
    print(paste(team, season, 'week', week))
    response = gemini(
      prompt = video_data_by_week$llm_prompt[i], 
      model = "2.5-flash",
      temperature = 0.1 
    )
    
    res_list = jsonlite::fromJSON(str_extract(response, "(?s)\\{.*\\}"))
    
    response_df_team = as.data.frame(res_list$team_intent) %>% 
      mutate(team = team, season = season, week = week)
    
    response_df_player = as.data.frame(res_list$player_features) %>% 
      mutate(team = team, season = season, week = week)
    
    team_llm_results = rbind(teams_llm_results, response_df_team)
    if(nrow(response_df_player) > 0)
    {
      players_llm_results = rbind(players_llm_results, response_df_player)
    } else {
      player_llm_results = NULL
    }
    Sys.sleep(10)
  }
  return(list(team_llm_results, players_llm_results))
}


