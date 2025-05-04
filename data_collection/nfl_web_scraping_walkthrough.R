#custom function created that returns the html doc for a given url.
#Use gpt to help you get set up with Chromote.

library(rvest)
library(chromote)
library(dplyr)
library(stringr)

#once you have it set up, use this to initialize a chromote session. You might have to do this periodically.
b = ChromoteSession$new()

get_html = function(url, max_retries = 3, wait_between_retries = 5) {
  retries = 0
  
  while (retries < max_retries) { #try up to 3 times. if it's still not working, then stop.
    tryCatch({
      # Attempt to navigate to the URL
      b$Page$navigate(url)
      Sys.sleep(5) #wait 5 seconds to give it time to load
      
      # Attempt to evaluate the page content
      result = b$Runtime$evaluate("document.documentElement.outerHTML")
      
      # Check if result is valid, and if so, pull the html doc
      if (!is.null(result) && !is.null(result[["result"]][["value"]])) {
        html_content <- result[["result"]][["value"]]
        html_doc <- read_html(html_content) #this is a function from rvest package that is widely used. Sometimes you can just use this directly, but I find Chromote works more widely.
        return(html_doc)  # Return HTML document if successful
      }
      
    }, error = function(e) {
      # Handle any errors by retrying
      retries = retries + 1
      Sys.sleep(wait_between_retries)
      print(paste("Retry:", retries))
      
      # Reset ChromoteSession if necessary
      if (retries < max_retries) {
        b = ChromoteSession$new()
        print("new session created")
      }
    })
    
    
  }
  
  
  if(retries == max_retries)
  {
    print(paste(max_retries, "tries unsuccessful"))
  }
}

#next, we want to pull the list of players. The player list website is organized by the letter of their name. So we will loop through all letters of the alphabet.
#a very important thing with web scraping is if you want to dynamicaly generate URLs like this, you have to look at the URLs and often you will find a pattern.
#so for this, the url is identical for each one, the only difference is the letter at the end.
#Go to the page https://www.pro-football-reference.com/players/a so you can see what it looks like.


player_df = rbind()

for (let in LETTERS)
{
  player_list = get_html(url = paste0("https://www.pro-football-reference.com/players/",let))
  if (any(str_detect(player_list %>% html_nodes("p") %>% as.character(), 'block traffic')))
  {
    print("Site detected scraping. Try again in one hour.")
    break
  }
  
  #if you go into the page source of this site and scroll down to the bottom, you can see the class tag labeled section_content and div tag labeled div_players
  #is where most of the useful content is stored.
  #use the html_nodes function below to pull from here. the dot indicates class and # indicates div name.
  player_list_content = player_list %>% html_nodes(".section_content#div_players")
  
  #within this, you can pull the p tag. Scroll in the html code and you can see everything in this section that is inside a p tag. here we extract this below,
  #and use html_text(trim = TRUE) to get rid of the html tags in the final output.
  total_info = player_list_content %>% html_nodes("p") %>% html_text(trim = TRUE)
  
  #now it's just using text mining and regular expressions to extract the actual information. Look at the html page source so you can see the format that these things
  years = str_extract(total_info, '[0-9]+-[0-9]+')
  min_year = sapply(strsplit(years, "-"), function(x) x[1])
  max_year = sapply(strsplit(years, "-"), function(x) x[2])
  positions = gsub('\\(|\\)','',str_extract(total_info, "\\(.*\\)"))
  names = gsub(' \\(', '', str_extract(total_info, '.+\\('))
  
  #below, you can extract a link using the a tag (indicating there is a link) and the href tag (that stores the link itself).
  links = player_list_content %>% html_nodes("a") %>% html_attr("href")
  
  df = data.frame(names, positions, min_year, max_year,links)
  
  if(!is.null(position_filter))
  {
    df = df %>% filter(str_detect(positions,position_filter) & max_year >= year_cutoff)
  }
  
  player_df = rbind(player_df, df)
}

