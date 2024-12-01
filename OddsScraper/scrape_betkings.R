# Load necessary libraries
library(httr)
library(tidyverse)
library(rvest)
library(jsonlite)
library(glue)

# Get main landing page url
betkings_main_nba_url <- "https://www.betkings.com.au/sportsbook/Basketball/United%20States/NBA"

# use read_html_live to get each href link with class 'event-participants'
betkings_main_nba_page <- read_html_live(betkings_main_nba_url)

# Extract all href links with class 'event-participants'
betkings_main_nba_links <-
  betkings_main_nba_page %>%
  html_nodes(".event-participants") %>%
  html_attr("href")

# Extract the event ID from the href links
event_ids <- str_extract(betkings_main_nba_links, "(?<=\\?e=)\\d+")

# Define URLs with meaningful names
player_points_url <- glue("https://site-gateway.betkings.com.au/sportsbook/event/{event_ids}?includeMarketGroups=PLAYER_POINTS&locale=ENG")
player_rebounds_url <- "https://site-gateway.betkings.com.au/sportsbook/event/{event_ids}?includeMarketGroups=PLAYER_REBOUNDS&locale=ENG"
player_assists_url <- "https://site-gateway.betkings.com.au/sportsbook/event/{event_ids}?includeMarketGroups=PLAYER_ASSISTS&locale=ENG"
player_blocks_url <- "https://site-gateway.betkings.com.au/sportsbook/event/{event_ids}?includeMarketGroups=PLAYER_BLOCKS&locale=ENG"
player_steals_url <- "https://site-gateway.betkings.com.au/sportsbook/event/{event_ids}?includeMarketGroups=PLAYER_STEALS&locale=ENG"
player_three_pointers_url <- "https://site-gateway.betkings.com.au/sportsbook/event/{event_ids}?includeMarketGroups=PLAYER_THREE_POINTERS&locale=ENG"
player_par_url <- "https://site-gateway.betkings.com.au/sportsbook/event/{event_ids}?includeMarketGroups=PLAYER_POINTS_ASSISTS_AND_REBOUNDS&locale=ENG"

# Function to retrieve JSON data and save it to a named list
get_json_data <- function(url) {
  response <- GET(
    url,
    add_headers(
      `accept` = "application/json",
      `accept-encoding` = "gzip, deflate, br, zstd",
      `accept-language` = "en-US,en;q=0.9",
      `content-type` = "application/json",
      `device` = "desktop",
      `origin` = "https://www.betkings.com.au",
      `priority` = "u=1, i",
      `referer` = "https://www.betkings.com.au/",
      `sec-ch-ua` = '"Chromium";v="130", "Google Chrome";v="130", "Not?A_Brand";v="99"',
      `sec-ch-ua-mobile` = "?0",
      `sec-ch-ua-platform` = '"Windows"',
      `sec-fetch-dest` = "empty",
      `sec-fetch-mode` = "cors",
      `sec-fetch-site` = "same-site",
      `user-agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/130.0.0.0 Safari/537.36",
      `version` = "3.41.2",
      `x-local-language` = "en-US",
      `x-local-platform` = "Browser",
      `x-local-time-zone` = "Australia/Adelaide",
      `x-project-id` = "2"
    )
  )
  
  if (status_code(response) == 200) {
    data <- content(response, as = "text")
    json_data <- fromJSON(data, flatten = TRUE)
    return(json_data)
  } else {
    cat("Failed to retrieve data. Status code:", status_code(response), "\n")
    return(NULL)
  }
}

# Function to take a list of JSON data and extract the necessary information----
process_props <- function(json_data) {
  # Get the match name and market type
  match_name <- json_data$name
  market <- json_data$marketTypes[[1]]$name
  markets <- json_data$marketTypes[[1]]$markets
  
  # Loop over markets and extract player data
  prop_name_1 <- c()
  prop_name_2 <- c()
  price_1 <- c()
  price_2 <- c()
  
  for (i in seq_along(markets)) {
    prop_name_1 <- c(prop_name_1, markets[[i]]$outcome[[1]]$name)
    prop_name_2 <- c(prop_name_2, markets[[i]]$outcome[[2]]$name)
    price_1 <- c(price_1, markets[[i]]$outcome[[1]]$odds)
    price_2 <- c(price_2, markets[[i]]$outcome[[2]]$odds)
  }
  
  # Combine
  prop_name <- c(prop_name_1, prop_name_2)
  price <- c(price_1, price_2)
  
  # Create a tibble with the extracted data
  prop_data <- tibble(
    match_name = match_name,
    market = market,
    prop_name = prop_name,
    price = price
  )
  
  return(prop_data)
}

#===============================================================================
# Get player points data
#===============================================================================

# Get JSON data for player points
player_points_data <- map(player_points_url, get_json_data)

# Extract data from JSON
player_points_df <-
  map(player_points_data, process_props) %>% 
  bind_rows()

# Get Just Overs
player_points_overs <-
  player_points_df |>
  filter(str_detect(prop_name, "Over")) %>% 
  separate(prop_name, into = c("player_name", "line"), sep = " Over ") %>%
  



