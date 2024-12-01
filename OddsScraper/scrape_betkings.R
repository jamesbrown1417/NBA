# Load necessary libraries
library(httr)
library(tidyverse)
library(rvest)
library(jsonlite)
library(glue)

# Get teams table
teams <-
  read_csv("Data/all_teams.csv")

# Get player names table
player_names_all <-
  read_csv("Data/all_rosters.csv") |>
  select(player_full_name = PLAYER, TeamID) |> 
  left_join(teams[, c("id", "full_name")], by = c("TeamID" = "id")) |> 
  mutate(first_initial = str_sub(player_full_name, 1, 1)) |>
  mutate(surname = str_extract(player_full_name, "(?<=\\s).*$")) |> 
  mutate(join_name = paste(first_initial, surname, sep = " ")) |> 
  rename(team_name = full_name)

# unique join names
player_names_unique <-
  player_names_all |>
  group_by(join_name) |> 
  filter(n() == 1) |> 
  ungroup()

# Non unique names (take first two letters of first name)
player_names_non_unique <-
  player_names_all |>
  group_by(join_name) |> 
  filter(n() > 1) |> 
  mutate(first_initial = str_sub(player_full_name, 1, 2)) |>
  mutate(join_name = paste(first_initial, surname, sep = " ")) |> 
  ungroup()

player_names <-
  bind_rows(player_names_unique, player_names_non_unique) |> 
  mutate(join_name = ifelse(player_full_name == "Keyontae Johnson", "Key Johnson", join_name)) |> 
  mutate(join_name = ifelse(player_full_name == "Miles Bridges", "Mil Bridges", join_name)) |> 
  mutate(join_name = ifelse(player_full_name == "Jaylin Williams", "Jay Williams", join_name))

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
player_rebounds_url <- glue("https://site-gateway.betkings.com.au/sportsbook/event/{event_ids}?includeMarketGroups=PLAYER_REBOUNDS&locale=ENG")
player_assists_url <- glue("https://site-gateway.betkings.com.au/sportsbook/event/{event_ids}?includeMarketGroups=PLAYER_ASSISTS&locale=ENG")
player_blocks_url <- glue("https://site-gateway.betkings.com.au/sportsbook/event/{event_ids}?includeMarketGroups=PLAYER_BLOCKS&locale=ENG")
player_steals_url <- glue("https://site-gateway.betkings.com.au/sportsbook/event/{event_ids}?includeMarketGroups=PLAYER_STEALS&locale=ENG")
player_three_pointers_url <- glue("https://site-gateway.betkings.com.au/sportsbook/event/{event_ids}?includeMarketGroups=PLAYER_THREE_POINTERS&locale=ENG")
player_par_url <- glue("https://site-gateway.betkings.com.au/sportsbook/event/{event_ids}?includeMarketGroups=PLAYER_POINTS_ASSISTS_AND_REBOUNDS&locale=ENG")

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
  
  # Number of markets to iterate over
  num_markets <- length(json_data$marketTypes)
  
  # list of length num_markets to store data
  prop_data_list <- list()
  
  for (j in 1:num_markets) {
    # Get the match name and market type
    match_name <- json_data$name
    market <- json_data$marketTypes[[j]]$name
    markets <- json_data$marketTypes[[j]]$markets
    
    # Loop over markets and extract player data
    prop_name_1 <- c()
    prop_name_2 <- c()
    price_1 <- c()
    price_2 <- c()
    
    for (i in seq_along(markets)) {
      prop_name_1 <- c(prop_name_1, markets[[i]]$outcome[[1]]$name)
      price_1 <- c(price_1, markets[[i]]$outcome[[1]]$odds)
      
      # Check if there are two outcomes
      if (length(markets[[i]]$outcome) == 1) {
        prop_name_2 <- c(prop_name_2, NA)
        price_2 <- c(price_2, NA)
        next
      }
      prop_name_2 <- c(prop_name_2, markets[[i]]$outcome[[2]]$name)
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
    
    prop_data_list[[j]] <- prop_data
  }
  
  output_tibble <-
    bind_rows(prop_data_list) %>% 
    filter(!is.na(prop_name))
  
  names(output_tibble) <- c("match_name", "market_name", "prop_name", "price")
  
  return(output_tibble)
}

#===============================================================================
# Get player points data
#===============================================================================

# Get JSON data for player points
player_points_data <- map(player_points_url, get_json_data)

# Get safe version of process_props
process_props <- safely(process_props)

# Extract data from JSON
player_points_df <-
  map(player_points_data, process_props) %>%
  # Keep result part
  map("result") %>%
  bind_rows()

# Get Just Overs
player_points_overs <-
  player_points_df |>
  filter(str_detect(prop_name, "Over")) %>% 
  separate(prop_name, into = c("player_name", "line"), sep = " Over ") %>%
  mutate(line = str_remove(line, "\\(")) %>%
  mutate(line = str_remove(line, "\\)")) %>%
  mutate(line = as.numeric(line)) %>%
  transmute(match = match_name, market_name = "Player Points", player_name, line, over_price = price)

# Get Just Unders
player_points_unders <-
  player_points_df |>
  filter(str_detect(prop_name, "Under")) %>% 
  separate(prop_name, into = c("player_name", "line"), sep = " Under ") %>%
  mutate(line = str_remove(line, "\\(")) %>%
  mutate(line = str_remove(line, "\\)")) %>%
  mutate(line = as.numeric(line)) %>%
  transmute(match = match_name, market_name = "Player Points", player_name, line, under_price = price)

# Combine
player_points_all <-
  player_points_overs %>%
  left_join(player_points_unders) %>%
  left_join(player_names[,c("player_full_name", "team_name")], by = c("player_name" = "player_full_name")) %>%
  rename(player_team = team_name) %>% 
  separate(match, into = c("home_team", "away_team"), sep = " vs ", remove = FALSE) %>%
  mutate(opposition_team = ifelse(home_team == player_team, away_team, home_team)) %>%
  mutate(match = paste(home_team, "v", away_team, sep = " ")) %>% 
  transmute(
    match,
    home_team,
    away_team,
    market_name,
    player_name,
    player_team,
    opposition_team,
    line,
    over_price,
    under_price,
    agency = "Sportsbet"
  )

#===============================================================================
# Get player assists data
#===============================================================================

# Get JSON data for player assists
player_assists_data <- map(player_assists_url, get_json_data)

# Extract data from JSON
player_assists_df <-
  map(player_assists_data, process_props) %>% 
  bind_rows()

# Get Just Overs
player_assists_overs <-
  player_assists_df |>
  filter(str_detect(prop_name, "Over")) %>% 
  separate(prop_name, into = c("player_name", "line"), sep = " Over ") %>%
  mutate(line = str_remove(line, "\\(")) %>%
  mutate(line = str_remove(line, "\\)")) %>%
  mutate(line = as.numeric(line)) %>%
  transmute(match = match_name, market_name = "Player Assists", player_name, line, over_price = price)

# Get Just Unders
player_assists_unders <-
  player_assists_df |>
  filter(str_detect(prop_name, "Under")) %>% 
  separate(prop_name, into = c("player_name", "line"), sep = " Under ") %>%
  mutate(line = str_remove(line, "\\(")) %>%
  mutate(line = str_remove(line, "\\)")) %>%
  mutate(line = as.numeric(line)) %>%
  transmute(match = match_name, market_name = "Player Assists", player_name, line, under_price = price)

# Combine
player_assists_all <-
  player_assists_overs %>%
  left_join(player_assists_unders) %>%
  left_join(player_names[,c("player_full_name", "team_name")], by = c("player_name" = "player_full_name")) %>%
  rename(player_team = team_name) %>% 
  separate(match, into = c("home_team", "away_team"), sep = " vs ", remove = FALSE) %>%
  mutate(opposition_team = ifelse(home_team == player_team, away_team, home_team)) %>%
  mutate(match = paste(home_team, "v", away_team, sep = " ")) %>% 
  transmute(
    match,
    home_team,
    away_team,
    market_name,
    player_name,
    player_team,
    opposition_team,
    line,
    over_price,
    under_price,
    agency = "Sportsbet"
  )

#===============================================================================
# Get player rebounds data
#===============================================================================

# Get JSON data for player rebounds
player_rebounds_data <- map(player_rebounds_url, get_json_data)

# Extract data from JSON
player_rebounds_df <-
  map(player_rebounds_data, process_props) %>% 
  bind_rows()

# Get Just Overs
player_rebounds_overs <-
  player_rebounds_df |>
  filter(str_detect(prop_name, "Over")) %>% 
  separate(prop_name, into = c("player_name", "line"), sep = " Over ") %>%
  mutate(line = str_remove(line, "\\(")) %>%
  mutate(line = str_remove(line, "\\)")) %>%
  mutate(line = as.numeric(line)) %>%
  transmute(match = match_name, market_name = "Player Rebounds", player_name, line, over_price = price)

# Get Just Unders
player_rebounds_unders <-
  player_rebounds_df |>
  filter(str_detect(prop_name, "Under")) %>% 
  separate(prop_name, into = c("player_name", "line"), sep = " Under ") %>%
  mutate(line = str_remove(line, "\\(")) %>%
  mutate(line = str_remove(line, "\\)")) %>%
  mutate(line = as.numeric(line)) %>%
  transmute(match = match_name, market_name = "Player Rebounds", player_name, line, under_price = price)

# Combine
player_rebounds_all <-
  player_rebounds_overs %>%
  left_join(player_rebounds_unders) %>%
  left_join(player_names[,c("player_full_name", "team_name")], by = c("player_name" = "player_full_name")) %>%
  rename(player_team = team_name) %>% 
  separate(match, into = c("home_team", "away_team"), sep = " vs ", remove = FALSE) %>%
  mutate(opposition_team = ifelse(home_team == player_team, away_team, home_team)) %>%
  mutate(match = paste(home_team, "v", away_team, sep = " ")) %>% 
  transmute(
    match,
    home_team,
    away_team,
    market_name,
    player_name,
    player_team,
    opposition_team,
    line,
    over_price,
    under_price,
    agency = "Sportsbet"
  )
