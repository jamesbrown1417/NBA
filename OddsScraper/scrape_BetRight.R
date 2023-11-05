# Libraries
library(tidyverse)
library(rvest)
library(httr2)
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
  mutate(join_name = ifelse(player_full_name == "Jaylin Williams", "Jay Williams", join_name)) |> 
  mutate(join_name = ifelse(player_full_name == "Bojan Bogdanovic", "Boj Bogdanovic", join_name))

# URL to get responses
# betright_url = "https://next-api.betright.com.au/Sports/Category?categoryId=60982" # IN Season Tournament - may need to change for regular NBA
betright_url = "https://next-api.betright.com.au/Sports/Category?categoryId=54" # Regular Season - change for IN Season Tournament

# Make request and get response
betright_response <-
  request(betright_url) |>
  req_perform() |> 
  resp_body_json()

# Get matches
matches <- betright_response$masterCategories[[1]]$categories[[1]]$masterEvents

# Keep only matches
matches <-
  matches |> 
  keep(~ .x$masterEventClassName == "Matches")

# Function to extract market info from response---------------------------------
get_market_info <- function(market) {
  
  # Market info
  markets_name = market$eventName
  market_propositions = market$outcomeName
  market_prices = market$price
  
  # Output Tibble
  tibble(market = markets_name,
         propositions = market_propositions,
         prices = market_prices)
}


# Function to extract match info from response----------------------------------
get_match_info <- function(matches) {
  # Match info
  match_name = matches$masterEventName
  match_start_time = matches$minAdvertisedStartTime
  match_id = matches$masterEventId
  
  # Market info
  market_info = map(matches$markets, get_market_info) |> bind_rows()
  
  # Output Tibble
  tibble(
    match = match_name,
    match_id = match_id,
    start_time = match_start_time,
    market_name = market_info$market,
    propositions = market_info$propositions,
    prices = market_info$prices
  )
}

# Map functions to data
all_betright_markets <-
  map(matches, get_match_info) |> bind_rows()

#===============================================================================
# Head to head markets
#===============================================================================

# Home teams
home_teams <-
  all_betright_markets |>
  separate(match, into = c("away_team", "home_team"), sep = " @ ", remove = FALSE) |>
  filter(str_detect(market_name, "Money Line")) |> 
  mutate(market_name = "Head To Head") |> 
  group_by(match) |> 
  filter(row_number() == 1) |> 
  rename(home_win = prices) |> 
  select(-propositions)

# Away teams
away_teams <-
  all_betright_markets |>
  separate(match, into = c("away_team", "home_team"), sep = " @ ", remove = FALSE) |>
  filter(str_detect(market_name, "Money Line")) |> 
  mutate(market_name = "Head To Head") |>
  group_by(match) |> 
  filter(row_number() == 2) |> 
  rename(away_win = prices) |> 
  select(-propositions)

# Combine
betright_head_to_head_markets <-
  home_teams |>
  left_join(away_teams) |> 
  select(match, start_time, market_name, home_team, home_win, away_team, away_win) |> 
  mutate(margin = round((1/home_win + 1/away_win), digits = 3)) |> 
  mutate(agency = "BetRight")

# Fix team names
betright_head_to_head_markets <-
  betright_head_to_head_markets |> 
  mutate(match = paste(home_team, "v", away_team))

# Write to csv
write_csv(betright_head_to_head_markets, "Data/scraped_odds/betright_h2h.csv")

#===============================================================================
# Player Props
#===============================================================================

# Get API URL for each market type----------------------------------------------

# Player Points
player_points_links <-
  glue("https://next-api.betright.com.au/Sports/MasterEvent?masterEventId={unique(all_betright_markets$match_id)}&groupTypeCode=G670&format=json")

# Player Points O/U Links
player_points_over_under_links <-
  glue("https://next-api.betright.com.au/Sports/MasterEvent?masterEventId={unique(all_betright_markets$match_id)}&groupTypeCode=G671&format=json")

# Player Assists Links
player_assists_links <-
  glue("https://next-api.betright.com.au/Sports/MasterEvent?masterEventId={unique(all_betright_markets$match_id)}&groupTypeCode=G672&format=json")

# Player Assists O/U Links
player_assists_over_under_links <-
  glue("https://next-api.betright.com.au/Sports/MasterEvent?masterEventId={unique(all_betright_markets$match_id)}&groupTypeCode=G678&format=json")

# Player Rebounds Links
player_rebounds_links <-
  glue("https://next-api.betright.com.au/Sports/MasterEvent?masterEventId={unique(all_betright_markets$match_id)}&groupTypeCode=G673&format=json")

# Player Rebounds O/U Links
player_rebounds_over_under_links <-
  glue("https://next-api.betright.com.au/Sports/MasterEvent?masterEventId={unique(all_betright_markets$match_id)}&groupTypeCode=G677&format=json")

# Player 3 Pointers Links
player_3_pointers_links <-
  glue("https://next-api.betright.com.au/Sports/MasterEvent?masterEventId={unique(all_betright_markets$match_id)}&groupTypeCode=G674&format=json")

# Player Blocks Links
player_blocks_links <-
  glue("https://next-api.betright.com.au/Sports/MasterEvent?masterEventId={unique(all_betright_markets$match_id)}&groupTypeCode=G675&format=json")

# Player Steals Links
player_steals_links <-
  glue("https://next-api.betright.com.au/Sports/MasterEvent?masterEventId={unique(all_betright_markets$match_id)}&groupTypeCode=G676&format=json")

# Function to extract prop data from links--------------------------------------

get_prop_data <- function(link) {
  
  # Get response
  response <-
    request(link) |>
    req_perform() |> 
    resp_body_json()
  
  # Empty vectors to append to
  event_name <- c()
  outcome_title <- c()
  outcome_name <- c()
  price <- c()
  
  for (event in response$events) {
    for (outcome in event$outcomes) {
      event_name <- c(event_name, event$eventName)
      outcome_title <- c(outcome_title, outcome$eventName)
      outcome_name <- c(outcome_name, outcome$outcomeName)
      price <- c(price, outcome$price)
    }
  }
  
  # Output Tibble
  tibble(
    event_name = event_name,
    outcome_title = outcome_title,
    outcome_name = outcome_name,
    price = price,
    link
  )
}

# Safe version of function
safe_get_prop_data <- safely(get_prop_data)

# Get player points data--------------------------------------------------------

# Match names to join
match_names <-
  all_betright_markets |>
  distinct(match, match_id)

# All Data

# Points
betright_player_points_alternate <-
  map(player_points_links, safe_get_prop_data) |> 
  map("result") |>
  bind_rows() |> 
  rename(match_id = link) |> 
  mutate(match_id = as.integer(str_extract(match_id, "[0-9]{4,7}"))) |> 
  left_join(match_names) |> 
  filter(!is.na(outcome_name))

# Points O/U
betright_player_points_over_under_all <-
  map(player_points_over_under_links, safe_get_prop_data) |> 
  map("result") |>
  bind_rows() |> 
  rename(match_id = link) |> 
  mutate(match_id = as.integer(str_extract(match_id, "[0-9]{4,7}"))) |> 
  left_join(match_names) |> 
  filter(!is.na(outcome_name))

# Combine
betright_player_points_all <-
  bind_rows(
    betright_player_points_alternate,
    betright_player_points_over_under_all
  )

# Get Overs (over under markets)
betright_player_points_overs <-
  betright_player_points_all |>
  filter(str_detect(outcome_title, "Over/Under")) |>
  filter(str_detect(outcome_name, "Over")) |> 
  separate(outcome_name, into = c("player_name", "line"), sep = " Over ") |> 
  mutate(line = as.numeric(line)) |> 
  rename(over_price = price) |> 
  select(match, player_name, line, over_price)

# Get Unders (over under markets)
betright_player_points_unders <-
  betright_player_points_all |>
  filter(str_detect(outcome_title, "Over/Under")) |>
  filter(str_detect(outcome_name, "Under")) |> 
  separate(outcome_name, into = c("player_name", "line"), sep = " Under ") |> 
  mutate(line = as.numeric(line)) |> 
  rename(under_price = price) |> 
  select(match, player_name, line, under_price)

# Get alternate player points markets
betright_alternate_points <-
  betright_player_points_all |>
  filter(str_detect(outcome_title, "Over/Under", negate = TRUE)) |> 
  mutate(player_name = str_remove(outcome_name, " \\d+\\+$")) |> 
  mutate(line = str_extract(outcome_name, "\\d+\\+$")) |>
  mutate(line = str_remove(line, "\\+$")) |>
  mutate(line = as.integer(line) - 0.5) |> 
  rename(over_price = price) |> 
  select(match, player_name, line, over_price)

# Combine
betright_player_points <-
  betright_player_points_overs |>
  bind_rows(betright_alternate_points) |>
  left_join(betright_player_points_unders) |> 
  mutate(agency = "BetRight") |> 
  mutate(market_type = "Player Points") |> 
  separate(match, into = c("away_team", "home_team"), sep = " @ ") |>
  mutate(player_name = case_when(
    str_detect(player_name, "R.J. Barrett") ~ "RJ Barrett",
    str_detect(player_name, "Cameron Thomas") ~ "Cam Thomas",
    str_detect(player_name, "Xavier Tillman Sr.") ~ "Xavier Tillman",
    str_detect(player_name, "Alperen Sengün") ~ "Alperen Sengun",
    str_detect(player_name, "Lauri Elias Markkanen") ~ "Lauri Markkanen",
    str_detect(player_name, "Jae'Sean  Tate") ~ "Jae'Sean Tate",
    .default = player_name)) |> 
  left_join(player_names[,c("player_full_name", "team_name")], by = c("player_name" = "player_full_name")) |> 
  mutate(opposition_team = case_when(
    team_name == away_team ~ home_team,
    team_name == home_team ~ away_team)) |>
  transmute(
    match = paste0(home_team, " v ", away_team),
    home_team,
    away_team,
    market_name = "Player Points",
    player_name,
    player_team = team_name,
    line,
    over_price,
    under_price,
    agency = "BetRight",
    opposition_team
  )

# Get Player Assists Data-------------------------------------------------------

# All Data
betright_player_assists_alternate <-
  map(player_assists_links, safe_get_prop_data) |> 
  map("result") |>
  bind_rows() |> 
  rename(match_id = link) |> 
  mutate(match_id = as.integer(str_extract(match_id, "[0-9]{4,7}"))) |> 
  left_join(match_names) |> 
  filter(!is.na(outcome_name))

# Assists O/U
betright_player_assists_over_under_all <-
  map(player_assists_over_under_links, safe_get_prop_data) |> 
  map("result") |>
  bind_rows() |> 
  rename(match_id = link) |> 
  mutate(match_id = as.integer(str_extract(match_id, "[0-9]{4,7}"))) |> 
  left_join(match_names) |> 
  filter(!is.na(outcome_name))

# Combine
betright_player_assists_all <-
  bind_rows(
    betright_player_assists_alternate,
    betright_player_assists_over_under_all
  )

# Get Overs (over under markets)
betright_player_assists_overs <-
  betright_player_assists_all |>
  filter(str_detect(outcome_title, "Over/Under")) |>
  filter(str_detect(outcome_name, "Over")) |> 
  separate(outcome_name, into = c("player_name", "line"), sep = " Over ") |> 
  mutate(line = as.numeric(line)) |> 
  rename(over_price = price) |> 
  select(match, player_name, line, over_price)

# Get Unders (over under markets)
betright_player_assists_unders <-
  betright_player_assists_all |>
  filter(str_detect(outcome_title, "Over/Under")) |>
  filter(str_detect(outcome_name, "Under")) |> 
  separate(outcome_name, into = c("player_name", "line"), sep = " Under ") |> 
  mutate(line = as.numeric(line)) |> 
  rename(under_price = price) |> 
  select(match, player_name, line, under_price)

# Get alternate player assists markets
betright_alternate_assists <-
  betright_player_assists_all |>
  filter(str_detect(outcome_title, "Over/Under", negate = TRUE)) |> 
  mutate(player_name = str_remove(outcome_name, " \\d+\\+$")) |> 
  mutate(line = str_extract(outcome_name, "\\d+\\+$")) |>
  mutate(line = str_remove(line, "\\+$")) |>
  mutate(line = as.integer(line) - 0.5) |> 
  rename(over_price = price) |> 
  select(match, player_name, line, over_price)

# Combine
betright_player_assists <-
  betright_player_assists_overs |>
  bind_rows(betright_alternate_assists) |>
  left_join(betright_player_assists_unders) |> 
  mutate(agency = "BetRight") |> 
  mutate(market_type = "Player Assists") |> 
  separate(match, into = c("away_team", "home_team"), sep = " @ ") |>
  mutate(player_name = case_when(
    str_detect(player_name, "R.J. Barrett") ~ "RJ Barrett",
    str_detect(player_name, "Cameron Thomas") ~ "Cam Thomas",
    str_detect(player_name, "Xavier Tillman Sr.") ~ "Xavier Tillman",
    str_detect(player_name, "Alperen Sengün") ~ "Alperen Sengun",
    str_detect(player_name, "Lauri Elias Markkanen") ~ "Lauri Markkanen",
    str_detect(player_name, "Jae'Sean  Tate") ~ "Jae'Sean Tate",
    .default = player_name)) |> 
  left_join(player_names[,c("player_full_name", "team_name")], by = c("player_name" = "player_full_name")) |> 
  mutate(opposition_team = case_when(
    team_name == away_team ~ home_team,
    team_name == home_team ~ away_team)) |>
  transmute(
    match = paste0(home_team, " v ", away_team),
    home_team,
    away_team,
    market_name = "Player Assists",
    player_name,
    player_team = team_name,
    line,
    over_price,
    under_price,
    agency = "BetRight",
    opposition_team
  )

# Get Player Rebounds Data-------------------------------------------------------

# All Data
betright_player_rebounds_alternate <-
  map(player_rebounds_links, safe_get_prop_data) |> 
  map("result") |>
  bind_rows() |> 
  rename(match_id = link) |> 
  mutate(match_id = as.integer(str_extract(match_id, "[0-9]{4,7}"))) |> 
  left_join(match_names) |> 
  filter(!is.na(outcome_name))

# Rebounds O/U
betright_player_rebounds_over_under_all <-
  map(player_rebounds_over_under_links, safe_get_prop_data) |> 
  map("result") |>
  bind_rows() |> 
  rename(match_id = link) |> 
  mutate(match_id = as.integer(str_extract(match_id, "[0-9]{4,7}"))) |> 
  left_join(match_names) |> 
  filter(!is.na(outcome_name))

# Combine
betright_player_rebounds_all <-
  bind_rows(
    betright_player_rebounds_alternate,
    betright_player_rebounds_over_under_all
  )

# Get Overs (over under markets)
betright_player_rebounds_overs <-
  betright_player_rebounds_all |>
  filter(str_detect(outcome_title, "Over/Under")) |>
  filter(str_detect(outcome_name, "Over")) |> 
  separate(outcome_name, into = c("player_name", "line"), sep = " Over ") |> 
  mutate(line = as.numeric(line)) |> 
  rename(over_price = price) |> 
  select(match, player_name, line, over_price)

# Get Unders (over under markets)
betright_player_rebounds_unders <-
  betright_player_rebounds_all |>
  filter(str_detect(outcome_title, "Over/Under")) |>
  filter(str_detect(outcome_name, "Under")) |> 
  separate(outcome_name, into = c("player_name", "line"), sep = " Under ") |> 
  mutate(line = as.numeric(line)) |> 
  rename(under_price = price) |> 
  select(match, player_name, line, under_price)

# Get alternate player rebounds markets
betright_alternate_rebounds <-
  betright_player_rebounds_all |>
  filter(str_detect(outcome_title, "Over/Under", negate = TRUE)) |> 
  mutate(player_name = str_remove(outcome_name, " \\d+\\+$")) |> 
  mutate(line = str_extract(outcome_name, "\\d+\\+$")) |>
  mutate(line = str_remove(line, "\\+$")) |>
  mutate(line = as.integer(line) - 0.5) |> 
  rename(over_price = price) |> 
  select(match, player_name, line, over_price)

# Combine
betright_player_rebounds <-
  betright_player_rebounds_overs |>
  bind_rows(betright_alternate_rebounds) |>
  left_join(betright_player_rebounds_unders) |> 
  mutate(agency = "BetRight") |> 
  mutate(market_type = "Player Rebounds") |> 
  separate(match, into = c("away_team", "home_team"), sep = " @ ") |>
  mutate(player_name = case_when(
    str_detect(player_name, "R.J. Barrett") ~ "RJ Barrett",
    str_detect(player_name, "Cameron Thomas") ~ "Cam Thomas",
    str_detect(player_name, "Xavier Tillman Sr.") ~ "Xavier Tillman",
    str_detect(player_name, "Alperen Sengün") ~ "Alperen Sengun",
    str_detect(player_name, "Lauri Elias Markkanen") ~ "Lauri Markkanen",
    str_detect(player_name, "Jae'Sean  Tate") ~ "Jae'Sean Tate",
    .default = player_name)) |> 
  left_join(player_names[,c("player_full_name", "team_name")], by = c("player_name" = "player_full_name")) |> 
  mutate(opposition_team = case_when(
    team_name == away_team ~ home_team,
    team_name == home_team ~ away_team)) |>
  transmute(
    match = paste0(home_team, " v ", away_team),
    home_team,
    away_team,
    market_name = "Player Rebounds",
    player_name,
    player_team = team_name,
    line,
    over_price,
    under_price,
    agency = "BetRight",
    opposition_team
  )

# Get player three pointers data------------------------------------------------

# All Data
betright_player_three_pointers_all <-
  map(player_3_pointers_links, safe_get_prop_data) |> 
  map("result") |>
  bind_rows() |> 
  rename(match_id = link) |> 
  mutate(match_id = as.integer(str_extract(match_id, "[0-9]{4,7}"))) |> 
  left_join(match_names) |> 
  filter(!is.na(outcome_name))

# Get Overs (over under markets)
betright_player_three_pointers_overs <-
  betright_player_three_pointers_all |>
  filter(str_detect(outcome_title, "Over/Under")) |>
  filter(str_detect(outcome_name, "Over")) |> 
  separate(outcome_name, into = c("player_name", "line"), sep = " Over ") |> 
  mutate(line = as.numeric(line)) |> 
  rename(over_price = price) |> 
  select(match, player_name, line, over_price)

# Get Unders (over under markets)
betright_player_three_pointers_unders <-
  betright_player_three_pointers_all |>
  filter(str_detect(outcome_title, "Over/Under")) |>
  filter(str_detect(outcome_name, "Under")) |> 
  separate(outcome_name, into = c("player_name", "line"), sep = " Under ") |> 
  mutate(line = as.numeric(line)) |> 
  rename(under_price = price) |> 
  select(match, player_name, line, under_price)

# Get alternate player three_pointers markets
betright_alternate_three_pointers <-
  betright_player_three_pointers_all |>
  filter(str_detect(outcome_title, "Over/Under", negate = TRUE)) |> 
  mutate(player_name = str_remove(outcome_name, " \\d+\\+$")) |> 
  mutate(line = str_extract(outcome_name, "\\d+\\+$")) |>
  mutate(line = str_remove(line, "\\+$")) |>
  mutate(line = as.integer(line) - 0.5) |> 
  rename(over_price = price) |> 
  select(match, player_name, line, over_price)

# Combine
betright_player_three_pointers <-
  betright_player_three_pointers_overs |>
  bind_rows(betright_alternate_three_pointers) |>
  left_join(betright_player_three_pointers_unders) |> 
  mutate(agency = "BetRight") |> 
  mutate(market_type = "Player Three Pointers") |> 
  separate(match, into = c("away_team", "home_team"), sep = " @ ") |>
  mutate(player_name = case_when(
    str_detect(player_name, "R.J. Barrett") ~ "RJ Barrett",
    str_detect(player_name, "Cameron Thomas") ~ "Cam Thomas",
    str_detect(player_name, "Xavier Tillman Sr.") ~ "Xavier Tillman",
    str_detect(player_name, "Alperen Sengün") ~ "Alperen Sengun",
    str_detect(player_name, "Lauri Elias Markkanen") ~ "Lauri Markkanen",
    str_detect(player_name, "Jae'Sean  Tate") ~ "Jae'Sean Tate",
    .default = player_name)) |> 
  left_join(player_names[,c("player_full_name", "team_name")], by = c("player_name" = "player_full_name")) |> 
  mutate(opposition_team = case_when(
    team_name == away_team ~ home_team,
    team_name == home_team ~ away_team)) |>
  transmute(
    match = paste0(home_team, " v ", away_team),
    home_team,
    away_team,
    market_name = "Player Three Pointers",
    player_name,
    player_team = team_name,
    line,
    over_price,
    under_price,
    agency = "BetRight",
    opposition_team
  )

# Get Player Blocks Data--------------------------------------------------------

# All Data
betright_player_blocks_all <-
  map(player_blocks_links, safe_get_prop_data) |> 
  map("result") |>
  bind_rows() |> 
  rename(match_id = link) |> 
  mutate(match_id = as.integer(str_extract(match_id, "[0-9]{4,7}"))) |> 
  left_join(match_names) |> 
  filter(!is.na(outcome_name))

# Get Overs (over under markets)
betright_player_blocks_overs <-
  betright_player_blocks_all |>
  filter(str_detect(outcome_title, "Over/Under")) |>
  filter(str_detect(outcome_name, "Over")) |> 
  separate(outcome_name, into = c("player_name", "line"), sep = " Over ") |> 
  mutate(line = as.numeric(line)) |> 
  rename(over_price = price) |> 
  select(match, player_name, line, over_price)

# Get Unders (over under markets)
betright_player_blocks_unders <-
  betright_player_blocks_all |>
  filter(str_detect(outcome_title, "Over/Under")) |>
  filter(str_detect(outcome_name, "Under")) |> 
  separate(outcome_name, into = c("player_name", "line"), sep = " Under ") |> 
  mutate(line = as.numeric(line)) |> 
  rename(under_price = price) |> 
  select(match, player_name, line, under_price)

# Get alternate player blocks markets
betright_alternate_blocks <-
  betright_player_blocks_all |>
  filter(str_detect(outcome_title, "Over/Under", negate = TRUE)) |> 
  mutate(player_name = str_remove(outcome_name, " \\d+\\+$")) |> 
  mutate(line = str_extract(outcome_name, "\\d+\\+$")) |>
  mutate(line = str_remove(line, "\\+$")) |>
  mutate(line = as.integer(line) - 0.5) |> 
  rename(over_price = price) |> 
  select(match, player_name, line, over_price)

# Combine
betright_player_blocks <-
  betright_player_blocks_overs |>
  bind_rows(betright_alternate_blocks) |>
  left_join(betright_player_blocks_unders) |> 
  mutate(agency = "BetRight") |> 
  mutate(market_type = "Player Blocks") |> 
  separate(match, into = c("away_team", "home_team"), sep = " @ ") |>
  mutate(player_name = case_when(
    str_detect(player_name, "R.J. Barrett") ~ "RJ Barrett",
    str_detect(player_name, "Cameron Thomas") ~ "Cam Thomas",
    str_detect(player_name, "Xavier Tillman Sr.") ~ "Xavier Tillman",
    str_detect(player_name, "Alperen Sengün") ~ "Alperen Sengun",
    str_detect(player_name, "Lauri Elias Markkanen") ~ "Lauri Markkanen",
    str_detect(player_name, "Jae'Sean  Tate") ~ "Jae'Sean Tate",
    .default = player_name)) |> 
  left_join(player_names[,c("player_full_name", "team_name")], by = c("player_name" = "player_full_name")) |> 
  mutate(opposition_team = case_when(
    team_name == away_team ~ home_team,
    team_name == home_team ~ away_team)) |>
  transmute(
    match = paste0(home_team, " v ", away_team),
    home_team,
    away_team,
    market_name = "Player Blocks",
    player_name,
    player_team = team_name,
    line,
    over_price,
    under_price,
    agency = "BetRight",
    opposition_team
  )

# Get Player Steals Data--------------------------------------------------------

# All Data
betright_player_steals_all <-
  map(player_steals_links, safe_get_prop_data) |> 
  map("result") |>
  bind_rows() |> 
  rename(match_id = link) |> 
  mutate(match_id = as.integer(str_extract(match_id, "[0-9]{4,7}"))) |> 
  left_join(match_names) |> 
  filter(!is.na(outcome_name))

# Get Overs (over under markets)
betright_player_steals_overs <-
  betright_player_steals_all |>
  filter(str_detect(outcome_title, "Over/Under")) |>
  filter(str_detect(outcome_name, "Over")) |> 
  separate(outcome_name, into = c("player_name", "line"), sep = " Over ") |> 
  mutate(line = as.numeric(line)) |> 
  rename(over_price = price) |> 
  select(match, player_name, line, over_price)

# Get Unders (over under markets)
betright_player_steals_unders <-
  betright_player_steals_all |>
  filter(str_detect(outcome_title, "Over/Under")) |>
  filter(str_detect(outcome_name, "Under")) |> 
  separate(outcome_name, into = c("player_name", "line"), sep = " Under ") |> 
  mutate(line = as.numeric(line)) |> 
  rename(under_price = price) |> 
  select(match, player_name, line, under_price)

# Get alternate player steals markets
betright_alternate_steals <-
  betright_player_steals_all |>
  filter(str_detect(outcome_title, "Over/Under", negate = TRUE)) |> 
  mutate(player_name = str_remove(outcome_name, " \\d+\\+$")) |> 
  mutate(line = str_extract(outcome_name, "\\d+\\+$")) |>
  mutate(line = str_remove(line, "\\+$")) |>
  mutate(line = as.integer(line) - 0.5) |> 
  rename(over_price = price) |> 
  select(match, player_name, line, over_price)

# Combine
betright_player_steals <-
  betright_player_steals_overs |>
  bind_rows(betright_alternate_steals) |>
  left_join(betright_player_steals_unders) |> 
  mutate(agency = "BetRight") |> 
  mutate(market_type = "Player Steals") |> 
  separate(match, into = c("away_team", "home_team"), sep = " @ ") |>
  mutate(player_name = case_when(
    str_detect(player_name, "R.J. Barrett") ~ "RJ Barrett",
    str_detect(player_name, "Cameron Thomas") ~ "Cam Thomas",
    str_detect(player_name, "Xavier Tillman Sr.") ~ "Xavier Tillman",
    str_detect(player_name, "Alperen Sengün") ~ "Alperen Sengun",
    str_detect(player_name, "Lauri Elias Markkanen") ~ "Lauri Markkanen",
    str_detect(player_name, "Jae'Sean  Tate") ~ "Jae'Sean Tate",
    .default = player_name)) |> 
  left_join(player_names[,c("player_full_name", "team_name")], by = c("player_name" = "player_full_name")) |> 
  mutate(opposition_team = case_when(
    team_name == away_team ~ home_team,
    team_name == home_team ~ away_team)) |>
  transmute(
    match = paste0(home_team, " v ", away_team),
    home_team,
    away_team,
    market_name = "Player Steals",
    player_name,
    player_team = team_name,
    line,
    over_price,
    under_price,
    agency = "BetRight",
    opposition_team
  )

#===============================================================================
# Write to CSV
#===============================================================================

betright_player_points |> write_csv("Data/scraped_odds/betright_player_points.csv")
betright_player_assists |> write_csv("Data/scraped_odds/betright_player_assists.csv")
betright_player_rebounds |> write_csv("Data/scraped_odds/betright_player_rebounds.csv")
betright_player_three_pointers |> write_csv("Data/scraped_odds/betright_player_three_pointers.csv")
betright_player_blocks |> write_csv("Data/scraped_odds/betright_player_blocks.csv")
betright_player_steals |> write_csv("Data/scraped_odds/betright_player_steals.csv")
