# Libraries
library(tidyverse)
library(rvest)
library(httr2)
library(glue)

# Get teams table
teams <-
  read_csv("Data/all_teams.csv")

# Function to fix team names
source("Scripts/fix_team_names.R")
source("Scripts/fix_player_names.R")

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
betright_url = "https://next-api.betright.com.au/Sports/Category?categoryId=54" # Regular Season - change for In Season Tournament

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

# Player Stats
all_links <-
  glue("https://next-api.betright.com.au/Sports/MasterEventEvents?masterEventId={unique(all_betright_markets$match_id)}")

# Function to extract prop data from links--------------------------------------

get_prop_data <- function(link) {
  
  # Get response
  response <-
    request(link) |>
    req_perform() |> 
    resp_body_json()
  
  # Empty vectors to append to
  event_name <- c()
  event_id <- c()
  outcome_title <- c()
  outcome_name <- c()
  outcome_id <- c()
  group_by_header <- c()
  fixed_market_id <- c()
  price <- c()
  
  for (event in response$events) {
    for (outcome in event$outcomes) {
      event_name <- c(event_name, event$eventName)
      event_id <- c(event_id, event$eventId)
      outcome_title <- c(outcome_title, outcome$eventName)
      outcome_name <- c(outcome_name, outcome$outcomeName)
      outcome_id <- c(outcome_id, outcome$outcomeId)
      group_by_header <- c(group_by_header, outcome$groupByHeader)
      fixed_market_id <- c(fixed_market_id, outcome$fixedMarketId)
      price <- c(price, outcome$price)
    }
  }
  
  # Output Tibble
  tibble(
    event_name = event_name,
    event_id = event_id,
    outcome_title = outcome_title,
    outcome_name = outcome_name,
    outcome_id = outcome_id,
    group_by_header = group_by_header,
    fixed_market_id = fixed_market_id,
    price = price,
    link
  )
}

# Safe version of function
safe_get_prop_data <- safely(get_prop_data)

# Match names to join
match_names <-
  all_betright_markets |>
  distinct(match, match_id)

# All props
all_props <-
map(all_links, safe_get_prop_data) |> 
  map("result") |>
  bind_rows() |>
  rename(match_id = link) |> 
  mutate(match_id = as.integer(str_extract(match_id, "[0-9]{4,7}"))) |> 
  left_join(match_names) |> 
  filter(!is.na(outcome_name)) 

#===============================================================================
# Player Points
#===============================================================================

# Get all player points (alternate)
betright_player_points_alternate <-
  all_props |> 
  filter(str_detect(event_name, "^Player Points \\-")) |>
  separate(event_name, into = c("market_name", "player_name"), sep = " - ") |>
  mutate(player_name = str_remove_all(player_name, " \\(.*\\)")) |>
  mutate(player_name = str_replace_all(player_name, "  ", " ")) |>  
  mutate(player_name = fix_player_names(player_name)) |>
  left_join(player_names[, c("player_full_name", "team_name")], by = c("player_name" = "player_full_name")) |>
  rename(player_team = team_name) |>
  separate(match, into = c("away_team", "home_team"), sep = " @ ", remove = FALSE) |> 
  mutate(opposition_team = if_else(home_team == player_team, away_team, home_team)) |>
  mutate(agency = "BetRight") |>
  mutate(line = str_extract(outcome_name, "\\d+\\.?\\d*")) |>
  mutate(line = as.numeric(line) - 0.5) |>
  select(
    "match",
    "home_team",
    "away_team",
    "market_name",
    "player_name",
    "player_team",
    "line",
    "over_price" = "price",
    "agency",
    "event_id",
    "outcome_name",
    "outcome_id",
    "fixed_market_id",
    "opposition_team"
  ) |> 
  mutate(home_team = fix_team_names(home_team)) |>
  mutate(away_team = fix_team_names(away_team)) |>
  mutate(player_team = fix_team_names(player_team)) |>
  mutate(opposition_team = fix_team_names(opposition_team)) |> 
  mutate(match = paste(home_team, away_team, sep = " v "))

# Get player points (lines) - Overs
betright_player_points_lines_overs <-
  all_props |> 
  filter(str_detect(event_name, "^Player Points Over\\/Under.*")) |>
  filter(str_detect(outcome_name, "Over")) |>
  separate(event_name, into = c("market_name", "player_name"), sep = " - ") |>
  mutate(player_name = str_remove_all(player_name, " \\(.*\\)")) |> 
  mutate(player_name = str_replace_all(player_name, "  ", " ")) |>
  mutate(player_name = fix_player_names(player_name)) |>
  left_join(player_names[, c("player_full_name", "team_name")], by = c("player_name" = "player_full_name")) |>
  rename(player_team = team_name) |>
  separate(match, into = c("away_team", "home_team"), sep = " @ ", remove = FALSE) |>
  mutate(opposition_team = if_else(home_team == player_team, away_team, home_team)) |>
  mutate(agency = "BetRight") |>
  mutate(line = str_extract(outcome_name, "\\d+\\.?\\d*")) |>
  mutate(line = as.numeric(line)) |>
  mutate(market_name = "Player Points") |> 
  select(
    "match",
    "home_team",
    "away_team",
    "market_name",
    "player_name",
    "player_team",
    "line",
    "over_price" = "price",
    "agency",
    "event_id",
    "outcome_name",
    "outcome_id",
    "fixed_market_id",
    "opposition_team"
  ) |>
  mutate(home_team = fix_team_names(home_team)) |>
  mutate(away_team = fix_team_names(away_team)) |>
  mutate(player_team = fix_team_names(player_team)) |>
  mutate(opposition_team = fix_team_names(opposition_team)) |>
  mutate(match = paste(home_team, away_team, sep = " v "))

# Get player points (lines) - Unders
betright_player_points_lines_unders <-
  all_props |> 
  filter(str_detect(event_name, "^Player Points Over\\/Under.*")) |>
  filter(str_detect(outcome_name, "Under")) |>
  separate(event_name, into = c("market_name", "player_name"), sep = " - ") |>
  mutate(player_name = str_remove_all(player_name, " \\(.*\\)")) |> 
  mutate(player_name = str_replace_all(player_name, "  ", " ")) |>
  mutate(player_name = fix_player_names(player_name)) |>
  left_join(player_names[, c("player_full_name", "team_name")], by = c("player_name" = "player_full_name")) |>
  rename(player_team = team_name) |>
  separate(match, into = c("away_team", "home_team"), sep = " @ ", remove = FALSE) |>
  mutate(opposition_team = if_else(home_team == player_team, away_team, home_team)) |>
  mutate(agency = "BetRight") |>
  mutate(line = str_extract(outcome_name, "\\d+\\.?\\d*")) |>
  mutate(line = as.numeric(line)) |>
  mutate(market_name = "Player Points") |> 
  select(
    "match",
    "home_team",
    "away_team",
    "market_name",
    "player_name",
    "player_team",
    "line",
    "under_price" = "price",
    "agency",
    "event_id",
    "outcome_name_unders" = "outcome_name",
    "outcome_id_unders" = "outcome_id",
    "fixed_market_id_unders" = "fixed_market_id",
    "opposition_team"
  ) |>
  mutate(home_team = fix_team_names(home_team)) |>
  mutate(away_team = fix_team_names(away_team)) |>
  mutate(player_team = fix_team_names(player_team)) |>
  mutate(opposition_team = fix_team_names(opposition_team)) |>
  mutate(match = paste(home_team, away_team, sep = " v "))

# Get all player points together
betright_player_points <-
  bind_rows(betright_player_points_alternate,
            betright_player_points_lines_overs) |> 
  full_join(betright_player_points_lines_unders)

#===============================================================================
# Player Assists
#===============================================================================

betright_player_assists_alternate <-
  all_props |> 
  filter(str_detect(event_name, "^Player Assists -")) |>
  separate(event_name, into = c("market_name", "player_name"), sep = " - ") |>
  mutate(player_name = str_remove_all(player_name, " \\(.*\\)")) |>
  mutate(player_name = str_replace_all(player_name, "  ", " ")) |>  
  mutate(player_name = fix_player_names(player_name)) |>
  left_join(player_names[, c("player_full_name", "team_name")], by = c("player_name" = "player_full_name")) |>
  rename(player_team = team_name) |>
  separate(match, into = c("away_team", "home_team"), sep = " @ ", remove = FALSE) |> 
  mutate(opposition_team = if_else(home_team == player_team, away_team, home_team)) |>
  mutate(agency = "BetRight") |>
  mutate(line = str_extract(outcome_name, "\\d+\\.?\\d*")) |>
  mutate(line = as.numeric(line) - 0.5) |>
  select(
    "match",
    "home_team",
    "away_team",
    "market_name",
    "player_name",
    "player_team",
    "line",
    "over_price" = "price",
    "agency",
    "event_id",
    "outcome_name",
    "outcome_id",
    "fixed_market_id",
    "opposition_team"
  ) |> 
  mutate(home_team = fix_team_names(home_team)) |>
  mutate(away_team = fix_team_names(away_team)) |>
  mutate(player_team = fix_team_names(player_team)) |>
  mutate(opposition_team = fix_team_names(opposition_team)) |> 
  mutate(match = paste(home_team, away_team, sep = " v "))

betright_player_assists_lines_overs <-
  all_props |> 
  filter(str_detect(event_name, "^Player Assists Over\\/Under.*")) |>
  filter(str_detect(outcome_name, "Over")) |>
  separate(event_name, into = c("market_name", "player_name"), sep = " - ") |>
  mutate(player_name = str_remove_all(player_name, " \\(.*\\)")) |> 
  mutate(player_name = str_replace_all(player_name, "  ", " ")) |>
  mutate(player_name = fix_player_names(player_name)) |>
  left_join(player_names[, c("player_full_name", "team_name")], by = c("player_name" = "player_full_name")) |>
  rename(player_team = team_name) |>
  separate(match, into = c("away_team", "home_team"), sep = " @ ", remove = FALSE) |>
  mutate(opposition_team = if_else(home_team == player_team, away_team, home_team)) |>
  mutate(agency = "BetRight") |>
  mutate(line = str_extract(outcome_name, "\\d+\\.?\\d*")) |>
  mutate(line = as.numeric(line)) |>
  mutate(market_name = "Player Assists") |> 
  select(
    "match",
    "home_team",
    "away_team",
    "market_name",
    "player_name",
    "player_team",
    "line",
    "over_price" = "price",
    "agency",
    "event_id",
    "outcome_name",
    "outcome_id",
    "fixed_market_id",
    "opposition_team"
  ) |>
  mutate(home_team = fix_team_names(home_team)) |>
  mutate(away_team = fix_team_names(away_team)) |>
  mutate(player_team = fix_team_names(player_team)) |>
  mutate(opposition_team = fix_team_names(opposition_team)) |>
  mutate(match = paste(home_team, away_team, sep = " v "))

betright_player_assists_lines_unders <-
  all_props |> 
  filter(str_detect(event_name, "^Player Assists Over\\/Under.*")) |>
  filter(str_detect(outcome_name, "Under")) |>
  separate(event_name, into = c("market_name", "player_name"), sep = " - ") |>
  mutate(player_name = str_remove_all(player_name, " \\(.*\\)")) |> 
  mutate(player_name = str_replace_all(player_name, "  ", " ")) |>
  mutate(player_name = fix_player_names(player_name)) |>
  left_join(player_names[, c("player_full_name", "team_name")], by = c("player_name" = "player_full_name")) |>
  rename(player_team = team_name) |>
  separate(match, into = c("away_team", "home_team"), sep = " @ ", remove = FALSE) |>
  mutate(opposition_team = if_else(home_team == player_team, away_team, home_team)) |>
  mutate(agency = "BetRight") |>
  mutate(line = str_extract(outcome_name, "\\d+\\.?\\d*")) |>
  mutate(line = as.numeric(line)) |>
  mutate(market_name = "Player Assists") |> 
  select(
    "match",
    "home_team",
    "away_team",
    "market_name",
    "player_name",
    "player_team",
    "line",
    "under_price" = "price",
    "agency",
    "event_id",
    "outcome_name_unders" = "outcome_name",
    "outcome_id_unders" = "outcome_id",
    "fixed_market_id_unders" = "fixed_market_id",
    "opposition_team"
  ) |>
  mutate(home_team = fix_team_names(home_team)) |>
  mutate(away_team = fix_team_names(away_team)) |>
  mutate(player_team = fix_team_names(player_team)) |>
  mutate(opposition_team = fix_team_names(opposition_team)) |>
  mutate(match = paste(home_team, away_team, sep = " v "))

betright_player_assists <-
  bind_rows(betright_player_assists_alternate,
            betright_player_assists_lines_overs) |> 
  full_join(betright_player_assists_lines_unders)

#===============================================================================
# Player Rebounds
#===============================================================================

betright_player_rebounds_alternate <-
  all_props |> 
  filter(str_detect(event_name, "^Player Rebounds -")) |>
  separate(event_name, into = c("market_name", "player_name"), sep = " - ") |>
  mutate(player_name = str_remove_all(player_name, " \\(.*\\)")) |>
  mutate(player_name = str_replace_all(player_name, "  ", " ")) |>  
  mutate(player_name = fix_player_names(player_name)) |>
  left_join(player_names[, c("player_full_name", "team_name")], by = c("player_name" = "player_full_name")) |>
  rename(player_team = team_name) |>
  separate(match, into = c("away_team", "home_team"), sep = " @ ", remove = FALSE) |> 
  mutate(opposition_team = if_else(home_team == player_team, away_team, home_team)) |>
  mutate(agency = "BetRight") |>
  mutate(line = str_extract(outcome_name, "\\d+\\.?\\d*")) |>
  mutate(line = as.numeric(line) - 0.5) |>
  select(
    "match",
    "home_team",
    "away_team",
    "market_name",
    "player_name",
    "player_team",
    "line",
    "over_price" = "price",
    "agency",
    "event_id",
    "outcome_name",
    "outcome_id",
    "fixed_market_id",
    "opposition_team"
  ) |> 
  mutate(home_team = fix_team_names(home_team)) |>
  mutate(away_team = fix_team_names(away_team)) |>
  mutate(player_team = fix_team_names(player_team)) |>
  mutate(opposition_team = fix_team_names(opposition_team)) |> 
  mutate(match = paste(home_team, away_team, sep = " v "))

betright_player_rebounds_lines_overs <-
  all_props |> 
  filter(str_detect(event_name, "^Player Rebounds Over\\/Under.*")) |>
  filter(str_detect(outcome_name, "Over")) |>
  separate(event_name, into = c("market_name", "player_name"), sep = " - ") |>
  mutate(player_name = str_remove_all(player_name, " \\(.*\\)")) |> 
  mutate(player_name = str_replace_all(player_name, "  ", " ")) |>
  mutate(player_name = fix_player_names(player_name)) |>
  left_join(player_names[, c("player_full_name", "team_name")], by = c("player_name" = "player_full_name")) |>
  rename(player_team = team_name) |>
  separate(match, into = c("away_team", "home_team"), sep = " @ ", remove = FALSE) |>
  mutate(opposition_team = if_else(home_team == player_team, away_team, home_team)) |>
  mutate(agency = "BetRight") |>
  mutate(line = str_extract(outcome_name, "\\d+\\.?\\d*")) |>
  mutate(line = as.numeric(line)) |>
  mutate(market_name = "Player Rebounds") |> 
  select(
    "match",
    "home_team",
    "away_team",
    "market_name",
    "player_name",
    "player_team",
    "line",
    "over_price" = "price",
    "agency",
    "event_id",
    "outcome_name",
    "outcome_id",
    "fixed_market_id",
    "opposition_team"
  ) |>
  mutate(home_team = fix_team_names(home_team)) |>
  mutate(away_team = fix_team_names(away_team)) |>
  mutate(player_team = fix_team_names(player_team)) |>
  mutate(opposition_team = fix_team_names(opposition_team)) |>
  mutate(match = paste(home_team, away_team, sep = " v "))

betright_player_rebounds_lines_unders <-
  all_props |> 
  filter(str_detect(event_name, "^Player Rebounds Over\\/Under.*")) |>
  filter(str_detect(outcome_name, "Under")) |>
  separate(event_name, into = c("market_name", "player_name"), sep = " - ") |>
  mutate(player_name = str_remove_all(player_name, " \\(.*\\)")) |> 
  mutate(player_name = str_replace_all(player_name, "  ", " ")) |>
  mutate(player_name = fix_player_names(player_name)) |>
  left_join(player_names[, c("player_full_name", "team_name")], by = c("player_name" = "player_full_name")) |>
  rename(player_team = team_name) |>
  separate(match, into = c("away_team", "home_team"), sep = " @ ", remove = FALSE) |>
  mutate(opposition_team = if_else(home_team == player_team, away_team, home_team)) |>
  mutate(agency = "BetRight") |>
  mutate(line = str_extract(outcome_name, "\\d+\\.?\\d*")) |>
  mutate(line = as.numeric(line)) |>
  mutate(market_name = "Player Rebounds") |> 
  select(
    "match",
    "home_team",
    "away_team",
    "market_name",
    "player_name",
    "player_team",
    "line",
    "under_price" = "price",
    "agency",
    "event_id",
    "outcome_name_unders" = "outcome_name",
    "outcome_id_unders" = "outcome_id",
    "fixed_market_id_unders" = "fixed_market_id",
    "opposition_team"
  ) |>
  mutate(home_team = fix_team_names(home_team)) |>
  mutate(away_team = fix_team_names(away_team)) |>
  mutate(player_team = fix_team_names(player_team)) |>
  mutate(opposition_team = fix_team_names(opposition_team)) |>
  mutate(match = paste(home_team, away_team, sep = " v "))

betright_player_rebounds <-
  bind_rows(betright_player_rebounds_alternate,
            betright_player_rebounds_lines_overs) |> 
  full_join(betright_player_rebounds_lines_unders)

#===============================================================================
# Player Threes
#===============================================================================

betright_player_three_pointers_alternate <-
  all_props |> 
  filter(str_detect(event_name, "^Player Three Pointers -")) |>
  separate(event_name, into = c("market_name", "player_name"), sep = " - ") |>
  mutate(player_name = str_remove_all(player_name, " \\(.*\\)")) |>
  mutate(player_name = str_replace_all(player_name, "  ", " ")) |>  
  mutate(player_name = fix_player_names(player_name)) |>
  left_join(player_names[, c("player_full_name", "team_name")], by = c("player_name" = "player_full_name")) |>
  rename(player_team = team_name) |>
  separate(match, into = c("away_team", "home_team"), sep = " @ ", remove = FALSE) |> 
  mutate(opposition_team = if_else(home_team == player_team, away_team, home_team)) |>
  mutate(agency = "BetRight") |>
  mutate(line = str_extract(outcome_name, "\\d+\\.?\\d*")) |>
  mutate(line = as.numeric(line) - 0.5) |>
  mutate(market_name = "Player Threes") |> 
  select(
    "match",
    "home_team",
    "away_team",
    "market_name",
    "player_name",
    "player_team",
    "line",
    "over_price" = "price",
    "agency",
    "event_id",
    "outcome_name",
    "outcome_id",
    "fixed_market_id",
    "opposition_team"
  ) |> 
  mutate(home_team = fix_team_names(home_team)) |>
  mutate(away_team = fix_team_names(away_team)) |>
  mutate(player_team = fix_team_names(player_team)) |>
  mutate(opposition_team = fix_team_names(opposition_team)) |> 
  mutate(match = paste(home_team, away_team, sep = " v "))

betright_player_three_pointers_lines_overs <-
  all_props |> 
  filter(str_detect(event_name, "^Player Three Pointers Over\\/Under.*")) |>
  filter(str_detect(outcome_name, "Over")) |>
  separate(event_name, into = c("market_name", "player_name"), sep = " - ") |>
  mutate(player_name = str_remove_all(player_name, " \\(.*\\)")) |> 
  mutate(player_name = str_replace_all(player_name, "  ", " ")) |>
  mutate(player_name = fix_player_names(player_name)) |>
  left_join(player_names[, c("player_full_name", "team_name")], by = c("player_name" = "player_full_name")) |>
  rename(player_team = team_name) |>
  separate(match, into = c("away_team", "home_team"), sep = " @ ", remove = FALSE) |>
  mutate(opposition_team = if_else(home_team == player_team, away_team, home_team)) |>
  mutate(agency = "BetRight") |>
  mutate(line = str_extract(outcome_name, "\\d+\\.?\\d*")) |>
  mutate(line = as.numeric(line)) |>
  mutate(market_name = "Player Threes") |> 
  select(
    "match",
    "home_team",
    "away_team",
    "market_name",
    "player_name",
    "player_team",
    "line",
    "over_price" = "price",
    "agency",
    "event_id",
    "outcome_name",
    "outcome_id",
    "fixed_market_id",
    "opposition_team"
  ) |>
  mutate(home_team = fix_team_names(home_team)) |>
  mutate(away_team = fix_team_names(away_team)) |>
  mutate(player_team = fix_team_names(player_team)) |>
  mutate(opposition_team = fix_team_names(opposition_team)) |>
  mutate(match = paste(home_team, away_team, sep = " v "))

betright_player_three_pointers_lines_unders <-
  all_props |> 
  filter(str_detect(event_name, "^Player Three Pointers Over\\/Under.*")) |>
  filter(str_detect(outcome_name, "Under")) |>
  separate(event_name, into = c("market_name", "player_name"), sep = " - ") |>
  mutate(player_name = str_remove_all(player_name, " \\(.*\\)")) |> 
  mutate(player_name = str_replace_all(player_name, "  ", " ")) |>
  mutate(player_name = fix_player_names(player_name)) |>
  left_join(player_names[, c("player_full_name", "team_name")], by = c("player_name" = "player_full_name")) |>
  rename(player_team = team_name) |>
  separate(match, into = c("away_team", "home_team"), sep = " @ ", remove = FALSE) |>
  mutate(opposition_team = if_else(home_team == player_team, away_team, home_team)) |>
  mutate(agency = "BetRight") |>
  mutate(line = str_extract(outcome_name, "\\d+\\.?\\d*")) |>
  mutate(line = as.numeric(line)) |>
  mutate(market_name = "Player Threes") |> 
  select(
    "match",
    "home_team",
    "away_team",
    "market_name",
    "player_name",
    "player_team",
    "line",
    "under_price" = "price",
    "agency",
    "event_id",
    "outcome_name_unders" = "outcome_name",
    "outcome_id_unders" = "outcome_id",
    "fixed_market_id_unders" = "fixed_market_id",
    "opposition_team"
  ) |>
  mutate(home_team = fix_team_names(home_team)) |>
  mutate(away_team = fix_team_names(away_team)) |>
  mutate(player_team = fix_team_names(player_team)) |>
  mutate(opposition_team = fix_team_names(opposition_team)) |>
  mutate(match = paste(home_team, away_team, sep = " v "))

betright_player_three_pointers <-
  bind_rows(betright_player_three_pointers_alternate,
            betright_player_three_pointers_lines_overs) |> 
  full_join(betright_player_three_pointers_lines_unders)

#===============================================================================
# Player Blocks
#===============================================================================

betright_player_blocks_alternate <-
  all_props |> 
  filter(str_detect(event_name, "^Player Blocks -")) |>
  separate(event_name, into = c("market_name", "player_name"), sep = " - ") |>
  mutate(player_name = str_remove_all(player_name, " \\(.*\\)")) |>
  mutate(player_name = str_replace_all(player_name, "  ", " ")) |>  
  mutate(player_name = fix_player_names(player_name)) |>
  left_join(player_names[, c("player_full_name", "team_name")], by = c("player_name" = "player_full_name")) |>
  rename(player_team = team_name) |>
  separate(match, into = c("away_team", "home_team"), sep = " @ ", remove = FALSE) |> 
  mutate(opposition_team = if_else(home_team == player_team, away_team, home_team)) |>
  mutate(agency = "BetRight") |>
  mutate(line = str_extract(outcome_name, "\\d+\\.?\\d*")) |>
  mutate(line = as.numeric(line) - 0.5) |>
  select(
    "match",
    "home_team",
    "away_team",
    "market_name",
    "player_name",
    "player_team",
    "line",
    "over_price" = "price",
    "agency",
    "event_id",
    "outcome_name",
    "outcome_id",
    "fixed_market_id",
    "opposition_team"
  ) |> 
  mutate(home_team = fix_team_names(home_team)) |>
  mutate(away_team = fix_team_names(away_team)) |>
  mutate(player_team = fix_team_names(player_team)) |>
  mutate(opposition_team = fix_team_names(opposition_team)) |> 
  mutate(match = paste(home_team, away_team, sep = " v "))

betright_player_blocks_lines_overs <-
  all_props |> 
  filter(str_detect(event_name, "^Player Blocks Over\\/Under.*")) |>
  filter(str_detect(outcome_name, "Over")) |>
  separate(event_name, into = c("market_name", "player_name"), sep = " - ") |>
  mutate(player_name = str_remove_all(player_name, " \\(.*\\)")) |> 
  mutate(player_name = str_replace_all(player_name, "  ", " ")) |>
  mutate(player_name = fix_player_names(player_name)) |>
  left_join(player_names[, c("player_full_name", "team_name")], by = c("player_name" = "player_full_name")) |>
  rename(player_team = team_name) |>
  separate(match, into = c("away_team", "home_team"), sep = " @ ", remove = FALSE) |>
  mutate(opposition_team = if_else(home_team == player_team, away_team, home_team)) |>
  mutate(agency = "BetRight") |>
  mutate(line = str_extract(outcome_name, "\\d+\\.?\\d*")) |>
  mutate(line = as.numeric(line)) |>
  mutate(market_name = "Player Blocks") |> 
  select(
    "match",
    "home_team",
    "away_team",
    "market_name",
    "player_name",
    "player_team",
    "line",
    "over_price" = "price",
    "agency",
    "event_id",
    "outcome_name",
    "outcome_id",
    "fixed_market_id",
    "opposition_team"
  ) |>
  mutate(home_team = fix_team_names(home_team)) |>
  mutate(away_team = fix_team_names(away_team)) |>
  mutate(player_team = fix_team_names(player_team)) |>
  mutate(opposition_team = fix_team_names(opposition_team)) |>
  mutate(match = paste(home_team, away_team, sep = " v "))

betright_player_blocks_lines_unders <-
  all_props |> 
  filter(str_detect(event_name, "^Player Blocks Over\\/Under.*")) |>
  filter(str_detect(outcome_name, "Under")) |>
  separate(event_name, into = c("market_name", "player_name"), sep = " - ") |>
  mutate(player_name = str_remove_all(player_name, " \\(.*\\)")) |> 
  mutate(player_name = str_replace_all(player_name, "  ", " ")) |>
  mutate(player_name = fix_player_names(player_name)) |>
  left_join(player_names[, c("player_full_name", "team_name")], by = c("player_name" = "player_full_name")) |>
  rename(player_team = team_name) |>
  separate(match, into = c("away_team", "home_team"), sep = " @ ", remove = FALSE) |>
  mutate(opposition_team = if_else(home_team == player_team, away_team, home_team)) |>
  mutate(agency = "BetRight") |>
  mutate(line = str_extract(outcome_name, "\\d+\\.?\\d*")) |>
  mutate(line = as.numeric(line)) |>
  mutate(market_name = "Player Blocks") |> 
  select(
    "match",
    "home_team",
    "away_team",
    "market_name",
    "player_name",
    "player_team",
    "line",
    "under_price" = "price",
    "agency",
    "event_id",
    "outcome_name_unders" = "outcome_name",
    "outcome_id_unders" = "outcome_id",
    "fixed_market_id_unders" = "fixed_market_id",
    "opposition_team"
  ) |>
  mutate(home_team = fix_team_names(home_team)) |>
  mutate(away_team = fix_team_names(away_team)) |>
  mutate(player_team = fix_team_names(player_team)) |>
  mutate(opposition_team = fix_team_names(opposition_team)) |>
  mutate(match = paste(home_team, away_team, sep = " v "))

betright_player_blocks <-
  bind_rows(betright_player_blocks_alternate,
            betright_player_blocks_lines_overs) |> 
  full_join(betright_player_blocks_lines_unders)

#===============================================================================
# Player Steals
#===============================================================================

betright_player_steals_alternate <-
  all_props |> 
  filter(str_detect(event_name, "^Player Steals -")) |>
  separate(event_name, into = c("market_name", "player_name"), sep = " - ") |>
  mutate(player_name = str_remove_all(player_name, " \\(.*\\)")) |>
  mutate(player_name = str_replace_all(player_name, "  ", " ")) |>  
  mutate(player_name = fix_player_names(player_name)) |>
  left_join(player_names[, c("player_full_name", "team_name")], by = c("player_name" = "player_full_name")) |>
  rename(player_team = team_name) |>
  separate(match, into = c("away_team", "home_team"), sep = " @ ", remove = FALSE) |> 
  mutate(opposition_team = if_else(home_team == player_team, away_team, home_team)) |>
  mutate(agency = "BetRight") |>
  mutate(line = str_extract(outcome_name, "\\d+\\.?\\d*")) |>
  mutate(line = as.numeric(line) - 0.5) |>
  select(
    "match",
    "home_team",
    "away_team",
    "market_name",
    "player_name",
    "player_team",
    "line",
    "over_price" = "price",
    "agency",
    "event_id",
    "outcome_name",
    "outcome_id",
    "fixed_market_id",
    "opposition_team"
  ) |> 
  mutate(home_team = fix_team_names(home_team)) |>
  mutate(away_team = fix_team_names(away_team)) |>
  mutate(player_team = fix_team_names(player_team)) |>
  mutate(opposition_team = fix_team_names(opposition_team)) |> 
  mutate(match = paste(home_team, away_team, sep = " v "))

betright_player_steals_lines_overs <-
  all_props |> 
  filter(str_detect(event_name, "^Player Steals Over\\/Under.*")) |>
  filter(str_detect(outcome_name, "Over")) |>
  separate(event_name, into = c("market_name", "player_name"), sep = " - ") |>
  mutate(player_name = str_remove_all(player_name, " \\(.*\\)")) |> 
  mutate(player_name = str_replace_all(player_name, "  ", " ")) |>
  mutate(player_name = fix_player_names(player_name)) |>
  left_join(player_names[, c("player_full_name", "team_name")], by = c("player_name" = "player_full_name")) |>
  rename(player_team = team_name) |>
  separate(match, into = c("away_team", "home_team"), sep = " @ ", remove = FALSE) |>
  mutate(opposition_team = if_else(home_team == player_team, away_team, home_team)) |>
  mutate(agency = "BetRight") |>
  mutate(line = str_extract(outcome_name, "\\d+\\.?\\d*")) |>
  mutate(line = as.numeric(line)) |>
  mutate(market_name = "Player Steals") |> 
  select(
    "match",
    "home_team",
    "away_team",
    "market_name",
    "player_name",
    "player_team",
    "line",
    "over_price" = "price",
    "agency",
    "event_id",
    "outcome_name",
    "outcome_id",
    "fixed_market_id",
    "opposition_team"
  ) |>
  mutate(home_team = fix_team_names(home_team)) |>
  mutate(away_team = fix_team_names(away_team)) |>
  mutate(player_team = fix_team_names(player_team)) |>
  mutate(opposition_team = fix_team_names(opposition_team)) |>
  mutate(match = paste(home_team, away_team, sep = " v "))

betright_player_steals_lines_unders <-
  all_props |> 
  filter(str_detect(event_name, "^Player Steals Over\\/Under.*")) |>
  filter(str_detect(outcome_name, "Under")) |>
  separate(event_name, into = c("market_name", "player_name"), sep = " - ") |>
  mutate(player_name = str_remove_all(player_name, " \\(.*\\)")) |> 
  mutate(player_name = str_replace_all(player_name, "  ", " ")) |>
  mutate(player_name = fix_player_names(player_name)) |>
  left_join(player_names[, c("player_full_name", "team_name")], by = c("player_name" = "player_full_name")) |>
  rename(player_team = team_name) |>
  separate(match, into = c("away_team", "home_team"), sep = " @ ", remove = FALSE) |>
  mutate(opposition_team = if_else(home_team == player_team, away_team, home_team)) |>
  mutate(agency = "BetRight") |>
  mutate(line = str_extract(outcome_name, "\\d+\\.?\\d*")) |>
  mutate(line = as.numeric(line)) |>
  mutate(market_name = "Player Steals") |> 
  select(
    "match",
    "home_team",
    "away_team",
    "market_name",
    "player_name",
    "player_team",
    "line",
    "under_price" = "price",
    "agency",
    "event_id",
    "outcome_name_unders" = "outcome_name",
    "outcome_id_unders" = "outcome_id",
    "fixed_market_id_unders" = "fixed_market_id",
    "opposition_team"
  ) |>
  mutate(home_team = fix_team_names(home_team)) |>
  mutate(away_team = fix_team_names(away_team)) |>
  mutate(player_team = fix_team_names(player_team)) |>
  mutate(opposition_team = fix_team_names(opposition_team)) |>
  mutate(match = paste(home_team, away_team, sep = " v "))

betright_player_steals <-
  bind_rows(betright_player_steals_alternate,
            betright_player_steals_lines_overs) |> 
  full_join(betright_player_steals_lines_unders)

#===============================================================================
# Player PRAs
#===============================================================================

betright_player_pras_alternate <-
  all_props |> 
  filter(str_detect(event_name, "^Player Points \\& Assists \\& Rebounds -")) |>
  separate(event_name, into = c("market_name", "player_name"), sep = " - ") |>
  mutate(player_name = str_remove_all(player_name, " \\(.*\\)")) |>
  mutate(player_name = str_replace_all(player_name, "  ", " ")) |>  
  left_join(player_names[, c("player_full_name", "team_name")], by = c("player_name" = "player_full_name")) |>
  rename(player_team = team_name) |>
  separate(match, into = c("away_team", "home_team"), sep = " @ ", remove = FALSE) |> 
  mutate(opposition_team = if_else(home_team == player_team, away_team, home_team)) |>
  mutate(agency = "BetRight") |>
  mutate(line = str_extract(outcome_name, "\\d+\\.?\\d*")) |>
  mutate(line = as.numeric(line) - 0.5) |>
  mutate(market_name = "Player PRAs") |> 
  select(
    "match",
    "home_team",
    "away_team",
    "market_name",
    "player_name",
    "player_team",
    "line",
    "over_price" = "price",
    "agency",
    "event_id",
    "outcome_name",
    "outcome_id",
    "fixed_market_id",
    "opposition_team"
  ) |> 
  mutate(home_team = fix_team_names(home_team)) |>
  mutate(away_team = fix_team_names(away_team)) |>
  mutate(player_team = fix_team_names(player_team)) |>
  mutate(opposition_team = fix_team_names(opposition_team)) |> 
  mutate(match = paste(home_team, away_team, sep = " v "))

betright_player_pras_lines_overs <-
  all_props |> 
  filter(str_detect(event_name, "^Player Points \\& Assists \\& Rebounds Over\\/Under.*")) |>
  filter(str_detect(outcome_name, "Over")) |>
  separate(event_name, into = c("market_name", "player_name"), sep = " - ") |>
  mutate(player_name = str_remove_all(player_name, " \\(.*\\)")) |> 
  mutate(player_name = str_replace_all(player_name, "  ", " ")) |>
  mutate(player_name = fix_player_names(player_name)) |>
  left_join(player_names[, c("player_full_name", "team_name")], by = c("player_name" = "player_full_name")) |>
  rename(player_team = team_name) |>
  separate(match, into = c("away_team", "home_team"), sep = " @ ", remove = FALSE) |>
  mutate(opposition_team = if_else(home_team == player_team, away_team, home_team)) |>
  mutate(agency = "BetRight") |>
  mutate(line = str_extract(outcome_name, "\\d+\\.?\\d*")) |>
  mutate(line = as.numeric(line)) |>
  mutate(market_name = "Player PRAs") |> 
  select(
    "match",
    "home_team",
    "away_team",
    "market_name",
    "player_name",
    "player_team",
    "line",
    "over_price" = "price",
    "agency",
    "event_id",
    "outcome_name",
    "outcome_id",
    "fixed_market_id",
    "opposition_team"
  ) |>
  mutate(home_team = fix_team_names(home_team)) |>
  mutate(away_team = fix_team_names(away_team)) |>
  mutate(player_team = fix_team_names(player_team)) |>
  mutate(opposition_team = fix_team_names(opposition_team)) |>
  mutate(match = paste(home_team, away_team, sep = " v "))

betright_player_pras_lines_unders <-
  all_props |> 
  filter(str_detect(event_name, "^Player Points \\& Assists \\& Rebounds Over\\/Under.*")) |>
  filter(str_detect(outcome_name, "Under")) |>
  separate(event_name, into = c("market_name", "player_name"), sep = " - ") |>
  mutate(player_name = str_remove_all(player_name, " \\(.*\\)")) |> 
  mutate(player_name = str_replace_all(player_name, "  ", " ")) |>
  mutate(player_name = fix_player_names(player_name)) |>
  left_join(player_names[, c("player_full_name", "team_name")], by = c("player_name" = "player_full_name")) |>
  rename(player_team = team_name) |>
  separate(match, into = c("away_team", "home_team"), sep = " @ ", remove = FALSE) |>
  mutate(opposition_team = if_else(home_team == player_team, away_team, home_team)) |>
  mutate(agency = "BetRight") |>
  mutate(line = str_extract(outcome_name, "\\d+\\.?\\d*")) |>
  mutate(line = as.numeric(line)) |>
  mutate(market_name = "Player PRAs") |> 
  select(
    "match",
    "home_team",
    "away_team",
    "market_name",
    "player_name",
    "player_team",
    "line",
    "under_price" = "price",
    "agency",
    "event_id",
    "outcome_name_unders" = "outcome_name",
    "outcome_id_unders" = "outcome_id",
    "fixed_market_id_unders" = "fixed_market_id",
    "opposition_team"
  ) |>
  mutate(home_team = fix_team_names(home_team)) |>
  mutate(away_team = fix_team_names(away_team)) |>
  mutate(player_team = fix_team_names(player_team)) |>
  mutate(opposition_team = fix_team_names(opposition_team)) |>
  mutate(match = paste(home_team, away_team, sep = " v "))

betright_player_pras <-
  bind_rows(betright_player_pras_alternate,
            betright_player_pras_lines_overs) |> 
  full_join(betright_player_pras_lines_unders)

#===============================================================================
# Write to CSV
#===============================================================================

betright_player_points |> write_csv("Data/scraped_odds/betright_player_points.csv")
betright_player_assists |> write_csv("Data/scraped_odds/betright_player_assists.csv")
betright_player_rebounds |> write_csv("Data/scraped_odds/betright_player_rebounds.csv")
betright_player_three_pointers |> write_csv("Data/scraped_odds/betright_player_threes.csv")
betright_player_blocks |> write_csv("Data/scraped_odds/betright_player_blocks.csv")
betright_player_steals |> write_csv("Data/scraped_odds/betright_player_steals.csv")
betright_player_pras |> write_csv("Data/scraped_odds/betright_player_pras.csv")
