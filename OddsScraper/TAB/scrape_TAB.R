# Libraries
library(tidyverse)
library(rvest)
library(httr2)
library(jsonlite)

# Fix team names and players functions
source("Scripts/fix_team_names.R")
source("Scripts/fix_player_names.R")

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
  mutate(join_name = ifelse(player_full_name == "Jabari Walker", "Jab Walker", join_name)) |> 
  mutate(join_name = ifelse(player_full_name == "Miles Bridges", "Mil Bridges", join_name)) |> 
  mutate(join_name = ifelse(player_full_name == "Jaylin Williams", "Jay Williams", join_name)) |> 
  mutate(join_name = ifelse(player_full_name == "Bogdan Bogdanović", "Bog Bogdanovic", join_name)) |>
  mutate(join_name = ifelse(player_full_name == "Bojan Bogdanović", "Boj Bogdanovic", join_name)) |>
  mutate(join_name = ifelse(player_full_name == "Stephen Curry", "St Curry", join_name)) |> 
  mutate(join_name = ifelse(player_full_name == "James Johnson", "Jam Johnson", join_name))

standardize_to_initials <- function(player_names) {
  trimmed_names <- str_trim(player_names)
  
  case_when(
    # Check if already in initial format (1-2 letters + space + lastname) - case insensitive
    str_detect(trimmed_names, "^[A-Za-z]{1,2}\\s+[A-Za-z]+") ~ trimmed_names,
    
    # Convert full names to initial + lastname format (3+ letters + space + lastname)
    str_detect(trimmed_names, "^[A-Za-z]{3,}\\s+[A-Za-z]+") ~ {
      first_initial <- str_extract(trimmed_names, "^[A-Za-z]")
      last_name <- str_extract(trimmed_names, "\\s+([A-Za-z]+)$") |> str_trim()
      paste(first_initial, last_name)
    },
    
    # Fallback for any other format
    .default = trimmed_names
  )
}

main_tab <- function() {
# Get response body
tab_response <- fromJSON("OddsScraper/TAB/tab_response.json")

# Function to extract market info from response---------------------------------
get_market_info <- function(markets) {
    
    # Market info
    markets_name = markets$betOption
    market_propositions = markets$propositions
    
    # Output Tibble
    tibble(market = markets_name,
           propositions = market_propositions)
}

# Function to extract match info from response----------------------------------
get_match_info <- function(matches) {
    # Match info
    match_name = matches$name
    match_round = matches$round
    match_start_time = matches$startTime
    
    # Market info
    market_info = map(matches$markets, get_market_info) |> bind_rows()
    
    # Output Tibble
    tibble(
        match = match_name,
        round = match_round,
        start_time = match_start_time,
        market_name = market_info$market,
        propositions = market_info$propositions
    )
}

# List of matches
matches <- map(1:nrow(tab_response$matches), ~ tab_response$matches[., ])
    
# Map functions to data
all_tab_markets <-
    map(matches, get_match_info) |> bind_rows()

# Expand list col into multiple cols
all_tab_markets <-
  all_tab_markets |>
  unnest(cols = c(propositions)) |> 
  select(any_of(c("match",
                  "round",
                  "start_time",
                  "market_name")),
         prop_id = id,
         prop_name = name,
         price = returnWin)

#===============================================================================
# Head to head markets
#===============================================================================

# Home teams
home_teams <-
    all_tab_markets |>
    separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
    filter(market_name == "Head To Head") |> 
    group_by(match) |> 
    filter(row_number() == 1) |> 
    rename(home_win = price) |> 
    select(-prop_name, -prop_id)

# Away teams
away_teams <-
    all_tab_markets |>
  separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
    filter(market_name == "Head To Head") |> 
    group_by(match) |> 
    filter(row_number() == 2) |> 
    rename(away_win = price) |> 
    select(-prop_name, -prop_id)

# Combine
tab_head_to_head_markets <-
    home_teams |>
    left_join(away_teams) |> 
    select(match, start_time, market_name, home_team, home_win, away_team, away_win) |> 
    mutate(margin = round((1/home_win + 1/away_win), digits = 3)) |> 
    mutate(agency = "TAB")

# Fix team names
tab_head_to_head_markets <-
    tab_head_to_head_markets |> 
    mutate(home_team = fix_team_names(home_team)) |>
    mutate(away_team = fix_team_names(away_team)) |>
    mutate(match = paste(home_team, "v", away_team))

# Write to csv
write_csv(tab_head_to_head_markets, "Data/scraped_odds/tab_h2h.csv")

#===============================================================================
# Total line markets
#===============================================================================

# Under lines
under_lines <-
    all_tab_markets |>
    filter(market_name == "Total Points Over/Under") |> 
    filter(str_detect(prop_name, "Under")) |> 
    mutate(total_points_line = as.numeric(str_extract(prop_name, "\\d+\\.\\d+"))) |>
    select(match, start_time, market_name, total_points_line, under_price = price, under_prop_id = prop_id)

# Over lines
over_lines <-
    all_tab_markets |>
    filter(market_name == "Total Points Over/Under") |> 
    filter(str_detect(prop_name, "Over")) |> 
    mutate(total_points_line = as.numeric(str_extract(prop_name, "\\d+\\.\\d+"))) |>
    select(match, start_time, market_name, total_points_line, over_price = price, prop_id)

# Combine
tab_total_line_markets <-
    under_lines |>
    left_join(over_lines) |> 
    select(match, start_time, market_name, total_points_line, under_price, over_price) |> 
    mutate(margin = round((1/under_price + 1/over_price), digits = 3)) |> 
    mutate(agency = "TAB")

# Fix team names
tab_total_line_markets <-
    tab_total_line_markets |> 
    separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
    mutate(match = paste(home_team, "v", away_team))

# Write to csv
write_csv(tab_total_line_markets, "Data/scraped_odds/tab_total_points.csv")

#===============================================================================
# Player Points
#===============================================================================

# Filter to player points markets
player_points_markets <-
    all_tab_markets |> 
    filter(str_detect(market_name, "Player Points$"))

# Alternate Player Points
alternate_player_points_markets <-
    all_tab_markets |> 
    filter(str_detect(market_name, "\\d+\\+ Points$"))

# Extract player names
player_points_markets <-
    player_points_markets |> 
    filter(str_detect(prop_name, "Over|Under")) |>
    mutate(player_name = str_extract(prop_name, "^.*(?=\\s(\\d+))")) |> 
    mutate(player_name = str_remove_all(player_name, "( Over)|( Under)")) |> 
    mutate(line = str_extract(prop_name, "[0-9\\.]{1,4}")) |> 
    mutate(line = as.numeric(line)) |>
    mutate(type = str_detect(prop_name, "Over|\\+")) |> 
    mutate(type = ifelse(type, "Over", "Under")) |>
    mutate(player_name = standardize_to_initials(player_name)) |>
    mutate(player_name = fix_player_names_tab_initials(player_name)) |>
    left_join(player_names[, c("player_full_name", "join_name")], by = c("player_name" = "join_name")) |> 
    select(-player_name) |>
    rename(player_name = player_full_name)

alternate_player_points_markets <-
    alternate_player_points_markets |>
    mutate(player_name = str_remove(prop_name, " \\(.*\\)")) |>
    mutate(line = str_extract(market_name, "\\d+")) |> 
    mutate(line = as.numeric(line) - 0.5) |> 
    transmute(match, market_name = "Player Points", player_name, line, over_price = price, prop_id) |> 
    mutate(player_name = fix_player_names(player_name))

# Over lines
over_lines <-
    player_points_markets |> 
    filter(type == "Over") |> 
    mutate(market_name = "Player Points") |>
    select(match, market_name, player_name, line, over_price = price, prop_id) |> 
    bind_rows(alternate_player_points_markets)

# Under lines
under_lines <-
    player_points_markets |> 
    filter(type == "Under") |> 
    mutate(market_name = "Player Points") |>
    select(match, market_name, player_name, line, under_price = price, under_prop_id = prop_id)

# Combine
tab_player_points_markets <-
    over_lines |>
    full_join(under_lines) |> 
    select(match, market_name, player_name, line, over_price, under_price, prop_id, under_prop_id) |> 
    mutate(agency = "TAB")

# Fix team names
tab_player_points_markets <-
    tab_player_points_markets |> 
    separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
    mutate(match = paste(home_team, "v", away_team)) |> 
  left_join(player_names_all[, c("player_full_name", "team_name")], by = c("player_name" = "player_full_name"))

#===============================================================================
# Player Assists
#===============================================================================

# Filter to player assists markets
player_assists_markets <-
    all_tab_markets |> 
    filter(str_detect(market_name, "Player Assists$"))

# Alternate Player Assists
alternate_player_assists_markets <-
    all_tab_markets |> 
    filter(str_detect(market_name, "\\d+\\+ Assists$"))

# Extract player names
player_assists_markets <-
    player_assists_markets |> 
    filter(str_detect(prop_name, "Over|Under")) |>
    mutate(player_name = str_extract(prop_name, "^.*(?=\\s(\\d+))")) |> 
    mutate(player_name = str_remove_all(player_name, "( Over)|( Under)")) |> 
    mutate(line = str_extract(prop_name, "[0-9\\.]{1,4}")) |> 
    mutate(line = as.numeric(line)) |>
    mutate(type = str_detect(prop_name, "Over|\\+")) |> 
    mutate(type = ifelse(type, "Over", "Under")) |> 
    mutate(line = if_else(market_name == "Alternate Player Assists", line - 0.5, line)) |>
    mutate(player_name = standardize_to_initials(player_name)) |>
    mutate(player_name = fix_player_names_tab_initials(player_name)) |>
    left_join(player_names[, c("player_full_name", "join_name")], by = c("player_name" = "join_name")) |> 
    select(-player_name) |> 
    rename(player_name = player_full_name)

alternate_player_assists_markets <-
    alternate_player_assists_markets |>
    mutate(player_name = str_remove(prop_name, " \\(.*\\)")) |>
    mutate(line = str_extract(market_name, "\\d+")) |> 
    mutate(line = as.numeric(line) - 0.5) |> 
    transmute(match, market_name = "Player Assists", player_name, line, over_price = price, prop_id) |>
    mutate(player_name = fix_player_names(player_name))

# Over lines
over_lines <-
    player_assists_markets |> 
    filter(type == "Over") |> 
    mutate(market_name = "Player Assists") |>
    select(match, market_name, player_name, line, over_price = price, prop_id) |> 
    bind_rows(alternate_player_assists_markets)

# Under lines
under_lines <-
    player_assists_markets |> 
    filter(type == "Under") |> 
    mutate(market_name = "Player Assists") |>
    select(match, market_name, player_name, line, under_price = price, under_prop_id = prop_id)

# Combine
tab_player_assists_markets <-
    over_lines |>
    full_join(under_lines) |> 
    select(match, market_name, player_name, line, over_price, under_price, prop_id, under_prop_id) |> 
    mutate(agency = "TAB")

# Fix team names
tab_player_assists_markets <-
    tab_player_assists_markets |> 
    separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
    mutate(match = paste(home_team, "v", away_team)) |> 
  left_join(player_names_all[, c("player_full_name", "team_name")], by = c("player_name" = "player_full_name"))

#===============================================================================
# Player Rebounds
#===============================================================================

# Filter to player rebounds markets
player_rebounds_markets <-
    all_tab_markets |> 
    filter(str_detect(market_name, "Player Rebounds$"))

# Alternate Player Rebounds
alternate_player_rebounds_markets <-
    all_tab_markets |> 
    filter(str_detect(market_name, "\\d+\\+ Rebounds$"))

# Extract player names
player_rebounds_markets <-
    player_rebounds_markets |> 
    filter(str_detect(prop_name, "Over|Under")) |>
    mutate(player_name = str_extract(prop_name, "^.*(?=\\s(\\d+))")) |> 
    mutate(player_name = str_remove_all(player_name, "( Over)|( Under)")) |> 
    mutate(line = str_extract(prop_name, "[0-9\\.]{1,4}")) |> 
    mutate(line = as.numeric(line)) |>
    mutate(type = str_detect(prop_name, "Over|\\+")) |> 
    mutate(type = ifelse(type, "Over", "Under")) |> 
    mutate(line = if_else(market_name == "Alternate Player Rebounds", line - 0.5, line)) |>
    mutate(player_name = standardize_to_initials(player_name)) |>
    mutate(player_name = fix_player_names_tab_initials(player_name)) |>
    left_join(player_names[, c("player_full_name", "join_name")], by = c("player_name" = "join_name")) |> 
    select(-player_name) |> 
    rename(player_name = player_full_name)

alternate_player_rebounds_markets <-
    alternate_player_rebounds_markets |>
    mutate(player_name = str_remove(prop_name, " \\(.*\\)")) |>
    mutate(line = str_extract(market_name, "\\d+")) |>
    mutate(line = as.numeric(line) - 0.5) |>
    transmute(match, market_name = "Player Rebounds", player_name, line, over_price = price, prop_id) |>
    mutate(player_name = fix_player_names(player_name))

# Over lines
over_lines <-
    player_rebounds_markets |> 
    filter(type == "Over") |> 
    mutate(market_name = "Player Rebounds") |>
    select(match, market_name, player_name, line, over_price = price, prop_id) |> 
    bind_rows(alternate_player_rebounds_markets)

# Under lines
under_lines <-
    player_rebounds_markets |> 
    filter(type == "Under") |> 
    mutate(market_name = "Player Rebounds") |>
    select(match, market_name, player_name, line, under_price = price, under_prop_id = prop_id)

# Combine
tab_player_rebounds_markets <-
    over_lines |>
    full_join(under_lines) |> 
    select(match, market_name, player_name, line, over_price, under_price, prop_id, under_prop_id) |> 
    mutate(agency = "TAB")

# Fix team names
tab_player_rebounds_markets <-
    tab_player_rebounds_markets |> 
    separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
    mutate(match = paste(home_team, "v", away_team)) |> 
  left_join(player_names_all[, c("player_full_name", "team_name")], by = c("player_name" = "player_full_name"))

#===============================================================================
# Player PRAs
#===============================================================================

# Filter to player PRAs markets
player_pras_markets <-
  all_tab_markets |> 
  filter(str_detect(market_name, "Points\\+Rebounds\\+Assists"))

# Alternate Player Rebounds
alternate_player_pras_markets <-
  all_tab_markets |> 
  filter(str_detect(market_name, "PRA"))

# Extract player names
player_pras_markets <-
  player_pras_markets |> 
  filter(str_detect(prop_name, "Over|Under")) |>
  mutate(player_name = str_extract(prop_name, "^.*(?=\\s(\\d+))")) |> 
  mutate(player_name = str_remove_all(player_name, "( Over)|( Under)")) |> 
  mutate(line = str_extract(prop_name, "[0-9\\.]{1,4}")) |> 
  mutate(line = as.numeric(line)) |>
  mutate(type = str_detect(prop_name, "Over|\\+")) |> 
  mutate(type = ifelse(type, "Over", "Under")) |>
  mutate(player_name = standardize_to_initials(player_name)) |>
  mutate(player_name = fix_player_names_tab_initials(player_name)) |>
  left_join(player_names[, c("player_full_name", "join_name")], by = c("player_name" = "join_name")) |> 
  select(-player_name) |> 
  rename(player_name = player_full_name)

alternate_player_pras_markets_1 <-
  alternate_player_pras_markets |>
  filter(str_detect(market_name, "\\d+\\+ PRA")) |>
  mutate(player_name = str_remove(prop_name, " \\(.*\\)")) |>
  mutate(line = str_extract(market_name, "\\d+")) |>
  mutate(line = as.numeric(line) - 0.5) |>
  transmute(match, market_name = "Player PRAs", player_name, line, over_price = price, prop_id) |>
  mutate(player_name = fix_player_names(player_name))

alternate_player_pras_markets_2 <-
  alternate_player_pras_markets |>
  filter(str_detect(market_name, "Alternate")) |> 
  mutate(player_name = str_extract(prop_name, "^.*(?=\\s(\\d+))")) |> 
  mutate(line = str_extract(prop_name, "[0-9\\.]{1,4}")) |> 
  mutate(line = as.numeric(line) - 0.5) |>
  transmute(match, market_name = "Player PRAs", player_name, line, over_price = price, prop_id) |>
  mutate(player_name = fix_player_names(player_name))

alternate_player_pras_markets <-
  bind_rows(alternate_player_pras_markets_1, alternate_player_pras_markets_2)

# Over lines
over_lines <-
  player_pras_markets |> 
  filter(type == "Over") |> 
  mutate(market_name = "Player PRAs") |>
  select(match, market_name, player_name, line, over_price = price, prop_id) |> 
  bind_rows(alternate_player_pras_markets)

# Under lines
under_lines <-
  player_pras_markets |> 
  filter(type == "Under") |> 
  mutate(market_name = "Player PRAs") |>
  select(match, market_name, player_name, line, under_price = price, under_prop_id = prop_id)

# Combine
tab_player_pras_markets <-
  over_lines |>
  full_join(under_lines) |> 
  select(match, market_name, player_name, line, over_price, under_price, prop_id, under_prop_id) |> 
  mutate(agency = "TAB")

# Fix team names
tab_player_pras_markets <-
  tab_player_pras_markets |> 
  separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
  mutate(match = paste(home_team, "v", away_team)) |> 
  left_join(player_names_all[, c("player_full_name", "team_name")], by = c("player_name" = "player_full_name"))

#===============================================================================
# Player PAs
#===============================================================================

# Filter to player PAs markets
player_pas_markets <-
  all_tab_markets |> 
  filter(str_detect(market_name, "Points and Assists"))

# Alternate Player Rebounds
alternate_player_pras_markets <-
  all_tab_markets |> 
  filter(str_detect(market_name, "PRA"))

# Extract player names
player_pras_markets <-
  player_pras_markets |> 
  filter(str_detect(prop_name, "Over|Under")) |>
  mutate(player_name = str_extract(prop_name, "^.*(?=\\s(\\d+))")) |> 
  mutate(player_name = str_remove_all(player_name, "( Over)|( Under)")) |> 
  mutate(line = str_extract(prop_name, "[0-9\\.]{1,4}")) |> 
  mutate(line = as.numeric(line)) |>
  mutate(type = str_detect(prop_name, "Over|\\+")) |> 
  mutate(type = ifelse(type, "Over", "Under")) |>
  mutate(player_name = standardize_to_initials(player_name)) |>
  mutate(player_name = fix_player_names_tab_initials(player_name)) |>
  left_join(player_names[, c("player_full_name", "join_name")], by = c("player_name" = "join_name")) |> 
  select(-player_name) |> 
  rename(player_name = player_full_name)

alternate_player_pras_markets_1 <-
  alternate_player_pras_markets |>
  filter(str_detect(market_name, "\\d+\\+ PRA")) |>
  mutate(player_name = str_remove(prop_name, " \\(.*\\)")) |>
  mutate(line = str_extract(market_name, "\\d+")) |>
  mutate(line = as.numeric(line) - 0.5) |>
  transmute(match, market_name = "Player PRAs", player_name, line, over_price = price, prop_id) |>
  mutate(player_name = fix_player_names(player_name))

alternate_player_pras_markets_2 <-
  alternate_player_pras_markets |>
  filter(str_detect(market_name, "Alternate")) |> 
  mutate(player_name = str_extract(prop_name, "^.*(?=\\s(\\d+))")) |> 
  mutate(line = str_extract(prop_name, "[0-9\\.]{1,4}")) |> 
  mutate(line = as.numeric(line) - 0.5) |>
  transmute(match, market_name = "Player PRAs", player_name, line, over_price = price, prop_id) |>
  mutate(player_name = fix_player_names(player_name))

alternate_player_pras_markets <-
  bind_rows(alternate_player_pras_markets_1, alternate_player_pras_markets_2)

# Over lines
over_lines <-
  player_pras_markets |> 
  filter(type == "Over") |> 
  mutate(market_name = "Player PRAs") |>
  select(match, market_name, player_name, line, over_price = price, prop_id) |> 
  bind_rows(alternate_player_pras_markets)

# Under lines
under_lines <-
  player_pras_markets |> 
  filter(type == "Under") |> 
  mutate(market_name = "Player PRAs") |>
  select(match, market_name, player_name, line, under_price = price, under_prop_id = prop_id)

# Combine
tab_player_pras_markets <-
  over_lines |>
  full_join(under_lines) |> 
  select(match, market_name, player_name, line, over_price, under_price, prop_id, under_prop_id) |> 
  mutate(agency = "TAB")

# Fix team names
tab_player_pras_markets <-
  tab_player_pras_markets |> 
  separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
  mutate(match = paste(home_team, "v", away_team)) |> 
  left_join(player_names_all[, c("player_full_name", "team_name")], by = c("player_name" = "player_full_name"))

#===============================================================================
# Player Blocks
#===============================================================================

# Filter to player block markets
player_blocks_markets <-
  all_tab_markets |> 
  filter(str_detect(market_name, "^Player Blocks"))

# Alternate Player Blocks
alternate_player_blocks_markets <-
  all_tab_markets |> 
  filter(str_detect(market_name, "Blocks$"))

# Extract player names
player_blocks_markets <-
  player_blocks_markets |> 
  filter(str_detect(prop_name, "Over|Under")) |>
  mutate(player_name = str_extract(prop_name, "^.*(?=\\s(\\d+))")) |> 
  mutate(player_name = str_remove_all(player_name, "( Over)|( Under)")) |> 
  mutate(line = str_extract(prop_name, "[0-9\\.]{1,4}")) |> 
  mutate(line = as.numeric(line)) |>
  mutate(type = str_detect(prop_name, "Over|\\+")) |> 
  mutate(type = ifelse(type, "Over", "Under")) |>
  mutate(player_name = standardize_to_initials(player_name)) |>
  mutate(player_name = fix_player_names_tab_initials(player_name)) |>
  left_join(player_names[, c("player_full_name", "join_name")], by = c("player_name" = "join_name")) |> 
  select(-player_name) |> 
  rename(player_name = player_full_name)

alternate_player_blocks_markets_1 <-
  alternate_player_blocks_markets |>
  filter(str_detect(market_name, "\\d+\\+ Blocks")) |>
  mutate(player_name = str_remove(prop_name, " \\(.*\\)")) |>
  mutate(line = str_extract(market_name, "\\d+")) |>
  mutate(line = as.numeric(line) - 0.5) |>
  transmute(match, market_name = "Player Blocks", player_name, line, over_price = price, prop_id) |>
  mutate(player_name = fix_player_names(player_name))

alternate_player_blocks_markets_2 <-
  alternate_player_blocks_markets |>
  filter(str_detect(market_name, "Alternate")) |> 
  mutate(player_name = str_extract(prop_name, "^.*(?=\\s(\\d+))")) |> 
  mutate(line = str_extract(prop_name, "[0-9\\.]{1,4}")) |> 
  mutate(line = as.numeric(line) - 0.5) |>
  transmute(match, market_name = "Player Blocks", player_name, line, over_price = price, prop_id) |>
  mutate(player_name = fix_player_names(player_name))

alternate_player_blocks_markets <-
  bind_rows(alternate_player_blocks_markets_1, alternate_player_blocks_markets_2)

# Over lines
over_lines <-
  player_blocks_markets |> 
  filter(type == "Over") |> 
  mutate(market_name = "Player Blocks") |>
  select(match, market_name, player_name, line, over_price = price, prop_id) |> 
  bind_rows(alternate_player_blocks_markets)

# Under lines
under_lines <-
  player_blocks_markets |> 
  filter(type == "Under") |> 
  mutate(market_name = "Player Blocks") |>
  select(match, market_name, player_name, line, under_price = price, under_prop_id = prop_id)

# Combine
tab_player_blocks_markets <-
  over_lines |>
  full_join(under_lines) |> 
  select(match, market_name, player_name, line, over_price, under_price, prop_id, under_prop_id) |> 
  mutate(agency = "TAB")

# Fix team names
tab_player_blocks_markets <-
  tab_player_blocks_markets |> 
  separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
  mutate(match = paste(home_team, "v", away_team)) |> 
  left_join(player_names_all[, c("player_full_name", "team_name")], by = c("player_name" = "player_full_name"))

#===============================================================================
# Player Steals
#===============================================================================

# Filter to player steal markets
player_steals_markets <-
  all_tab_markets |> 
  filter(str_detect(market_name, "^Player Steals"))

# Alternate Player Steals
alternate_player_steals_markets <-
  all_tab_markets |> 
  filter(str_detect(market_name, "Steals$"))

# Extract player names
player_steals_markets <-
  player_steals_markets |> 
  filter(str_detect(prop_name, "Over|Under")) |>
  mutate(player_name = str_extract(prop_name, "^.*(?=\\s(\\d+))")) |> 
  mutate(player_name = str_remove_all(player_name, "( Over)|( Under)")) |> 
  mutate(line = str_extract(prop_name, "[0-9\\.]{1,4}")) |> 
  mutate(line = as.numeric(line)) |>
  mutate(type = str_detect(prop_name, "Over|\\+")) |> 
  mutate(type = ifelse(type, "Over", "Under")) |>
  mutate(player_name = standardize_to_initials(player_name)) |>
  mutate(player_name = fix_player_names_tab_initials(player_name)) |>
  left_join(player_names[, c("player_full_name", "join_name")], by = c("player_name" = "join_name")) |> 
  select(-player_name) |> 
  rename(player_name = player_full_name)

alternate_player_steals_markets_1 <-
  alternate_player_steals_markets |>
  filter(str_detect(market_name, "\\d+\\+ Steals")) |>
  mutate(player_name = str_remove(prop_name, " \\(.*\\)")) |>
  mutate(line = str_extract(market_name, "\\d+")) |>
  mutate(line = as.numeric(line) - 0.5) |>
  transmute(match, market_name = "Player Steals", player_name, line, over_price = price, prop_id) |>
  mutate(player_name = fix_player_names(player_name))

alternate_player_steals_markets_2 <-
  alternate_player_steals_markets |>
  filter(str_detect(market_name, "Alternate")) |> 
  mutate(player_name = str_extract(prop_name, "^.*(?=\\s(\\d+))")) |> 
  mutate(line = str_extract(prop_name, "[0-9\\.]{1,4}")) |> 
  mutate(line = as.numeric(line) - 0.5) |>
  transmute(match, market_name = "Player Steals", player_name, line, over_price = price, prop_id) |>
  mutate(player_name = fix_player_names(player_name))

alternate_player_steals_markets <-
  bind_rows(alternate_player_steals_markets_1, alternate_player_steals_markets_2)

# Over lines
over_lines <-
  player_steals_markets |> 
  filter(type == "Over") |> 
  mutate(market_name = "Player Steals") |>
  select(match, market_name, player_name, line, over_price = price, prop_id) |> 
  bind_rows(alternate_player_steals_markets)

# Under lines
under_lines <-
  player_steals_markets |> 
  filter(type == "Under") |> 
  mutate(market_name = "Player Steals") |>
  select(match, market_name, player_name, line, under_price = price, under_prop_id = prop_id)

# Combine
tab_player_steals_markets <-
  over_lines |>
  full_join(under_lines) |> 
  select(match, market_name, player_name, line, over_price, under_price, prop_id, under_prop_id) |> 
  mutate(agency = "TAB")

# Fix team names
tab_player_steals_markets <-
  tab_player_steals_markets |> 
  separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
  mutate(match = paste(home_team, "v", away_team)) |> 
  left_join(player_names_all[, c("player_full_name", "team_name")], by = c("player_name" = "player_full_name"))

#===============================================================================
# Player Threes
#===============================================================================

# Filter to player threes markets
player_threes_markets <-
  all_tab_markets |> 
  filter(str_detect(market_name, "^Player Threes"))

# Alternate Player Threes
alternate_player_threes_markets <-
  all_tab_markets |> 
  filter(str_detect(market_name, "Threes$"))

# Extract player names
player_threes_markets <-
  player_threes_markets |> 
  filter(str_detect(prop_name, "Over|Under")) |>
  mutate(player_name = str_extract(prop_name, "^.*(?=\\s(\\d+))")) |> 
  mutate(player_name = str_remove_all(player_name, "( Over)|( Under)")) |> 
  mutate(line = str_extract(prop_name, "[0-9\\.]{1,4}")) |> 
  mutate(line = as.numeric(line)) |>
  mutate(type = str_detect(prop_name, "Over|\\+")) |> 
  mutate(type = ifelse(type, "Over", "Under")) |>
  mutate(player_name = standardize_to_initials(player_name)) |>
  mutate(player_name = fix_player_names_tab_initials(player_name)) |>
  left_join(player_names[, c("player_full_name", "join_name")], by = c("player_name" = "join_name")) |> 
  select(-player_name) |> 
  rename(player_name = player_full_name)

alternate_player_threes_markets_1 <-
  alternate_player_threes_markets |>
  filter(str_detect(market_name, "\\d+\\+ Threes")) |>
  mutate(player_name = str_remove(prop_name, " \\(.*\\)")) |>
  mutate(line = str_extract(market_name, "\\d+")) |>
  mutate(line = as.numeric(line) - 0.5) |>
  transmute(match, market_name = "Player Threes", player_name, line, over_price = price, prop_id) |>
  mutate(player_name = fix_player_names(player_name))

alternate_player_threes_markets_2 <-
  alternate_player_threes_markets |>
  filter(str_detect(market_name, "Alternate")) |> 
  mutate(player_name = str_extract(prop_name, "^.*(?=\\s(\\d+))")) |> 
  mutate(line = str_extract(prop_name, "[0-9\\.]{1,4}")) |> 
  mutate(line = as.numeric(line) - 0.5) |>
  transmute(match, market_name = "Player Threes", player_name, line, over_price = price, prop_id) |>
  mutate(player_name = fix_player_names(player_name))

alternate_player_threes_markets <-
  bind_rows(alternate_player_threes_markets_1, alternate_player_threes_markets_2)

# Over lines
over_lines <-
  player_threes_markets |> 
  filter(type == "Over") |> 
  mutate(market_name = "Player Threes") |>
  select(match, market_name, player_name, line, over_price = price, prop_id) |> 
  bind_rows(alternate_player_threes_markets)

# Under lines
under_lines <-
  player_threes_markets |> 
  filter(type == "Under") |> 
  mutate(market_name = "Player Threes") |>
  select(match, market_name, player_name, line, under_price = price, under_prop_id = prop_id)

# Combine
tab_player_threes_markets <-
  over_lines |>
  full_join(under_lines) |> 
  select(match, market_name, player_name, line, over_price, under_price, prop_id, under_prop_id) |> 
  mutate(agency = "TAB")

# Fix team names
tab_player_threes_markets <-
  tab_player_threes_markets |> 
  separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
  mutate(match = paste(home_team, "v", away_team)) |> 
  left_join(player_names_all[, c("player_full_name", "team_name")], by = c("player_name" = "player_full_name"))

#===============================================================================
# Fix team names----------------------------------------------------------------
#===============================================================================

# Fix Team Names----------------------------------------------------------------

# Points
tab_player_points_markets <-
  tab_player_points_markets |>
  mutate(home_team = fix_team_names(home_team)) |>
  mutate(away_team = fix_team_names(away_team)) |>
  mutate(match = paste(home_team, away_team, sep = " v ")) |>
  mutate(opposition_team = case_when(
    team_name == home_team ~ away_team,
    team_name == away_team ~ home_team
  )) |>
  select(
    match,
    home_team,
    away_team,
    player_name,
    player_team = team_name,
    opposition_team,
    market_name,
    line,
    over_price,
    under_price,
    agency,
    prop_id,
    under_prop_id
  )

# Assists
tab_player_assists_markets <-
  tab_player_assists_markets |>
  mutate(home_team = fix_team_names(home_team)) |>
  mutate(away_team = fix_team_names(away_team)) |>
  mutate(match = paste(home_team, away_team, sep = " v ")) |>
  mutate(opposition_team = case_when(
    team_name == home_team ~ away_team,
    team_name == away_team ~ home_team
  )) |>
  select(
    match,
    home_team,
    away_team,
    player_name,
    player_team = team_name,
    opposition_team,
    market_name,
    line,
    over_price,
    under_price,
    agency,
    prop_id,
    under_prop_id
  )

# Rebounds
tab_player_rebounds_markets <-
  tab_player_rebounds_markets |>
  mutate(home_team = fix_team_names(home_team)) |>
  mutate(away_team = fix_team_names(away_team)) |>
  mutate(match = paste(home_team, away_team, sep = " v ")) |>
  mutate(opposition_team = case_when(
    team_name == home_team ~ away_team,
    team_name == away_team ~ home_team
  )) |>
  select(
    match,
    home_team,
    away_team,
    player_name,
    player_team = team_name,
    opposition_team,
    market_name,
    line,
    over_price,
    under_price,
    agency,
    prop_id,
    under_prop_id
  )

# PRAs
tab_player_pras_markets <-
  tab_player_pras_markets |>
  mutate(home_team = fix_team_names(home_team)) |>
  mutate(away_team = fix_team_names(away_team)) |>
  mutate(match = paste(home_team, away_team, sep = " v ")) |>
  mutate(opposition_team = case_when(
    team_name == home_team ~ away_team,
    team_name == away_team ~ home_team
  )) |>
  select(
    match,
    home_team,
    away_team,
    player_name,
    player_team = team_name,
    opposition_team,
    market_name,
    line,
    over_price,
    under_price,
    agency,
    prop_id,
    under_prop_id
  )

# Blocks
tab_player_blocks_markets <-
  tab_player_blocks_markets |>
  mutate(home_team = fix_team_names(home_team)) |>
  mutate(away_team = fix_team_names(away_team)) |>
  mutate(match = paste(home_team, away_team, sep = " v ")) |>
  mutate(opposition_team = case_when(
    team_name == home_team ~ away_team,
    team_name == away_team ~ home_team
  )) |>
  select(
    match,
    home_team,
    away_team,
    player_name,
    player_team = team_name,
    opposition_team,
    market_name,
    line,
    over_price,
    under_price,
    agency,
    prop_id,
    under_prop_id
  )

# Steals
tab_player_steals_markets <-
  tab_player_steals_markets |>
  mutate(home_team = fix_team_names(home_team)) |>
  mutate(away_team = fix_team_names(away_team)) |>
  mutate(match = paste(home_team, away_team, sep = " v ")) |>
  mutate(opposition_team = case_when(
    team_name == home_team ~ away_team,
    team_name == away_team ~ home_team
  )) |>
  select(
    match,
    home_team,
    away_team,
    player_name,
    player_team = team_name,
    opposition_team,
    market_name,
    line,
    over_price,
    under_price,
    agency,
    prop_id,
    under_prop_id
  )

# Threes
tab_player_threes_markets <-
  tab_player_threes_markets |>
  mutate(home_team = fix_team_names(home_team)) |>
  mutate(away_team = fix_team_names(away_team)) |>
  mutate(match = paste(home_team, away_team, sep = " v ")) |>
  mutate(opposition_team = case_when(
    team_name == home_team ~ away_team,
    team_name == away_team ~ home_team
  )) |>
  select(
    match,
    home_team,
    away_team,
    player_name,
    player_team = team_name,
    opposition_team,
    market_name,
    line,
    over_price,
    under_price,
    agency,
    prop_id,
    under_prop_id
  )

#===============================================================================
# Write to CSV------------------------------------------------------------------
#===============================================================================

tab_player_points_markets |> write_csv("Data/scraped_odds/tab_player_points.csv")
tab_player_assists_markets |> write_csv("Data/scraped_odds/tab_player_assists.csv")
tab_player_rebounds_markets |> write_csv("Data/scraped_odds/tab_player_rebounds.csv")
tab_player_pras_markets |> write_csv("Data/scraped_odds/tab_player_pras.csv")
tab_player_blocks_markets |> write_csv("Data/scraped_odds/tab_player_blocks.csv")
tab_player_steals_markets |> write_csv("Data/scraped_odds/tab_player_steals.csv")
tab_player_threes_markets |> write_csv("Data/scraped_odds/tab_player_threes.csv")
}

#===============================================================================
# Run safe function
#===============================================================================

safe_main_tab <- safely(main_tab)
safe_main_tab()
