# Libraries
library(tidyverse)
library(rvest)
library(httr2)

# URL to get responses
tab_url = "https://api.beta.tab.com.au/v1/tab-info-service/sports/Basketball/competitions/NBA?jurisdiction=NSW&numTopMarkets=5"

# Fix team names function
source("Scripts/fix_team_names.R")

main_tab <- function() {

# Make request and get response
tab_response <-
    request(tab_url) |>
    req_perform() |> 
    resp_body_json()

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

# Map functions to data
all_tab_markets <-
    map(tab_response$matches, get_match_info) |> bind_rows()

# Expand list col into multiple cols
all_tab_markets <-
all_tab_markets |>
    unnest_wider(col = propositions, names_sep = "_") |>
    select(any_of(c("match",
           "round",
           "start_time",
           "market_name")),
           prop_name = propositions_name,
           price = propositions_returnWin)

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
    select(-prop_name)

# Away teams
away_teams <-
    all_tab_markets |>
    filter(market_name == "Head To Head") |> 
    group_by(match) |> 
    filter(row_number() == 2) |> 
    rename(away_win = price) |> 
    select(-prop_name)

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
    select(match, start_time, market_name, total_points_line, under_price = price)

# Over lines
over_lines <-
    all_tab_markets |>
    filter(market_name == "Total Points Over/Under") |> 
    filter(str_detect(prop_name, "Over")) |> 
    mutate(total_points_line = as.numeric(str_extract(prop_name, "\\d+\\.\\d+"))) |>
    select(match, start_time, market_name, total_points_line, over_price = price)

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
    mutate(type = ifelse(type, "Over", "Under"))

alternate_player_points_markets <-
    alternate_player_points_markets |>
    mutate(player_name = str_remove(prop_name, " \\(.*\\)")) |>
    mutate(line = str_extract(market_name, "\\d+")) |> 
    mutate(line = as.numeric(line) - 0.5) |> 
    transmute(match, market_name = "Player Points", player_name, line, over_price = price)


# Over lines
over_lines <-
    player_points_markets |> 
    filter(type == "Over") |> 
    mutate(market_name = "Player Points") |>
    select(match, market_name, player_name, line, over_price = price) |> 
    bind_rows(alternate_player_points_markets)

# Under lines
under_lines <-
    player_points_markets |> 
    filter(type == "Under") |> 
    mutate(market_name = "Player Points") |>
    select(match, market_name, player_name, line, under_price = price)

# Combine
tab_player_points_markets <-
    over_lines |>
    full_join(under_lines) |> 
    select(match, market_name, player_name, line, over_price, under_price) |> 
    mutate(agency = "TAB")

# Fix team names
tab_player_points_markets <-
    tab_player_points_markets |> 
    separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
    mutate(match = paste(home_team, "v", away_team))

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
    mutate(line = if_else(market_name == "Alternate Player Assists", line - 0.5, line))

alternate_player_assists_markets <-
    alternate_player_assists_markets |>
    mutate(player_name = str_remove(prop_name, " \\(.*\\)")) |>
    mutate(line = str_extract(market_name, "\\d+")) |> 
    mutate(line = as.numeric(line) - 0.5) |> 
    transmute(match, market_name = "Player Assists", player_name, line, over_price = price)

# Over lines
over_lines <-
    player_assists_markets |> 
    filter(type == "Over") |> 
    mutate(market_name = "Player Assists") |>
    select(match, market_name, player_name, line, over_price = price) |> 
    bind_rows(alternate_player_assists_markets)

# Under lines
under_lines <-
    player_assists_markets |> 
    filter(type == "Under") |> 
    mutate(market_name = "Player Assists") |>
    select(match, market_name, player_name, line, under_price = price)

# Combine
tab_player_assists_markets <-
    over_lines |>
    full_join(under_lines) |> 
    select(match, market_name, player_name, line, over_price, under_price) |> 
    mutate(agency = "TAB")

# Fix team names
tab_player_assists_markets <-
    tab_player_assists_markets |> 
    separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
    mutate(match = paste(home_team, "v", away_team))

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
    mutate(line = if_else(market_name == "Alternate Player Rebounds", line - 0.5, line))

alternate_player_rebounds_markets <-
    alternate_player_rebounds_markets |>
    mutate(player_name = str_remove(prop_name, " \\(.*\\)")) |>
    mutate(line = str_extract(market_name, "\\d+")) |>
    mutate(line = as.numeric(line) - 0.5) |>
    transmute(match, market_name = "Player Rebounds", player_name, line, over_price = price)

# Over lines
over_lines <-
    player_rebounds_markets |> 
    filter(type == "Over") |> 
    mutate(market_name = "Player Rebounds") |>
    select(match, market_name, player_name, line, over_price = price) |> 
    bind_rows(alternate_player_rebounds_markets)

# Under lines
under_lines <-
    player_rebounds_markets |> 
    filter(type == "Under") |> 
    mutate(market_name = "Player Rebounds") |>
    select(match, market_name, player_name, line, under_price = price)

# Combine
tab_player_rebounds_markets <-
    over_lines |>
    full_join(under_lines) |> 
    select(match, market_name, player_name, line, over_price, under_price) |> 
    mutate(agency = "TAB")

# Fix team names
tab_player_rebounds_markets <-
    tab_player_rebounds_markets |> 
    separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
    mutate(match = paste(home_team, "v", away_team))

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
  mutate(type = ifelse(type, "Over", "Under"))

alternate_player_pras_markets_1 <-
  alternate_player_pras_markets |>
  filter(str_detect(market_name, "\\d+\\+ PRA")) |>
  mutate(player_name = str_remove(prop_name, " \\(.*\\)")) |>
  mutate(line = str_extract(market_name, "\\d+")) |>
  mutate(line = as.numeric(line) - 0.5) |>
  transmute(match, market_name = "Player PRAs", player_name, line, over_price = price)

alternate_player_pras_markets_2 <-
  alternate_player_pras_markets |>
  filter(str_detect(market_name, "Alternate")) |> 
  mutate(player_name = str_extract(prop_name, "^.*(?=\\s(\\d+))")) |> 
  mutate(line = str_extract(prop_name, "[0-9\\.]{1,4}")) |> 
  mutate(line = as.numeric(line) - 0.5) |>
  transmute(match, market_name = "Player PRAs", player_name, line, over_price = price)

alternate_player_pras_markets <-
  bind_rows(alternate_player_pras_markets_1, alternate_player_pras_markets_2)

# Over lines
over_lines <-
  player_pras_markets |> 
  filter(type == "Over") |> 
  mutate(market_name = "Player PRAs") |>
  select(match, market_name, player_name, line, over_price = price) |> 
  bind_rows(alternate_player_pras_markets)

# Under lines
under_lines <-
  player_pras_markets |> 
  filter(type == "Under") |> 
  mutate(market_name = "Player PRAs") |>
  select(match, market_name, player_name, line, under_price = price)

# Combine
tab_player_pras_markets <-
  over_lines |>
  full_join(under_lines) |> 
  select(match, market_name, player_name, line, over_price, under_price) |> 
  mutate(agency = "TAB")

# Fix team names
tab_player_pras_markets <-
  tab_player_pras_markets |> 
  separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
  mutate(match = paste(home_team, "v", away_team))

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
  mutate(type = ifelse(type, "Over", "Under"))

alternate_player_blocks_markets_1 <-
  alternate_player_blocks_markets |>
  filter(str_detect(market_name, "\\d+\\+ Blocks")) |>
  mutate(player_name = str_remove(prop_name, " \\(.*\\)")) |>
  mutate(line = str_extract(market_name, "\\d+")) |>
  mutate(line = as.numeric(line) - 0.5) |>
  transmute(match, market_name = "Player Blocks", player_name, line, over_price = price)

alternate_player_blocks_markets_2 <-
  alternate_player_blocks_markets |>
  filter(str_detect(market_name, "Alternate")) |> 
  mutate(player_name = str_extract(prop_name, "^.*(?=\\s(\\d+))")) |> 
  mutate(line = str_extract(prop_name, "[0-9\\.]{1,4}")) |> 
  mutate(line = as.numeric(line) - 0.5) |>
  transmute(match, market_name = "Player Blocks", player_name, line, over_price = price)

alternate_player_blocks_markets <-
  bind_rows(alternate_player_blocks_markets_1, alternate_player_blocks_markets_2)

# Over lines
over_lines <-
  player_blocks_markets |> 
  filter(type == "Over") |> 
  mutate(market_name = "Player Blocks") |>
  select(match, market_name, player_name, line, over_price = price) |> 
  bind_rows(alternate_player_blocks_markets)

# Under lines
under_lines <-
  player_blocks_markets |> 
  filter(type == "Under") |> 
  mutate(market_name = "Player Blocks") |>
  select(match, market_name, player_name, line, under_price = price)

# Combine
tab_player_blocks_markets <-
  over_lines |>
  full_join(under_lines) |> 
  select(match, market_name, player_name, line, over_price, under_price) |> 
  mutate(agency = "TAB")

# Fix team names
tab_player_blocks_markets <-
  tab_player_blocks_markets |> 
  separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
  mutate(match = paste(home_team, "v", away_team))

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
  mutate(type = ifelse(type, "Over", "Under"))

alternate_player_steals_markets_1 <-
  alternate_player_steals_markets |>
  filter(str_detect(market_name, "\\d+\\+ Steals")) |>
  mutate(player_name = str_remove(prop_name, " \\(.*\\)")) |>
  mutate(line = str_extract(market_name, "\\d+")) |>
  mutate(line = as.numeric(line) - 0.5) |>
  transmute(match, market_name = "Player Steals", player_name, line, over_price = price)

alternate_player_steals_markets_2 <-
  alternate_player_steals_markets |>
  filter(str_detect(market_name, "Alternate")) |> 
  mutate(player_name = str_extract(prop_name, "^.*(?=\\s(\\d+))")) |> 
  mutate(line = str_extract(prop_name, "[0-9\\.]{1,4}")) |> 
  mutate(line = as.numeric(line) - 0.5) |>
  transmute(match, market_name = "Player Steals", player_name, line, over_price = price)

alternate_player_steals_markets <-
  bind_rows(alternate_player_steals_markets_1, alternate_player_steals_markets_2)

# Over lines
over_lines <-
  player_steals_markets |> 
  filter(type == "Over") |> 
  mutate(market_name = "Player Steals") |>
  select(match, market_name, player_name, line, over_price = price) |> 
  bind_rows(alternate_player_steals_markets)

# Under lines
under_lines <-
  player_steals_markets |> 
  filter(type == "Under") |> 
  mutate(market_name = "Player Steals") |>
  select(match, market_name, player_name, line, under_price = price)

# Combine
tab_player_steals_markets <-
  over_lines |>
  full_join(under_lines) |> 
  select(match, market_name, player_name, line, over_price, under_price) |> 
  mutate(agency = "TAB")

# Fix team names
tab_player_steals_markets <-
  tab_player_steals_markets |> 
  separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
  mutate(match = paste(home_team, "v", away_team))

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
  mutate(type = ifelse(type, "Over", "Under"))

alternate_player_threes_markets_1 <-
  alternate_player_threes_markets |>
  filter(str_detect(market_name, "\\d+\\+ Threes")) |>
  mutate(player_name = str_remove(prop_name, " \\(.*\\)")) |>
  mutate(line = str_extract(market_name, "\\d+")) |>
  mutate(line = as.numeric(line) - 0.5) |>
  transmute(match, market_name = "Player Threes", player_name, line, over_price = price)

alternate_player_threes_markets_2 <-
  alternate_player_threes_markets |>
  filter(str_detect(market_name, "Alternate")) |> 
  mutate(player_name = str_extract(prop_name, "^.*(?=\\s(\\d+))")) |> 
  mutate(line = str_extract(prop_name, "[0-9\\.]{1,4}")) |> 
  mutate(line = as.numeric(line) - 0.5) |>
  transmute(match, market_name = "Player Threes", player_name, line, over_price = price)

alternate_player_threes_markets <-
  bind_rows(alternate_player_threes_markets_1, alternate_player_threes_markets_2)

# Over lines
over_lines <-
  player_threes_markets |> 
  filter(type == "Over") |> 
  mutate(market_name = "Player Threes") |>
  select(match, market_name, player_name, line, over_price = price) |> 
  bind_rows(alternate_player_threes_markets)

# Under lines
under_lines <-
  player_threes_markets |> 
  filter(type == "Under") |> 
  mutate(market_name = "Player Threes") |>
  select(match, market_name, player_name, line, under_price = price)

# Combine
tab_player_threes_markets <-
  over_lines |>
  full_join(under_lines) |> 
  select(match, market_name, player_name, line, over_price, under_price) |> 
  mutate(agency = "TAB")

# Fix team names
tab_player_threes_markets <-
  tab_player_threes_markets |> 
  separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
  mutate(match = paste(home_team, "v", away_team))

#===============================================================================
# Fix team and player names-----------------------------------------------------
#===============================================================================

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

# Fix player names--------------------------------------------------------------

# Points
tab_player_points_markets <-
  tab_player_points_markets |>
  mutate(first_initial = str_sub(player_name, 1, 1)) |>
  mutate(surname = str_extract(player_name, "(?<=\\s).*$")) |>
  mutate(join_name = paste(first_initial, surname, sep = " ")) |>
  select(-first_initial,-surname) |>
  mutate(join_name = case_when(
    str_detect(player_name, "^Don.* M.*$") ~ "Do Mitchell",
    str_detect(player_name, "C-Pope") ~ "K Caldwell-Pope",
    str_detect(player_name, "M Porter") ~ "M Porter Jr.",
    str_detect(player_name, "St.* Curry$") ~ "St Curry",
    str_detect(player_name, "A-kounmpo") ~ "G Antetokounmpo",
    str_detect(player_name, "Ke.* Johnson$") ~ "Ke Johnson",
    str_detect(player_name, "Ke.* Murray$") ~ "Ke Murray",
    str_detect(player_name, "J.* Butler$") ~ "Ji Butler",
    str_detect(player_name, "An.* Wiggins$") ~ "An Wiggins",
    str_detect(player_name, "Tr.* Young$") ~ "Tr Young",
    str_detect(player_name, "Jr.* Holiday$") ~ "Jr Holiday",
    str_detect(player_name, "Ja.* Green$") ~ "Ja Green",
    str_detect(player_name, "Jal.* Williams$") ~ "Ja Williams",
    str_detect(player_name, "Jal.* Wiliams$") ~ "Ja Williams",
    str_detect(player_name, "Z.* Wiliamson$") ~ "Z Williamson",
    str_detect(player_name, "C.* Cuni.*$") ~ "C Cunningham",
    str_detect(player_name, "Au.* Thompson$") ~ "Au Thompson",
    str_detect(player_name, "Ty.* Jones$") ~ "Ty Jones",
    str_detect(player_name, "Valanciunas") ~ "J Valanciunas",
    str_detect(player_name, "F.* Wagnr$") ~ "F Wagner",
    str_detect(player_name, "Haliburto") ~ "T Haliburton",
    str_detect(player_name, "P.* Wshington$") ~ "P Washington",
    str_detect(player_name, "W.* Carter.*$") ~ "W Carter Jr.",
    str_detect(player_name, "J.* Jackson.*$") ~ "J Jackson Jr.",
    str_detect(player_name, "B Brown Jr") ~ "B Brown",
    str_detect(player_name, "La.* Ball$") ~ "La Ball",
    str_detect(player_name, "Te.* Mann$") ~ "Te Mann",
    str_detect(player_name, "Mik.* Bridges$") ~ "Mi Bridges",
    str_detect(player_name, "G-Alexander") ~ "S Gilgeous-Alexander",
    str_detect(player_name, "Larry Nance") ~ "L Nance Jr.",
    str_detect(player_name, "V Wembanyam") ~ "V Wembanyama",
    str_detect(player_name, "T Hrt-Tuckr") ~ "T Horton-Tucker",
    str_detect(player_name, "M Bagley") ~ "M Bagley III",
    str_detect(player_name, "Ki Lewis Jr") ~ "K Lewis Jr.",
    str_detect(player_name, "Tre Jones") ~ "Tr Jones",
    .default = join_name
  )) |>
  left_join(player_names |> select(join_name, player_full_name, team_name),
            by = c("join_name"))

# Rebounds
tab_player_rebounds_markets <-
  tab_player_rebounds_markets |>
  mutate(first_initial = str_sub(player_name, 1, 1)) |>
  mutate(surname = str_extract(player_name, "(?<=\\s).*$")) |>
  mutate(join_name = paste(first_initial, surname, sep = " ")) |>
  select(-first_initial,-surname) |>
  mutate(join_name = case_when(
    str_detect(player_name, "^Don.* M.*$") ~ "Do Mitchell",
    str_detect(player_name, "C-Pope") ~ "K Caldwell-Pope",
    str_detect(player_name, "M Porter") ~ "M Porter Jr.",
    str_detect(player_name, "St.* Curry$") ~ "St Curry",
    str_detect(player_name, "A-kounmpo") ~ "G Antetokounmpo",
    str_detect(player_name, "Ke.* Johnson$") ~ "Ke Johnson",
    str_detect(player_name, "Ke.* Murray$") ~ "Ke Murray",
    str_detect(player_name, "J.* Butler$") ~ "Ji Butler",
    str_detect(player_name, "An.* Wiggins$") ~ "An Wiggins",
    str_detect(player_name, "Tr.* Young$") ~ "Tr Young",
    str_detect(player_name, "Jr.* Holiday$") ~ "Jr Holiday",
    str_detect(player_name, "Ja.* Green$") ~ "Ja Green",
    str_detect(player_name, "Jal.* Williams$") ~ "Ja Williams",
    str_detect(player_name, "Jal.* Wiliams$") ~ "Ja Williams",
    str_detect(player_name, "Z.* Wiliamson$") ~ "Z Williamson",
    str_detect(player_name, "C.* Cuni.*$") ~ "C Cunningham",
    str_detect(player_name, "Au.* Thompson$") ~ "Au Thompson",
    str_detect(player_name, "Ty.* Jones$") ~ "Ty Jones",
    str_detect(player_name, "Valanciunas") ~ "J Valanciunas",
    str_detect(player_name, "F.* Wagnr$") ~ "F Wagner",
    str_detect(player_name, "Haliburto") ~ "T Haliburton",
    str_detect(player_name, "P.* Wshington$") ~ "P Washington",
    str_detect(player_name, "W.* Carter.*$") ~ "W Carter Jr.",
    str_detect(player_name, "J.* Jackson.*$") ~ "J Jackson Jr.",
    str_detect(player_name, "B Brown Jr") ~ "B Brown",
    str_detect(player_name, "La.* Ball$") ~ "La Ball",
    str_detect(player_name, "Te.* Mann$") ~ "Te Mann",
    str_detect(player_name, "Mik.* Bridges$") ~ "Mi Bridges",
    str_detect(player_name, "G-Alexander") ~ "S Gilgeous-Alexander",
    str_detect(player_name, "Larry Nance") ~ "L Nance Jr.",
    str_detect(player_name, "V Wembanyam") ~ "V Wembanyama",
    str_detect(player_name, "T Hrt-Tuckr") ~ "T Horton-Tucker",
    str_detect(player_name, "M Bagley") ~ "M Bagley III",
    str_detect(player_name, "Ki Lewis Jr") ~ "K Lewis Jr.",
    str_detect(player_name, "Tre Jones") ~ "Tr Jones",
    .default = join_name
  )) |>
  left_join(player_names |> select(join_name, player_full_name, team_name),
            by = c("join_name"))

# Assists
tab_player_assists_markets <-
  tab_player_assists_markets |>
  mutate(first_initial = str_sub(player_name, 1, 1)) |>
  mutate(surname = str_extract(player_name, "(?<=\\s).*$")) |>
  mutate(join_name = paste(first_initial, surname, sep = " ")) |>
  select(-first_initial,-surname) |>
  mutate(join_name = case_when(
    str_detect(player_name, "^Don.* M.*$") ~ "Do Mitchell",
    str_detect(player_name, "C-Pope") ~ "K Caldwell-Pope",
    str_detect(player_name, "M Porter") ~ "M Porter Jr.",
    str_detect(player_name, "St.* Curry$") ~ "St Curry",
    str_detect(player_name, "A-kounmpo") ~ "G Antetokounmpo",
    str_detect(player_name, "Ke.* Johnson$") ~ "Ke Johnson",
    str_detect(player_name, "Ke.* Murray$") ~ "Ke Murray",
    str_detect(player_name, "J.* Butler$") ~ "Ji Butler",
    str_detect(player_name, "An.* Wiggins$") ~ "An Wiggins",
    str_detect(player_name, "Tr.* Young$") ~ "Tr Young",
    str_detect(player_name, "Jr.* Holiday$") ~ "Jr Holiday",
    str_detect(player_name, "Ja.* Green$") ~ "Ja Green",
    str_detect(player_name, "Jal.* Williams$") ~ "Ja Williams",
    str_detect(player_name, "Jal.* Wiliams$") ~ "Ja Williams",
    str_detect(player_name, "Z.* Wiliamson$") ~ "Z Williamson",
    str_detect(player_name, "C.* Cuni.*$") ~ "C Cunningham",
    str_detect(player_name, "Au.* Thompson$") ~ "Au Thompson",
    str_detect(player_name, "Ty.* Jones$") ~ "Ty Jones",
    str_detect(player_name, "Valanciunas") ~ "J Valanciunas",
    str_detect(player_name, "F.* Wagnr$") ~ "F Wagner",
    str_detect(player_name, "Haliburto") ~ "T Haliburton",
    str_detect(player_name, "P.* Wshington$") ~ "P Washington",
    str_detect(player_name, "W.* Carter.*$") ~ "W Carter Jr.",
    str_detect(player_name, "J.* Jackson.*$") ~ "J Jackson Jr.",
    str_detect(player_name, "B Brown Jr") ~ "B Brown",
    str_detect(player_name, "La.* Ball$") ~ "La Ball",
    str_detect(player_name, "Te.* Mann$") ~ "Te Mann",
    str_detect(player_name, "Mik.* Bridges$") ~ "Mi Bridges",
    str_detect(player_name, "G-Alexander") ~ "S Gilgeous-Alexander",
    str_detect(player_name, "Larry Nance") ~ "L Nance Jr.",
    str_detect(player_name, "V Wembanyam") ~ "V Wembanyama",
    str_detect(player_name, "T Hrt-Tuckr") ~ "T Horton-Tucker",
    str_detect(player_name, "M Bagley") ~ "M Bagley III",
    str_detect(player_name, "Ki Lewis Jr") ~ "K Lewis Jr.",
    str_detect(player_name, "Tre Jones") ~ "Tr Jones",
    .default = join_name
  )) |>
  left_join(player_names |> select(join_name, player_full_name, team_name),
            by = c("join_name"))

# PRAs
tab_player_pras_markets <-
  tab_player_pras_markets |>
  mutate(first_initial = str_sub(player_name, 1, 1)) |>
  mutate(surname = str_extract(player_name, "(?<=\\s).*$")) |>
  mutate(join_name = paste(first_initial, surname, sep = " ")) |>
  select(-first_initial,-surname) |>
  mutate(join_name = case_when(
    str_detect(player_name, "^Don.* M.*$") ~ "Do Mitchell",
    str_detect(player_name, "C-Pope") ~ "K Caldwell-Pope",
    str_detect(player_name, "M Porter") ~ "M Porter Jr.",
    str_detect(player_name, "St.* Curry$") ~ "St Curry",
    str_detect(player_name, "A-kounmpo") ~ "G Antetokounmpo",
    str_detect(player_name, "Ke.* Johnson$") ~ "Ke Johnson",
    str_detect(player_name, "Ke.* Murray$") ~ "Ke Murray",
    str_detect(player_name, "J.* Butler$") ~ "Ji Butler",
    str_detect(player_name, "An.* Wiggins$") ~ "An Wiggins",
    str_detect(player_name, "Tr.* Young$") ~ "Tr Young",
    str_detect(player_name, "Jr.* Holiday$") ~ "Jr Holiday",
    str_detect(player_name, "Ja.* Green$") ~ "Ja Green",
    str_detect(player_name, "Jal.* Williams$") ~ "Ja Williams",
    str_detect(player_name, "Jal.* Wiliams$") ~ "Ja Williams",
    str_detect(player_name, "Z.* Wiliamson$") ~ "Z Williamson",
    str_detect(player_name, "C.* Cuni.*$") ~ "C Cunningham",
    str_detect(player_name, "Au.* Thompson$") ~ "Au Thompson",
    str_detect(player_name, "Ty.* Jones$") ~ "Ty Jones",
    str_detect(player_name, "Valanciunas") ~ "J Valanciunas",
    str_detect(player_name, "F.* Wagnr$") ~ "F Wagner",
    str_detect(player_name, "Haliburto") ~ "T Haliburton",
    str_detect(player_name, "P.* Wshington$") ~ "P Washington",
    str_detect(player_name, "W.* Carter.*$") ~ "W Carter Jr.",
    str_detect(player_name, "J.* Jackson.*$") ~ "J Jackson Jr.",
    str_detect(player_name, "B Brown Jr") ~ "B Brown",
    str_detect(player_name, "La.* Ball$") ~ "La Ball",
    str_detect(player_name, "Te.* Mann$") ~ "Te Mann",
    str_detect(player_name, "Mik.* Bridges$") ~ "Mi Bridges",
    str_detect(player_name, "G-Alexander") ~ "S Gilgeous-Alexander",
    str_detect(player_name, "Larry Nance") ~ "L Nance Jr.",
    str_detect(player_name, "V Wembanyam") ~ "V Wembanyama",
    str_detect(player_name, "T Hrt-Tuckr") ~ "T Horton-Tucker",
    str_detect(player_name, "M Bagley") ~ "M Bagley III",
    str_detect(player_name, "Ki Lewis Jr") ~ "K Lewis Jr.",
    str_detect(player_name, "Tre Jones") ~ "Tr Jones",
    .default = join_name
  )) |>
  left_join(player_names |> select(join_name, player_full_name, team_name),
            by = c("join_name"))

# Blocks
tab_player_blocks_markets <-
  tab_player_blocks_markets |>
  mutate(first_initial = str_sub(player_name, 1, 1)) |>
  mutate(surname = str_extract(player_name, "(?<=\\s).*$")) |>
  mutate(join_name = paste(first_initial, surname, sep = " ")) |>
  select(-first_initial,-surname) |>
  mutate(join_name = case_when(
    str_detect(player_name, "^Don.* M.*$") ~ "Do Mitchell",
    str_detect(player_name, "C-Pope") ~ "K Caldwell-Pope",
    str_detect(player_name, "M Porter") ~ "M Porter Jr.",
    str_detect(player_name, "St.* Curry$") ~ "St Curry",
    str_detect(player_name, "A-kounmpo") ~ "G Antetokounmpo",
    str_detect(player_name, "Ke.* Johnson$") ~ "Ke Johnson",
    str_detect(player_name, "Ke.* Murray$") ~ "Ke Murray",
    str_detect(player_name, "J.* Butler$") ~ "Ji Butler",
    str_detect(player_name, "An.* Wiggins$") ~ "An Wiggins",
    str_detect(player_name, "Tr.* Young$") ~ "Tr Young",
    str_detect(player_name, "Jr.* Holiday$") ~ "Jr Holiday",
    str_detect(player_name, "Ja.* Green$") ~ "Ja Green",
    str_detect(player_name, "Jal.* Williams$") ~ "Ja Williams",
    str_detect(player_name, "Jal.* Wiliams$") ~ "Ja Williams",
    str_detect(player_name, "Z.* Wiliamson$") ~ "Z Williamson",
    str_detect(player_name, "C.* Cuni.*$") ~ "C Cunningham",
    str_detect(player_name, "Au.* Thompson$") ~ "Au Thompson",
    str_detect(player_name, "Ty.* Jones$") ~ "Ty Jones",
    str_detect(player_name, "Valanciunas") ~ "J Valanciunas",
    str_detect(player_name, "F.* Wagnr$") ~ "F Wagner",
    str_detect(player_name, "Haliburto") ~ "T Haliburton",
    str_detect(player_name, "P.* Wshington$") ~ "P Washington",
    str_detect(player_name, "W.* Carter.*$") ~ "W Carter Jr.",
    str_detect(player_name, "J.* Jackson.*$") ~ "J Jackson Jr.",
    str_detect(player_name, "B Brown Jr") ~ "B Brown",
    str_detect(player_name, "La.* Ball$") ~ "La Ball",
    str_detect(player_name, "Te.* Mann$") ~ "Te Mann",
    str_detect(player_name, "Mik.* Bridges$") ~ "Mi Bridges",
    str_detect(player_name, "G-Alexander") ~ "S Gilgeous-Alexander",
    str_detect(player_name, "Larry Nance") ~ "L Nance Jr.",
    str_detect(player_name, "V Wembanyam") ~ "V Wembanyama",
    str_detect(player_name, "T Hrt-Tuckr") ~ "T Horton-Tucker",
    str_detect(player_name, "M Bagley") ~ "M Bagley III",
    str_detect(player_name, "Ki Lewis Jr") ~ "K Lewis Jr.",
    str_detect(player_name, "Tre Jones") ~ "Tr Jones",
    .default = join_name
  )) |>
  left_join(player_names |> select(join_name, player_full_name, team_name),
            by = c("join_name"))

# Steals
tab_player_steals_markets <-
  tab_player_steals_markets |>
  mutate(first_initial = str_sub(player_name, 1, 1)) |>
  mutate(surname = str_extract(player_name, "(?<=\\s).*$")) |>
  mutate(join_name = paste(first_initial, surname, sep = " ")) |>
  select(-first_initial,-surname) |>
  mutate(join_name = case_when(
    str_detect(player_name, "^Don.* M.*$") ~ "Do Mitchell",
    str_detect(player_name, "C-Pope") ~ "K Caldwell-Pope",
    str_detect(player_name, "M Porter") ~ "M Porter Jr.",
    str_detect(player_name, "St.* Curry$") ~ "St Curry",
    str_detect(player_name, "A-kounmpo") ~ "G Antetokounmpo",
    str_detect(player_name, "Ke.* Johnson$") ~ "Ke Johnson",
    str_detect(player_name, "Ke.* Murray$") ~ "Ke Murray",
    str_detect(player_name, "J.* Butler$") ~ "Ji Butler",
    str_detect(player_name, "An.* Wiggins$") ~ "An Wiggins",
    str_detect(player_name, "Tr.* Young$") ~ "Tr Young",
    str_detect(player_name, "Jr.* Holiday$") ~ "Jr Holiday",
    str_detect(player_name, "Ja.* Green$") ~ "Ja Green",
    str_detect(player_name, "Jal.* Williams$") ~ "Ja Williams",
    str_detect(player_name, "Jal.* Wiliams$") ~ "Ja Williams",
    str_detect(player_name, "Z.* Wiliamson$") ~ "Z Williamson",
    str_detect(player_name, "C.* Cuni.*$") ~ "C Cunningham",
    str_detect(player_name, "Au.* Thompson$") ~ "Au Thompson",
    str_detect(player_name, "Ty.* Jones$") ~ "Ty Jones",
    str_detect(player_name, "Valanciunas") ~ "J Valanciunas",
    str_detect(player_name, "F.* Wagnr$") ~ "F Wagner",
    str_detect(player_name, "Haliburto") ~ "T Haliburton",
    str_detect(player_name, "P.* Wshington$") ~ "P Washington",
    str_detect(player_name, "W.* Carter.*$") ~ "W Carter Jr.",
    str_detect(player_name, "J.* Jackson.*$") ~ "J Jackson Jr.",
    str_detect(player_name, "B Brown Jr") ~ "B Brown",
    str_detect(player_name, "La.* Ball$") ~ "La Ball",
    str_detect(player_name, "Te.* Mann$") ~ "Te Mann",
    str_detect(player_name, "Mik.* Bridges$") ~ "Mi Bridges",
    str_detect(player_name, "G-Alexander") ~ "S Gilgeous-Alexander",
    str_detect(player_name, "Larry Nance") ~ "L Nance Jr.",
    str_detect(player_name, "V Wembanyam") ~ "V Wembanyama",
    str_detect(player_name, "T Hrt-Tuckr") ~ "T Horton-Tucker",
    str_detect(player_name, "M Bagley") ~ "M Bagley III",
    str_detect(player_name, "Ki Lewis Jr") ~ "K Lewis Jr.",
    str_detect(player_name, "Tre Jones") ~ "Tr Jones",
    .default = join_name
  )) |>
  left_join(player_names |> select(join_name, player_full_name, team_name),
            by = c("join_name"))

# Threes
tab_player_threes_markets <-
  tab_player_threes_markets |>
  mutate(first_initial = str_sub(player_name, 1, 1)) |>
  mutate(surname = str_extract(player_name, "(?<=\\s).*$")) |>
  mutate(join_name = paste(first_initial, surname, sep = " ")) |>
  select(-first_initial,-surname) |>
  mutate(join_name = case_when(
    str_detect(player_name, "^Don.* M.*$") ~ "Do Mitchell",
    str_detect(player_name, "C-Pope") ~ "K Caldwell-Pope",
    str_detect(player_name, "M Porter") ~ "M Porter Jr.",
    str_detect(player_name, "St.* Curry$") ~ "St Curry",
    str_detect(player_name, "A-kounmpo") ~ "G Antetokounmpo",
    str_detect(player_name, "Ke.* Johnson$") ~ "Ke Johnson",
    str_detect(player_name, "Ke.* Murray$") ~ "Ke Murray",
    str_detect(player_name, "J.* Butler$") ~ "Ji Butler",
    str_detect(player_name, "An.* Wiggins$") ~ "An Wiggins",
    str_detect(player_name, "Tr.* Young$") ~ "Tr Young",
    str_detect(player_name, "Jr.* Holiday$") ~ "Jr Holiday",
    str_detect(player_name, "Ja.* Green$") ~ "Ja Green",
    str_detect(player_name, "Jal.* Williams$") ~ "Ja Williams",
    str_detect(player_name, "Jal.* Wiliams$") ~ "Ja Williams",
    str_detect(player_name, "Z.* Wiliamson$") ~ "Z Williamson",
    str_detect(player_name, "C.* Cuni.*$") ~ "C Cunningham",
    str_detect(player_name, "Au.* Thompson$") ~ "Au Thompson",
    str_detect(player_name, "Ty.* Jones$") ~ "Ty Jones",
    str_detect(player_name, "Valanciunas") ~ "J Valanciunas",
    str_detect(player_name, "F.* Wagnr$") ~ "F Wagner",
    str_detect(player_name, "Haliburto") ~ "T Haliburton",
    str_detect(player_name, "P.* Wshington$") ~ "P Washington",
    str_detect(player_name, "W.* Carter.*$") ~ "W Carter Jr.",
    str_detect(player_name, "J.* Jackson.*$") ~ "J Jackson Jr.",
    str_detect(player_name, "B Brown Jr") ~ "B Brown",
    str_detect(player_name, "La.* Ball$") ~ "La Ball",
    str_detect(player_name, "Te.* Mann$") ~ "Te Mann",
    str_detect(player_name, "Mik.* Bridges$") ~ "Mi Bridges",
    str_detect(player_name, "G-Alexander") ~ "S Gilgeous-Alexander",
    str_detect(player_name, "Larry Nance") ~ "L Nance Jr.",
    str_detect(player_name, "V Wembanyam") ~ "V Wembanyama",
    str_detect(player_name, "T Hrt-Tuckr") ~ "T Horton-Tucker",
    str_detect(player_name, "M Bagley") ~ "M Bagley III",
    str_detect(player_name, "Ki Lewis Jr") ~ "K Lewis Jr.",
    str_detect(player_name, "Tre Jones") ~ "Tr Jones",
    .default = join_name
  )) |>
  left_join(player_names |> select(join_name, player_full_name, team_name),
            by = c("join_name"))


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
    player_name = player_full_name,
    player_team = team_name,
    opposition_team,
    market_name,
    line,
    over_price,
    under_price,
    agency
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
    player_name = player_full_name,
    player_team = team_name,
    opposition_team,
    market_name,
    line,
    over_price,
    under_price,
    agency
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
    player_name = player_full_name,
    player_team = team_name,
    opposition_team,
    market_name,
    line,
    over_price,
    under_price,
    agency
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
    player_name = player_full_name,
    player_team = team_name,
    opposition_team,
    market_name,
    line,
    over_price,
    under_price,
    agency
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
    player_name = player_full_name,
    player_team = team_name,
    opposition_team,
    market_name,
    line,
    over_price,
    under_price,
    agency
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
    player_name = player_full_name,
    player_team = team_name,
    opposition_team,
    market_name,
    line,
    over_price,
    under_price,
    agency
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
    player_name = player_full_name,
    player_team = team_name,
    opposition_team,
    market_name,
    line,
    over_price,
    under_price,
    agency
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