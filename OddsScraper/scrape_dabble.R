competitions_api_url = "https://api.dabble.com.au/competitions/active/?sportId=01408294-cb34-4cc0-8ab1-504f5c4c6e1f"
fixture_url = "https://api.dabble.com.au/sportfixtures/details/adf1e4c1-768f-4f14-b089-99bc7e66273d"

# Libraries
library(tidyverse)
library(rvest)
library(httr2)
library(jsonlite)

# URL to get responses
competitions_api_url = "https://api.dabble.com.au/competitions/090c2877-4d13-4f6e-8faf-886092153c58/sport-fixtures"

# Fix team names function
source("Scripts/fix_team_names.R")

# Make request and get response
dabble_response <-
    request(competitions_api_url) |>
    req_perform() |>
    resp_body_json()

# Function to get fixture details
get_fixture_details <- function(data) {
  match <- data$name
  match_id <- data$id
  
  # Get Prices Information
  all_prices <-
    map_dfr(data$prices, ~{
      tibble(
        id = .x$id,
        marketId = .x$marketId,
        selectionId = .x$selectionId,
        price = .x$price
      )
    })
  
  # Get Markets Information
  all_markets <-
    map_dfr(data$markets, ~{
      tibble(
        market_name = .x$name,
        marketId = .x$id
      )
    })
  
  # Get Selections Information
  all_selections <-
    map_dfr(data$selections, ~{
      tibble(
        selection_name = .x$name,
        selectionId = .x$id
      )
    })

  # Return tibble
    all_prices |>
    left_join(all_markets) |> 
    left_join(all_selections) |> 
    mutate(match = match, id = match_id)

}

# Map over data
data_list <- data <- dabble_response$data
fixture_details <- map_dfr(data_list, get_fixture_details)

#===============================================================================
# Get H2H Data
#===============================================================================

all_h2h <-
  fixture_details |> 
  filter(market_name == "Match Result (Inc OT)") |> 
  separate(match, into = c("away_team", "home_team"), sep = " @ ") |>
  mutate(home_team = fix_team_names(home_team),
         away_team = fix_team_names(away_team)) |> 
  mutate(match = paste(home_team, "v", away_team)) |>
  mutate(selection_name = fix_team_names(selection_name))

# Home Teams
home_teams <-
  all_h2h |>
  mutate(market_name = "Head To Head") |> 
  filter(home_team == selection_name) |>
  rename(home_win = price) |>
  select(match, home_team, away_team, market_name, home_win)

# Away teams
away_teams <-
  all_h2h |>
  mutate(market_name = "Head To Head") |> 
  filter(away_team == selection_name) |>
  rename(away_win = price) |>
  select(match, home_team, away_team, market_name, away_win)

# Combine
dabble_head_to_head_markets <-
  home_teams |>
  left_join(away_teams) |> 
  select(match, market_name, home_team, home_win, away_team, away_win) |> 
  mutate(margin = round((1/home_win + 1/away_win), digits = 3)) |> 
  mutate(agency = "Dabble")

# Write to csv
write_csv(dabble_head_to_head_markets, "Data/scraped_odds/dabble_h2h.csv")

#===============================================================================
# Get Fixture Details
#===============================================================================

fixtures <-
  all_h2h |>
  distinct(match, id)
  
#===============================================================================
# Prop Data
#===============================================================================

# Get List of Fixture URLs
fixture_urls <- paste0("https://api.dabble.com.au/sportfixtures/details/", fixtures$id)

# Function to get fixture details
get_fixture_details <- function(url) {
  
  # Get response from URL
  fixture_response <-
    request(url) |>
    req_perform() |>
    resp_body_json()
  
  # Get Prices Information
    all_prices <-
    map_dfr(fixture_response$data$prices, ~{
      tibble(
        id = .x$id,
        marketId = .x$marketId,
        selectionId = .x$selectionId,
        price = .x$price
      )
    })

    # Get Markets Information
    all_markets <-
      map_dfr(fixture_response$data$markets, ~{
        tibble(
          market_name = .x$name,
          id = .x$id
        )
      })
    
    
    # Get Selections Information
    all_selections <-
      map_dfr(fixture_response$data$selections, ~{
        tibble(
          selection_name = .x$name,
          id = .x$id
        )
      })
    
    # Combine together
    all_prices |>
      left_join(all_markets, by = c("marketId" = "id")) |>
      left_join(all_selections, by = c("selectionId" = "id")) |> 
      mutate(match_id = str_remove(url, "https://api.dabble.com.au/sportfixtures/details/"))
}

# Map over data
prop_data <- map_dfr(fixture_urls, get_fixture_details, .progress = TRUE)

# Add match details
prop_data <-
  prop_data |>
  left_join(fixtures, by = c("match_id" = "id"))

#===============================================================================
# Get Player Points
#===============================================================================

# Filter to player points markets
player_points_markets <-
  prop_data |> 
  filter(str_detect(market_name, "Points O/U"))

# Alternate Player Points
alternate_player_points_markets <-
  prop_data |> 
  filter(str_detect(market_name, "To Score"))

# Extract player names
player_points_markets <-
  player_points_markets |> 
  filter(str_detect(selection_name, "Over|Under")) |>
  mutate(player_name = str_extract(selection_name, "^.*(?=\\s(\\d+))")) |> 
  mutate(player_name = str_remove_all(player_name, "( Over)|( Under)")) |> 
  mutate(line = str_extract(selection_name, "[0-9]{1}[0-9\\.]{1,4}")) |> 
  mutate(line = as.numeric(line)) |>
  mutate(type = str_detect(selection_name, "Over|\\+")) |> 
  mutate(type = ifelse(type, "Over", "Under"))

alternate_player_points_markets <-
  alternate_player_points_markets |>
  mutate(player_name = str_remove(selection_name, " \\(.*\\)")) |>
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
dabble_player_points_markets <-
  over_lines |>
  full_join(under_lines) |> 
  select(match, market_name, player_name, line, over_price, under_price) |> 
  mutate(agency = "Dabble") |> 
  distinct(match, market_name, player_name, line, .keep_all = TRUE)

#===============================================================================
# Get Player Assists
#===============================================================================

# Filter to player assists markets
player_assists_markets <-
  prop_data |> 
  filter(str_detect(market_name, "Assists O/U"))

# Alternate Player Assists
alternate_player_assists_markets <-
  prop_data |> 
  filter(str_detect(market_name, "To Have")) |> 
  filter(str_detect(market_name, "Assists$")) |> 
  filter(str_detect(market_name, "Quarter", negate = TRUE))

# Extract player names
player_assists_markets <-
  player_assists_markets |> 
  filter(str_detect(selection_name, "Over|Under")) |>
  mutate(player_name = str_extract(selection_name, "^.*(?=\\s(\\d+))")) |> 
  mutate(player_name = str_remove_all(player_name, "( Over)|( Under)")) |> 
  mutate(line = str_extract(selection_name, "[0-9]{1}[0-9\\.]{1,4}")) |> 
  mutate(line = as.numeric(line)) |>
  mutate(type = str_detect(selection_name, "Over|\\+")) |> 
  mutate(type = ifelse(type, "Over", "Under"))

alternate_player_assists_markets <-
  alternate_player_assists_markets |>
  mutate(player_name = str_remove(selection_name, " \\(.*\\)")) |>
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
dabble_player_assists_markets <-
  over_lines |>
  full_join(under_lines) |> 
  select(match, market_name, player_name, line, over_price, under_price) |> 
  mutate(agency = "Dabble") |> 
  distinct(match, market_name, player_name, line, .keep_all = TRUE)

#===============================================================================
# Get Player Rebounds
#===============================================================================

# Filter to player rebounds markets
player_rebounds_markets <-
  prop_data |> 
  filter(str_detect(market_name, "Rebounds O/U"))

# Alternate Player Rebounds
alternate_player_rebounds_markets <-
  prop_data |> 
  filter(str_detect(market_name, "To Have")) |> 
  filter(str_detect(market_name, "Rebounds$")) |> 
  filter(str_detect(market_name, "Quarter", negate = TRUE))

# Extract player names
player_rebounds_markets <-
  player_rebounds_markets |> 
  filter(str_detect(selection_name, "Over|Under")) |>
  mutate(player_name = str_extract(selection_name, "^.*(?=\\s(\\d+))")) |> 
  mutate(player_name = str_remove_all(player_name, "( Over)|( Under)")) |> 
  mutate(line = str_extract(selection_name, "[0-9]{1}[0-9\\.]{1,4}")) |> 
  mutate(line = as.numeric(line)) |>
  mutate(type = str_detect(selection_name, "Over|\\+")) |> 
  mutate(type = ifelse(type, "Over", "Under"))

alternate_player_rebounds_markets <-
  alternate_player_rebounds_markets |>
  mutate(player_name = str_remove(selection_name, " \\(.*\\)")) |>
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
dabble_player_rebounds_markets <-
  over_lines |>
  full_join(under_lines) |> 
  select(match, market_name, player_name, line, over_price, under_price) |> 
  mutate(agency = "Dabble") |> 
  distinct(match, market_name, player_name, line, .keep_all = TRUE)

#===============================================================================
# Get Player PRAs
#===============================================================================

# Filter to player pras markets
player_pras_markets <-
  prop_data |> 
  filter(str_detect(market_name, "Pts \\+ Reb \\+ Ast O/U"))

# Alternate Player PRAs
alternate_player_pras_markets <-
  prop_data |> 
  filter(str_detect(market_name, "To Have")) |> 
  filter(str_detect(market_name, "Pts \\+ Reb \\+ Ast$")) |> 
  filter(str_detect(market_name, "Quarter", negate = TRUE))

# Extract player names
player_pras_markets <-
  player_pras_markets |> 
  filter(str_detect(selection_name, "Over|Under")) |>
  mutate(player_name = str_extract(selection_name, "^.*(?=\\s(\\d+))")) |> 
  mutate(player_name = str_remove_all(player_name, "( Over)|( Under)")) |> 
  mutate(line = str_extract(selection_name, "[0-9]{1}[0-9\\.]{1,4}")) |> 
  mutate(line = as.numeric(line)) |>
  mutate(type = str_detect(selection_name, "Under")) |> 
  mutate(type = ifelse(type, "Under", "Over"))

alternate_player_pras_markets <-
  alternate_player_pras_markets |>
  mutate(player_name = str_remove(selection_name, " \\(.*\\)")) |>
  mutate(line = str_extract(market_name, "\\d+")) |> 
  mutate(line = as.numeric(line) - 0.5) |> 
  transmute(match, market_name = "Player PRAs", player_name, line, over_price = price)

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
dabble_player_pras_markets <-
  over_lines |>
  full_join(under_lines) |> 
  select(match, market_name, player_name, line, over_price, under_price) |> 
  mutate(agency = "Dabble") |> 
  distinct(match, market_name, player_name, line, .keep_all = TRUE)

#===============================================================================
# Get Player Threes
#===============================================================================

# Filter to player threes markets
player_threes_markets <-
  prop_data |> 
  filter(str_detect(market_name, "Threes Made O/U"))

# Alternate Player Threes
alternate_player_threes_markets <-
  prop_data |> 
  filter(str_detect(market_name, "To Have")) |> 
  filter(str_detect(market_name, "Threes Made$")) |> 
  filter(str_detect(market_name, "Quarter", negate = TRUE))

# Extract player names
player_threes_markets <-
  player_threes_markets |> 
  filter(str_detect(selection_name, "Over|Under")) |>
  mutate(player_name = str_extract(selection_name, "^.*(?=\\s(\\d+))")) |> 
  mutate(player_name = str_remove_all(player_name, "( Over)|( Under)")) |> 
  mutate(line = str_extract(selection_name, "[0-9]{1}[0-9\\.]{1,4}")) |> 
  mutate(line = as.numeric(line)) |>
  mutate(type = str_detect(selection_name, "Over|\\+")) |> 
  mutate(type = ifelse(type, "Over", "Under"))

alternate_player_threes_markets <-
  alternate_player_threes_markets |>
  mutate(player_name = str_remove(selection_name, " \\(.*\\)")) |>
  mutate(line = str_extract(market_name, "\\d+")) |> 
  mutate(line = as.numeric(line) - 0.5) |> 
  transmute(match, market_name = "Player Threes", player_name, line, over_price = price)

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
dabble_player_threes_markets <-
  over_lines |>
  full_join(under_lines) |> 
  select(match, market_name, player_name, line, over_price, under_price) |> 
  mutate(agency = "Dabble") |> 
  distinct(match, market_name, player_name, line, .keep_all = TRUE)

#===============================================================================
# Get Player Steals
#===============================================================================

# Filter to player steals markets
player_steals_markets <-
  prop_data |> 
  filter(str_detect(market_name, "Steals O/U"))

# Alternate Player Steals
alternate_player_steals_markets <-
  prop_data |> 
  filter(str_detect(market_name, "To Have")) |> 
  filter(str_detect(market_name, "Steals$")) |> 
  filter(str_detect(market_name, "Quarter", negate = TRUE))

# Extract player names
player_steals_markets <-
  player_steals_markets |> 
  filter(str_detect(selection_name, "Over|Under")) |>
  mutate(player_name = str_extract(selection_name, "^.*(?=\\s(\\d+))")) |> 
  mutate(player_name = str_remove_all(player_name, "( Over)|( Under)")) |> 
  mutate(line = str_extract(selection_name, "[0-9]{1}[0-9\\.]{1,4}")) |> 
  mutate(line = as.numeric(line)) |>
  mutate(type = str_detect(selection_name, "Over|\\+")) |> 
  mutate(type = ifelse(type, "Over", "Under"))

alternate_player_steals_markets <-
  alternate_player_steals_markets |>
  mutate(player_name = str_remove(selection_name, " \\(.*\\)")) |>
  mutate(line = str_extract(market_name, "\\d+")) |> 
  mutate(line = as.numeric(line) - 0.5) |> 
  transmute(match, market_name = "Player Steals", player_name, line, over_price = price)

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
dabble_player_steals_markets <-
  over_lines |>
  full_join(under_lines) |> 
  select(match, market_name, player_name, line, over_price, under_price) |> 
  mutate(agency = "Dabble") |> 
  distinct(match, market_name, player_name, line, .keep_all = TRUE)

#===============================================================================
# Get Player Blocks
#===============================================================================

# Filter to player blocks markets
player_blocks_markets <-
  prop_data |> 
  filter(str_detect(market_name, "Blocks O/U"))

# Alternate Player Blocks
alternate_player_blocks_markets <-
  prop_data |> 
  filter(str_detect(market_name, "To Have")) |> 
  filter(str_detect(market_name, "Blocks$")) |> 
  filter(str_detect(market_name, "Quarter", negate = TRUE))

# Extract player names
player_blocks_markets <-
  player_blocks_markets |> 
  filter(str_detect(selection_name, "Over|Under")) |>
  mutate(player_name = str_extract(selection_name, "^.*(?=\\s(\\d+))")) |> 
  mutate(player_name = str_remove_all(player_name, "( Over)|( Under)")) |> 
  mutate(line = str_extract(selection_name, "[0-9]{1}[0-9\\.]{1,4}")) |> 
  mutate(line = as.numeric(line)) |>
  mutate(type = str_detect(selection_name, "Over|\\+")) |> 
  mutate(type = ifelse(type, "Over", "Under"))

alternate_player_blocks_markets <-
  alternate_player_blocks_markets |>
  mutate(player_name = str_remove(selection_name, " \\(.*\\)")) |>
  mutate(line = str_extract(market_name, "\\d+")) |> 
  mutate(line = as.numeric(line) - 0.5) |> 
  transmute(match, market_name = "Player Blocks", player_name, line, over_price = price)

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
dabble_player_blocks_markets <-
  over_lines |>
  full_join(under_lines) |> 
  select(match, market_name, player_name, line, over_price, under_price) |> 
  mutate(agency = "Dabble") |> 
  distinct(match, market_name, player_name, line, .keep_all = TRUE)

#===============================================================================
# Fix team and player names-----------------------------------------------------
#===============================================================================

# Get teams dabblele
teams <-
  read_csv("Data/all_teams.csv")

# Get player names dabblele
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
  select(player_name = player_full_name, player_team = team_name)

# Fix player names--------------------------------------------------------------

# Points
dabble_player_points_markets <-
  dabble_player_points_markets |>
  mutate(player_name = case_when(
    str_detect(player_name, "PJ Washington") ~ "P.J. Washington",
    .default = player_name
  )) |>
  left_join(player_names_unique,
            by = c("player_name"))

# Rebounds
dabble_player_rebounds_markets <-
  dabble_player_rebounds_markets |>
  mutate(player_name = case_when(
    str_detect(player_name, "PJ Washington") ~ "P.J. Washington",
    .default = player_name
  )) |>
  left_join(player_names_unique,
            by = c("player_name"))

# Assists
dabble_player_assists_markets <-
  dabble_player_assists_markets |>
  mutate(player_name = case_when(
    str_detect(player_name, "PJ Washington") ~ "P.J. Washington",
    .default = player_name
  )) |>
  left_join(player_names_unique,
            by = c("player_name"))

# Steals
dabble_player_steals_markets <-
  dabble_player_steals_markets |>
  mutate(player_name = case_when(
    str_detect(player_name, "PJ Washington") ~ "P.J. Washington",
    .default = player_name
  )) |>
  left_join(player_names_unique,
            by = c("player_name"))

# Blocks
dabble_player_blocks_markets <-
  dabble_player_blocks_markets |>
  mutate(player_name = case_when(
    str_detect(player_name, "PJ Washington") ~ "P.J. Washington",
    .default = player_name
  )) |>
  left_join(player_names_unique,
            by = c("player_name"))

# Threes
dabble_player_threes_markets <-
  dabble_player_threes_markets |>
  mutate(player_name = case_when(
    str_detect(player_name, "PJ Washington") ~ "P.J. Washington",
    .default = player_name
  )) |>
  left_join(player_names_unique,
            by = c("player_name"))

# PRAs
dabble_player_pras_markets <-
  dabble_player_pras_markets |>
  mutate(player_name = case_when(
    str_detect(player_name, "PJ Washington") ~ "P.J. Washington",
    .default = player_name
  )) |>
  left_join(player_names_unique,
            by = c("player_name"))

# Fix Team Names----------------------------------------------------------------

# Points
dabble_player_points_markets <-
  dabble_player_points_markets |>
  separate(match, c("home_team", "away_team"), sep = " v ") |>
  mutate(home_team = fix_team_names(home_team)) |>
  mutate(away_team = fix_team_names(away_team)) |>
  mutate(match = paste(home_team, away_team, sep = " v ")) |>
  mutate(opposition_team = case_when(
    player_team == home_team ~ away_team,
    player_team == away_team ~ home_team
  )) |>
  select(
    match,
    home_team,
    away_team,
    player_name,
    player_team,
    opposition_team,
    market_name,
    line,
    over_price,
    under_price,
    agency
  )

# Assists
dabble_player_assists_markets <-
  dabble_player_assists_markets |>
  separate(match, c("home_team", "away_team"), sep = " v ") |>
  mutate(home_team = fix_team_names(home_team)) |>
  mutate(away_team = fix_team_names(away_team)) |>
  mutate(match = paste(home_team, away_team, sep = " v ")) |>
  mutate(opposition_team = case_when(
    player_team == home_team ~ away_team,
    player_team == away_team ~ home_team
  )) |>
  select(
    match,
    home_team,
    away_team,
    player_name,
    player_team,
    opposition_team,
    market_name,
    line,
    over_price,
    under_price,
    agency
  )

# Rebounds
dabble_player_rebounds_markets <-
  dabble_player_rebounds_markets |>
  separate(match, c("home_team", "away_team"), sep = " v ") |>
  mutate(home_team = fix_team_names(home_team)) |>
  mutate(away_team = fix_team_names(away_team)) |>
  mutate(match = paste(home_team, away_team, sep = " v ")) |>
  mutate(opposition_team = case_when(
    player_team == home_team ~ away_team,
    player_team == away_team ~ home_team
  )) |>
  select(
    match,
    home_team,
    away_team,
    player_name,
    player_team,
    opposition_team,
    market_name,
    line,
    over_price,
    under_price,
    agency
  )

# PRAs
dabble_player_pras_markets <-
  dabble_player_pras_markets |>
  separate(match, c("home_team", "away_team"), sep = " v ") |>
  mutate(home_team = fix_team_names(home_team)) |>
  mutate(away_team = fix_team_names(away_team)) |>
  mutate(match = paste(home_team, away_team, sep = " v ")) |>
  mutate(opposition_team = case_when(
    player_team == home_team ~ away_team,
    player_team == away_team ~ home_team
  )) |>
  select(
    match,
    home_team,
    away_team,
    player_name,
    player_team,
    opposition_team,
    market_name,
    line,
    over_price,
    under_price,
    agency
  )

# Blocks
dabble_player_blocks_markets <-
  dabble_player_blocks_markets |>
  separate(match, c("home_team", "away_team"), sep = " v ") |>
  mutate(home_team = fix_team_names(home_team)) |>
  mutate(away_team = fix_team_names(away_team)) |>
  mutate(match = paste(home_team, away_team, sep = " v ")) |>
  mutate(opposition_team = case_when(
    player_team == home_team ~ away_team,
    player_team == away_team ~ home_team
  )) |>
  select(
    match,
    home_team,
    away_team,
    player_name,
    player_team,
    opposition_team,
    market_name,
    line,
    over_price,
    under_price,
    agency
  )

# Steals
dabble_player_steals_markets <-
  dabble_player_steals_markets |>
  separate(match, c("home_team", "away_team"), sep = " v ") |>
  mutate(home_team = fix_team_names(home_team)) |>
  mutate(away_team = fix_team_names(away_team)) |>
  mutate(match = paste(home_team, away_team, sep = " v ")) |>
  mutate(opposition_team = case_when(
    player_team == home_team ~ away_team,
    player_team == away_team ~ home_team
  )) |>
  select(
    match,
    home_team,
    away_team,
    player_name,
    player_team,
    opposition_team,
    market_name,
    line,
    over_price,
    under_price,
    agency
  )

# Threes
dabble_player_threes_markets <-
  dabble_player_threes_markets |>
  separate(match, c("home_team", "away_team"), sep = " v ") |>
  mutate(home_team = fix_team_names(home_team)) |>
  mutate(away_team = fix_team_names(away_team)) |>
  mutate(match = paste(home_team, away_team, sep = " v ")) |>
  mutate(opposition_team = case_when(
    player_team == home_team ~ away_team,
    player_team == away_team ~ home_team
  )) |>
  select(
    match,
    home_team,
    away_team,
    player_name,
    player_team,
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

dabble_player_points_markets |> write_csv("Data/scraped_odds/dabble_player_points.csv")
dabble_player_assists_markets |> write_csv("Data/scraped_odds/dabble_player_assists.csv")
dabble_player_rebounds_markets |> write_csv("Data/scraped_odds/dabble_player_rebounds.csv")
dabble_player_pras_markets |> write_csv("Data/scraped_odds/dabble_player_pras.csv")
dabble_player_blocks_markets |> write_csv("Data/scraped_odds/dabble_player_blocks.csv")
dabble_player_steals_markets |> write_csv("Data/scraped_odds/dabble_player_steals.csv")
dabble_player_threes_markets |> write_csv("Data/scraped_odds/dabble_player_threes.csv")
