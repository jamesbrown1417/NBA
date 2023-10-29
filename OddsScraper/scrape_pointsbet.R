# Libraries
library(tidyverse)
library(rvest)
library(httr2)
library(jsonlite)
library(tidyjson)


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

pointsbet_h2h_main <- function() {

# URL of website
pointsbet_url = "https://api.au.pointsbet.com/api/v2/competitions/7176/events/featured?includeLive=false"

# Make request and get response
pointsbet_response <-
    request(pointsbet_url) |>
    req_perform() |> 
    resp_body_json()

# List of matches and data
events <- pointsbet_response$events

# Loop through to get all data--------------------------------------------------

# Create empty vectors
match_names <- c()
match_starts_at <- c()
home_teams <- c()
away_teams <- c()
event_names <- c()
outcome_names <- c()
outcome_prices <- c()
keys <- c()

# Loop through events
for (match in events) {
    for (market in match$specialFixedOddsMarkets) {
        for (outcome in market$outcomes) {
            # Append data to vectors
            match_names <- c(match_names, match$name)
            match_starts_at <- c(match_starts_at, match$startsAt)
            home_teams <- c(home_teams, match$homeTeam)
            away_teams <- c(away_teams, match$awayTeam)
            event_names <- c(event_names, market$eventName)
            outcome_names <- c(outcome_names, outcome$name)
            outcome_prices <- c(outcome_prices, outcome$price)
            keys <- c(keys, match$key)
        }
    }
}

# Output tibble
pointsbet_data <-
    tibble(
        match = match_names,
        start_time = match_starts_at,
        home_team = home_teams,
        away_team = away_teams,
        event = event_names,
        outcome = outcome_names,
        price = outcome_prices
    ) |>
    mutate(match = paste(home_team, "v", away_team)) |>
    relocate(match, .before = start_time)

#===============================================================================
# Head to head markets
#===============================================================================

# Filter to head to head markets
pointsbet_data_h2h <-
    pointsbet_data |> 
    filter(event == "Moneyline") 
               
# Home Teams
pointsbet_data_h2h_home <-
    pointsbet_data_h2h |> 
    filter(home_team == outcome) |>
    select(match, start_time, market = event, home_team, home_win = price)

# Away Teams
pointsbet_data_h2h_away <-
    pointsbet_data_h2h |> 
    filter(away_team == outcome) |>
    select(match, start_time, market = event, away_team, away_win = price)

# Combine
pointsbet_h2h <-
    full_join(pointsbet_data_h2h_home, pointsbet_data_h2h_away, by = c("match", "start_time", "market")) |> 
    mutate(market = "Head To Head") |>
    select(match, start_time, market_name = market, home_team, home_win, away_team, away_win) |> 
    mutate(margin = round((1/home_win + 1/away_win), digits = 3)) |> 
    mutate(agency = "Pointsbet")

# Write to csv
write_csv(pointsbet_h2h, "Data/scraped_odds/pointsbet_h2h.csv")

#===============================================================================
# Player Props
#===============================================================================

# Get unique keys
keys <- unique(keys)

# Get each match's api page
match_urls <- paste0("https://api.au.pointsbet.com/api/mes/v3/events/", keys)

# Create a function that gets the player props from each URL
get_player_props <- function(url) {
    # Make request and get response
    pointsbet_response <-
        request(url) |>
        req_perform() |>
        resp_body_json()
    
    # Loop through to get prop data---------------------------------------------
    
    # Create empty vectors
    match_names <- c()
    market_names <- c()
    outcome_names <- c()
    outcome_types <- c()
    outcome_prices <- c()

    # Loop through events
    for (market in pointsbet_response$fixedOddsMarkets) {
        for (outcome in market$outcomes) {
            # Append data to vectors
            match_names <- c(match_names, pointsbet_response$name)
            
            if (!is.null(market$name)) {
                market_names <- c(market_names, market$name)
            } else {
                market_names <- c(market_names, NA)
            }
            
            if (!is.null(outcome$name)) {
                outcome_names <- c(outcome_names, outcome$name)
            } else {
                outcome_names <- c(outcome_names, NA)
            }

            if (!is.null(outcome$outcomeType)) {
                outcome_types <- c(outcome_types, outcome$outcomeType)
            } else {
                outcome_types <- c(outcome_types, NA)
            }
            
            if (!is.null(outcome$price)) {
                outcome_prices <- c(outcome_prices, outcome$price)
            } else {
                outcome_prices <- c(outcome_prices, NA)
            }
        }
    }
    
    # Output tibble
        tibble(
            match = match_names,
            market = market_names,
            outcome = outcome_names,
            outcome_type = outcome_types,
            price = outcome_prices
        )
}

# Map function to each URL
pointsbet_data_player_props <- map_df(match_urls, get_player_props)

#===============================================================================
# Player Points
#===============================================================================

# Player points alternative totals----------------------------------------------

# Filter list to player points
pointsbet_player_points_lines <-
    pointsbet_data_player_props |>
    filter(str_detect(market, "Points To Get|Alternate Points")) |>
    mutate(line = str_extract(outcome, "[0-9]{1,2}")) |>
    mutate(line = as.numeric(line) - 0.5) |>
    mutate(match = str_replace(match, "@", "v")) |> 
    mutate(outcome = str_remove(outcome, " To Score.*$")) |> 
    separate(match, into = c("away_team", "home_team"), sep = " v ", remove = FALSE) |> 
    mutate(outcome = case_when(outcome == "Lebron James" ~ "LeBron James",
                               outcome == "D'angelo Russell" ~ "D'Angelo Russell",
                               outcome == "K. Caldwell-Pope" ~ "Kentavious Caldwell-Pope",
                               outcome == "De'andre Hunter" ~ "De'Andre Hunter",
                               outcome == "Lamelo Ball" ~ "LaMelo Ball",
                               outcome == "Fred Vanvleet" ~ "Fred VanVleet",
                               outcome == "Demar Derozan" ~ "DeMar DeRozan",
                               outcome == "Joshua Giddey" ~ "Josh Giddey",
                               outcome == "Rj Barrett" ~ "RJ Barrett",
                               outcome == "Michael Porter" ~ "Michael Porter Jr.",
                               outcome == "Wendell Carter" ~ "Wendell Carter Jr.",
                               outcome == "Zach Lavine" ~ "Zach LaVine",
                               outcome == "S. Gilgeous-Alexander" ~ "Shai Gilgeous-Alexander",
                               outcome == "De'aaron Fox" ~ "De'Aaron Fox",
                               outcome == "Gary Payton Ii" ~ "Gary Payton II",
                               outcome == "Nicolas Claxton" ~ "Nic Claxton",
                               outcome == "Gary Trent" ~ "Gary Trent Jr.",
                               outcome == "Cj Mccollum" ~ "CJ McCollum",
                               .default = outcome)) |>
    left_join(player_names[, c("player_full_name", "team_name")], by = c("outcome" = "player_full_name")) |>
    mutate(opposition_team = if_else(home_team == team_name, away_team, home_team)) |>
      transmute(
        match,
        home_team,
        away_team,
        market_name = "Player Points",
        player_name = outcome,
        player_team = team_name,
        opposition_team,
        line,
        over_price = price,
        agency = "Pointsbet")

# Player points over / under----------------------------------------------------

# Filter list to player points over under
pointsbet_player_points_over_under <-
    pointsbet_data_player_props |>
    filter(str_detect(market, "Player Points Over/Under"))

# Get Overs
pointsbet_player_points_over <-
    pointsbet_player_points_over_under |> 
    filter(str_detect(outcome, "Over")) |>
    mutate(player_name = outcome) |>
    separate(outcome, into = c("player_name", "line"), sep = " Over ") |>
    mutate(line = as.numeric(line)) |> 
  mutate(match = str_replace(match, "@", "v")) |> 
  separate(match, into = c("away_team", "home_team"), sep = " v ", remove = FALSE) |> 
  mutate(player_name = case_when(player_name == "Lebron James" ~ "LeBron James",
                             player_name == "D'angelo Russell" ~ "D'Angelo Russell",
                             player_name == "K. Caldwell-Pope" ~ "Kentavious Caldwell-Pope",
                             player_name == "De'andre Hunter" ~ "De'Andre Hunter",
                             player_name == "Lamelo Ball" ~ "LaMelo Ball",
                             player_name == "Fred Vanvleet" ~ "Fred VanVleet",
                             player_name == "Demar Derozan" ~ "DeMar DeRozan",
                             player_name == "Joshua Giddey" ~ "Josh Giddey",
                             player_name == "Rj Barrett" ~ "RJ Barrett",
                             player_name == "Michael Porter" ~ "Michael Porter Jr.",
                             player_name == "Wendell Carter" ~ "Wendell Carter Jr.",
                             player_name == "Zach Lavine" ~ "Zach LaVine",
                             player_name == "S. Gilgeous-Alexander" ~ "Shai Gilgeous-Alexander",
                             player_name == "De'aaron Fox" ~ "De'Aaron Fox",
                             player_name == "Gary Payton Ii" ~ "Gary Payton II",
                             player_name == "Nicolas Claxton" ~ "Nic Claxton",
                             player_name == "Gary Trent" ~ "Gary Trent Jr.",
                             player_name == "Cj Mccollum" ~ "CJ McCollum",
                             .default = player_name)) |>
  left_join(player_names[, c("player_full_name", "team_name")], by = c("player_name" = "player_full_name")) |>
  mutate(opposition_team = if_else(home_team == team_name, away_team, home_team)) |> 
  transmute(
    match,
    home_team,
    away_team,
    market_name = "Player Points",
    player_name,
    player_team = team_name,
    opposition_team,
    line,
    over_price = price,
    agency = "Pointsbet")
    
# Get Unders
pointsbet_player_points_under <-
  pointsbet_player_points_over_under |> 
  filter(str_detect(outcome, "Under")) |>
  mutate(player_name = outcome) |>
  separate(outcome, into = c("player_name", "line"), sep = " Under ") |>
  mutate(line = as.numeric(line)) |> 
  mutate(match = str_replace(match, "@", "v")) |> 
  separate(match, into = c("away_team", "home_team"), sep = " v ", remove = FALSE) |> 
  mutate(player_name = case_when(player_name == "Lebron James" ~ "LeBron James",
                                 player_name == "D'angelo Russell" ~ "D'Angelo Russell",
                                 player_name == "K. Caldwell-Pope" ~ "Kentavious Caldwell-Pope",
                                 player_name == "De'andre Hunter" ~ "De'Andre Hunter",
                                 player_name == "Lamelo Ball" ~ "LaMelo Ball",
                                 player_name == "Fred Vanvleet" ~ "Fred VanVleet",
                                 player_name == "Demar Derozan" ~ "DeMar DeRozan",
                                 player_name == "Joshua Giddey" ~ "Josh Giddey",
                                 player_name == "Rj Barrett" ~ "RJ Barrett",
                                 player_name == "Michael Porter" ~ "Michael Porter Jr.",
                                 player_name == "Wendell Carter" ~ "Wendell Carter Jr.",
                                 player_name == "Zach Lavine" ~ "Zach LaVine",
                                 player_name == "S. Gilgeous-Alexander" ~ "Shai Gilgeous-Alexander",
                                 player_name == "De'aaron Fox" ~ "De'Aaron Fox",
                                 player_name == "Gary Payton Ii" ~ "Gary Payton II",
                                 player_name == "Nicolas Claxton" ~ "Nic Claxton",
                                 player_name == "Gary Trent" ~ "Gary Trent Jr.",
                                 player_name == "Cj Mccollum" ~ "CJ McCollum",
                                 .default = player_name)) |>
  left_join(player_names[, c("player_full_name", "team_name")], by = c("player_name" = "player_full_name")) |>
  mutate(opposition_team = if_else(home_team == team_name, away_team, home_team)) |> 
  transmute(
    match,
    home_team,
    away_team,
    market_name = "Player Points",
    player_name,
    player_team = team_name,
    opposition_team,
    line,
    under_price = price,
    agency = "Pointsbet")

# Combine overs and unders
pointsbet_player_points_over_under <- 
    pointsbet_player_points_over |>
    left_join(pointsbet_player_points_under) |>
    select(match, home_team, away_team, market_name, player_name, player_team, opposition_team, line, over_price, under_price, agency)

#===============================================================================
# Player Assists
#===============================================================================

# Player assists alternative totals----------------------------------------------

# Filter list to player assists
pointsbet_player_assists_lines <-
    pointsbet_data_player_props |>
    filter(str_detect(market, "Assists To Get|Alternate Assists")) |>
    mutate(line = str_extract(outcome, "[0-9]{1,2}")) |>
    mutate(line = as.numeric(line) - 0.5) |>
    mutate(match = str_replace(match, "@", "v")) |> 
    mutate(outcome = str_remove(outcome, " To Get.*$")) |> 
    separate(match, into = c("away_team", "home_team"), sep = " v ", remove = FALSE) |> 
    mutate(outcome = case_when(outcome == "Lebron James" ~ "LeBron James",
                               outcome == "D'angelo Russell" ~ "D'Angelo Russell",
                               outcome == "K. Caldwell-Pope" ~ "Kentavious Caldwell-Pope",
                               outcome == "De'andre Hunter" ~ "De'Andre Hunter",
                               outcome == "Lamelo Ball" ~ "LaMelo Ball",
                               outcome == "Fred Vanvleet" ~ "Fred VanVleet",
                               outcome == "Demar Derozan" ~ "DeMar DeRozan",
                               outcome == "Joshua Giddey" ~ "Josh Giddey",
                               outcome == "Rj Barrett" ~ "RJ Barrett",
                               outcome == "Michael Porter" ~ "Michael Porter Jr.",
                               outcome == "Wendell Carter" ~ "Wendell Carter Jr.",
                               outcome == "Zach Lavine" ~ "Zach LaVine",
                               outcome == "S. Gilgeous-Alexander" ~ "Shai Gilgeous-Alexander",
                               outcome == "De'aaron Fox" ~ "De'Aaron Fox",
                               outcome == "Gary Payton Ii" ~ "Gary Payton II",
                               outcome == "Nicolas Claxton" ~ "Nic Claxton",
                               outcome == "Gary Trent" ~ "Gary Trent Jr.",
                               outcome == "Cj Mccollum" ~ "CJ McCollum",
                               .default = outcome)) |>
    left_join(player_names[, c("player_full_name", "team_name")], by = c("outcome" = "player_full_name")) |>
    mutate(opposition_team = if_else(home_team == team_name, away_team, home_team)) |>
    transmute(
        match,
        home_team,
        away_team,
        market_name = "Player Assists",
        player_name = outcome,
        player_team = team_name,
        opposition_team,
        line,
        over_price = price,
        agency = "Pointsbet")

# Player assists over / under----------------------------------------------------

# Filter list to player assists over under
pointsbet_player_assists_over_under <-
    pointsbet_data_player_props |>
    filter(str_detect(market, "Player Assists Over/Under"))

# Get Overs
pointsbet_player_assists_over <-
    pointsbet_player_assists_over_under |> 
    filter(str_detect(outcome, "Over")) |>
    mutate(player_name = outcome) |>
    separate(outcome, into = c("player_name", "line"), sep = " Over ") |>
    mutate(line = as.numeric(line)) |> 
    mutate(match = str_replace(match, "@", "v")) |> 
    separate(match, into = c("away_team", "home_team"), sep = " v ", remove = FALSE) |> 
    mutate(player_name = case_when(player_name == "Lebron James" ~ "LeBron James",
                                   player_name == "D'angelo Russell" ~ "D'Angelo Russell",
                                   player_name == "K. Caldwell-Pope" ~ "Kentavious Caldwell-Pope",
                                   player_name == "De'andre Hunter" ~ "De'Andre Hunter",
                                   player_name == "Lamelo Ball" ~ "LaMelo Ball",
                                   player_name == "Fred Vanvleet" ~ "Fred VanVleet",
                                   player_name == "Demar Derozan" ~ "DeMar DeRozan",
                                   player_name == "Joshua Giddey" ~ "Josh Giddey",
                                   player_name == "Rj Barrett" ~ "RJ Barrett",
                                   player_name == "Michael Porter" ~ "Michael Porter Jr.",
                                   player_name == "Wendell Carter" ~ "Wendell Carter Jr.",
                                   player_name == "Zach Lavine" ~ "Zach LaVine",
                                   player_name == "S. Gilgeous-Alexander" ~ "Shai Gilgeous-Alexander",
                                   player_name == "De'aaron Fox" ~ "De'Aaron Fox",
                                   player_name == "Gary Payton Ii" ~ "Gary Payton II",
                                   player_name == "Nicolas Claxton" ~ "Nic Claxton",
                                   player_name == "Gary Trent" ~ "Gary Trent Jr.",
                                   player_name == "Cj Mccollum" ~ "CJ McCollum",
                                   .default = player_name)) |>
    left_join(player_names[, c("player_full_name", "team_name")], by = c("player_name" = "player_full_name")) |>
    mutate(opposition_team = if_else(home_team == team_name, away_team, home_team)) |> 
    transmute(
        match,
        home_team,
        away_team,
        market_name = "Player Assists",
        player_name,
        player_team = team_name,
        opposition_team,
        line,
        over_price = price,
        agency = "Pointsbet")

# Get Unders
pointsbet_player_assists_under <-
    pointsbet_player_assists_over_under |> 
    filter(str_detect(outcome, "Under")) |>
    mutate(player_name = outcome) |>
    separate(outcome, into = c("player_name", "line"), sep = " Under ") |>
    mutate(line = as.numeric(line)) |> 
    mutate(match = str_replace(match, "@", "v")) |> 
    separate(match, into = c("away_team", "home_team"), sep = " v ", remove = FALSE) |> 
    mutate(player_name = case_when(player_name == "Lebron James" ~ "LeBron James",
                                   player_name == "D'angelo Russell" ~ "D'Angelo Russell",
                                   player_name == "K. Caldwell-Pope" ~ "Kentavious Caldwell-Pope",
                                   player_name == "De'andre Hunter" ~ "De'Andre Hunter",
                                   player_name == "Lamelo Ball" ~ "LaMelo Ball",
                                   player_name == "Fred Vanvleet" ~ "Fred VanVleet",
                                   player_name == "Demar Derozan" ~ "DeMar DeRozan",
                                   player_name == "Joshua Giddey" ~ "Josh Giddey",
                                   player_name == "Rj Barrett" ~ "RJ Barrett",
                                   player_name == "Michael Porter" ~ "Michael Porter Jr.",
                                   player_name == "Wendell Carter" ~ "Wendell Carter Jr.",
                                   player_name == "Zach Lavine" ~ "Zach LaVine",
                                   player_name == "S. Gilgeous-Alexander" ~ "Shai Gilgeous-Alexander",
                                   player_name == "De'aaron Fox" ~ "De'Aaron Fox",
                                   player_name == "Gary Payton Ii" ~ "Gary Payton II",
                                   player_name == "Nicolas Claxton" ~ "Nic Claxton",
                                   player_name == "Gary Trent" ~ "Gary Trent Jr.",
                                   player_name == "Cj Mccollum" ~ "CJ McCollum",
                                   .default = player_name)) |>
    left_join(player_names[, c("player_full_name", "team_name")], by = c("player_name" = "player_full_name")) |>
    mutate(opposition_team = if_else(home_team == team_name, away_team, home_team)) |> 
    transmute(
        match,
        home_team,
        away_team,
        market_name = "Player Assists",
        player_name,
        player_team = team_name,
        opposition_team,
        line,
        under_price = price,
        agency = "Pointsbet")

# Combine overs and unders
pointsbet_player_assists_over_under <- 
    pointsbet_player_assists_over |>
    left_join(pointsbet_player_assists_under) |>
    select(match, home_team, away_team, market_name, player_name, player_team, opposition_team, line, over_price, under_price, agency)

#===============================================================================
# Player Rebounds
#===============================================================================

# Player rebounds alternative totals----------------------------------------------

# Filter list to player rebounds
pointsbet_player_rebounds_lines <-
    pointsbet_data_player_props |>
    filter(str_detect(market, "Rebounds To Get|Alternate Rebounds")) |>
    mutate(line = str_extract(outcome, "[0-9]{1,2}")) |>
    mutate(line = as.numeric(line) - 0.5) |>
    mutate(match = str_replace(match, "@", "v")) |> 
    mutate(outcome = str_remove(outcome, " To Get.*$")) |> 
    separate(match, into = c("away_team", "home_team"), sep = " v ", remove = FALSE) |> 
    mutate(outcome = case_when(outcome == "Lebron James" ~ "LeBron James",
                               outcome == "D'angelo Russell" ~ "D'Angelo Russell",
                               outcome == "K. Caldwell-Pope" ~ "Kentavious Caldwell-Pope",
                               outcome == "De'andre Hunter" ~ "De'Andre Hunter",
                               outcome == "Lamelo Ball" ~ "LaMelo Ball",
                               outcome == "Fred Vanvleet" ~ "Fred VanVleet",
                               outcome == "Demar Derozan" ~ "DeMar DeRozan",
                               outcome == "Joshua Giddey" ~ "Josh Giddey",
                               outcome == "Rj Barrett" ~ "RJ Barrett",
                               outcome == "Michael Porter" ~ "Michael Porter Jr.",
                               outcome == "Wendell Carter" ~ "Wendell Carter Jr.",
                               outcome == "Zach Lavine" ~ "Zach LaVine",
                               outcome == "S. Gilgeous-Alexander" ~ "Shai Gilgeous-Alexander",
                               outcome == "De'aaron Fox" ~ "De'Aaron Fox",
                               outcome == "Gary Payton Ii" ~ "Gary Payton II",
                               outcome == "Nicolas Claxton" ~ "Nic Claxton",
                               outcome == "Gary Trent" ~ "Gary Trent Jr.",
                               outcome == "Cj Mccollum" ~ "CJ McCollum",
                               .default = outcome)) |>
    left_join(player_names[, c("player_full_name", "team_name")], by = c("outcome" = "player_full_name")) |>
    mutate(opposition_team = if_else(home_team == team_name, away_team, home_team)) |>
    transmute(
        match,
        home_team,
        away_team,
        market_name = "Player Rebounds",
        player_name = outcome,
        player_team = team_name,
        opposition_team,
        line,
        over_price = price,
        agency = "Pointsbet")

# Player rebounds over / under----------------------------------------------------


# Filter list to player assists over under
pointsbet_player_rebounds_over_under <-
    pointsbet_data_player_props |>
    filter(str_detect(market, "Player Rebounds Over/Under"))

# Get Overs
pointsbet_player_rebounds_over <-
    pointsbet_player_rebounds_over_under |> 
    filter(str_detect(outcome, "Over")) |>
    mutate(player_name = outcome) |>
    separate(outcome, into = c("player_name", "line"), sep = " Over ") |>
    mutate(line = as.numeric(line)) |> 
    mutate(match = str_replace(match, "@", "v")) |> 
    separate(match, into = c("away_team", "home_team"), sep = " v ", remove = FALSE) |> 
    mutate(player_name = case_when(player_name == "Lebron James" ~ "LeBron James",
                                   player_name == "D'angelo Russell" ~ "D'Angelo Russell",
                                   player_name == "K. Caldwell-Pope" ~ "Kentavious Caldwell-Pope",
                                   player_name == "De'andre Hunter" ~ "De'Andre Hunter",
                                   player_name == "Lamelo Ball" ~ "LaMelo Ball",
                                   player_name == "Fred Vanvleet" ~ "Fred VanVleet",
                                   player_name == "Demar Derozan" ~ "DeMar DeRozan",
                                   player_name == "Joshua Giddey" ~ "Josh Giddey",
                                   player_name == "Rj Barrett" ~ "RJ Barrett",
                                   player_name == "Michael Porter" ~ "Michael Porter Jr.",
                                   player_name == "Wendell Carter" ~ "Wendell Carter Jr.",
                                   player_name == "Zach Lavine" ~ "Zach LaVine",
                                   player_name == "S. Gilgeous-Alexander" ~ "Shai Gilgeous-Alexander",
                                   player_name == "De'aaron Fox" ~ "De'Aaron Fox",
                                   player_name == "Gary Payton Ii" ~ "Gary Payton II",
                                   player_name == "Nicolas Claxton" ~ "Nic Claxton",
                                   player_name == "Gary Trent" ~ "Gary Trent Jr.",
                                   player_name == "Cj Mccollum" ~ "CJ McCollum",
                                   .default = player_name)) |>
    left_join(player_names[, c("player_full_name", "team_name")], by = c("player_name" = "player_full_name")) |>
    mutate(opposition_team = if_else(home_team == team_name, away_team, home_team)) |> 
    transmute(
        match,
        home_team,
        away_team,
        market_name = "Player Rebounds",
        player_name,
        player_team = team_name,
        opposition_team,
        line,
        over_price = price,
        agency = "Pointsbet")

# Get Unders
pointsbet_player_rebounds_under <-
    pointsbet_player_rebounds_over_under |> 
    filter(str_detect(outcome, "Under")) |>
    mutate(player_name = outcome) |>
    separate(outcome, into = c("player_name", "line"), sep = " Under ") |>
    mutate(line = as.numeric(line)) |> 
    mutate(match = str_replace(match, "@", "v")) |> 
    separate(match, into = c("away_team", "home_team"), sep = " v ", remove = FALSE) |> 
    mutate(player_name = case_when(player_name == "Lebron James" ~ "LeBron James",
                                   player_name == "D'angelo Russell" ~ "D'Angelo Russell",
                                   player_name == "K. Caldwell-Pope" ~ "Kentavious Caldwell-Pope",
                                   player_name == "De'andre Hunter" ~ "De'Andre Hunter",
                                   player_name == "Lamelo Ball" ~ "LaMelo Ball",
                                   player_name == "Fred Vanvleet" ~ "Fred VanVleet",
                                   player_name == "Demar Derozan" ~ "DeMar DeRozan",
                                   player_name == "Joshua Giddey" ~ "Josh Giddey",
                                   player_name == "Rj Barrett" ~ "RJ Barrett",
                                   player_name == "Michael Porter" ~ "Michael Porter Jr.",
                                   player_name == "Wendell Carter" ~ "Wendell Carter Jr.",
                                   player_name == "Zach Lavine" ~ "Zach LaVine",
                                   player_name == "S. Gilgeous-Alexander" ~ "Shai Gilgeous-Alexander",
                                   player_name == "De'aaron Fox" ~ "De'Aaron Fox",
                                   player_name == "Gary Payton Ii" ~ "Gary Payton II",
                                   player_name == "Nicolas Claxton" ~ "Nic Claxton",
                                   player_name == "Gary Trent" ~ "Gary Trent Jr.",
                                   player_name == "Cj Mccollum" ~ "CJ McCollum",
                                   .default = player_name)) |>
    left_join(player_names[, c("player_full_name", "team_name")], by = c("player_name" = "player_full_name")) |>
    mutate(opposition_team = if_else(home_team == team_name, away_team, home_team)) |> 
    transmute(
        match,
        home_team,
        away_team,
        market_name = "Player Rebounds",
        player_name,
        player_team = team_name,
        opposition_team,
        line,
        under_price = price,
        agency = "Pointsbet")

# Combine overs and unders
pointsbet_player_rebounds_over_under <- 
    pointsbet_player_rebounds_over |>
    left_join(pointsbet_player_rebounds_under) |>
    select(match, home_team, away_team, market_name, player_name, player_team, opposition_team, line, over_price, under_price, agency)

#===============================================================================
# Write to CSV
#===============================================================================

# Points
pointsbet_player_points_lines |>
    bind_rows(pointsbet_player_points_over_under) |>
    mutate(match = paste(home_team, away_team, sep = " v ")) |> 
    select(
        "match",
        "home_team",
        "away_team",
        "market_name",
        "player_name",
        "player_team",
        "line",
        "over_price",
        "under_price",
        "agency",
        "opposition_team"
    ) |>
    mutate(market_name = "Player Points") |>
    mutate(agency = "Pointsbet") |> 
    write_csv("Data/scraped_odds/pointsbet_player_points.csv")

# Rebounds
pointsbet_player_rebounds_lines |>
    bind_rows(pointsbet_player_rebounds_over_under) |>
  mutate(match = paste(home_team, away_team, sep = " v ")) |> 
    select(
        "match",
        "home_team",
        "away_team",
        "market_name",
        "player_name",
        "player_team",
        "line",
        "over_price",
        "under_price",
        "agency",
        "opposition_team"
    ) |>
    mutate(market_name = "Player Rebounds") |>
    mutate(agency = "Pointsbet") |> 
    write_csv("Data/scraped_odds/pointsbet_player_rebounds.csv")

# Assists
pointsbet_player_assists_lines |>
    bind_rows(pointsbet_player_assists_over_under) |>
  mutate(match = paste(home_team, away_team, sep = " v ")) |> 
    select(
        "match",
        "home_team",
        "away_team",
        "market_name",
        "player_name",
        "player_team",
        "line",
        "over_price",
        "under_price",
        "agency",
        "opposition_team"
    ) |>
    mutate(market_name = "Player Assists") |>
    mutate(agency = "Pointsbet") |> 
    write_csv("Data/scraped_odds/pointsbet_player_assists.csv")
}

##%######################################################%##
#                                                          #
####                   Run functions                    ####
#                                                          #
##%######################################################%##

# This runs both the props and head to head as they use same info
h2h_safe_pointsbet <- safely(pointsbet_h2h_main)

# Run functions
h2h_safe_pointsbet()
