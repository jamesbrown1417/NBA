# Libraries
library(tidyverse)
library(rvest)
library(httr2)

# Load user functions
source("Scripts/04-helper-functions.R")

# Get player name and team data
player_names_teams <-
    read_csv("Data/supercoach-data.csv") |> 
    mutate(first_initial = str_sub(player_first_name, 1, 1)) |>
    select(player_first_name, first_initial, player_last_name, player_team) |> 
    mutate(player_name_initials = paste(first_initial, player_last_name, sep = " ")) |> 
    mutate(player_full_name = paste(player_first_name, player_last_name, sep = " "))

# URL to get responses
bluebet_url = "https://web20-api.bluebet.com.au/SportsCategory?CategoryId=39384&format=json"

# Make request and get response
bluebet_response <-
    request(bluebet_url) |>
    req_perform() |> 
    resp_body_json()

# Get matches
matches <- bluebet_response$MasterCategories[[1]]$Categories[[1]]$MasterEvents

# Function to extract market info from response---------------------------------
get_market_info <- function(market) {
    
    # Market info
    markets_name = market$EventName
    market_propositions = market$OutcomeName
    market_prices = market$Price
    
    # Output Tibble
    tibble(market = markets_name,
           propositions = market_propositions,
           prices = market_prices)
}


# Function to extract match info from response----------------------------------
get_match_info <- function(matches) {
    # Match info
    match_name = matches$MasterEventName
    match_start_time = matches$MinAdvertisedStartTime
    match_id = matches$MasterEventId
    
    # Market info
    market_info = map(matches$Markets, get_market_info) |> bind_rows()
    
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
all_bluebet_markets <-
    map(matches, get_match_info) |> bind_rows()

#===============================================================================
# Head to head markets
#===============================================================================

# Home teams
home_teams <-
    all_bluebet_markets |>
    separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
    filter(str_detect(market_name, "Money Line")) |> 
    mutate(market_name = "Head To Head") |> 
    group_by(match) |> 
    filter(row_number() == 1) |> 
    rename(home_win = prices) |> 
    select(-propositions)

# Away teams
away_teams <-
    all_bluebet_markets |>
    separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
    filter(str_detect(market_name, "Money Line")) |> 
    mutate(market_name = "Head To Head") |>
    group_by(match) |> 
    filter(row_number() == 2) |> 
    rename(away_win = prices) |> 
    select(-propositions)

# Combine
bluebet_head_to_head_markets <-
    home_teams |>
    left_join(away_teams) |> 
    select(match, start_time, market_name, home_team, home_win, away_team, away_win) |> 
    mutate(margin = round((1/home_win + 1/away_win), digits = 3)) |> 
    mutate(agency = "Bluebet")

# Fix team names
bluebet_head_to_head_markets <-
    bluebet_head_to_head_markets |> 
    mutate(home_team = fix_team_names(home_team)) |>
    mutate(away_team = fix_team_names(away_team)) |>
    mutate(match = paste(home_team, "v", away_team))

# Write to csv
write_csv(bluebet_head_to_head_markets, "Data/scraped_odds/bluebet_h2h.csv")

#===============================================================================
# Player Props
#===============================================================================

# Get API URL for each market type----------------------------------------------

# Player Points
player_points_links <-
    glue("https://web20-api.bluebet.com.au/MasterEvent?GroupTypeCode=G199&MasterEventId={unique(all_bluebet_markets$match_id)}&format=json")

# Player Assists Links
player_assists_links <-
    glue("https://web20-api.bluebet.com.au/MasterEvent?GroupTypeCode=G198&MasterEventId={unique(all_bluebet_markets$match_id)}&format=json")

# Player Rebounds Links
player_rebounds_links <-
    glue("https://web20-api.bluebet.com.au/MasterEvent?GroupTypeCode=G200&MasterEventId={unique(all_bluebet_markets$match_id)}&format=json")

# Player 3 Pointers Links
player_3_pointers_links <-
    glue("https://web20-api.bluebet.com.au/MasterEvent?GroupTypeCode=G202&MasterEventId={unique(all_bluebet_markets$match_id)}&format=json")

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
    
    for (event in response$Events) {
        for (outcome in event$Outcomes) {
            event_name <- c(event_name, event$EventName)
            outcome_title <- c(outcome_title, outcome$EventName)
            outcome_name <- c(outcome_name, outcome$OutcomeName)
            price <- c(price, outcome$Price)
        }
    }
    
    # Output Tibble
    tibble(
        event_name = event_name,
        outcome_title = outcome_title,
        outcome_name = outcome_name,
        price = price
    )
}

# Safe version of function
safe_get_prop_data <- safely(get_prop_data)

# Get player points data--------------------------------------------------------

bluebet_player_points <-
map(player_points_links, safe_get_prop_data) |> 
    map("result") |>
    bind_rows() |>
    separate(outcome_title, into = c("market_name", "player_name"), sep = " - ") |>
    mutate(match = str_extract(event_name, "\\(.*\\)")) |>
    mutate(match = str_remove(match, "\\(|\\)")) |>
    separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
    mutate(home_team = fix_team_names(home_team)) |>
    mutate(away_team = fix_team_names(away_team)) |>
    mutate(match = paste(home_team, "v", away_team)) |>
    mutate(player_name = str_replace(player_name, "^Mitch", "Mitchell")) |>
    left_join(player_names_teams[, c("player_full_name", "player_team")], by = c("player_name" = "player_full_name")) |>
    mutate(opposition_team = if_else(home_team == player_team, away_team, home_team)) |>
    mutate(agency = "BlueBet") |>
    mutate(line = as.numeric(str_remove(outcome_name, "\\+")) - 0.5) |>
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
        "opposition_team"
    )

# Get player assists data-------------------------------------------------------
bluebet_player_assists <-
    map(player_assists_links, safe_get_prop_data) |> 
    map("result") |>
    bind_rows() |>
    separate(outcome_title, into = c("market_name", "player_name"), sep = " - ") |>
    mutate(match = str_extract(event_name, "\\(.*\\)")) |>
    mutate(match = str_remove(match, "\\(|\\)")) |>
    separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
    mutate(home_team = fix_team_names(home_team)) |>
    mutate(away_team = fix_team_names(away_team)) |>
    mutate(match = paste(home_team, "v", away_team)) |>
    mutate(player_name = str_replace(player_name, "^Mitch", "Mitchell")) |>
    left_join(player_names_teams[, c("player_full_name", "player_team")], by = c("player_name" = "player_full_name")) |>
    mutate(opposition_team = if_else(home_team == player_team, away_team, home_team)) |>
    mutate(agency = "BlueBet") |>
    mutate(line = as.numeric(str_remove(outcome_name, "\\+")) - 0.5) |>
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
        "opposition_team"
    )

# Get player rebounds data------------------------------------------------------
bluebet_player_rebounds <-
    map(player_rebounds_links, safe_get_prop_data) |> 
    map("result") |>
    bind_rows() |>
    separate(outcome_title, into = c("market_name", "player_name"), sep = " - ") |>
    mutate(match = str_extract(event_name, "\\(.*\\)")) |>
    mutate(match = str_remove(match, "\\(|\\)")) |>
    separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
    mutate(home_team = fix_team_names(home_team)) |>
    mutate(away_team = fix_team_names(away_team)) |>
    mutate(match = paste(home_team, "v", away_team)) |>
    mutate(player_name = str_replace(player_name, "^Mitch", "Mitchell")) |>
    left_join(player_names_teams[, c("player_full_name", "player_team")], by = c("player_name" = "player_full_name")) |>
    mutate(opposition_team = if_else(home_team == player_team, away_team, home_team)) |>
    mutate(agency = "BlueBet") |>
    mutate(line = as.numeric(str_remove(outcome_name, "\\+")) - 0.5) |>
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
        "opposition_team"
    )

#===============================================================================
# Write to CSV
#===============================================================================

bluebet_player_points |> write_csv("Data/scraped_odds/bluebet_player_points.csv")
bluebet_player_assists |> write_csv("Data/scraped_odds/bluebet_player_assists.csv")
bluebet_player_rebounds |> write_csv("Data/scraped_odds/bluebet_player_rebounds.csv")
