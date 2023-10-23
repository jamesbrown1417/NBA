# Libraries
library(tidyverse)
library(rvest)
library(httr2)
library(glue)

# Load user functions
source("Scripts/04-helper-functions.R")

# Get player name and team data
player_names_teams <-
    read_csv("Data/supercoach-data.csv") |> 
    mutate(first_initial = str_sub(player_first_name, 1, 1)) |>
    select(player_first_name, first_initial, player_last_name, player_team) |> 
    mutate(player_name_initials = paste(first_initial, player_last_name, sep = " ")) |> 
    mutate(player_full_name = paste(player_first_name, player_last_name, sep = " "))


# Get schedule
nbl_schedule <- read_rds("Data/season_schedule_2023_2024.rds")

# Add current datetime
nbl_schedule <- nbl_schedule |> 
    mutate(current_timestamp = Sys.time()) |> 
    mutate(home_team = fix_team_names(home_team), away_team = fix_team_names(away_team)) |> 
    mutate(match = paste(home_team, "v", away_team, sep = " "))

# Get home teams and away teams next match
home_teams <-
    nbl_schedule |> 
    rename(team = home_team, opposition_team = away_team) 

away_teams <-
    nbl_schedule |> 
    rename(team = away_team, opposition_team = home_team)

# Get next match
next_match <-
    bind_rows(home_teams, away_teams) |>
    arrange(team, start_time) |> 
    filter(current_timestamp <= start_time) |> 
    group_by(team) |>
    slice_head(n = 1) |>
    mutate(team = fix_team_names(team), opposition_team = fix_team_names(opposition_team))

# URL to get responses
betright_url = "https://next-api.betright.com.au/Sports/Category?categoryId=110"

# Make request and get response
betright_response <-
    request(betright_url) |>
    req_perform() |> 
    resp_body_json()

# Get events list
events_list <- betright_response$masterCategories[[1]]$categories[[1]]$masterEvents

# Get only elements of list with masterEventClassName = "Matches"
events_list <- map(events_list, function(x) if (x$masterEventClassName == "Matches") x else NULL) |> compact()

# Function to extract market info from response---------------------------------
get_market_info <- function(markets) {
    
    # Market info
    markets_name = markets$eventName
    market_propositions = markets$outcomeName
    market_prices = markets$price
    
    # Output Tibble
    tibble(market = markets_name,
           propositions = market_propositions,
           prices = market_prices)
}


# Function to extract match info from response----------------------------------
get_match_info <- function(matches) {
    # Match info
    match_name = matches$masterEventName
    match_start_time = matches$minAdvertisedStartTimeUtc
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
    map(events_list, get_match_info) |> bind_rows()

#===============================================================================
# Head to head markets
#===============================================================================

# Home teams
home_teams <-
    all_betright_markets |>
    separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
    filter(market_name == "Money Line") |> 
    mutate(market_name = "Head To Head") |> 
    group_by(match) |> 
    filter(row_number() == 1) |> 
    rename(home_win = prices) |> 
    select(-propositions)

# Away teams
away_teams <-
    all_betright_markets |>
    separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
    filter(market_name == "Money Line") |> 
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
    mutate(home_team = fix_team_names(home_team)) |>
    mutate(away_team = fix_team_names(away_team)) |>
    mutate(match = paste(home_team, "v", away_team))

# Write to csv
write_csv(betright_head_to_head_markets, "Data/scraped_odds/betright_h2h.csv")

#===============================================================================
# Player Props
#===============================================================================

# Get API URL for each market type----------------------------------------------

# Player Stats
player_points_links <-
    glue("https://next-api.betright.com.au/Sports/MasterEvent?masterEventId={unique(all_betright_markets$match_id)}&groupTypeCode=G670")

player_assist_links <-
    glue("https://next-api.betright.com.au/Sports/MasterEvent?masterEventId={unique(all_betright_markets$match_id)}&groupTypeCode=G672")

player_rebound_links <-
    glue("https://next-api.betright.com.au/Sports/MasterEvent?masterEventId={unique(all_betright_markets$match_id)}&groupTypeCode=G673")


player_stats_links <- c(player_points_links,player_assist_links,player_rebound_links)

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
        price = price
    )
}

# Safe version of function
safe_get_prop_data <- safely(get_prop_data)

# Get all player stats
betright_player_stats <-
    map(player_stats_links, safe_get_prop_data) |> 
    map("result") |>
    bind_rows() |>
    separate(outcome_title, into = c("market_name", "player_name"), sep = " - ") |>
    mutate(player_name = str_remove_all(player_name, " \\(.*\\)")) |>
    mutate(player_name = str_replace_all(player_name, "  ", " ")) |>  
    mutate(player_name = str_replace(player_name, "^Mitch", "Mitchell")) |>
    mutate(player_name = str_replace(player_name, "Lee Jr.", "Lee")) |>
    left_join(player_names_teams[, c("player_full_name", "player_team")], by = c("player_name" = "player_full_name")) |>
    left_join(next_match[, c("team", "opposition_team", "match")], by = c("player_team" = "team")) |> 
    separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |> 
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
        "opposition_team"
    )

# Get player points data--------------------------------------------------------
betright_player_stats |> 
    filter(market_name == "Player Points") |>
    write_csv("Data/scraped_odds/betright_player_points.csv")

# Get player rebounds data------------------------------------------------------
betright_player_stats |> 
    filter(market_name == "Player Rebounds") |>
    write_csv("Data/scraped_odds/betright_player_rebounds.csv")

# Get player assists data-------------------------------------------------------
betright_player_stats |> 
    filter(market_name == "Player Assists") |>
    write_csv("Data/scraped_odds/betright_player_assists.csv")

