# Libraries
library(tidyverse)
library(rvest)
library(httr2)
library(jsonlite)
library(tidyjson)

# Load user functions
source("Scripts/04-helper-functions.R")

# URL of website
betr_url = "https://api.betr.ext.thebetmakers.com/api/v4/combined/sports/basecompetition/events-with-markets?base_competition_id=1000634&main_markets_only=false"

# Make request and get response
betr_response <-
    request(betr_url) |>
    req_perform() |> 
    resp_body_json()

# List of matches and data
events <- betr_response$data$competitions[[1]]$events

# Extract info in a loop--------------------------------------------------------

# Create empty vectors
outcome_names <- c()
outcome_prices <- c()
market_names <- c()
match_names <- c()
match_start_dates <- c()

# Loop through events
for (match in events) {
    for (market in match$markets) {
        for (outcome in market$selections) {
            # Append data to vectors
            outcome_names <- c(outcome_names, outcome$name)
            outcome_prices <- c(outcome_prices, outcome$price)
            market_names <- c(market_names, market$name)
            match_names <- c(match_names, match$name)
            match_start_dates <- c(match_start_dates, match$start_date)
        }
    }
}

# Output tibble
betr_data <-
    tibble(
        match = match_names,
        start_time = match_start_dates,
        market = market_names,
        outcome = outcome_names,
        price = outcome_prices
    ) |> 
    separate(match, c("home_team", "away_team"), sep = " v ") |> 
    mutate(home_team = fix_team_names(home_team),
           away_team = fix_team_names(away_team)) |> 
    mutate(match = paste(home_team, "v", away_team)) |> 
    relocate(match, .before = start_time)

#===============================================================================
# Head to head markets
#===============================================================================

# Filter to head to head markets
betr_data_h2h <-
    betr_data |> 
    filter(market == "head to head") |> 
    separate(match, c("home_team", "away_team"), sep = " v ", remove = FALSE)

# Home Teams
betr_data_h2h_home <-
    betr_data_h2h |> 
    filter(home_team == outcome) |>
    select(match, start_time, market, home_team, home_win = price)

# Away Teams
betr_data_h2h_away <-
    betr_data_h2h |> 
    filter(away_team == outcome) |>
    select(match, start_time, market, away_team, away_win = price)

# Combine
betr_h2h <-
    full_join(betr_data_h2h_home, betr_data_h2h_away, by = c("match", "start_time", "market")) |> 
    mutate(market = "Head To Head") |>
    select(match, start_time, market_name = market, home_team, home_win, away_team, away_win) |> 
    mutate(margin = round((1/home_win + 1/away_win), digits = 3)) |> 
    mutate(agency = "Betr")

# Write to csv
write_csv(betr_h2h, "Data/scraped_odds/betr_h2h.csv")
