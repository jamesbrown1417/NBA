# Libraries
library(tidyverse)
library(rvest)
library(httr2)
library(jsonlite)

# URL to get responses
competitions_api_url = "https://fixture.palmerbet.online/fixtures/sports/1c2eeb3a-6bab-4ac2-b434-165cc350180f/matches?sportType=basketball&pageSize=24&channel=website"

# Fix team names function
source("Scripts/fix_team_names.R")

# Make request and get response
palmerbet_response <-
  request(competitions_api_url) |>
  req_perform() |>
  resp_body_json()

# Get palmerbet matches
matches <- palmerbet_response$matches

# Map over list, get home team title and away team title------------------------
get_fixture_name <- function(match) {
  home_team <- match$homeTeam$title
  away_team <- match$awayTeam$title
  return(paste(fix_team_names(home_team), "v", fix_team_names(away_team)))
}

fixture_names <- map_chr(matches, get_fixture_name)

# Get event IDs
get_event_id <- function(match) {
  event_id <- match$eventId
  return(event_id)
}

event_ids <- map_chr(matches, get_event_id)

# Map over list, get markets----------------------------------------------------
get_markets <- function(match) {
  markets <- match$additionalMarkets
  return(markets)
}

markets_list <-
  map(matches, get_markets)

extract_market_data <- function(list_of_markets) {
  # Parse the JSON string into a list
  market_data <- list_of_markets
  
  # Extract the relevant information into a tibble
  tibble(
    marketId = map_chr(market_data, ~ .x$marketId[[1]]),
    marketTitle = map_chr(market_data, ~ .x$title[[1]]),
    marketType = map_chr(market_data, ~ .x$type[[1]]),
    handicap = map_dbl(market_data, ~ ifelse(is.null(.x$handicap), NA, .x$handicap[[1]]))
  ) %>%
    # Unnest outcomes
    mutate(outcomes = map(market_data, ~ .x$outcomes)) %>%
    unnest_longer(outcomes) %>%
    mutate(
      outcomeId = map_chr(outcomes, ~ .x$outcomeId[[1]]),
      outcomeTitle = map_chr(outcomes, ~ .x$title[[1]]),
      price = map_dbl(outcomes, ~ .x$price[[1]]),
      outcomeType = map_chr(outcomes, ~ .x$type[[1]])
    ) %>%
    select(-outcomes)  # Drop the nested outcomes column
}

markets_data <-
  map(markets_list, extract_market_data)

markets_data_full <-
  map2(markets_data, fixture_names, ~ mutate(.x, match = .y)) |> 
  map2(event_ids, ~ mutate(.x, event_id = .y)) |>
  bind_rows()

# Get URL to get props----------------------------------------------------------

# Distinct IDs
distinct_ids <-
  markets_data_full %>%
  distinct(event_id)

prop_urls <- glue::glue("https://fixture.palmerbet.online/fixtures/sports/matches/{distinct_ids$event_id}/markets?sportType=basketball&pageSize=1000&channel=website")

# Make Request for each---------------------------------------------------------

# Function to get props
get_props <- function(url) {
  # Make request
  response <- request(url) |>
    req_perform() |>
    resp_body_json()
  
  # Get props
  props <- response$markets
  
  # Extract the relevant information into a tibble
  sub_markets_tibble <-
  tibble(
    marketTitle = map_chr(props, ~ .x$title),
    ID = map_chr(props, ~ .x$id),
    links = map_chr(props, ~ .x$`_links`[[1]]$href),
    url = url)
  
  # Return
  return(sub_markets_tibble)
}
