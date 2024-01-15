# Libraries
library(tidyverse)
library(rvest)
library(httr2)
library(glue)

# URL to get responses
betmakers_url = "https://api.betdeluxe.com.au/api/v2/combined/sports/basecompetition/events-with-markets?base_competition_id=1000059"

# Fix team names function
source("Scripts/fix_team_names.R")

# Make request and get response
betmakers_response <-
  request(betmakers_url) |>
  req_perform() |> 
  resp_body_json()

matches <- betmakers_response$data$competitions[[1]]$events

match_names <- c()
start_dates <- c()
fixture_id <- c()

for (match in matches) {
  match_names <- c(match_names, match$name)
  start_dates <- c(start_dates, match$start_date)
  fixture_id <- c(fixture_id, match$sportcast_uri)
}

betmakers_data <-
  tibble(
    match = match_names,
    start_time = start_dates,
    fixture_id = fixture_id
  ) |> 
  separate(match, c("home_team", "away_team"), sep = " v ") |> 
  mutate(home_team = fix_team_names(home_team),
         away_team = fix_team_names(away_team)) |> 
  mutate(match = paste(home_team, "v", away_team)) |> 
  relocate(match, .before = start_time) |> 
  mutate(fixture_id = str_extract(fixture_id, "key=.*\\&FixtureId=.*\\&"))

#===============================================================================
# Get SGM Service URLs
#===============================================================================

# SGM URLs
sgm_urls <- glue("https://betmakers-prod-gen2.sportcastlive.com/public/getmarketsV2/?{betmakers_data$fixture_id}culture=en-GB&returnFilters=true")

# Function to get SGM data
get_sgm_data <- function(url) {
  
  # Make request and get response
  sgm_response <-
    request(url) |>
    req_perform() |> 
    resp_body_json()
  
  # Get data
  payload <- sgm_response$PayLoad
  
  # empty vectors to append to
  MarketId <- c()
  MarketLabelId <- c()
  MarketName <- c()
  EntityId <- c()
  GlobalIdLong <- c()
  GlobalIdShort <- c()
  Id <- c()
  Selection <- c()
  Items <- c()
  
  # Add in loop
  for (match in payload) {
    for (item in match$Filter$Items) {
      # Append values to vectors, checking for null (NA)
      MarketName <- c(MarketName, ifelse(is.null(match$Label), NA, match$Label))
      MarketLabelId <- c(MarketLabelId, ifelse(is.null(match$LabelId), NA, match$LabelId))
      MarketId <- c(MarketId, ifelse(is.null(match$Id), NA, match$Id))
      Id <- c(Id, ifelse(is.null(item$Id), NA, item$Id))
      EntityId <- c(EntityId, ifelse(is.null(item$EntityId), NA, item$EntityId))
      GlobalIdShort <- c(GlobalIdShort, ifelse(is.null(item$GlobalIdShort), NA, item$GlobalIdShort))
      GlobalIdLong <- c(GlobalIdLong, ifelse(is.null(item$GlobalIdLong), NA, item$GlobalIdLong))
      Selection <- c(Selection, ifelse(is.null(item$Value), NA, item$Value))
      Items <- c(Items, ifelse(is.null(item$Items), NA, item$Items))
    }
  }
  
  
  # Output tibble
  sgm_data <-
    tibble(
      MarketId = MarketId,
      MarketLabelId = MarketLabelId,
      MarketName = MarketName,
      EntityId = EntityId,
      # GlobalIdLong = GlobalIdLong,
      # GlobalIdShort = GlobalIdShort,
      Id = Id,
      Selection = Selection,
      Items = Items
    )

}
