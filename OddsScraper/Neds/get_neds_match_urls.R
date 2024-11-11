# Libraries
library(tidyverse)
library(rvest)
library(httr2)

# Read in json
neds_response <- jsonlite::fromJSON("OddsScraper/Neds/neds_response.json")

# Initialize empty lists to store data
event_name <- character()
event_id <- character()
competition_name <- character()

# Extract event IDs and names from JSON response
for (value in neds_response$events) {
  event_name <- c(event_name, ifelse(is.null(value$name), NA, value$name))
  event_id <- c(event_id, ifelse(is.null(value$id), NA, value$id))
  competition_name <- c(competition_name, ifelse(is.null(value$competition$name), NA, value$competition$name))
}

# Create a data frame from the vectors
df <- data.frame(event_name, event_id, competition_name)

# Filter the data frame to only include matches with ' vs ' in the event name
df <- df |> filter(str_detect(event_name, ' vs '))

# Only get NBA Games
df <- df |> filter(str_detect(competition_name, 'NBA'))

df$url <-
  paste0("https://www.neds.com.au/sports/basketball/usa/nba/",
         tolower(gsub(" ", "-", df$event_name)),
         "/",
         df$event_id)

# Write out as csv
write_csv(df, "OddsScraper/Neds/neds_nba_match_urls.csv")
