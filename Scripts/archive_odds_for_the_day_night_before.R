#===============================================================================
# Libraries and functions
#===============================================================================

library(tidyverse)
library(glue)
`%notin%` <- Negate(`%in%`)

#===============================================================================
# Read in scraped odds
#===============================================================================

# Get all scraped odds files and combine
all_odds_files <-
  list.files("Data/processed_odds", full.names = TRUE) |>
  map(read_rds) |>
  reduce(bind_rows) |> 
  mutate(timestamp = Sys.time()) |> 
  relocate(timestamp, .before = match) |> 
  write_csv(glue("Data/odds_archive/night_before/scraped_odds_night_before_{Sys.Date() + days(1)}.csv"))