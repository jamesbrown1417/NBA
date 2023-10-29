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
  list.files("Data/scraped_odds", full.names = TRUE) |>
  map(read_csv) |>
  reduce(bind_rows) |> 
  mutate(timestamp = Sys.time()) |> 
  relocate(timestamp, .before = match) |> 
  write_csv(glue("Data/odds_archive/scraped_odds_{Sys.Date()}.csv"))