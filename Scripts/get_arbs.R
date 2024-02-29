# library(shiny)
library(bslib)
library(gridlayout)
library(DT)
library(tidyverse)
library(googlesheets4)
library(googledrive)


#===============================================================================
# Read in Data
#===============================================================================

# # Run all odds scraping scripts-----------------------------------------------
run_scraping <- function(script_name) {
  tryCatch({
    source(script_name)
  }, error = function(e) {
    cat("Odds not released yet for:", script_name, "\n")
  })
}

# Run all odds scraping scripts
# run_scraping("OddsScraper/scrape_betr.R")
run_scraping("OddsScraper/scrape_BetRight.R")
# run_scraping("OddsScraper/scrape_Palmerbet.R")
run_scraping("OddsScraper/scrape_pointsbet.R")
run_scraping("OddsScraper/scrape_sportsbet.R")
run_scraping("OddsScraper/scrape_TAB.R")
run_scraping("OddsScraper/scrape_TopSport.R")
# run_scraping("OddsScraper/scrape_bet365.R")
run_scraping("OddsScraper/scrape_bluebet.R")
run_scraping("OddsScraper/scrape_neds.R")
run_scraping("OddsScraper/scrape_unibet.R")

##%######################################################%##
#                                                          #
####                   Player Points                    ####
#                                                          #
##%######################################################%##

# Get all scraped odds files and combine
all_player_points <-
  list.files("Data/scraped_odds",
             full.names = TRUE,
             pattern = "player_points") |>
  map(read_csv) |>
  # Ignore null elements
  keep( ~ nrow(.x) > 0) |>
  reduce(bind_rows) |>
  arrange(player_name, line, desc(over_price)) |>
  select(-matches("id"))

##%######################################################%##
#                                                          #
####                   Player Assists                   ####
#                                                          #
##%######################################################%##

# Get all scraped odds files and combine
all_player_assists <-
  list.files("Data/scraped_odds",
             full.names = TRUE,
             pattern = "player_assists") |>
  map(read_csv) |>
  # Ignore null elements
  keep( ~ nrow(.x) > 0) |>
  reduce(bind_rows) |>
  select(-matches("id"))

##%######################################################%##
#                                                          #
####                  Player Rebounds                   ####
#                                                          #
##%######################################################%##

# Get all scraped odds files and combine
all_player_rebounds <-
  list.files("Data/scraped_odds",
             full.names = TRUE,
             pattern = "player_rebounds") |>
  map(read_csv) |>
  # Ignore null elements
  keep( ~ nrow(.x) > 0) |>
  reduce(bind_rows) |>
  select(-matches("id"))

##%######################################################%##
#                                                          #
####                   Player Steals                    ####
#                                                          #
##%######################################################%##

# Get all scraped odds files and combine
all_player_steals <-
  list.files("Data/scraped_odds",
             full.names = TRUE,
             pattern = "player_steals") |>
  map(read_csv) |>
  # Ignore null elements
  keep( ~ nrow(.x) > 0) |>
  reduce(bind_rows) |>
  select(-matches("id"))

##%######################################################%##
#                                                          #
####                   Player Blocks                    ####
#                                                          #
##%######################################################%##

# Get all scraped odds files and combine
all_player_blocks <-
  list.files("Data/scraped_odds",
             full.names = TRUE,
             pattern = "player_blocks") |>
  map(read_csv) |>
  # Ignore null elements
  keep( ~ nrow(.x) > 0) |>
  reduce(bind_rows) |>
  select(-matches("id"))

##%######################################################%##
#                                                          #
####                   Player Threes                    ####
#                                                          #
##%######################################################%##

# Get all scraped odds files and combine
all_player_threes <-
  list.files("Data/scraped_odds",
             full.names = TRUE,
             pattern = "player_threes") |>
  map(read_csv) |>
  # Ignore null elements
  keep( ~ nrow(.x) > 0) |>
  reduce(bind_rows) |>
  select(-matches("id"))

##%######################################################%##
#                                                          #
####                    Player PRAs                     ####
#                                                          #
##%######################################################%##

# Get all scraped odds files and combine
all_player_pras <-
  list.files("Data/scraped_odds",
             full.names = TRUE,
             pattern = "player_pras") |>
  map(read_csv) |>
  # Ignore null elements
  keep( ~ nrow(.x) > 0) |>
  reduce(bind_rows) |>
  select(-matches("id"))

##%######################################################%##
#                                                          #
####   Get all over under comparisons of same market    ####
#                                                          #
##%######################################################%##

# Points------------------------------------------------------------------------
points_unders <-
  all_player_points |>
  filter(market_name == "Player Points") |>
  select(
    match,
    market_name,
    player_name,
    player_team,
    line,
    under_price,
    opposition_team,
    agency
  ) |>
  filter(!is.na(under_price)) |>
  rename(under_agency = agency)

points_overs <-
  all_player_points |>
  filter(market_name == "Player Points") |>
  select(
    match,
    market_name,
    player_name,
    player_team,
    line,
    over_price,
    opposition_team,
    agency
  ) |>
  rename(over_agency = agency)

points_arbs <-
  points_unders |>
  inner_join(
    points_overs,
    by = c(
      "match",
      "market_name",
      "player_name",
      "player_team",
      "line",
      "opposition_team"
    ),
    relationship = "many-to-many"
  ) |>
  relocate(under_price, .after = over_price) |>
  mutate(margin = 1 / under_price + 1 / over_price) |>
  arrange(margin) |>
  mutate(margin = (1 - margin)) |>
  mutate(margin = 100 * margin) |>
  filter(margin > 0) |>
  distinct(match, player_name, line, over_agency, under_agency, .keep_all = TRUE) |>
  relocate(over_price, over_agency, under_price, under_agency, .after = opposition_team)

# Rebounds----------------------------------------------------------------------
rebounds_unders <-
  all_player_rebounds |>
  filter(market_name == "Player Rebounds") |>
  select(
    match,
    market_name,
    player_name,
    player_team,
    line,
    under_price,
    opposition_team,
    agency
  ) |>
  filter(!is.na(under_price)) |>
  rename(under_agency = agency)

rebounds_overs <-
  player_rebounds_data |>
  filter(market_name == "Player Rebounds") |>
  select(
    match,
    market_name,
    player_name,
    player_team,
    line,
    over_price,
    opposition_team,
    agency
  ) |>
  rename(over_agency = agency)

rebounds_arbs <-
  rebounds_unders |>
  inner_join(
    rebounds_overs,
    by = c(
      "match",
      
      "market_name",
      "player_name",
      "player_team",
      "line",
      "opposition_team"
    ),
    relationship = "many-to-many"
  ) |>
  relocate(under_price, .after = over_price) |>
  mutate(margin = 1 / under_price + 1 / over_price) |>
  arrange(margin) |>
  mutate(margin = (1 - margin)) |>
  mutate(margin = 100 * margin) |>
  filter(margin > 0) |>
  distinct(match, player_name, line, over_agency, under_agency, .keep_all = TRUE) |>
  relocate(over_price, over_agency, under_price, under_agency, .after = opposition_team)


# Assists-----------------------------------------------------------------------
assists_unders <-
  all_player_assists |>
  filter(market_name == "Player Assists") |>
  select(
    match,
    market_name,
    player_name,
    player_team,
    line,
    under_price,
    opposition_team,
    agency
  ) |>
  filter(!is.na(under_price)) |>
  rename(under_agency = agency)

assists_overs <-
  player_assists_data |>
  filter(market_name == "Player Assists") |>
  select(
    match,
    market_name,
    player_name,
    player_team,
    line,
    over_price,
    opposition_team,
    agency
  ) |>
  rename(over_agency = agency)

assists_arbs <-
  assists_unders |>
  inner_join(
    assists_overs,
    by = c(
      "match",
      
      "market_name",
      "player_name",
      "player_team",
      "line",
      "opposition_team"
    ),
    relationship = "many-to-many"
  ) |>
  relocate(under_price, .after = over_price) |>
  mutate(margin = 1 / under_price + 1 / over_price) |>
  arrange(margin) |>
  mutate(margin = (1 - margin)) |>
  mutate(margin = 100 * margin) |>
  filter(margin > 0) |>
  distinct(match, player_name, line, over_agency, under_agency, .keep_all = TRUE) |>
  relocate(over_price, over_agency, under_price, under_agency, .after = opposition_team)


# PRAs--------------------------------------------------------------------------
pra_unders <-
  all_player_pras |>
  filter(market_name == "Player PRAs") |>
  select(
    match,
    market_name,
    player_name,
    player_team,
    line,
    under_price,
    opposition_team,
    agency
  ) |>
  filter(!is.na(under_price)) |>
  rename(under_agency = agency)

pra_overs <-
  player_pras_data |>
  filter(market_name == "Player PRAs") |>
  select(
    match,
    market_name,
    player_name,
    player_team,
    line,
    over_price,
    opposition_team,
    agency
  ) |>
  rename(over_agency = agency)

pra_arbs <-
  pra_unders |>
  inner_join(
    pra_overs,
    by = c(
      "match",
      
      "market_name",
      "player_name",
      "player_team",
      "line",
      "opposition_team"
    ),
    relationship = "many-to-many"
  ) |>
  relocate(under_price, .after = over_price) |>
  mutate(margin = 1 / under_price + 1 / over_price) |>
  arrange(margin) |>
  mutate(margin = (1 - margin)) |>
  mutate(margin = 100 * margin) |>
  filter(margin > 0) |>
  distinct(match, player_name, line, over_agency, under_agency, .keep_all = TRUE) |>
  relocate(over_price, over_agency, under_price, under_agency, .after = opposition_team)


# Threes------------------------------------------------------------------------
threes_unders <-
  all_player_threes |>
  filter(market_name == "Player Threes") |>
  select(
    match,
    market_name,
    player_name,
    player_team,
    line,
    under_price,
    opposition_team,
    agency
  ) |>
  filter(!is.na(under_price)) |>
  rename(under_agency = agency)

threes_overs <-
  player_threes_data |>
  filter(market_name == "Player Threes") |>
  select(
    match,
    market_name,
    player_name,
    player_team,
    line,
    over_price,
    opposition_team,
    agency
  ) |>
  rename(over_agency = agency)

threes_arbs <-
  threes_unders |>
  inner_join(
    threes_overs,
    by = c(
      "match",
      
      "market_name",
      "player_name",
      "player_team",
      "line",
      "opposition_team"
    ),
    relationship = "many-to-many"
  ) |>
  relocate(under_price, .after = over_price) |>
  mutate(margin = 1 / under_price + 1 / over_price) |>
  arrange(margin) |>
  mutate(margin = (1 - margin)) |>
  mutate(margin = 100 * margin) |>
  filter(margin > 0) |>
  distinct(match, player_name, line, over_agency, under_agency, .keep_all = TRUE) |>
  relocate(over_price, over_agency, under_price, under_agency, .after = opposition_team)


# Steals------------------------------------------------------------------------
steals_unders <-
  all_player_steals |>
  filter(market_name == "Player Steals") |>
  select(
    match,
    market_name,
    player_name,
    player_team,
    line,
    under_price,
    opposition_team,
    agency
  ) |>
  filter(!is.na(under_price)) |>
  rename(under_agency = agency)

steals_overs <-
  player_steals_data |>
  filter(market_name == "Player Steals") |>
  select(
    match,
    market_name,
    player_name,
    player_team,
    line,
    over_price,
    opposition_team,
    agency
  ) |>
  rename(over_agency = agency)

steals_arbs <-
  steals_unders |>
  inner_join(
    steals_overs,
    by = c(
      "match",
      
      "market_name",
      "player_name",
      "player_team",
      "line",
      "opposition_team"
    ),
    relationship = "many-to-many"
  ) |>
  relocate(under_price, .after = over_price) |>
  mutate(margin = 1 / under_price + 1 / over_price) |>
  arrange(margin) |>
  mutate(margin = (1 - margin)) |>
  mutate(margin = 100 * margin) |>
  filter(margin > 0) |>
  distinct(match, player_name, line, over_agency, under_agency, .keep_all = TRUE) |>
  relocate(over_price, over_agency, under_price, under_agency, .after = opposition_team)


# Blocks------------------------------------------------------------------------
blocks_unders <-
  all_player_blocks |>
  filter(market_name == "Player Blocks") |>
  select(
    match,
    market_name,
    player_name,
    player_team,
    line,
    under_price,
    opposition_team,
    agency
  ) |>
  filter(!is.na(under_price)) |>
  rename(under_agency = agency)

blocks_overs <-
  player_blocks_data |>
  filter(market_name == "Player Blocks") |>
  select(
    match,
    market_name,
    player_name,
    player_team,
    line,
    over_price,
    opposition_team,
    agency
  ) |>
  rename(over_agency = agency)

blocks_arbs <-
  blocks_unders |>
  inner_join(
    blocks_overs,
    by = c(
      "match",
      
      "market_name",
      "player_name",
      "player_team",
      "line",
      "opposition_team"
    ),
    relationship = "many-to-many"
  ) |>
  relocate(under_price, .after = over_price) |>
  mutate(margin = 1 / under_price + 1 / over_price) |>
  arrange(margin) |>
  mutate(margin = (1 - margin)) |>
  mutate(margin = 100 * margin) |>
  filter(margin > 0) |>
  distinct(match, player_name, line, over_agency, under_agency, .keep_all = TRUE) |>
  relocate(over_price, over_agency, under_price, under_agency, .after = opposition_team)

#===============================================================================
# Get all ARBs together
#===============================================================================

all_arbs <-
  bind_rows(
    points_arbs,
    assists_arbs,
    rebounds_arbs,
    blocks_arbs,
    steals_arbs,
    threes_arbs,
    pra_arbs
  ) |>
  arrange(desc(margin))

