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
run_scraping("OddsScraper/scrape_bet365.R")
run_scraping("OddsScraper/scrape_bluebet.R")
run_scraping("OddsScraper/Neds/scrape_neds.R")
run_scraping("OddsScraper/scrape_unibet.R")
run_scraping("OddsScraper/scrape_dabble.R")

##%######################################################%##
#                                                          #
####                    Head to Head                    ####
#                                                          #
##%######################################################%##

# Get all scraped odds files and combine
all_odds_files <-
  list.files("Data/scraped_odds", full.names = TRUE, pattern = "h2h") |>
  map(read_csv) |>
  # Keep if nrow of dataframe greater than 0
  keep(~nrow(.x) > 0) |>
  reduce(bind_rows)

# Get Start Times
start_times <-
  all_odds_files |>
  filter(agency == "TAB") |> 
  select(match, start_time) |>
  distinct(match, .keep_all = TRUE)

# For each match, get all home wins
all_home <-
  all_odds_files |>
  arrange(match, start_time, desc(home_win)) |>
  select(match, start_time, market_name, home_team, home_win, home_agency = agency) |> 
  mutate(start_time = date(start_time)) |> 
  select(-start_time)

# For each match, get all away wins
all_away <-
  all_odds_files |>
  arrange(match, start_time, desc(away_win)) |>
  select(match, start_time, market_name, away_team, away_win, away_agency = agency) |> 
  mutate(start_time = date(start_time)) |> 
  select(-start_time)

# Combine
all_odds_h2h <-
  all_home |>
  full_join(all_away, relationship = "many-to-many", by = c("match", "market_name")) |>
  mutate(margin = (1/home_win + 1/away_win)) |> 
  mutate(margin = round(100*(margin - 1), digits = 3)) |> 
  arrange(margin)

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

# H2H---------------------------------------------------------------------------
h2h_arbs <-
  all_odds_h2h |> 
  mutate(margin = -1*margin) |> 
  filter(margin > 0)

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
  # filter(margin > 0) |>
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
  all_player_rebounds |>
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
  # filter(margin > 0) |>
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
  all_player_assists |>
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
  # filter(margin > 0) |>
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
  all_player_pras |>
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
  # filter(margin > 0) |>
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
  all_player_threes |>
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
  # filter(margin > 0) |>
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
  all_player_steals |>
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
  # filter(margin > 0) |>
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
  all_player_blocks |>
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
  # filter(margin > 0) |>
  distinct(match, player_name, line, over_agency, under_agency, .keep_all = TRUE) |>
  relocate(over_price, over_agency, under_price, under_agency, .after = opposition_team)

#===============================================================================
# Get all ARBs together
#===============================================================================

# Function to calculate hedge prices
hedge_price <- function(odds_A, odds_B, stake_a) {
  return((odds_A/odds_B) * stake_a)
}

# Fix team names
start_times <-
  start_times |> 
  separate(match, into = c("home_team", "away_team"), sep = " v ") |>
  mutate(home_team = fix_team_names(home_team),
         away_team = fix_team_names(away_team)) |> 
  mutate(match = paste(home_team, away_team, sep = " v ")) |> 
  select(-home_team, -away_team)

# get current GMT datetime
current_time <- Sys.time()
gmt_time <- as.POSIXct(current_time, tz = "GMT")
gmt_time_dttm <- as_datetime(gmt_time)

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
  arrange(desc(margin)) |> 
  filter(!is.na(player_name)) |> 
  left_join(start_times, by = "match") |>
  # Filter out cases where current time is more than 5 mins after start time
  filter(gmt_time_dttm < start_time) |>
  select(-start_time)

all_arbs |> write_rds("Data/all_arbs.rds")

# H2H Arbs
h2h_arbs

# Separate into Neds and Unibet and the rest
neds_arbs <-
  all_arbs |>
  filter(margin > 0) |> 
  filter(over_agency %in% c("Neds") | under_agency %in% c("Neds")) |> 
  mutate(neds_stake = ifelse(over_agency == "Neds", (25 / (over_price - 1)), (25 / (under_price - 1)))) |> 
  mutate(other_stake = hedge_price(over_price, under_price, neds_stake)) |> 
  # Round stakes to nearest 5
  mutate(neds_stake = round(neds_stake / 5) * 5) |>
  mutate(other_stake = round(other_stake / 5) * 5) |>
  mutate(profit_neds = ((neds_stake *over_price) - neds_stake) - other_stake) |> 
  mutate(profit_other = ((other_stake *under_price) - other_stake) - neds_stake)

unibet_arbs <-
  all_arbs |>
  filter(margin > 0) |> 
  filter(over_agency %in% c("Unibet") | under_agency %in% c("Unibet")) |> 
  filter(!over_agency %in% c("Neds") & !under_agency %in% c("Neds")) |> 
  mutate(unibet_stake = ifelse(over_agency == "Unibet", (100 / (over_price - 1)), (100 / (under_price - 1)))) |> 
  mutate(other_stake = hedge_price(over_price, under_price, unibet_stake))

other_arbs <-
  all_arbs |>
  filter(margin > 0) |> 
  filter(!over_agency %in% c("Neds", "Unibet") & !under_agency %in% c("Neds", "Unibet"))

#===============================================================================
# SGM ARBs
#===============================================================================

# # All lines that exist on all agencies
# all_arbs_all_agencies <-
#   all_arbs |> 
#   arrange(match, player_name, market_name, line, under_agency) |> 
#   filter(under_agency %in% c("BetRight", "Pointsbet", "Sportsbet", "TAB")) |>
#   distinct(match, player_name, market_name, line, under_agency, .keep_all = TRUE) |>
#   group_by(match, player_name, market_name, line) |>
#   filter(n() == 4) |> 
#   ungroup() |> 
#   arrange(desc(margin))
# 
# df <- all_arbs_all_agencies[3:4, ]
# 
# # Read in SGM Functions
# source("SGM/BetRight/betright_sgm.R")
# source("SGM/PointsBet/pointsbet_sgm.R")
# source("SGM/Sportsbet/sportsbet_sgm.R")
# source("SGM/Tab/tab_sgm.R")
# 
# # Function to take dataframe and return all possible outcome SGMs
# get_sgm_arbs <- function(df) {
#   
# player_names <- df$player_name
# prop_line <- df$line
# prop_type <- df$market_name
# 
# # Combo 1 Over-Over
# pointsbet_price_1 <- call_sgm_pointsbet(data = pointsbet_sgm, player_names, prop_line, prop_type, over_under = c("Overs", "Overs"))
# betright_price_1 <- call_sgm_betright(data = betright_sgm, player_names, prop_line, prop_type, over_under = c("Overs", "Overs"))
# sportsbet_price_1 <- call_sgm_sportsbet(data = sportsbet_sgm, player_names, prop_line, prop_type, over_under = c("Overs", "Overs"))
# tab_price_1 <- call_sgm_tab(data = tab_sgm, player_names, prop_line, prop_type, over_under = c("Overs", "Overs"))
# 
# # Combo 2 Over-Under
# pointsbet_price <- call_sgm_pointsbet(data = pointsbet_sgm, player_names, prop_line, prop_type, over_under = c("Overs", "Unders"))
# betright_price <- call_sgm_betright(data = betright_sgm, player_names, prop_line, prop_type, over_under = c("Overs", "Unders"))
# sportsbet_price <- call_sgm_sportsbet(data = sportsbet_sgm, player_names, prop_line, prop_type, over_under = c("Overs", "Unders"))
# tab_price <- call_sgm_tab(data = tab_sgm, player_names, prop_line, prop_type, over_under = c("Overs", "Unders"))
# 
# # Combo 3 Under-Over
# pointsbet_price <- call_sgm_pointsbet(data = pointsbet_sgm, player_names, prop_line, prop_type, over_under = c("Unders", "Overs"))
# betright_price <- call_sgm_betright(data = betright_sgm, player_names, prop_line, prop_type, over_under = c("Unders", "Overs"))
# sportsbet_price <- call_sgm_sportsbet(data = sportsbet_sgm, player_names, prop_line, prop_type, over_under = c("Unders", "Overs"))
# tab_price <- call_sgm_tab(data = tab_sgm, player_names, prop_line, prop_type, over_under = c("Unders", "Overs"))
# 
# # Combo 4 Under-Under
# pointsbet_price <- call_sgm_pointsbet(data = pointsbet_sgm, player_names, prop_line, prop_type, over_under = c("Unders", "Unders"))
# betright_price <- call_sgm_betright(data = betright_sgm, player_names, prop_line, prop_type, over_under = c("Unders", "Unders"))
# sportsbet_price <- call_sgm_sportsbet(data = sportsbet_sgm, player_names, prop_line, prop_type, over_under = c("Unders", "Unders"))
# tab_price <- call_sgm_tab(data = tab_sgm, player_names, prop_line, prop_type, over_under = c("Unders", "Unders"))
# 
# }
