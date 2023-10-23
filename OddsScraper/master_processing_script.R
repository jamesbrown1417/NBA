# Libraries and functions-------------------------------------------------------
library(tidyverse)
library(googlesheets4)
library(googledrive)

# Google sheets authentification -----------------------------------------------
options(gargle_oauth_cache = ".secrets")
drive_auth(cache = ".secrets", email = "cuzzy.punting@gmail.com")
gs4_auth(token = drive_token())

# Run all odds scraping scripts-------------------------------------------------
source("OddsScraper/scrape_betr.R")
source("OddsScraper/scrape_BetRight.R")
source("OddsScraper/scrape_Palmerbet.R")
source("OddsScraper/scrape_pointsbet.R")
source("OddsScraper/scrape_sportsbet.R")
source("OddsScraper/scrape_TAB.R")
source("OddsScraper/scrape_TopSport.R")
source("OddsScraper/scrape_bet365.R")
source("OddsScraper/scrape_bluebet.R")

##%######################################################%##
#                                                          #
####                    Head to Head                    ####
#                                                          #
##%######################################################%##

# Get all scraped odds files and combine
all_odds_files <-
    list.files("Data/scraped_odds", full.names = TRUE, pattern = "h2h") |>
    map(read_csv) |>
    reduce(bind_rows)

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

# Google Sheets-----------------------------------------------------
sheet <- gs4_find("NBL Data")
sheet_write(sheet, data = all_odds_h2h, sheet = "H2H")

##%######################################################%##
#                                                          #
####                    Total Points                    ####
#                                                          #
##%######################################################%##

# Get all scraped odds files and combine
all_totals_files <-
    list.files("Data/scraped_odds", full.names = TRUE, pattern = "total") |>
    map(read_csv) |>
    reduce(bind_rows) |> 
    mutate(market_name = "Total Points")

# For each match, get all unders
all_unders <-
    all_totals_files |>
    arrange(start_time, match, total_points_line, desc(under_price)) |>
    select(match, start_time, market_name, home_team, away_team, total_points_line, under_price, under_agency = agency)

# For each match, get all overs
all_overs <-
    all_totals_files |>
    arrange(start_time, match, total_points_line, desc(over_price)) |>
    select(match, start_time, market_name, home_team, away_team, total_points_line, over_price, over_agency = agency)

# Combine
all_odds_totals <-
    all_unders |>
    full_join(all_overs, relationship = "many-to-many") |>
    mutate(margin = (1/under_price + 1/over_price)) |> 
    mutate(margin = round(100*(margin - 1), digits = 3)) |> 
    arrange(margin)

# Google Sheets-----------------------------------------------------
sheet_write(sheet, data = all_odds_totals, sheet = "Total Points")

##%######################################################%##
#                                                          #
####                   Player Points                    ####
#                                                          #
##%######################################################%##

# Get all scraped odds files and combine
all_player_points <-
    list.files("Data/scraped_odds", full.names = TRUE, pattern = "player_points") |>
    map(read_csv) |>
    # Ignore null elements
    keep(~nrow(.x) > 0) |>
    reduce(bind_rows) |> 
    arrange(player_name, line, desc(over_price))

# Add to google sheets
sheet_write(sheet, data = all_player_points, sheet = "Player Points")

##%######################################################%##
#                                                          #
####                   Player Assists                   ####
#                                                          #
##%######################################################%##

# Get all scraped odds files and combine
all_player_assists <-
    list.files("Data/scraped_odds", full.names = TRUE, pattern = "player_assists") |>
    map(read_csv) |>
    # Ignore null elements
    keep(~nrow(.x) > 0) |>
    reduce(bind_rows)

# Add to google sheets
sheet_write(sheet, data = all_player_assists, sheet = "Player Assists")

##%######################################################%##
#                                                          #
####                  Player Rebounds                   ####
#                                                          #
##%######################################################%##

# Get all scraped odds files and combine
all_player_rebounds <-
    list.files("Data/scraped_odds", full.names = TRUE, pattern = "player_rebounds") |>
    map(read_csv) |>
    # Ignore null elements
    keep(~nrow(.x) > 0) |>
    reduce(bind_rows)

# Add to google sheets
sheet_write(sheet, data = all_player_rebounds, sheet = "Player Rebounds")
