# Libraries and functions-------------------------------------------------------
library(tidyverse)
library(googlesheets4)
library(googledrive)

# Get empirical probability function
source("Scripts/get_empirical_probabilities.R")

# Google sheets authentification -----------------------------------------------
options(gargle_oauth_cache = ".secrets")
drive_auth(cache = ".secrets", email = "cuzzy.punting@gmail.com")
gs4_auth(token = drive_token())

# # Run all odds scraping scripts-------------------------------------------------
run_scraping <- function(script_name) {
  tryCatch({
    source(script_name)
  }, error = function(e) {
    cat("Odds not released yet for:", script_name, "\n")
  })
}

# Run all odds scraping scripts
# run_scraping("OddsScraper/scrape_betr.R")
# run_scraping("OddsScraper/scrape_BetRight.R")
# run_scraping("OddsScraper/scrape_Palmerbet.R")
run_scraping("OddsScraper/scrape_pointsbet.R")
run_scraping("OddsScraper/scrape_sportsbet.R")
run_scraping("OddsScraper/scrape_TAB.R")
run_scraping("OddsScraper/scrape_TopSport.R")
# run_scraping("OddsScraper/scrape_bet365.R")
run_scraping("OddsScraper/scrape_bluebet.R")
run_scraping("OddsScraper/scrape_neds.R")

# Get schedule
NBA_schedule <-
  read_csv("Data/NBA_schedule.csv") |> 
  filter(match_date_utc >= lubridate::now(tzone = "UTC")) |>
  transmute(match = paste(home_team_name, away_team_name, sep = " v "),
            start_time = match_date_utc) |> 
  arrange(start_time, match) |> 
  group_by(match) |>
  slice_head(n = 1) |> 
  ungroup() |> 
  mutate(start_time = with_tz(start_time, tzone = "Australia/Adelaide"))

# Get all games between today and one week
next_week_games <-
read_csv("Data/NBA_schedule.csv") |> 
  filter(match_date_utc >= lubridate::now(tzone = "UTC")) |> 
  mutate(one_week = lubridate::now(tzone = "UTC") + lubridate::weeks(1)) |>
  filter(match_date_utc <= one_week) |> 
  transmute(match = paste(home_team_name, away_team_name, sep = " v "),
            start_time = match_date_utc) |> 
  mutate(start_time = with_tz(start_time, tzone = "Australia/Adelaide"))

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
    arrange(margin) |> 
  left_join(NBA_schedule, by = "match") |> 
  relocate(start_time, .after = match) |> 
  filter(match %in% next_week_games$match)

# Google Sheets-----------------------------------------------------
sheet <- gs4_find("NBA Data")
sheet_write(sheet, data = all_odds_h2h, sheet = "H2H")

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

# Add empirical probabilities---------------------------------------------------

# Points
distinct_point_combos <-
    all_player_points |> 
    distinct(player_name, line)

player_emp_probs <-
    pmap(distinct_point_combos, get_empirical_prob, "PTS", .progress = TRUE) |> 
    bind_rows()

all_player_points <-
  all_player_points |>
  mutate(
    implied_prob_over = 1 / over_price,
    implied_prob_under = 1 / under_price
  ) |>
  left_join(player_emp_probs, by = c("player_name", "line")) |>
  rename(empirical_prob_over = empirical_prob) |>
  mutate(empirical_prob_under = 1 - empirical_prob_over) |>
  mutate(
    diff_over = empirical_prob_over - implied_prob_over,
    diff_under = empirical_prob_under - implied_prob_under
  ) |>
  relocate(agency, .after = diff_under) |>
  mutate_if(is.double, round, 2) |>
  filter(!is.na(opposition_team)) |>
  group_by(player_name, line) |>
  mutate(
    min_implied_prob = min(implied_prob_over, na.rm = TRUE),
    max_implied_prob = max(implied_prob_over, na.rm = TRUE)
  ) |>
  mutate(variation = max_implied_prob - min_implied_prob) |>
  ungroup() |>
  select(-min_implied_prob,-max_implied_prob) |>
  arrange(desc(variation), player_name, desc(over_price), line) |>
  left_join(NBA_schedule, by = "match") |>
  relocate(start_time, .after = match) |> 
  filter(match %in% next_week_games$match)

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

# Add empirical probabilities---------------------------------------------------

# Assists
distinct_assist_combos <-
    all_player_assists |> 
    distinct(player_name, line)

player_emp_probs_assists <-
    pmap(distinct_assist_combos, get_empirical_prob, "AST", .progress = TRUE) |> 
    bind_rows()

all_player_assists <-
  all_player_assists |>
  mutate(
    implied_prob_over = 1 / over_price,
    implied_prob_under = 1 / under_price
  ) |>
  left_join(player_emp_probs_assists, by = c("player_name", "line")) |>
  rename(empirical_prob_over = empirical_prob) |>
  mutate(empirical_prob_under = 1 - empirical_prob_over) |>
  mutate(
    diff_over = empirical_prob_over - implied_prob_over,
    diff_under = empirical_prob_under - implied_prob_under
  ) |>
  relocate(agency, .after = diff_under) |>
  mutate_if(is.double, round, 2) |>
  filter(!is.na(opposition_team)) |>
  group_by(player_name, line) |>
  mutate(
    min_implied_prob = min(implied_prob_over, na.rm = TRUE),
    max_implied_prob = max(implied_prob_over, na.rm = TRUE)
  ) |>
  mutate(variation = max_implied_prob - min_implied_prob) |>
  ungroup() |>
  select(-min_implied_prob,-max_implied_prob) |>
  arrange(desc(variation), player_name, desc(over_price), line) |>
  left_join(NBA_schedule, by = "match") |>
  relocate(start_time, .after = match) |> 
  filter(match %in% next_week_games$match)

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

# Add empirical probabilities---------------------------------------------------

# Rebounds
distinct_rebound_combos <-
    all_player_rebounds |> 
    distinct(player_name, line)

player_emp_probs_rebounds <-
    pmap(distinct_rebound_combos, get_empirical_prob, "REB", .progress = TRUE) |> 
    bind_rows()

all_player_rebounds <-
  all_player_rebounds |>
  mutate(
    implied_prob_over = 1 / over_price,
    implied_prob_under = 1 / under_price
  ) |>
  left_join(player_emp_probs_rebounds, by = c("player_name", "line")) |>
  rename(empirical_prob_over = empirical_prob) |>
  mutate(empirical_prob_under = 1 - empirical_prob_over) |>
  mutate(
    diff_over = empirical_prob_over - implied_prob_over,
    diff_under = empirical_prob_under - implied_prob_under
  ) |>
  relocate(agency, .after = diff_under) |>
  mutate_if(is.double, round, 2) |>
  filter(!is.na(opposition_team)) |>
  group_by(player_name, line) |>
  mutate(
    min_implied_prob = min(implied_prob_over, na.rm = TRUE),
    max_implied_prob = max(implied_prob_over, na.rm = TRUE)
  ) |>
  mutate(variation = max_implied_prob - min_implied_prob) |>
  ungroup() |>
  select(-min_implied_prob,-max_implied_prob) |>
  arrange(desc(variation), player_name, desc(over_price), line) |>
  left_join(NBA_schedule, by = "match") |>
  relocate(start_time, .after = match) |> 
  filter(match %in% next_week_games$match)

# Add to google sheets
sheet_write(sheet, data = all_player_rebounds, sheet = "Player Rebounds")
