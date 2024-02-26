#===============================================================================
# Libraries and functions
#===============================================================================

library(tidyverse)
library(zoo)
library(future)
library(furrr)
`%notin%` <- Negate(`%in%`)

# Set up parallel processing
plan(multisession)

# Function to convert time to decimal-------------------------------------------
convert_time_to_decimal_hms <- function(time_obj) {
  # Convert to hms object
  time_obj <- hms(time_obj)
  
  # Extract hours and minutes
  hours <- hour(time_obj)
  minutes <- minute(time_obj)
  
  # Convert to decimal
  decimal_time <- hours + (minutes / 60)
  return(decimal_time)
}

#===============================================================================
# Read in odds
#===============================================================================

# Read in data------------------------------------------------------------------
# Read in everything in ../Data/odds_archive that is a csv file
all_scraped_odds <-
  list.files("Data/odds_archive", full.names = TRUE) |>
  keep( ~ str_detect(.x, ".csv$")) |>
  map(read_csv) |>
  # If date column exists, remove it
  map(~ if ("date" %in% colnames(.x)) {
    .x |> select(-date)
  } else {
    .x
  }) |>
  # If timestamp column exists as character, convert to datetime
  map(~ if ("timestamp" %in% colnames(.x)) {
    .x |> mutate(timestamp = as_datetime(timestamp))
  } else {
    .x
  }) |>
  reduce(bind_rows) |>
  filter(timestamp >= "2023-12-01") |>
  # Change start time to adelaide time
  mutate(start_time = start_time + hours(10) + minutes(30)) |> 
  mutate(date = as_date(start_time)) |> 
  mutate(home_away = if_else(opposition_team != home_team, "home", "away")) |> 
  mutate(market_name = str_to_title(market_name)) |>
  distinct(date, player_name, agency, market_name, line, over_price, under_price, .keep_all = TRUE)

#===============================================================================
# Read in season player data
#===============================================================================

# Read in stats-----------------------------------------------------------------

# Player Info
all_rosters <- read_csv("Data/all_rosters.csv")
all_teams <- read_csv("Data/all_teams.csv")
all_player_stats_2021_2022 <-
  read_csv("Data/all_player_stats_2021-2022.csv") |> mutate(SEASON_YEAR = "2021-22")
all_player_stats_2022_2023 <-
  read_csv("Data/all_player_stats_2022-2023.csv") |> mutate(SEASON_YEAR = "2022-23")
all_player_stats_2023_2024 <-
  read_csv("Data/all_player_stats_2023-2024.csv") |> mutate(SEASON_YEAR = "2023-24")

# Team Info
all_team_stats_2021_2022 <-
  read_csv("Data/advanced_box_scores_2021-2022.csv") |> mutate(SEASON_YEAR = "2021-22")
all_team_stats_2022_2023 <-
  read_csv("Data/advanced_box_scores_2022-2023.csv") |> mutate(SEASON_YEAR = "2022-23")
all_team_stats_2023_2024 <-
  read_csv("Data/advanced_box_scores_2023-2024.csv") |> mutate(SEASON_YEAR = "2023-24")

# Player Tracker Data
all_player_tracking_2023_2024 <-
  read_csv("Data/player_track_box_scores_2023-2024.csv") |> mutate(SEASON_YEAR = "2023-24")

# Combine player stats
all_player_stats <-
  all_player_stats_2023_2024 |>
  bind_rows(all_player_stats_2022_2023) |>
  bind_rows(all_player_stats_2021_2022) |>
  left_join(all_rosters[c("PLAYER", "PLAYER_ID")], by = c("personId" = "PLAYER_ID")) |>
  mutate(PRA = points + reboundsTotal + assists) |>
  rename(
    PLAYER_NAME = PLAYER,
    PTS = points,
    REB = reboundsTotal,
    AST = assists,
    STL = steals,
    BLK = blocks,
    FG3M = threePointersMade,
  ) |>
  mutate(MIN = convert_time_to_decimal_hms(minutes)) |>
  mutate(MIN = round(MIN, 2)) |>
  relocate(MIN, .after = minutes)

# Create Home / Away variable
all_player_stats <-
  all_player_stats |>
  mutate(team_full = paste(teamCity, teamName)) |>
  mutate(home_away = if_else(team_full == HOME_TEAM, "Home", "Away"))

# Get results data to join to odds----------------------------------------------
player_results <-
  all_player_stats |>
  filter(SEASON_YEAR == "2023-24") |> 
  mutate(HOME_TEAM = ifelse(HOME_TEAM == "LA Clippers", "Los Angeles Clippers", HOME_TEAM)) |>
  mutate(AWAY_TEAM = ifelse(AWAY_TEAM == "LA Clippers", "Los Angeles Clippers", AWAY_TEAM)) |>
  mutate(team_full = ifelse(team_full == "LA Clippers", "Los Angeles Clippers", team_full)) |>
  transmute(
    match = paste(HOME_TEAM, AWAY_TEAM, sep = " v "),
    player_name = PLAYER_NAME,
    team = team_full,
    date = GAME_DATE + days(1),
    points = PTS,
    rebounds = REB,
    assists = AST,
    steals = STL,
    blocks = BLK,
    minutes = MIN,
    PRA,
    FG3M
  ) |> 
  filter(!is.na(player_name)) |>
  filter(!is.na(minutes)) |> 
  arrange(player_name, team, date)

#===============================================================================
# Function to get rolling stats at a date - Overs
#===============================================================================

get_emp_probs_over <- function(player, stat) {
  
  # Get all games before date
  results_before_date <-
    player_results |>
    filter(player_name == player) |>
    arrange(player_name, team, desc(date)) |> 
    distinct(player_name, team, match, date, .keep_all = TRUE)
  
  # Get rolling 5, 10 and 15 game prob over line
  emp_probs <-
  results_before_date |> 
    group_by(player_name) |> 
    mutate(mean_last_5 = rollmean(!!sym(stat), 5, fill = NA, align = "left"),
           mean_last_10 = rollmean(!!sym(stat), 10, fill = NA, align = "left"),
           mean_last_15 = rollmean(!!sym(stat), 15, fill = NA, align = "left")) |>
    
    mutate(median_last_5 = rollmedian(!!sym(stat), 5, fill = NA, align = "left"),
           median_last_10 = rollmedian(!!sym(stat), 11, fill = NA, align = "left"),
           median_last_15 = rollmedian(!!sym(stat), 15, fill = NA, align = "left")) |>
    
    mutate(sd_last_5 = rollapply(!!sym(stat), 5, sd, fill = NA, align = "left"),
           sd_last_10 = rollapply(!!sym(stat), 10, sd, fill = NA, align = "left"),
           sd_last_15 = rollapply(!!sym(stat), 15, sd, fill = NA, align = "left")) |>
    
    mutate(max_last_5 = rollapply(!!sym(stat), 5, max, fill = NA, align = "left"),
           max_last_10 = rollapply(!!sym(stat), 10, max, fill = NA, align = "left"),
           max_last_15 = rollapply(!!sym(stat), 15, max, fill = NA, align = "left")) |>
    
    mutate(min_last_5 = rollapply(!!sym(stat), 5, min, fill = NA, align = "left"),
           min_last_10 = rollapply(!!sym(stat), 10, min, fill = NA, align = "left"),
           min_last_15 = rollapply(!!sym(stat), 15, min, fill = NA, align = "left")) |>
    
    ungroup() |>
    
    mutate(mean_last_5 = lead(mean_last_5),
           mean_last_10 = lead(mean_last_10),
           mean_last_15 = lead(mean_last_15)) |>
    
    mutate(median_last_5 = lead(median_last_5),
           median_last_10 = lead(median_last_10),
           median_last_15 = lead(median_last_15)) |>
    
    mutate(sd_last_5 = lead(sd_last_5),
           sd_last_10 = lead(sd_last_10),
           sd_last_15 = lead(sd_last_15)) |>
    
    mutate(max_last_5 = lead(max_last_5),
           max_last_10 = lead(max_last_10),
           max_last_15 = lead(max_last_15)) |>
    
    mutate(min_last_5 = lead(min_last_5),
           min_last_10 = lead(min_last_10),
           min_last_15 = lead(min_last_15)) |>
    
    select(match, player_name, date, any_of(stat), contains("last")) |>
    filter(!is.na(median_last_15))
   
  return(emp_probs)
}

#===============================================================================
# Function to get DVP at a date
#===============================================================================

#===============================================================================
# Get rolling stats dataset for points
#===============================================================================

# Get all data
all_points_odds <-
all_scraped_odds |>
  filter(market_name == "Player Points") |> 
  select(match, date, home_away, player_name, line, over_price, agency) |> 
  filter(!is.na(over_price)) |> 
  arrange(date, player_name, line, over_price) |>
  filter(over_price >= 1.15)

# Get distinct player, line, date combos
distinct_points_odds <-
  all_points_odds |>
  distinct(player_name) |>
  transmute(player = player_name, stat = "points")

# Apply function to get rolling stats at date
rolling_stats_points <-
  future_pmap(distinct_points_odds, get_emp_probs_over, .progress = TRUE) |> 
  bind_rows()

# Get whether a game was a back to back
back_to_back <- 
  player_results |> 
  filter(minutes > 0) |> 
  arrange(player_name, team, date) |>
  group_by(player_name) |>
  mutate(back_to_back = ifelse(lag(date) == date - 1, 1, 0)) |>
  ungroup() |> 
  select(match, date, player_name, back_to_back) |> 
  mutate(back_to_back = factor(back_to_back, labels = c("No", "Yes"))) |> 
  distinct(player_name, match, date, .keep_all = TRUE)

# Join scraped odds with rolling stats
all_points_odds <-
  all_points_odds |>
  inner_join(rolling_stats_points, by = c("match", "player_name", "date")) |> 
  inner_join(back_to_back, by = c("match", "player_name", "date")) |> 
  mutate(back_to_back = ifelse(is.na(back_to_back), "No", back_to_back))
  


