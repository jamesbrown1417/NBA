##%######################################################%##
#                                                          #
####          Get last  n games opponent stats          ####
####           for each team at a given date            ####
#                                                          #
##%######################################################%##

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
# Load packages
#===============================================================================

library(tidyverse)
library(zoo)

#===============================================================================
# Read in data
#===============================================================================

# Player Info
all_rosters <- read_csv("Data/all_rosters.csv")
all_teams <- read_csv("Data/all_teams.csv")
all_player_stats_2021_2022 <- read_csv("Data/all_player_stats_2021-2022.csv") |> mutate(SEASON_YEAR = "2021-22")
all_player_stats_2022_2023 <- read_csv("Data/all_player_stats_2022-2023.csv") |> mutate(SEASON_YEAR = "2022-23")
all_player_stats_2023_2024 <- read_csv("Data/all_player_stats_2023-2024.csv") |> mutate(SEASON_YEAR = "2023-24")
all_player_stats_2024_2025 <- read_csv("Data/all_player_stats_2024-2025.csv") |> mutate(SEASON_YEAR = "2024-25")

# Team Info
all_team_stats_2021_2022 <- read_csv("Data/advanced_box_scores_2021-2022.csv") |> mutate(SEASON_YEAR = "2021-22")
all_team_stats_2022_2023 <- read_csv("Data/advanced_box_scores_2022-2023.csv") |> mutate(SEASON_YEAR = "2022-23")
all_team_stats_2023_2024 <- read_csv("Data/advanced_box_scores_2023-2024.csv") |> mutate(SEASON_YEAR = "2023-24")
all_team_stats_2024_2025 <- read_csv("Data/advanced_box_scores_2024-2025.csv") |> mutate(SEASON_YEAR = "2024-25")

# Player Tracker Data
all_player_tracking_2024_2025 <- read_csv("Data/player_track_box_scores_2024-2025.csv") |> mutate(SEASON_YEAR = "2024-25")
all_player_tracking_2023_2024 <- read_csv("Data/player_track_box_scores_2023-2024.csv") |> mutate(SEASON_YEAR = "2023-24")


# Combine player stats
all_player_stats <-
  all_player_stats_2024_2025 |>
  bind_rows(all_player_stats_2023_2024) |>
  bind_rows(all_player_stats_2022_2023) |>
  bind_rows(all_player_stats_2021_2022) |>
  left_join(all_rosters[c("PLAYER", "PLAYER_ID")], by = c("personId" = "PLAYER_ID")) |> 
  mutate(PRA = points + reboundsTotal + assists) |>
  rename(PLAYER_NAME = PLAYER,
         PTS = points,
         REB = reboundsTotal,
         AST = assists,
         STL = steals,
         BLK = blocks) |> 
  mutate(MIN = convert_time_to_decimal_hms(minutes)) |> 
  mutate(MIN = round(MIN, 2)) |> 
  relocate(MIN, .after = minutes)

# Get Game Dates DF
game_dates <-
  all_player_stats |> 
  distinct(gameId, GAME_DATE)

# Get Home Teams DF
home_teams <-
  all_player_stats |> 
  distinct(gameId, HOME_TEAM)

# Get Away Teams DF
away_teams <-
  all_player_stats |> 
  distinct(gameId, AWAY_TEAM)

# Create Home / Away variable
all_player_stats <-
  all_player_stats |>
  mutate(team_full = paste(teamCity, teamName)) |>
  mutate(home_away = if_else(team_full == HOME_TEAM, "Home", "Away"))

#===============================================================================
# Convert player stats table into teams
#===============================================================================

team_stats_2024_2025 <-
all_player_stats_2024_2025 |>
  distinct(gameId, teamId, playerSlug, GAME_DATE, .keep_all = TRUE) |> 
  mutate(minutes = hms(minutes)) |> 
  mutate(mins = hour(minutes)) |> 
  mutate(seconds = minute(minutes)) |>
  mutate(minutes = mins + (seconds / 60)) |>
  select(-c(mins, seconds)) |>
  select(
    gameId,
    GAME_DATE,
    HOME_TEAM,
    AWAY_TEAM,
    teamName,
    teamCity,
    firstName,
    familyName,
    minutes,
    points,
    reboundsTotal,
    assists,
    steals,
    blocks,
    threes = threePointersMade) |>
  mutate(PRAs = points + reboundsTotal + assists) |> 
  group_by(gameId, GAME_DATE, HOME_TEAM, AWAY_TEAM, teamName, teamCity) |>
  summarise(
    minutes = sum(minutes, na.rm = TRUE),
    points = sum(points),
    rebounds = sum(reboundsTotal),
    assists = sum(assists),
    steals = sum(steals),
    blocks = sum(blocks),
    threes = sum(threes),
    PRAs = sum(PRAs)) |>
  ungroup() |> 
  mutate(team = paste(teamCity, teamName)) |>
  mutate(oppositionTeam = if_else(team == HOME_TEAM, AWAY_TEAM, HOME_TEAM)) |> 
  select(-teamName, -teamCity, -HOME_TEAM, -AWAY_TEAM) |>
  relocate(team, oppositionTeam, .after = GAME_DATE) |> 
  arrange(team, GAME_DATE) |> 
  rename(date = GAME_DATE) |> 
  mutate(date = date + days(1)) |> 
  filter(!is.na(oppositionTeam)) |> 
  mutate(minutes = if_else(minutes < 240, 240, minutes)) |> 
  mutate(norm_factor = 240 / minutes) |>
  mutate_at(vars(points:PRAs), ~ . * norm_factor) |> 
  select(-norm_factor, -minutes)
  
#===============================================================================
# Get team stats for each opposition - Means
#===============================================================================

stats_vs_opp_for_todays_bets <-
  team_stats_2024_2025 |> 
  select(oppositionTeam, date, points, rebounds, assists, steals, blocks, threes, PRAs) |> 
  arrange(oppositionTeam, date) |> 
  group_by(oppositionTeam) |>
  # Calculate rolling 10 game average
  mutate(rolling_10_game_points = rollmean(points, 10, fill = NA, align = "right"),
         rolling_10_game_assists = rollmean(assists, 10, fill = NA, align = "right"),
         rolling_10_game_rebounds = rollmean(rebounds, 10, fill = NA, align = "right"),
         rolling_10_game_steals = rollmean(steals, 10, fill = NA, align = "right"),
         rolling_10_game_blocks = rollmean(blocks, 10, fill = NA, align = "right"),
         rolling_10_game_threes = rollmean(threes, 10, fill = NA, align = "right"),
         rolling_10_game_PRAs = rollmean(PRAs, 10, fill = NA, align = "right")) |> 
  filter(!is.na(rolling_10_game_points))

stats_vs_opp <-
  team_stats_2024_2025 |> 
  select(oppositionTeam, date, points, rebounds, assists, steals, blocks, threes, PRAs) |> 
  arrange(oppositionTeam, date) |> 
  group_by(oppositionTeam) |>
  # Calculate rolling 10 game average
  mutate(rolling_10_game_points = rollmean(points, 10, fill = NA, align = "right"),
         rolling_10_game_assists = rollmean(assists, 10, fill = NA, align = "right"),
         rolling_10_game_rebounds = rollmean(rebounds, 10, fill = NA, align = "right"),
         rolling_10_game_steals = rollmean(steals, 10, fill = NA, align = "right"),
         rolling_10_game_blocks = rollmean(blocks, 10, fill = NA, align = "right"),
         rolling_10_game_threes = rollmean(threes, 10, fill = NA, align = "right"),
         rolling_10_game_PRAs = rollmean(PRAs, 10, fill = NA, align = "right")) |>
  mutate(rolling_10_game_points = lag(rolling_10_game_points),
         rolling_10_game_assists = lag(rolling_10_game_assists),
         rolling_10_game_rebounds = lag(rolling_10_game_rebounds),
         rolling_10_game_steals = lag(rolling_10_game_steals),
         rolling_10_game_threes = lag(rolling_10_game_threes),
         rolling_10_game_PRAs = lag(rolling_10_game_PRAs),
         rolling_10_game_blocks = lag(rolling_10_game_blocks)) |> 
  filter(!is.na(rolling_10_game_points))

#===============================================================================
# Get team stats for each opposition - Medians
#===============================================================================

stats_vs_opp_for_todays_bets_med <-
  team_stats_2024_2025 |> 
  select(oppositionTeam, date, points, rebounds, assists, steals, blocks, threes, PRAs) |> 
  arrange(oppositionTeam, date) |> 
  group_by(oppositionTeam) |>
  # Calculate rolling 15 game median
  mutate(rolling_15_game_points = rollmedian(points, 15, fill = NA, align = "right"),
         rolling_15_game_assists = rollmedian(assists, 15, fill = NA, align = "right"),
         rolling_15_game_rebounds = rollmedian(rebounds, 15, fill = NA, align = "right"),
         rolling_15_game_steals = rollmedian(steals, 15, fill = NA, align = "right"),
         rolling_15_game_blocks = rollmedian(blocks, 15, fill = NA, align = "right"),
         rolling_15_game_threes = rollmedian(threes, 15, fill = NA, align = "right"),
         rolling_15_game_PRAs = rollmedian(PRAs, 15, fill = NA, align = "right")) |> 
  filter(!is.na(rolling_15_game_points))

stats_vs_opp_med <-
  team_stats_2024_2025 |> 
  select(oppositionTeam, date, points, rebounds, assists, steals, blocks, threes, PRAs) |> 
  arrange(oppositionTeam, date) |> 
  group_by(oppositionTeam) |>
  # Calculate rolling 15 game average
  mutate(rolling_15_game_points = rollmedian(points, 15, fill = NA, align = "right"),
         rolling_15_game_assists = rollmedian(assists, 15, fill = NA, align = "right"),
         rolling_15_game_rebounds = rollmedian(rebounds, 15, fill = NA, align = "right"),
         rolling_15_game_steals = rollmedian(steals, 15, fill = NA, align = "right"),
         rolling_15_game_blocks = rollmedian(blocks, 15, fill = NA, align = "right"),
         rolling_15_game_threes = rollmedian(threes, 15, fill = NA, align = "right"),
         rolling_15_game_PRAs = rollmedian(PRAs, 15, fill = NA, align = "right")) |>
  mutate(rolling_15_game_points = lag(rolling_15_game_points),
         rolling_15_game_assists = lag(rolling_15_game_assists),
         rolling_15_game_rebounds = lag(rolling_15_game_rebounds),
         rolling_15_game_steals = lag(rolling_15_game_steals),
         rolling_15_game_threes = lag(rolling_15_game_threes),
         rolling_15_game_PRAs = lag(rolling_15_game_PRAs),
         rolling_15_game_blocks = lag(rolling_15_game_blocks)) |> 
  filter(!is.na(rolling_15_game_points))

#===============================================================================
# Write out as RDS
#===============================================================================

write_rds(stats_vs_opp, "Data/stats_vs_opp.rds")
write_rds(stats_vs_opp_for_todays_bets, "Data/stats_vs_opp_today.rds")
write_rds(stats_vs_opp_med, "Data/stats_vs_opp_med.rds")
write_rds(stats_vs_opp_for_todays_bets_med, "Data/stats_vs_opp_today_med.rds")

