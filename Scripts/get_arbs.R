# library(shiny)
library(bslib)
library(gridlayout)
library(DT)
library(tidyverse)
library(googlesheets4)
library(googledrive)

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
# Read in Data
#===============================================================================

# Player Info
all_rosters <- read_csv("Data/all_rosters.csv")
all_teams <- read_csv("Data/all_teams.csv")
all_player_stats_2021_2022 <- read_csv("Data/all_player_stats_2021-2022.csv") |> mutate(SEASON_YEAR = "2021-22")
all_player_stats_2022_2023 <- read_csv("Data/all_player_stats_2022-2023.csv") |> mutate(SEASON_YEAR = "2022-23")
all_player_stats_2023_2024 <- read_csv("Data/all_player_stats_2023-2024.csv") |> mutate(SEASON_YEAR = "2023-24")

# Team Info
all_team_stats_2021_2022 <- read_csv("Data/advanced_box_scores_2021-2022.csv") |> mutate(SEASON_YEAR = "2021-22")
all_team_stats_2022_2023 <- read_csv("Data/advanced_box_scores_2022-2023.csv") |> mutate(SEASON_YEAR = "2022-23")
all_team_stats_2023_2024 <- read_csv("Data/advanced_box_scores_2023-2024.csv") |> mutate(SEASON_YEAR = "2023-24")

# Player Tracker Data
all_player_tracking_2023_2024 <- read_csv("Data/player_track_box_scores_2023-2024.csv") |> mutate(SEASON_YEAR = "2023-24")

# Combine player stats
all_player_stats <-
  all_player_stats_2023_2024 |>
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

# Combine team stats
all_team_stats <-
  all_team_stats_2023_2024 |> 
  bind_rows(all_team_stats_2022_2023) |>
  bind_rows(all_team_stats_2021_2022) |>
  left_join(game_dates) |>
  left_join(home_teams) |> 
  left_join(away_teams) |>
  filter(!is.na(GAME_DATE)) |>
  transmute(
    gameId,
    teamId,
    teamName = paste(teamCity, teamName),
    homeTeam = HOME_TEAM,
    awayTeam = AWAY_TEAM,
    oppositionTeam = if_else(teamName == HOME_TEAM, AWAY_TEAM, HOME_TEAM),
    date = GAME_DATE,
    minutes,
    possessions,
    pacePer40,
    offensiveRating,
    defensiveRating,
    netRating,
    assistPercentage,
    defensiveReboundPercentage,
    offensiveReboundPercentage,
    reboundPercentage,
    trueShootingPercentage,
    effectiveFieldGoalPercentage,
    season = SEASON_YEAR)

# Create Home / Away variable
all_player_stats <-
  all_player_stats |>
  mutate(team_full = paste(teamCity, teamName)) |>
  mutate(home_away = if_else(team_full == HOME_TEAM, "Home", "Away"))

# Determine the operating system
os_type <- Sys.info()["sysname"]

# Google sheets authentication -------------------------------------------------
options(gargle_oauth_cache = ".secrets")
drive_auth(cache = ".secrets", email = "cuzzy.punting@gmail.com")
gs4_auth(token = drive_token())

# Conditional logic for loading data based on OS
if (os_type == "Windows") {
  # Read RDS Data for Windows
  player_points_data <- read_rds("Data/processed_odds/all_player_points.rds")
  player_assists_data <- read_rds("Data/processed_odds/all_player_assists.rds")
  player_rebounds_data <- read_rds("Data/processed_odds/all_player_rebounds.rds")
  player_pras_data <- read_rds("Data/processed_odds/all_player_pras.rds")
  player_steals_data <- read_rds("Data/processed_odds/all_player_steals.rds")
  player_threes_data <- read_rds("Data/processed_odds/all_player_threes.rds")
  player_blocks_data <- read_rds("Data/processed_odds/all_player_blocks.rds")
} else {
  # Google Sheets Data for other OS
  ss_name <- gs4_find("NBA Data")
  player_points_data <- read_sheet(ss = ss_name, sheet = "Player Points")
  player_assists_data <- read_sheet(ss = ss_name, sheet = "Player Assists")
  player_rebounds_data <- read_sheet(ss = ss_name, sheet = "Player Rebounds")
  player_pras_data <- read_sheet(ss = ss_name, sheet = "Player PRAs")
  player_steals_data <- read_sheet(ss = ss_name, sheet = "Player Steals")
  player_threes_data <- read_sheet(ss = ss_name, sheet = "Player Threes")
  player_blocks_data <- read_sheet(ss = ss_name, sheet = "Player Blocks")
}

# Add opposition defensive rating-----------------------------------------------

# Get defensive rating in last 5 games
def_rating_last_5 <-
  all_team_stats |> 
  arrange(teamName, desc(date)) |>
  group_by(teamName) |>
  slice_head(n = 5) |>
  summarise(def_rating = mean(defensiveRating, na.rm = TRUE)) |> 
  mutate(def_rating = round((def_rating - min(def_rating)) / (max(def_rating) - min(def_rating)) * 100, digits = 1))

# Get Pace per 40 vs opposition in last 5 games
pace_per_40_last_5 <-
  all_team_stats |> 
  arrange(oppositionTeam, desc(date)) |>
  group_by(oppositionTeam) |>
  slice_head(n = 5) |>
  summarise(pace_per_40 = mean(pacePer40, na.rm = TRUE)) |> 
  mutate(pace_per_40 = round((pace_per_40 - min(pace_per_40)) / (max(pace_per_40) - min(pace_per_40)) * 100, digits = 1))

# Add to player points
player_points_data <-
  player_points_data |> 
  left_join(def_rating_last_5, by = c("opposition_team" = "teamName")) |>
  left_join(pace_per_40_last_5, by = c("opposition_team" = "oppositionTeam"))

# Add to player assists
player_assists_data <-
  player_assists_data |> 
  left_join(def_rating_last_5, by = c("opposition_team" = "teamName")) |>
  left_join(pace_per_40_last_5, by = c("opposition_team" = "oppositionTeam"))

# Add to player rebounds
player_rebounds_data <-
  player_rebounds_data |> 
  left_join(def_rating_last_5, by = c("opposition_team" = "teamName")) |>
  left_join(pace_per_40_last_5, by = c("opposition_team" = "oppositionTeam"))

# Add to player pras
player_pras_data <-
  player_pras_data |> 
  left_join(def_rating_last_5, by = c("opposition_team" = "teamName")) |>
  left_join(pace_per_40_last_5, by = c("opposition_team" = "oppositionTeam"))

# Add to player blocks
player_blocks_data <-
  player_blocks_data |> 
  left_join(def_rating_last_5, by = c("opposition_team" = "teamName")) |>
  left_join(pace_per_40_last_5, by = c("opposition_team" = "oppositionTeam"))

# Add to player steals
player_steals_data <-
  player_steals_data |> 
  left_join(def_rating_last_5, by = c("opposition_team" = "teamName")) |>
  left_join(pace_per_40_last_5, by = c("opposition_team" = "oppositionTeam"))

# Add to player threes
player_threes_data <-
  player_threes_data |> 
  left_join(def_rating_last_5, by = c("opposition_team" = "teamName")) |>
  left_join(pace_per_40_last_5, by = c("opposition_team" = "oppositionTeam"))

##%######################################################%##
#                                                          #
####   Get all over under comparisons of same market    ####
#                                                          #
##%######################################################%##

# Points------------------------------------------------------------------------
points_unders <-
player_points_data |> 
  filter(market_name == "Player Points") |>
  select(match, start_time, market_name, player_name, player_team, line, under_price, opposition_team, agency) |>
  filter(!is.na(under_price)) |> 
  rename(under_agency = agency)

points_overs <-
player_points_data |>
  filter(market_name == "Player Points") |>
  select(match, start_time, market_name, player_name, player_team, line, over_price, opposition_team, agency) |> 
  rename(over_agency = agency)

points_arbs <-
points_unders |>
  inner_join(
    points_overs,
    by = c(
      "match",
      "start_time",
      "market_name",
      "player_name",
      "player_team",
      "line",
      "opposition_team"
    ),
    relationship = "many-to-many"
  ) |> 
  relocate(under_price, .after = over_price) |>
  mutate(margin = 1/under_price + 1/over_price) |> 
  arrange(margin) |> 
  mutate(margin = (1 - margin)) |>
  mutate(margin = 100*margin) |> 
  filter(margin > 0) |> 
  distinct(match, player_name, line, over_agency, under_agency, .keep_all = TRUE) |> 
  relocate(over_price, over_agency, under_price, under_agency, .after = opposition_team)

# Rebounds----------------------------------------------------------------------
rebounds_unders <-
player_rebounds_data |> 
  filter(market_name == "Player Rebounds") |>
  select(match, start_time, market_name, player_name, player_team, line, under_price, opposition_team, agency) |>
  filter(!is.na(under_price)) |> 
  rename(under_agency = agency)

rebounds_overs <-
player_rebounds_data |>
  filter(market_name == "Player Rebounds") |>
  select(match, start_time, market_name, player_name, player_team, line, over_price, opposition_team, agency) |> 
  rename(over_agency = agency)

rebounds_arbs <-
rebounds_unders |>
  inner_join(
    rebounds_overs,
    by = c(
      "match",
      "start_time",
      "market_name",
      "player_name",
      "player_team",
      "line",
      "opposition_team"
    ),
    relationship = "many-to-many"
  ) |> 
  relocate(under_price, .after = over_price) |>
  mutate(margin = 1/under_price + 1/over_price) |> 
  arrange(margin) |> 
  mutate(margin = (1 - margin)) |>
  mutate(margin = 100*margin) |> 
  filter(margin > 0) |> 
  distinct(match, player_name, line, over_agency, under_agency, .keep_all = TRUE) |> 
  relocate(over_price, over_agency, under_price, under_agency, .after = opposition_team)


# Assists-----------------------------------------------------------------------
assists_unders <-
  player_assists_data |> 
  filter(market_name == "Player Assists") |>
  select(match, start_time, market_name, player_name, player_team, line, under_price, opposition_team, agency) |>
  filter(!is.na(under_price)) |> 
  rename(under_agency = agency)

assists_overs <-
  player_assists_data |>
  filter(market_name == "Player Assists") |>
  select(match, start_time, market_name, player_name, player_team, line, over_price, opposition_team, agency) |> 
  rename(over_agency = agency)

assists_arbs <-
  assists_unders |>
  inner_join(
    assists_overs,
    by = c(
      "match",
      "start_time",
      "market_name",
      "player_name",
      "player_team",
      "line",
      "opposition_team"
    ),
    relationship = "many-to-many"
  ) |> 
  relocate(under_price, .after = over_price) |>
  mutate(margin = 1/under_price + 1/over_price) |> 
  arrange(margin) |> 
  mutate(margin = (1 - margin)) |>
  mutate(margin = 100*margin) |> 
  filter(margin > 0) |> 
  distinct(match, player_name, line, over_agency, under_agency, .keep_all = TRUE) |> 
  relocate(over_price, over_agency, under_price, under_agency, .after = opposition_team)


# PRAs--------------------------------------------------------------------------
pra_unders <-
  player_pras_data |> 
  filter(market_name == "Player PRAs") |>
  select(match, start_time, market_name, player_name, player_team, line, under_price, opposition_team, agency) |>
  filter(!is.na(under_price)) |> 
  rename(under_agency = agency)

pra_overs <-
  player_pras_data |>
  filter(market_name == "Player PRAs") |>
  select(match, start_time, market_name, player_name, player_team, line, over_price, opposition_team, agency) |> 
  rename(over_agency = agency)

pra_arbs <-
  pra_unders |>
  inner_join(
    pra_overs,
    by = c(
      "match",
      "start_time",
      "market_name",
      "player_name",
      "player_team",
      "line",
      "opposition_team"
    ),
    relationship = "many-to-many"
  ) |> 
  relocate(under_price, .after = over_price) |>
  mutate(margin = 1/under_price + 1/over_price) |> 
  arrange(margin) |> 
  mutate(margin = (1 - margin)) |>
  mutate(margin = 100*margin) |> 
  filter(margin > 0) |> 
  distinct(match, player_name, line, over_agency, under_agency, .keep_all = TRUE) |> 
  relocate(over_price, over_agency, under_price, under_agency, .after = opposition_team)


# Threes------------------------------------------------------------------------
threes_unders <-
  player_threes_data |> 
  filter(market_name == "Player Threes") |>
  select(match, start_time, market_name, player_name, player_team, line, under_price, opposition_team, agency) |>
  filter(!is.na(under_price)) |> 
  rename(under_agency = agency)

threes_overs <-
  player_threes_data |>
  filter(market_name == "Player Threes") |>
  select(match, start_time, market_name, player_name, player_team, line, over_price, opposition_team, agency) |> 
  rename(over_agency = agency)

threes_arbs <-
  threes_unders |>
  inner_join(
    threes_overs,
    by = c(
      "match",
      "start_time",
      "market_name",
      "player_name",
      "player_team",
      "line",
      "opposition_team"
    ),
    relationship = "many-to-many"
  ) |> 
  relocate(under_price, .after = over_price) |>
  mutate(margin = 1/under_price + 1/over_price) |> 
  arrange(margin) |> 
  mutate(margin = (1 - margin)) |>
  mutate(margin = 100*margin) |> 
  filter(margin > 0) |> 
  distinct(match, player_name, line, over_agency, under_agency, .keep_all = TRUE) |> 
  relocate(over_price, over_agency, under_price, under_agency, .after = opposition_team)


# Steals------------------------------------------------------------------------
steals_unders <-
  player_steals_data |> 
  filter(market_name == "Player Steals") |>
  select(match, start_time, market_name, player_name, player_team, line, under_price, opposition_team, agency) |>
  filter(!is.na(under_price)) |> 
  rename(under_agency = agency)

steals_overs <-
  player_steals_data |>
  filter(market_name == "Player Steals") |>
  select(match, start_time, market_name, player_name, player_team, line, over_price, opposition_team, agency) |> 
  rename(over_agency = agency)

steals_arbs <-
  steals_unders |>
  inner_join(
    steals_overs,
    by = c(
      "match",
      "start_time",
      "market_name",
      "player_name",
      "player_team",
      "line",
      "opposition_team"
    ),
    relationship = "many-to-many"
  ) |> 
  relocate(under_price, .after = over_price) |>
  mutate(margin = 1/under_price + 1/over_price) |> 
  arrange(margin) |> 
  mutate(margin = (1 - margin)) |>
  mutate(margin = 100*margin) |> 
  filter(margin > 0) |> 
  distinct(match, player_name, line, over_agency, under_agency, .keep_all = TRUE) |> 
  relocate(over_price, over_agency, under_price, under_agency, .after = opposition_team)


# Blocks------------------------------------------------------------------------
blocks_unders <-
  player_blocks_data |> 
  filter(market_name == "Player Blocks") |>
  select(match, start_time, market_name, player_name, player_team, line, under_price, opposition_team, agency) |>
  filter(!is.na(under_price)) |> 
  rename(under_agency = agency)

blocks_overs <-
  player_blocks_data |>
  filter(market_name == "Player Blocks") |>
  select(match, start_time, market_name, player_name, player_team, line, over_price, opposition_team, agency) |> 
  rename(over_agency = agency)

blocks_arbs <-
  blocks_unders |>
  inner_join(
    blocks_overs,
    by = c(
      "match",
      "start_time",
      "market_name",
      "player_name",
      "player_team",
      "line",
      "opposition_team"
    ),
    relationship = "many-to-many"
  ) |> 
  relocate(under_price, .after = over_price) |>
  mutate(margin = 1/under_price + 1/over_price) |> 
  arrange(margin) |> 
  mutate(margin = (1 - margin)) |>
  mutate(margin = 100*margin) |> 
  filter(margin > 0) |> 
  distinct(match, player_name, line, over_agency, under_agency, .keep_all = TRUE) |> 
  relocate(over_price, over_agency, under_price, under_agency, .after = opposition_team)

#===============================================================================
# Get all ARBs together
#===============================================================================

all_arbs <-
bind_rows(points_arbs,
          assists_arbs,
          rebounds_arbs,
          blocks_arbs,
          steals_arbs,
          threes_arbs,
          pra_arbs) |>
  arrange(desc(margin))



