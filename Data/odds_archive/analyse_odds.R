# Libraries and functions-------------------------------------------------------
library(tidyverse)

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

# Read in data------------------------------------------------------------------
scraped_odds_2023_12_23 <- read_csv("Data/odds_archive/scraped_odds_2023-12-23.csv")

# Read in stats-----------------------------------------------------------------

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
    season = SEASON_YEAR) |> 
  mutate(teamName = ifelse(teamName == "LA Clippers", "Los Angeles Clippers", teamName)) |>
  mutate(homeTeam = ifelse(homeTeam == "LA Clippers", "Los Angeles Clippers", homeTeam)) |>
  mutate(awayTeam = ifelse(awayTeam == "LA Clippers", "Los Angeles Clippers", awayTeam)) |>
  mutate(oppositionTeam = ifelse(oppositionTeam == "LA Clippers", "Los Angeles Clippers", oppositionTeam))

# Create Home / Away variable
all_player_stats <-
  all_player_stats |>
  mutate(team_full = paste(teamCity, teamName)) |>
  mutate(home_away = if_else(team_full == HOME_TEAM, "Home", "Away"))

# Get results data to join to odds----------------------------------------------
player_results <-
  all_player_stats |> 
  transmute(match = paste(HOME_TEAM, AWAY_TEAM, sep = " v "),
            player_name = PLAYER_NAME,
            team = team_full,
            date = GAME_DATE + days(1),
            points = PTS,
            rebounds = REB,
            assists = AST,
            steals = STL,
            blocks = BLK,
            minutes = MIN,
            PRA)
  
##%######################################################%##
#                                                          #
####             Get Player Points Markets              ####
#                                                          #
##%######################################################%##

#===============================================================================
# Overs
#===============================================================================

player_points_markets <-
  scraped_odds_2023_12_23 |>
  filter(market_name == "Player Points")

# Propogate Start Date forward if missing
player_points_markets <-
player_points_markets |>
  group_by(match) |> 
  fill(date, .direction = "downup") |> 
  ungroup() |> 
  left_join(player_results, by = c("match", "player_name", "date"))

# Get only the best market odds available---------------------------------------
player_points_markets_overs_best <-
  player_points_markets |>
  arrange(match,player_name,line, desc(over_price)) |> 
  group_by(match, player_name, line) |>
  slice_head(n = 1)
  
# Test Positive diff last 10 and season strategy--------------------------------
strategy_1 <-
  player_points_markets_overs_best |> 
  filter(diff_over_2023_24 >= 0 & diff_over_last_10 >= 0.03) |> 
  filter(!is.na(minutes)) |> 
  arrange(player_name, desc(diff_over_2023_24)) |> 
  mutate(result = ifelse(points >= line, (100*over_price) - 100, -100))

# Test Diff vs average market odds strategy-------------------------------------
strategy_2 <-
  player_points_markets |> 
  filter(!is.na(minutes)) |> 
  group_by(match, player_name, line) |>
  mutate(median_price = median(over_price)) |>
  mutate(diff_over_median = 1/median_price - 1/over_price) |>
  arrange(player_name, line, desc(diff_over_median)) |>
  slice_head(n = 1) |>
  ungroup() |>
  filter(diff_over_median >= 0.025) |>
  mutate(result = ifelse(points >= line, (100*over_price) - 100, -100))

#===============================================================================
# Unders
#===============================================================================

# Get only the best market odds available---------------------------------------
player_points_markets_unders_best <-
  player_points_markets |>
  filter(!is.na(under_price)) |>
  arrange(match,player_name,line, desc(under_price)) |> 
  group_by(match, player_name, line) |>
  slice_head(n = 1)

# Test Positive diff last 10 and season strategy--------------------------------
strategy_1 <-
  player_points_markets_unders_best |> 
  filter(diff_under_2023_24 >= 0 & diff_under_last_10 >= 0.03) |> 
  filter(!is.na(minutes)) |> 
  arrange(player_name, desc(diff_under_2023_24)) |> 
  mutate(result = ifelse(points >= line, (100*under_price) - 100, -100))

# Test Diff vs average market odds strategy-------------------------------------
strategy_2 <-
  player_points_markets |> 
  filter(!is.na(minutes)) |> 
  group_by(match, player_name, line) |>
  mutate(median_price = median(under_price)) |>
  mutate(diff_under_median = 1/median_price - 1/under_price) |>
  arrange(player_name, line, desc(diff_under_median)) |>
  slice_head(n = 1) |>
  ungroup() |>
  filter(diff_under_median >= 0.025) |>
  mutate(result = ifelse(points >= line, (100*under_price) - 100, -100))

##%######################################################%##
#                                                          #
####           Get Player Rebounds Markets              ####
#                                                          #
##%######################################################%##

#===============================================================================
# Overs
#===============================================================================

player_rebounds_markets <-
  scraped_odds_2023_12_23 |>
  filter(market_name == "Player Rebounds")

# Propogate Start Date forward if missing
player_rebounds_markets <-
  player_rebounds_markets |>
  group_by(match) |> 
  fill(date, .direction = "downup") |> 
  ungroup() |> 
  left_join(player_results, by = c("match", "player_name", "date"))

# Get only the best market odds available---------------------------------------
player_rebounds_markets_overs_best <-
  player_rebounds_markets |>
  arrange(match,player_name,line, desc(over_price)) |> 
  group_by(match, player_name, line) |>
  slice_head(n = 1)

# Test Positive diff last 10 and season strategy--------------------------------
strategy_1 <-
  player_rebounds_markets_overs_best |> 
  filter(agency != "Unibet") |> 
  filter(diff_over_2023_24 >= 0 & diff_over_last_10 >= 0.03) |> 
  filter(!is.na(minutes)) |> 
  arrange(player_name, desc(diff_over_2023_24)) |> 
  mutate(result = ifelse(rebounds >= line, (100*over_price) - 100, -100))

sum(strategy_1$result)

# Test Diff vs average market odds strategy-------------------------------------
strategy_2 <-
  player_rebounds_markets |> 
  filter(!is.na(minutes)) |> 
  group_by(match, player_name, line) |>
  mutate(median_price = median(over_price)) |>
  mutate(diff_over_median = 1/median_price - 1/over_price) |>
  arrange(player_name, line, desc(diff_over_median)) |>
  slice_head(n = 1) |>
  ungroup() |>
  filter(diff_over_median >= 0.025) |>
  mutate(result = ifelse(rebounds >= line, (100*over_price) - 100, -100))

sum(strategy_2$result)

#===============================================================================
# Unders
#===============================================================================

# Get only the best market odds available---------------------------------------
player_rebounds_markets_unders_best <-
  player_rebounds_markets |>
  filter(!is.na(under_price)) |>
  arrange(match,player_name,line, desc(under_price)) |> 
  group_by(match, player_name, line) |>
  slice_head(n = 1)

# Test Positive diff last 10 and season strategy--------------------------------
strategy_1 <-
  player_rebounds_markets_unders_best |> 
  filter(diff_under_2023_24 >= 0 & diff_under_last_10 >= 0.03) |> 
  filter(!is.na(minutes)) |> 
  arrange(player_name, desc(diff_under_2023_24)) |> 
  mutate(result = ifelse(rebounds >= line, (100*under_price) - 100, -100))

# Test Diff vs average market odds strategy-------------------------------------
strategy_2 <-
  player_rebounds_markets |> 
  filter(!is.na(minutes)) |> 
  group_by(match, player_name, line) |>
  mutate(median_price = median(under_price)) |>
  mutate(diff_under_median = 1/median_price - 1/under_price) |>
  arrange(player_name, line, desc(diff_under_median)) |>
  slice_head(n = 1) |>
  ungroup() |>
  filter(diff_under_median >= 0.025) |>
  mutate(result = ifelse(rebounds >= line, (100*under_price) - 100, -100))

##%######################################################%##
#                                                          #
####            Get Player Assists Markets              ####
#                                                          #
##%######################################################%##

#===============================================================================
# Overs
#===============================================================================

player_assists_markets <-
  scraped_odds_2023_12_23 |>
  filter(market_name == "Player Assists")

# Propogate Start Date forward if missing
player_assists_markets <-
  player_assists_markets |>
  group_by(match) |> 
  fill(date, .direction = "downup") |> 
  ungroup() |> 
  left_join(player_results, by = c("match", "player_name", "date"))

# Get only the best market odds available---------------------------------------
player_assists_markets_overs_best <-
  player_assists_markets |>
  arrange(match,player_name,line, desc(over_price)) |> 
  group_by(match, player_name, line) |>
  slice_head(n = 1)

# Test Positive diff last 10 and season strategy--------------------------------
strategy_1 <-
  player_assists_markets_overs_best |> 
  filter(agency != "Unibet") |> 
  filter(diff_over_2023_24 >= 0 & diff_over_last_10 >= 0.03) |> 
  filter(!is.na(minutes)) |> 
  arrange(player_name, desc(diff_over_2023_24)) |> 
  mutate(result = ifelse(assists >= line, (100*over_price) - 100, -100))

sum(strategy_1$result)

# Test Diff vs average market odds strategy-------------------------------------
strategy_2 <-
  player_assists_markets |> 
  filter(!is.na(minutes)) |> 
  group_by(match, player_name, line) |>
  mutate(median_price = median(over_price)) |>
  mutate(diff_over_median = 1/median_price - 1/over_price) |>
  arrange(player_name, line, desc(diff_over_median)) |>
  slice_head(n = 1) |>
  ungroup() |>
  filter(diff_over_median >= 0.025) |>
  mutate(result = ifelse(assists >= line, (100*over_price) - 100, -100))

sum(strategy_2$result)

##%######################################################%##
#                                                          #
####              Get Player PRAs Markets               ####
#                                                          #
##%######################################################%##

#===============================================================================
# Overs
#===============================================================================

player_pras_markets <-
  scraped_odds_2023_12_23 |>
  filter(market_name == "Player PRAs")

# Propogate Start Date forward if missing
player_pras_markets <-
  player_pras_markets |>
  group_by(match) |> 
  fill(date, .direction = "downup") |> 
  ungroup() |> 
  left_join(player_results, by = c("match", "player_name", "date"))

# Get only the best market odds available---------------------------------------
player_pras_markets_overs_best <-
  player_pras_markets |>
  arrange(match,player_name,line, desc(over_price)) |> 
  group_by(match, player_name, line) |>
  slice_head(n = 1)

# Test Positive diff last 10 and season strategy--------------------------------
strategy_1 <-
  player_pras_markets_overs_best |> 
  filter(agency != "Unibet") |> 
  filter(diff_over_2023_24 >= 0 & diff_over_last_10 >= 0.03) |> 
  filter(!is.na(minutes)) |> 
  arrange(player_name, desc(diff_over_2023_24)) |> 
  mutate(result = ifelse(PRA >= line, (100*over_price) - 100, -100))

sum(strategy_1$result)

# Test Diff vs average market odds strategy-------------------------------------
strategy_2 <-
  player_pras_markets |> 
  filter(!is.na(minutes)) |> 
  filter(agency != "Unibet") |> 
  group_by(match, player_name, line) |>
  mutate(median_price = median(over_price)) |>
  mutate(diff_over_median = 1/median_price - 1/over_price) |>
  arrange(player_name, line, desc(diff_over_median)) |>
  slice_head(n = 1) |>
  ungroup() |>
  filter(diff_over_median >= 0.025) |>
  mutate(result = ifelse(PRA >= line, (100*over_price) - 100, -100))

sum(strategy_2$result)
