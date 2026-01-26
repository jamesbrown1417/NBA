#===============================================================================
# Libraries and functions
#===============================================================================

library(tidyverse)
library(memoise)
library(digest)
`%notin%` <- Negate(`%in%`)

#===============================================================================
# Read in past season stats
#===============================================================================

combined_stats_2024_2025 <- read_csv("Data/all_player_stats_2024-2025.csv")
combined_stats_2025_2026 <- read_csv("Data/all_player_stats_2025-2026.csv")
all_rosters <- read_csv("Data/all_rosters.csv")

# Add names and rename vars
combined_stats_2024_2025 <-
  combined_stats_2024_2025 |>
  left_join(all_rosters[c("PLAYER", "PLAYER_ID")], by = c("personId" = "PLAYER_ID")) |>
  rename(
    PLAYER_NAME = PLAYER,
    PTS = points,
    REB = reboundsTotal,
    AST = assists,
    STL = steals,
    BLK = blocks,
    Threes = threePointersMade
  ) |> 
  mutate(PRA = PTS + REB + AST,
         RA = REB + AST,
         Stocks = STL + BLK
         )

combined_stats_2025_2026 <-
  combined_stats_2025_2026 |>
  left_join(all_rosters[c("PLAYER", "PLAYER_ID")], by = c("personId" = "PLAYER_ID")) |>
  rename(
    PLAYER_NAME = PLAYER,
    PTS = points,
    REB = reboundsTotal,
    AST = assists,
    STL = steals,
    BLK = blocks,
    Threes = threePointersMade
  ) |>
  mutate(PRA = PTS + REB + AST,
         RA = REB + AST,
         Stocks = STL + BLK
  )

#===============================================================================
# Pre-compute time window datasets (last 5, 10, 20 games)
#===============================================================================

# Combine both seasons for time window analysis
combined_stats_all_seasons <-
  combined_stats_2024_2025 |>
  bind_rows(combined_stats_2025_2026)

# Pre-compute last 5 games for all players
player_stats_last_5_global <-
  combined_stats_all_seasons |>
  group_by(personId) |>
  arrange(desc(GAME_DATE)) |>
  slice(1:5) |>
  ungroup()

# Pre-compute last 10 games for all players
player_stats_last_10_global <-
  combined_stats_all_seasons |>
  group_by(personId) |>
  arrange(desc(GAME_DATE)) |>
  slice(1:10) |>
  ungroup()

# Pre-compute last 20 games for all players
player_stats_last_20_global <-
  combined_stats_all_seasons |>
  group_by(personId) |>
  arrange(desc(GAME_DATE)) |>
  slice(1:20) |>
  ungroup()

# Clean up temporary combined dataset
rm(combined_stats_all_seasons)

#===============================================================================
# Create a function that takes a player name + line and returns their hit rate
#===============================================================================

get_empirical_prob <- function(player_name, line, stat, season) {
  
  # Choose the data based on the selected season
  if (season == "2024_2025") {
    player_stats <- combined_stats_2024_2025 |> filter(PLAYER_NAME == player_name) |> filter(!is.na(minutes))
  } else if (season == "2025_2026") {
    player_stats <- combined_stats_2025_2026 |> filter(PLAYER_NAME == player_name) |> filter(!is.na(minutes))
  } else {
    stop("Invalid season selected")
  }

  # Validate stat parameter
  valid_stats <- c("PTS", "REB", "AST", "STL", "BLK", "Threes", "PRA")
  if (!(stat %in% valid_stats)) {
    stop(paste("stat must be one of:", paste(valid_stats, collapse = ", ")))
  }

  # Initialize empirical_prob
  empirical_prob <- NULL

  # Get the stat column dynamically using tidy evaluation
  stat_column <- sym(stat)

  # Compute empirical probabilities for the season
  empirical_prob <-
    player_stats |>
    group_by(PLAYER_NAME) |>
    summarise(
      games_played = n(),
      empirical_prob = mean(!!stat_column >= line),
      empirical_prob_under = mean(!!stat_column < line)
    ) |>
    ungroup()

  # If season is 2024_2025, add last 5/10/20 statistics
  if (season == "2024_2025") {

    last_5 <- player_stats_last_5_global |>
      filter(PLAYER_NAME == player_name) |>
      summarise(
        empirical_prob_last_5 = mean(!!stat_column >= line),
        empirical_prob_under_last_5 = mean(!!stat_column < line)
      )

    last_10 <- player_stats_last_10_global |>
      filter(PLAYER_NAME == player_name) |>
      summarise(
        empirical_prob_last_10 = mean(!!stat_column >= line),
        empirical_prob_under_last_10 = mean(!!stat_column < line)
      )

    last_20 <- player_stats_last_20_global |>
      filter(PLAYER_NAME == player_name) |>
      summarise(
        empirical_prob_last_20 = mean(!!stat_column >= line),
        empirical_prob_under_last_20 = mean(!!stat_column < line)
      )

    empirical_prob <-
      empirical_prob |>
      bind_cols(last_5) |>
      bind_cols(last_10) |>
      bind_cols(last_20)
  }
  
  
  # Add line, player_name, and season information
  empirical_prob <- empirical_prob |> 
    mutate(line = line, 
           player_name = player_name, 
           season = season)
  
  if (season == "2024_2025") {
    empirical_prob <-
      empirical_prob
  }
  
  # Rename the empirical_prob column to include season
  new_col_name <- paste("empirical_prob", season, sep = "_")
  empirical_prob <- empirical_prob |>
    rename_with(~ new_col_name, .cols = "empirical_prob")

  # Return empirical probability
  return(empirical_prob)
}

#===============================================================================
# Add memoization for performance
#===============================================================================

# Create memoised version with in-memory cache
# This provides transparent caching for repeated queries
get_empirical_prob <- memoise(get_empirical_prob)