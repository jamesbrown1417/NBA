#===============================================================================
# Libraries and functions
#===============================================================================

library(tidyverse)
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

# Combined stats for rolling windows
combined_stats_all <- bind_rows(combined_stats_2024_2025, combined_stats_2025_2026)

# Precompute rolling windows (used by get_empirical_prob and furrr globals)
player_stats_last_5_global <-
  combined_stats_all |>
  group_by(personId) |>
  arrange(desc(GAME_DATE)) |>
  slice(1:5) |>
  ungroup()

player_stats_last_10_global <-
  combined_stats_all |>
  group_by(personId) |>
  arrange(desc(GAME_DATE)) |>
  slice(1:10) |>
  ungroup()

player_stats_last_20_global <-
  combined_stats_all |>
  group_by(personId) |>
  arrange(desc(GAME_DATE)) |>
  slice(1:20) |>
  ungroup()

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
  
  # Last 5/10/20 games (precomputed)
  player_stats_last_5 <- player_stats_last_5_global
  player_stats_last_10 <- player_stats_last_10_global
  player_stats_last_20 <- player_stats_last_20_global
  
  # Initialize empirical_prob
  empirical_prob <- NULL
  
  # Branch based on whether stat is PTS, REB or AST, etc.
  if (stat == "PTS") {
    empirical_prob <-
      player_stats |>
      group_by(PLAYER_NAME) |>
      summarise(games_played = n(),
                empirical_prob = mean(PTS >= line),
                empirical_prob_under = mean(PTS < line)) |>
      ungroup()
    
    if (season == "2024_2025") {
      last_5 <- player_stats_last_5 |>
        group_by(PLAYER_NAME) |>
        summarise(empirical_prob_last_5 = mean(PTS >= line),
                  empirical_prob_under_last_5 = mean(PTS < line)) |>
        ungroup()
      
      last_10 <- player_stats_last_10 |> 
        group_by(PLAYER_NAME) |>
        summarise(empirical_prob_last_10 = mean(PTS >= line),
                  empirical_prob_under_last_10 = mean(PTS < line)) |>
        ungroup()
      
      last_20 <- player_stats_last_20 |> 
        group_by(PLAYER_NAME) |>
        summarise(empirical_prob_last_20 = mean(PTS >= line),
                  empirical_prob_under_last_20 = mean(PTS < line)) |>
        ungroup()
      
      empirical_prob <-
        empirical_prob |>
        left_join(last_5, by = "PLAYER_NAME") |>
        left_join(last_10, by = "PLAYER_NAME") |>
        left_join(last_20, by = "PLAYER_NAME") |>
        select(-PLAYER_NAME)
    }
    
  } else if (stat == "REB") {
    empirical_prob <-
      player_stats |>
      group_by(PLAYER_NAME) |>
      summarise(games_played = n(),
                empirical_prob = mean(REB >= line),
                empirical_prob_under = mean(REB < line)) |>
      ungroup()
    
    if (season == "2024_2025") {
      last_5 <- player_stats_last_5 |>
        group_by(PLAYER_NAME) |>
        summarise(empirical_prob_last_5 = mean(REB >= line),
                  empirical_prob_under_last_5 = mean(REB < line)) |>
        ungroup()
      
      last_10 <- player_stats_last_10 |> 
        group_by(PLAYER_NAME) |>
        summarise(empirical_prob_last_10 = mean(REB >= line),
                  empirical_prob_under_last_10 = mean(REB < line)) |>
        ungroup()
      
      last_20 <- player_stats_last_20 |> 
        group_by(PLAYER_NAME) |>
        summarise(empirical_prob_last_20 = mean(REB >= line),
                  empirical_prob_under_last_20 = mean(REB < line)) |>
        ungroup()
      
      empirical_prob <-
        empirical_prob |>
        left_join(last_5, by = "PLAYER_NAME") |>
        left_join(last_10, by = "PLAYER_NAME") |>
        left_join(last_20, by = "PLAYER_NAME") |>
        select(-PLAYER_NAME)
    }
  } else if (stat == "AST") {
    empirical_prob <-
      player_stats |>
      group_by(PLAYER_NAME) |>
      summarise(games_played = n(),
                empirical_prob = mean(AST >= line),
                empirical_prob_under = mean(AST < line)) |>
      ungroup()
    
    if (season == "2024_2025") {
      last_5 <- player_stats_last_5 |>
        group_by(PLAYER_NAME) |>
        summarise(empirical_prob_last_5 = mean(AST >= line),
                  empirical_prob_under_last_5 = mean(AST < line)) |>
        ungroup()
      
      last_10 <- player_stats_last_10 |> 
        group_by(PLAYER_NAME) |>
        summarise(empirical_prob_last_10 = mean(AST >= line),
                  empirical_prob_under_last_10 = mean(AST < line)) |>
        ungroup()
      
      last_20 <- player_stats_last_20 |> 
        group_by(PLAYER_NAME) |>
        summarise(empirical_prob_last_20 = mean(AST >= line),
                  empirical_prob_under_last_20 = mean(AST < line)) |>
        ungroup()
      
      empirical_prob <-
        empirical_prob |>
        left_join(last_5, by = "PLAYER_NAME") |>
        left_join(last_10, by = "PLAYER_NAME") |>
        left_join(last_20, by = "PLAYER_NAME") |>
        select(-PLAYER_NAME)
    }
  } else if (stat == "STL") {
    empirical_prob <-
      player_stats |>
      group_by(PLAYER_NAME) |>
      summarise(games_played = n(),
                empirical_prob = mean(STL >= line),
                empirical_prob_under = mean(STL < line)) |>
      ungroup()
    
    if (season == "2024_2025") {
      last_5 <- player_stats_last_5 |>
        group_by(PLAYER_NAME) |>
        summarise(empirical_prob_last_5 = mean(STL >= line),
                  empirical_prob_under_last_5 = mean(STL < line)) |>
        ungroup()
      
      last_10 <- player_stats_last_10 |> 
        group_by(PLAYER_NAME) |>
        summarise(empirical_prob_last_10 = mean(STL >= line),
                  empirical_prob_under_last_10 = mean(STL < line)) |>
        ungroup()
      
      last_20 <- player_stats_last_20 |> 
        group_by(PLAYER_NAME) |>
        summarise(empirical_prob_last_20 = mean(STL >= line),
                  empirical_prob_under_last_20 = mean(STL < line)) |>
        ungroup()
      
      empirical_prob <-
        empirical_prob |>
        left_join(last_5, by = "PLAYER_NAME") |>
        left_join(last_10, by = "PLAYER_NAME") |>
        left_join(last_20, by = "PLAYER_NAME") |>
        select(-PLAYER_NAME)
    }
  } else if (stat == "BLK") {
    empirical_prob <-
      player_stats |>
      group_by(PLAYER_NAME) |>
      summarise(games_played = n(),
                empirical_prob = mean(BLK >= line),
                empirical_prob_under = mean(BLK < line)) |>
      ungroup()
    
    if (season == "2024_2025") {
      last_5 <- player_stats_last_5 |>
        group_by(PLAYER_NAME) |>
        summarise(empirical_prob_last_5 = mean(BLK >= line),
                  empirical_prob_under_last_5 = mean(BLK < line)) |>
        ungroup()
      
      last_10 <- player_stats_last_10 |> 
        group_by(PLAYER_NAME) |>
        summarise(empirical_prob_last_10 = mean(BLK >= line),
                  empirical_prob_under_last_10 = mean(BLK < line)) |>
        ungroup()
      
      last_20 <- player_stats_last_20 |> 
        group_by(PLAYER_NAME) |>
        summarise(empirical_prob_last_20 = mean(BLK >= line),
                  empirical_prob_under_last_20 = mean(BLK < line)) |>
        ungroup()
      
      empirical_prob <-
        empirical_prob |>
        left_join(last_5, by = "PLAYER_NAME") |>
        left_join(last_10, by = "PLAYER_NAME") |>
        left_join(last_20, by = "PLAYER_NAME") |>
        select(-PLAYER_NAME)
    }
  } else if (stat == "Threes") {
    empirical_prob <-
      player_stats |>
      group_by(PLAYER_NAME) |>
      summarise(games_played = n(),
                empirical_prob = mean(Threes >= line),
                empirical_prob_under = mean(Threes < line)) |>
      ungroup()
    
    if (season == "2024_2025") {
      last_5 <- player_stats_last_5 |>
        group_by(PLAYER_NAME) |>
        summarise(empirical_prob_last_5 = mean(Threes >= line),
                  empirical_prob_under_last_5 = mean(Threes < line)) |>
        ungroup()
      
      last_10 <- player_stats_last_10 |> 
        group_by(PLAYER_NAME) |>
        summarise(empirical_prob_last_10 = mean(Threes >= line),
                  empirical_prob_under_last_10 = mean(Threes < line)) |>
        ungroup()
      
      last_20 <- player_stats_last_20 |> 
        group_by(PLAYER_NAME) |>
        summarise(empirical_prob_last_20 = mean(Threes >= line),
                  empirical_prob_under_last_20 = mean(Threes < line)) |>
        ungroup()
      
      empirical_prob <-
        empirical_prob |>
        left_join(last_5, by = "PLAYER_NAME") |>
        left_join(last_10, by = "PLAYER_NAME") |>
        left_join(last_20, by = "PLAYER_NAME") |>
        select(-PLAYER_NAME)
    }
  } else if (stat == "PRA") {
    empirical_prob <-
      player_stats |>
      group_by(PLAYER_NAME) |>
      summarise(games_played = n(),
                empirical_prob = mean(PRA >= line),
                empirical_prob_under = mean(PRA < line)) |>
      ungroup()
    
    if (season == "2024_2025") {
      last_5 <- player_stats_last_5 |>
        group_by(PLAYER_NAME) |>
        summarise(empirical_prob_last_5 = mean(PRA >= line),
                  empirical_prob_under_last_5 = mean(PRA < line)) |>
        ungroup()
      
      last_10 <- player_stats_last_10 |> 
        group_by(PLAYER_NAME) |>
        summarise(empirical_prob_last_10 = mean(PRA >= line),
                  empirical_prob_under_last_10 = mean(PRA < line)) |>
        ungroup()
      
      last_20 <- player_stats_last_20 |> 
        group_by(PLAYER_NAME) |>
        summarise(empirical_prob_last_20 = mean(PRA >= line),
                  empirical_prob_under_last_20 = mean(PRA < line)) |>
        ungroup()
      
      empirical_prob <-
        empirical_prob |>
        left_join(last_5, by = "PLAYER_NAME") |>
        left_join(last_10, by = "PLAYER_NAME") |>
        left_join(last_20, by = "PLAYER_NAME") |>
        select(-PLAYER_NAME)
    }
  } else {
    stop("stat must be one of PTS, REB, AST, STL, BLK, Threes, or PRA")
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
