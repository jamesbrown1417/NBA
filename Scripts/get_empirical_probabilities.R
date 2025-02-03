#===============================================================================
# Libraries and functions
#===============================================================================

library(tidyverse)
`%notin%` <- Negate(`%in%`)

#===============================================================================
# Read in past season stats
#===============================================================================

combined_stats_2024_2025 <- read_csv("Data/all_player_stats_2024-2025.csv")
combined_stats_2023_2024 <- read_csv("Data/all_player_stats_2023-2024.csv")
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
  mutate(PRA = PTS + REB + AST)

combined_stats_2023_2024 <-
  combined_stats_2023_2024 |>
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
  mutate(PRA = PTS + REB + AST)

#===============================================================================
# Create a function that takes a player name + line and returns their hit rate
#===============================================================================

get_empirical_prob <- function(player_name, line, stat, season) {
  
  # Choose the data based on the selected season
  if (season == "2024_2025") {
    player_stats <- combined_stats_2024_2025 |> filter(PLAYER_NAME == player_name) |> filter(!is.na(minutes))
  } else if (season == "2023_2024") {
    player_stats <- combined_stats_2023_2024 |> filter(PLAYER_NAME == player_name) |> filter(!is.na(minutes))
  } else {
    stop("Invalid season selected")
  }
  
  # Last 10 games
  player_stats_last_10 <-
    combined_stats_2024_2025 |>
    bind_rows(combined_stats_2023_2024) |>
    group_by(personId) |> 
    arrange(desc(GAME_DATE)) |> 
    slice(1:10) |>
    ungroup()
  
  # Last 20 games
  player_stats_last_20 <-
    combined_stats_2024_2025 |>
    bind_rows(combined_stats_2023_2024) |>
    group_by(personId) |> 
    arrange(desc(GAME_DATE)) |> 
    slice(1:20) |>
    ungroup()
  
  # Initialize empirical_prob
  empirical_prob <- NULL
  
  # Branch based on whether stat is PTS, REB or AST, etc.
  if (stat == "PTS") {
    empirical_prob <-
      player_stats |> 
      group_by(PLAYER_NAME) |> 
      summarise(games_played = n(),
                empirical_prob = mean(PTS >= line)) |> 
      ungroup()
    
    if (season == "2024_2025") {
      last_10 <- player_stats_last_10 |> 
        group_by(PLAYER_NAME) |>
        summarise(empirical_prob_last_10 = mean(PTS >= line)) |> 
        ungroup()
      
      last_20 <- player_stats_last_20 |> 
        group_by(PLAYER_NAME) |>
        summarise(empirical_prob_last_20 = mean(PTS >= line)) |> 
        ungroup()
      
      empirical_prob <-
        empirical_prob |>
        left_join(last_10, by = "PLAYER_NAME") |>
        left_join(last_20, by = "PLAYER_NAME") |>
        select(-PLAYER_NAME)
    }
    
  } else if (stat == "REB") {
    empirical_prob <-
      player_stats |> 
      group_by(PLAYER_NAME) |> 
      summarise(games_played = n(),
                empirical_prob = mean(REB >= line)) |> 
      ungroup()
    
    if (season == "2024_2025") {
      last_10 <- player_stats_last_10 |> 
        group_by(PLAYER_NAME) |>
        summarise(empirical_prob_last_10 = mean(REB >= line)) |> 
        ungroup()
      
      last_20 <- player_stats_last_20 |> 
        group_by(PLAYER_NAME) |>
        summarise(empirical_prob_last_20 = mean(REB >= line)) |> 
        ungroup()
      
      empirical_prob <-
        empirical_prob |>
        left_join(last_10, by = "PLAYER_NAME") |>
        left_join(last_20, by = "PLAYER_NAME") |>
        select(-PLAYER_NAME)
    }
  } else if (stat == "AST") {
    empirical_prob <-
      player_stats |> 
      group_by(PLAYER_NAME) |> 
      summarise(games_played = n(),
                empirical_prob = mean(AST >= line)) |> 
      ungroup()
    
    if (season == "2024_2025") {
      last_10 <- player_stats_last_10 |> 
        group_by(PLAYER_NAME) |>
        summarise(empirical_prob_last_10 = mean(AST >= line)) |> 
        ungroup()
      
      last_20 <- player_stats_last_20 |> 
        group_by(PLAYER_NAME) |>
        summarise(empirical_prob_last_20 = mean(AST >= line)) |> 
        ungroup()
      
      empirical_prob <-
        empirical_prob |>
        left_join(last_10, by = "PLAYER_NAME") |>
        left_join(last_20, by = "PLAYER_NAME") |>
        select(-PLAYER_NAME)
    }
  } else if (stat == "STL") {
    empirical_prob <-
      player_stats |> 
      group_by(PLAYER_NAME) |> 
      summarise(games_played = n(),
                empirical_prob = mean(STL >= line)) |> 
      ungroup()
    
    if (season == "2024_2025") {
      last_10 <- player_stats_last_10 |> 
        group_by(PLAYER_NAME) |>
        summarise(empirical_prob_last_10 = mean(STL >= line)) |> 
        ungroup()
      
      last_20 <- player_stats_last_20 |> 
        group_by(PLAYER_NAME) |>
        summarise(empirical_prob_last_20 = mean(STL >= line)) |> 
        ungroup()
      
      empirical_prob <-
        empirical_prob |>
        left_join(last_10, by = "PLAYER_NAME") |>
        left_join(last_20, by = "PLAYER_NAME") |>
        select(-PLAYER_NAME)
    }
  } else if (stat == "BLK") {
    empirical_prob <-
      player_stats |> 
      group_by(PLAYER_NAME) |> 
      summarise(games_played = n(),
                empirical_prob = mean(BLK >= line)) |> 
      ungroup()
    
    if (season == "2024_2025") {
      last_10 <- player_stats_last_10 |> 
        group_by(PLAYER_NAME) |>
        summarise(empirical_prob_last_10 = mean(BLK >= line)) |> 
        ungroup()
      
      last_20 <- player_stats_last_20 |> 
        group_by(PLAYER_NAME) |>
        summarise(empirical_prob_last_20 = mean(BLK >= line)) |> 
        ungroup()
      
      empirical_prob <-
        empirical_prob |>
        left_join(last_10, by = "PLAYER_NAME") |>
        left_join(last_20, by = "PLAYER_NAME") |>
        select(-PLAYER_NAME)
    }
  } else if (stat == "Threes") {
    empirical_prob <-
      player_stats |> 
      group_by(PLAYER_NAME) |> 
      summarise(games_played = n(),
                empirical_prob = mean(Threes >= line)) |> 
      ungroup()
    
    if (season == "2024_2025") {
      last_10 <- player_stats_last_10 |> 
        group_by(PLAYER_NAME) |>
        summarise(empirical_prob_last_10 = mean(Threes >= line)) |> 
        ungroup()
      
      last_20 <- player_stats_last_20 |> 
        group_by(PLAYER_NAME) |>
        summarise(empirical_prob_last_20 = mean(Threes >= line)) |> 
        ungroup()
      
      empirical_prob <-
        empirical_prob |>
        left_join(last_10, by = "PLAYER_NAME") |>
        left_join(last_20, by = "PLAYER_NAME") |>
        select(-PLAYER_NAME)
    }
  } else if (stat == "PRA") {
    empirical_prob <-
      player_stats |> 
      group_by(PLAYER_NAME) |> 
      summarise(games_played = n(),
                empirical_prob = mean(PRA >= line)) |> 
      ungroup()
    
    if (season == "2024_2025") {
      last_10 <- player_stats_last_10 |> 
        group_by(PLAYER_NAME) |>
        summarise(empirical_prob_last_10 = mean(PRA >= line)) |> 
        ungroup()
      
      last_20 <- player_stats_last_20 |> 
        group_by(PLAYER_NAME) |>
        summarise(empirical_prob_last_20 = mean(PRA >= line)) |> 
        ungroup()
      
      empirical_prob <-
        empirical_prob |>
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