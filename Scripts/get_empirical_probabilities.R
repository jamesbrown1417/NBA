#===============================================================================
# Libraries and functions
#===============================================================================

library(tidyverse)
`%notin%` <- Negate(`%in%`)

#===============================================================================
# Read in past season stats
#===============================================================================

combined_stats_2022_2023 <- read_csv("Data/all_player_stats_2022-2023.csv")
combined_stats_2023_2024 <- read_csv("Data/all_player_stats_2023-2024.csv")
all_rosters <- read_csv("Data/all_rosters.csv")

# Add names and rename vars
combined_stats_2022_2023 <-
  combined_stats_2022_2023 |>
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
  if (season == "2022_2023") {
    player_stats <- combined_stats_2022_2023 |> filter(PLAYER_NAME == player_name)
  } else if (season == "2023_2024") {
    player_stats <- combined_stats_2023_2024 |> filter(PLAYER_NAME == player_name)
  } else {
    stop("Invalid season selected")
  }
  
  # Initialize empirical_prob
  empirical_prob <- NULL
  
  # Branch based on whether stat is PTS, REB or AST
  if (stat == "PTS") {
    empirical_prob <- player_stats |> 
      summarise(games_played = n(),
                empirical_prob = mean(PTS >= line))
  } else if (stat == "REB") {
    empirical_prob <- player_stats |> 
      summarise(games_played = n(),
                empirical_prob = mean(REB >= line))
  } else if (stat == "AST") {
    empirical_prob <- player_stats |> 
      summarise(games_played = n(),
                empirical_prob = mean(AST >= line))
  } else if (stat == "STL") {
    empirical_prob <- player_stats |> 
      summarise(games_played = n(),
                empirical_prob = mean(STL >= line))
  } else if (stat == "BLK") {
    empirical_prob <- player_stats |> 
      summarise(games_played = n(),
                empirical_prob = mean(BLK >= line))
  } else if (stat == "Threes") {
    empirical_prob <- player_stats |> 
      summarise(games_played = n(),
                empirical_prob = mean(Threes >= line))
  } else if (stat == "PRA") {
    empirical_prob <- player_stats |> 
      summarise(games_played = n(),
                empirical_prob = mean(PRA >= line))
  } else {
    stop("stat must be one of PTS, REB, AST, STL, BLK, Threes, or PRA")
  }
  
  # Add line, player_name, and season information
  empirical_prob <- empirical_prob |> 
    mutate(line = line, 
           player_name = player_name, 
           season = season)
  
  # Rename the empirical_prob column to include season
  new_col_name <- paste("empirical_prob", season, sep = "_")
  empirical_prob <- empirical_prob |> 
    rename_with(~ new_col_name, .cols = "empirical_prob")
  
  # Return empirical probability
  return(empirical_prob)
}

