#===============================================================================
# Libraries and functions
#===============================================================================

library(tidyverse)
`%notin%` <- Negate(`%in%`)

#===============================================================================
# Read in past season stats
#===============================================================================

combined_stats_2022_2023 <- read_csv("Data/all_player_stats_2022-2023.csv")

#===============================================================================
# Create a function that takes a player name + line and returns their hit rate
#===============================================================================

get_empirical_prob <- function(player_name, line, stat) {
    
    # Get player stats
    player_stats <- combined_stats_2022_2023 |> filter(PLAYER_NAME == player_name)
    
    # branch based on whether stat is PTS, REB or AST
    
    if (stat == "PTS") {
      # Get empirical probability
      empirical_prob <-
        player_stats |>
        summarise(games_played = n(),
                  empirical_prob = mean(PTS >= line)) |>
        mutate(line = line, player_name = player_name)
    } else if (stat == "REB") {
      # Get empirical probability
      empirical_prob <-
        player_stats |>
        summarise(games_played = n(),
                  empirical_prob = mean(REB >= line)) |>
        mutate(line = line, player_name = player_name)
    } else if (stat == "AST") {
      # Get empirical probability
      empirical_prob <-
        player_stats |>
        summarise(games_played = n(),
                  empirical_prob = mean(AST >= line)) |>
        mutate(line = line, player_name = player_name)
    } else {
      stop("stat must be one of PTS, REB or AST")
    }
    
    # Return empirical probability
    return(empirical_prob)
    
}
