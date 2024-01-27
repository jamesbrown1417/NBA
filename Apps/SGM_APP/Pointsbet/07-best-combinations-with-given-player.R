#===============================================================================
# Libraries and functions
#===============================================================================

library(tidyverse)

#===============================================================================
# Read in data
#===============================================================================

# Get Data
source("Pointsbet/pointsbet_sgm.R")

#===============================================================================
# Get all 2 way combinations
#===============================================================================

# All bets
pointsbet_sgm_bets <-
  pointsbet_sgm |> 
  select(match, player_name, player_team, market_name, line, price,type, contains("key"))

# Filter to only Overs
pointsbet_sgm_bets <-
  pointsbet_sgm_bets |> 
  # filter(type == "Overs") |> 
  distinct(match, player_name, market_name, line, type, .keep_all = TRUE)

# Odds below 1.2
pointsbet_sgm_bets_combine <-
  pointsbet_sgm_bets |> 
  filter(price < 1.2)

# Get Desired Player, line and Market
desired_player <- "Luke Kennard"
desired_market <- "Player Rebounds"
desired_line <- 3.5
desired_type <- "Unders"

# Get row number of desired player
desired_player_row <-
  pointsbet_sgm_bets |> 
  mutate(rn = row_number()) |> 
  filter(player_name == desired_player,
         market_name == desired_market,
         line == desired_line,
         type == desired_type)

# Add to odds below 1.2
pointsbet_sgm_bets_combine <-
  pointsbet_sgm_bets_combine |> 
  bind_rows(desired_player_row) |> 
  filter(match %in% desired_player_row$match)

# Get index of last row
last_row <- nrow(pointsbet_sgm_bets_combine)

# Generate all combinations of two rows where one is the desired player row num
row_combinations <- combn(nrow(pointsbet_sgm_bets_combine), 2)

# Get list of tibbles of the two selected rows
list_of_dataframes <-
  map(
    .x = seq_len(ncol(row_combinations)),
    .f = function(i) {
      pointsbet_sgm_bets_combine[row_combinations[, i], ] |> 
        mutate(combination = i)
    }
  )

# Keep only those where the match is the same, and desired player is one of the two rows
retained_combinations <-
  list_of_dataframes |> 
  # Keep only dataframes where first and second row match are equal
  keep(~.x$match[1] == .x$match[2]) |>
  # Keep only dataframes where the desired player is one of the two rows
  keep(~desired_player_row$OutcomeKey %in% .x$OutcomeKey) |> 
  # Keep only dataframes where the rows are not the same
  keep(~.x$OutcomeKey[1] != .x$OutcomeKey[2])

#===============================================================================
# Call function
#===============================================================================

# Custom function to apply call_sgm_pointsbet to a tibble
apply_sgm_function <- function(tibble) {
  
  # Random Pause between 0.5 and 0.7 seconds
  Sys.sleep(runif(1, 0.5, 0.7))
  
  # Call function
  call_sgm_pointsbet(
    data = tibble,
    player_names = tibble$player_name,
    prop_line = tibble$line,
    prop_type = tibble$market_name,
    over_under = tibble$type
  )
}

# Applying the function to each tibble in the list
results <- map(retained_combinations, apply_sgm_function, .progress = TRUE)

# Bind all results together
results <-
  results |>
  keep(~is.data.frame(.x)) |>
  bind_rows() |> 
  arrange(desc(Adjustment_Factor))
