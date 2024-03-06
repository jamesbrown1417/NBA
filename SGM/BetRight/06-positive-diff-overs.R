#===============================================================================
# Libraries and functions
#===============================================================================

library(tidyverse)

#===============================================================================
# Read in data
#===============================================================================

# Get Data
source("BetRight/betright_sgm.R")
player_disposals_data <- read_rds("../../Data/processed_odds/all_player_disposals.rds")
player_goals_data <- read_rds("../../Data/processed_odds/all_player_goals.rds")

# Get those where the betright Odds diff for the season is greater than 0
betright_positive <-
  player_disposals_data |>
  bind_rows(player_goals_data) |>
  arrange(player_name, market_name, line, desc(over_price)) |>
  filter(agency == "BetRight") |>
  transmute(match,
            player_name,
            player_team,
            market_name,
            line,
            price = over_price,
            type = "Overs",
            diff_over_2023) |> 
  filter(diff_over_2023 >= 0.05) |> 
  select(-price)

#===============================================================================
# Get all 2 way combinations
#===============================================================================

# All bets
betright_sgm_bets <-
  betright_sgm |> 
  select(match, player_name, player_team, market_name, line, price,type, group_by_header, event_id, outcome_name, outcome_id, fixed_market_id) |> 
  left_join(betright_positive) |> 
  filter(!is.na(diff_over_2023))

# Filter to only Overs
betright_sgm_bets <-
  betright_sgm_bets |> 
  filter(type == "Overs") |> 
  distinct(match, player_name, market_name, line, .keep_all = TRUE)

# Generate all combinations of two rows
row_combinations <- combn(nrow(betright_sgm_bets), 2)

# Get list of tibbles of the two selected rows
list_of_dataframes <-
  map(
    .x = seq_len(ncol(row_combinations)),
    .f = function(i) {
      betright_sgm_bets[row_combinations[, i], ] |> 
        mutate(combination = i)
    }
  )

# Keep only those where the match is the same, player name is the same and market name is not the same
retained_combinations <-
  list_of_dataframes |> 
  # Keep only dataframes where first and second row match are equal
  keep(~.x$match[1] == .x$match[2]) |> 
  keep(~.x$player_name[1] != .x$player_name[2])

#===============================================================================
# Call function
#===============================================================================

# Custom function to apply call_sgm_betright to a tibble
apply_sgm_function <- function(tibble) {
  
  # Random Pause between 0.4 and 0.8 seconds
  Sys.sleep(runif(1, 0.4, 0.8))
  
  # Call function
  call_sgm_betright(
    data = tibble,
    player_names = tibble$player_name,
    prop_line = tibble$line,
    prop_type = tibble$market_name,
    over_under = tibble$type
  )
}

# Make safe function
apply_sgm_function_safe <- safely(apply_sgm_function, otherwise = NA)

# Applying the function to each tibble in the list 
results <- map(retained_combinations, apply_sgm_function_safe, .progress = TRUE)

# Bind all results together
results <-
  results |>
  # keep only first part of list
  map(1) |>
  keep(~is.data.frame(.x)) |>
  bind_rows() |> 
  arrange(desc(Adjustment_Factor)) |> 
  mutate(Diff = 1/Unadjusted_Price - 1/Adjusted_Price) |> 
  mutate(Diff = round(Diff, 2)) |>
  arrange(desc(Diff))

