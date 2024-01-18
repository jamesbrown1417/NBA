#===============================================================================
# Libraries and functions
#===============================================================================

library(tidyverse)

#===============================================================================
# Read in data
#===============================================================================

#===============================================================================
# Get all 2 way combinations
#===============================================================================

# All bets
sportsbet_sgm_bets <-
sportsbet_sgm |> 
  select(match, player_name, player_team, market_name, line, price,type, contains("id"))

# Filter to only points and rebounds for now
sportsbet_sgm_bets <-
sportsbet_sgm_bets |> 
  filter(market_name %in% c("Player Points", "Player Assists")) |> 
  filter(type == "Overs") |> 
  filter(match == "Toronto Raptors v Chicago Bulls")

# Generate all combinations of two rows
row_combinations <- combn(nrow(sportsbet_sgm_bets), 2)

# Get list of tibbles of the two selected rows
list_of_dataframes <-
  map(
    .x = seq_len(ncol(row_combinations)),
    .f = function(i) {
      sportsbet_sgm_bets[row_combinations[, i], ] |> 
        mutate(combination = i)
    }
  )

# Keep only those where prod of price is between 1.4 and 3
retained_combinations <-
list_of_dataframes |> 
  keep(~prod(.x$price) >= 1.8 & prod(.x$price) <= 2.2)

#===============================================================================
# Call function
#===============================================================================

# Custom function to apply call_sgm_sportsbet to a tibble
apply_sgm_function <- function(tibble) {
  
  # Wait 0.5 seconds
  Sys.sleep(0.5)
  
  # Call function
  call_sgm_sportsbet(
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
  bind_rows(results) |> 
  arrange(desc(Adjustment_Factor))
