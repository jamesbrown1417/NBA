#===============================================================================
# Libraries and functions
#===============================================================================

library(tidyverse)

#===============================================================================
# Read in data
#===============================================================================

# Get Data
source("TAB/tab_sgm.R")
player_points_data <- read_rds("../../Data/processed_odds/all_player_points.rds")
player_assists_data <- read_rds("../../Data/processed_odds/all_player_assists.rds")
player_rebounds_data <- read_rds("../../Data/processed_odds/all_player_rebounds.rds")
player_pras_data <- read_rds("../../Data/processed_odds/all_player_pras.rds")
player_steals_data <- read_rds("../../Data/processed_odds/all_player_steals.rds")
player_threes_data <- read_rds("../../Data/processed_odds/all_player_threes.rds")
player_blocks_data <- read_rds("../../Data/processed_odds/all_player_blocks.rds")

# Get those markets where tab has the best odds in the market
tab_best <-
  player_points_data |>
  bind_rows(player_assists_data) |>
  bind_rows(player_rebounds_data) |>
  bind_rows(player_pras_data) |>
  bind_rows(player_steals_data) |>
  bind_rows(player_threes_data) |>
  bind_rows(player_blocks_data) |>
  arrange(player_name, market_name, line, desc(over_price)) |>
  group_by(player_name, market_name, line) |>
  slice_head(n = 1) |>
  ungroup() |>
  filter(agency == "TAB") |>
  transmute(match,
            player_name,
            player_team,
            market_name,
            line,
            price = over_price,
            type = "Overs",
            diff_over_2023_24,
            diff_over_last_10)

# Get those where the tab Odds diff for the season is greater than 0
tab_positive <-
  player_points_data |>
  bind_rows(player_assists_data) |>
  bind_rows(player_rebounds_data) |>
  bind_rows(player_pras_data) |>
  bind_rows(player_steals_data) |>
  bind_rows(player_threes_data) |>
  bind_rows(player_blocks_data) |>
  arrange(player_name, market_name, line, desc(over_price)) |>
  filter(agency == "TAB") |>
  transmute(match,
            player_name,
            player_team,
            market_name,
            line,
            price = over_price,
            type = "Overs",
            diff_over_2023_24,
            diff_over_last_10) |> 
  filter(diff_over_2023_24 > 0.05)

#===============================================================================
# Get all 2 way combinations
#===============================================================================

# All bets
tab_sgm_bets <-
  tab_sgm |> 
  select(match, player_name, player_team, market_name, line, price,type, contains("id"))

# Filter to only Unders
tab_sgm_bets <-
  tab_sgm_bets |> 
  filter(type == "Unders") |> 
  distinct(match, player_name, market_name, line, .keep_all = TRUE) |> 
  filter(!is.na(player_name))
           
# Generate all combinations of two rows
row_combinations <- combn(nrow(tab_sgm_bets), 2)

# Get list of tibbles of the two selected rows
list_of_dataframes <-
  map(
    .x = seq_len(ncol(row_combinations)),
    .f = function(i) {
      tab_sgm_bets[row_combinations[, i], ] |> 
        mutate(combination = i)
    }
  )

# Keep only those where the match is the same, player name is the same and market name is not the same
retained_combinations <-
  list_of_dataframes |> 
  # Keep only dataframes where first and second row match are equal
  keep(~.x$match[1] == .x$match[2]) |> 
  keep(~.x$player_name[1] == .x$player_name[2]) |>
  keep(~.x$market_name[1] != .x$market_name[2])

#===============================================================================
# Call function
#===============================================================================

# Custom function to apply call_sgm_tab to a tibble
apply_sgm_function <- function(tibble) {
  
  # Random Pause between 0.5 and 0.7 seconds
  Sys.sleep(runif(1, 0.5, 0.7))
  
  
  # Call function
  call_sgm_tab(
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
  filter(!is.na(Selections)) |> 
  arrange(desc(Adjustment_Factor)) |>
  mutate(Diff = 1/Unadjusted_Price - 1/Adjusted_Price) |> 
  mutate(Diff = round(Diff, 2)) |>
  arrange(desc(Diff))

