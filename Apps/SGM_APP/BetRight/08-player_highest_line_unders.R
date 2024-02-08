#===============================================================================
# Libraries and functions
#===============================================================================

library(tidyverse)

#===============================================================================
# Read in data
#===============================================================================

# Get Data
source("BetRight/betright_sgm.R")
source("get_last_15_same_player_prob.R")

player_positions <-
  read_csv("../../Data/player_positions_cleaning_the_glass.csv") |> 
  select(Player, Team, Pos)

player_dvp <- read_csv("../../Data/dvp_data.csv")

#===============================================================================
# Get all 2 way combinations
#===============================================================================

# All bets
betright_sgm_bets <-
  betright_sgm |> 
  select(match, player_name, player_team, market_name, line, price,type, group_by_header, event_id, outcome_name, outcome_id, fixed_market_id, type)

# Distinct lines
betright_sgm_bets <-
  betright_sgm_bets |> 
  filter(type == "Unders") |>
  distinct(match, player_name, market_name, line, type, .keep_all = TRUE) |> 
  mutate(outcome_key = paste(player_name, market_name, line)) |> 
  group_by(match, player_name, market_name) |> 
  filter(line == max(line))

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
  keep(~.x$player_name[1] == .x$player_name[2]) |>
  keep(~.x$market_name[1] != .x$market_name[2]) |>
  keep(~prod(.x$price) >= 1.6 & prod(.x$price) <= 3)

#===============================================================================
# Get player positions and DVP dataset
#===============================================================================

dvp_to_join <-
betright_sgm |> 
  select(player_name, opposition_team) |>
  mutate(opposition_team = ifelse(opposition_team == "Los Angeles Clippers", "LA Clippers", opposition_team)) |>
  left_join(player_positions, by = c("player_name" = "Player")) |>
  distinct(player_name, opposition_team, Team, Pos) |>
  left_join(player_dvp, by = c("opposition_team" = "Opponent", "Pos"), relationship = "many-to-many")

points_dvp <-
  dvp_to_join |> 
  filter(market_name == "Player Points") |> 
  transmute(player_name, points_DVP = round(dvp, 2))

rebounds_dvp <-
  dvp_to_join |> 
  filter(market_name == "Player Rebounds") |> 
  transmute(player_name, rebounds_DVP = round(dvp, 2))

assists_dvp <-
  dvp_to_join |> 
  filter(market_name == "Player Assists") |> 
  transmute(player_name, assists_DVP = round(dvp, 2))

#===============================================================================
# Call function
#===============================================================================

# Custom function to apply call_sgm_betright to a tibble
apply_sgm_function <- function(tibble) {
  
  # Random Pause between 0.5 and 0.7 seconds
  Sys.sleep(runif(1, 0.5, 0.8))
  
  
  # Call function
  call_sgm_betright(
    data = tibble,
    player_names = tibble$player_name,
    prop_line = tibble$line,
    prop_type = tibble$market_name,
    over_under = tibble$type
  )
}

# Create safe version of the function
apply_sgm_safe <- safely(apply_sgm_function)

# Applying the function to each tibble in the list
results <- map(retained_combinations, apply_sgm_safe, .progress = TRUE)

# Bind all results together
results <-
  results |>
  # Keep the result from safe function
  map(~.x$result) |>
  keep(~is.data.frame(.x)) |>
  bind_rows() |>
  arrange(desc(Adjustment_Factor)) |> 
  mutate(Diff = 1/Unadjusted_Price - 1/Adjusted_Price) |> 
  mutate(Diff = round(Diff, 2)) |>
  arrange(desc(Diff))

# Add empirical probabilities
emp_probs_last_15 <-
  pmap(results[1:2], get_sgm_emp_prob) |> 
  bind_rows()

final_data <-
  results |> 
  bind_cols(emp_probs_last_15) |> 
  mutate(edge = round(Emp_Prob_Both - 1/Adjusted_Price, 2)) |>
  mutate(player_name = str_extract(Selections, "^.*?(?=:)")) |>
  left_join(points_dvp, by = "player_name") |>
  left_join(rebounds_dvp, by = "player_name") |>
  left_join(assists_dvp, by = "player_name") |> 
  relocate(player_name, .before = Selections)


