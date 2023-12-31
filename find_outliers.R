
#===============================================================================
# Player Points
#===============================================================================

# Overs--------------------------------------------------------------
all_player_points_overs <-
  all_player_points |> 
  group_by(player_name, line, agency) |>
  arrange(player_name, line, desc(over_price)) |>
  slice_head(n = 1) |> 
  ungroup() |> 
  arrange(player_name, line, desc(over_price)) |> 
  group_by(player_name, line) |>
  filter(n() > 2) |>
  slice_head(n = 2) |>
  mutate(variation = max(implied_prob_over) - min(implied_prob_over)) |>
  ungroup() |>
  arrange(desc(variation), player_name, line)

player_points_overs_to_bet <-
  all_player_points_overs |> 
  group_by(player_name, line) |>
  slice_head(n = 1) |> 
  ungroup() |>
  arrange(desc(variation), player_name, line) |> 
  filter(variation >= 0.025) |> 
  filter(diff_over_last_10 >= 0 & diff_over_2023_24 >= 0) |> 
  filter(over_price >= 1.3)

# Unders-------------------------------------------------------------
all_player_points_unders <-
  all_player_points |> 
  filter(!is.na(under_price)) |> 
  group_by(player_name, line, agency) |>
  arrange(player_name, line, desc(under_price)) |>
  slice_head(n = 1) |> 
  ungroup() |> 
  arrange(player_name, line, desc(under_price)) |> 
  group_by(player_name, line) |>
  filter(n() > 2) |>
  slice_head(n = 2) |>
  mutate(variation = max(implied_prob_under) - min(implied_prob_under)) |>
  ungroup() |>
  arrange(desc(variation), player_name, line)

player_points_unders_to_bet <-
  all_player_points_unders |> 
  group_by(player_name, line) |>
  slice_head(n = 1) |> 
  ungroup() |>
  arrange(desc(variation), player_name, line) |> 
  filter(variation >= 0.025) |> 
  filter(diff_under_last_10 >= 0 & diff_under_2023_24 >= 0) |> 
  filter(under_price >= 1.3)

#===============================================================================
# Player Rebounds
#===============================================================================

# Overs--------------------------------------------------------------
all_player_rebounds_overs <-
  all_player_rebounds |> 
  group_by(player_name, line, agency) |>
  arrange(player_name, line, desc(over_price)) |>
  slice_head(n = 1) |> 
  ungroup() |> 
  arrange(player_name, line, desc(over_price)) |> 
  group_by(player_name, line) |>
  filter(n() > 2) |>
  slice_head(n = 2) |>
  mutate(variation = max(implied_prob_over) - min(implied_prob_over)) |>
  ungroup() |>
  arrange(desc(variation), player_name, line)

player_rebounds_overs_to_bet <-
  all_player_rebounds_overs |> 
  group_by(player_name, line) |>
  slice_head(n = 1) |> 
  ungroup() |>
  arrange(desc(variation), player_name, line) |> 
  filter(variation >= 0.025) |> 
  filter(diff_over_last_10 >= 0 & diff_over_2023_24 >= 0) |> 
  filter(over_price >= 1.3)

# Unders-------------------------------------------------------------
all_player_rebounds_unders <-
  all_player_rebounds |> 
  filter(!is.na(under_price)) |> 
  group_by(player_name, line, agency) |>
  arrange(player_name, line, desc(under_price)) |>
  slice_head(n = 1) |> 
  ungroup() |> 
  arrange(player_name, line, desc(under_price)) |> 
  group_by(player_name, line) |>
  filter(n() > 2) |>
  slice_head(n = 2) |>
  mutate(variation = max(implied_prob_under) - min(implied_prob_under)) |>
  ungroup() |>
  arrange(desc(variation), player_name, line)

player_rebounds_unders_to_bet <-
  all_player_rebounds_unders |> 
  group_by(player_name, line) |>
  slice_head(n = 1) |> 
  ungroup() |>
  arrange(desc(variation), player_name, line) |> 
  filter(variation >= 0.025) |> 
  filter(diff_under_last_10 >= 0 & diff_under_2023_24 >= 0) |> 
  filter(under_price >= 1.3)

#===============================================================================
# Player Assists
#===============================================================================

# Overs--------------------------------------------------------------
all_player_assists_overs <-
  all_player_assists |> 
  group_by(player_name, line, agency) |>
  arrange(player_name, line, desc(over_price)) |>
  slice_head(n = 1) |> 
  ungroup() |> 
  arrange(player_name, line, desc(over_price)) |> 
  group_by(player_name, line) |>
  filter(n() > 2) |>
  slice_head(n = 2) |>
  mutate(variation = max(implied_prob_over) - min(implied_prob_over)) |>
  ungroup() |>
  arrange(desc(variation), player_name, line)

player_assists_overs_to_bet <-
  all_player_assists_overs |> 
  group_by(player_name, line) |>
  slice_head(n = 1) |> 
  ungroup() |>
  arrange(desc(variation), player_name, line) |> 
  filter(variation >= 0.025) |> 
  filter(diff_over_last_10 >= 0 & diff_over_2023_24 >= 0) |> 
  filter(over_price >= 1.3)

# Unders-------------------------------------------------------------
all_player_assists_unders <-
  all_player_assists |> 
  filter(!is.na(under_price)) |> 
  group_by(player_name, line, agency) |>
  arrange(player_name, line, desc(under_price)) |>
  slice_head(n = 1) |> 
  ungroup() |> 
  arrange(player_name, line, desc(under_price)) |> 
  group_by(player_name, line) |>
  filter(n() > 2) |>
  slice_head(n = 2) |>
  mutate(variation = max(implied_prob_under) - min(implied_prob_under)) |>
  ungroup() |>
  arrange(desc(variation), player_name, line)

player_assists_unders_to_bet <-
  all_player_assists_unders |> 
  group_by(player_name, line) |>
  slice_head(n = 1) |> 
  ungroup() |>
  arrange(desc(variation), player_name, line) |> 
  filter(variation >= 0.025) |> 
  filter(diff_under_last_10 >= 0 & diff_under_2023_24 >= 0) |> 
  filter(under_price >= 1.3)

#===============================================================================
# Player PRAs
#===============================================================================

# Overs--------------------------------------------------------------
all_player_pras_overs <-
  all_player_pras |> 
  group_by(player_name, line, agency) |>
  arrange(player_name, line, desc(over_price)) |>
  slice_head(n = 1) |> 
  ungroup() |> 
  arrange(player_name, line, desc(over_price)) |> 
  group_by(player_name, line) |>
  filter(n() > 2) |>
  slice_head(n = 2) |>
  mutate(variation = max(implied_prob_over) - min(implied_prob_over)) |>
  ungroup() |>
  arrange(desc(variation), player_name, line)

player_pras_overs_to_bet <-
  all_player_pras_overs |> 
  group_by(player_name, line) |>
  slice_head(n = 1) |> 
  ungroup() |>
  arrange(desc(variation), player_name, line) |> 
  filter(variation >= 0.025) |> 
  filter(diff_over_last_10 >= 0 & diff_over_2023_24 >= 0) |> 
  filter(over_price >= 1.3)

# Unders-------------------------------------------------------------
all_player_pras_unders <-
  all_player_pras |> 
  filter(!is.na(under_price)) |> 
  group_by(player_name, line, agency) |>
  arrange(player_name, line, desc(under_price)) |>
  slice_head(n = 1) |> 
  ungroup() |> 
  arrange(player_name, line, desc(under_price)) |> 
  group_by(player_name, line) |>
  filter(n() > 2) |>
  slice_head(n = 2) |>
  mutate(variation = max(implied_prob_under) - min(implied_prob_under)) |>
  ungroup() |>
  arrange(desc(variation), player_name, line)

player_pras_unders_to_bet <-
  all_player_pras_unders |> 
  group_by(player_name, line) |>
  slice_head(n = 1) |> 
  ungroup() |>
  arrange(desc(variation), player_name, line) |> 
  filter(variation >= 0.025) |> 
  filter(diff_under_last_10 >= 0 & diff_under_2023_24 >= 0) |> 
  filter(under_price >= 1.3)

#===============================================================================
# Player Steals
#===============================================================================

# Overs--------------------------------------------------------------
all_player_steals_overs <-
  all_player_steals |> 
  group_by(player_name, line, agency) |>
  arrange(player_name, line, desc(over_price)) |>
  slice_head(n = 1) |> 
  ungroup() |> 
  arrange(player_name, line, desc(over_price)) |> 
  group_by(player_name, line) |>
  filter(n() > 1) |>
  slice_head(n = 2) |>
  mutate(variation = max(implied_prob_over) - min(implied_prob_over)) |>
  ungroup() |>
  arrange(desc(variation), player_name, line)

player_steals_overs_to_bet <-
  all_player_steals_overs |> 
  group_by(player_name, line) |>
  slice_head(n = 1) |> 
  ungroup() |>
  arrange(desc(variation), player_name, line) |> 
  filter(variation >= 0.025) |> 
  filter(diff_over_last_10 >= 0 & diff_over_2023_24 >= 0) |> 
  filter(over_price >= 1.3)

# Unders-------------------------------------------------------------
all_player_steals_unders <-
  all_player_steals |> 
  filter(!is.na(under_price)) |> 
  group_by(player_name, line, agency) |>
  arrange(player_name, line, desc(under_price)) |>
  slice_head(n = 1) |> 
  ungroup() |> 
  arrange(player_name, line, desc(under_price)) |> 
  group_by(player_name, line) |>
  filter(n() > 1) |>
  slice_head(n = 2) |>
  mutate(variation = max(implied_prob_under) - min(implied_prob_under)) |>
  ungroup() |>
  arrange(desc(variation), player_name, line)

player_steals_unders_to_bet <-
  all_player_steals_unders |> 
  group_by(player_name, line) |>
  slice_head(n = 1) |> 
  ungroup() |>
  arrange(desc(variation), player_name, line) |> 
  filter(variation >= 0.025) |> 
  filter(diff_under_last_10 >= 0 & diff_under_2023_24 >= 0) |> 
  filter(under_price >= 1.3)
