
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

