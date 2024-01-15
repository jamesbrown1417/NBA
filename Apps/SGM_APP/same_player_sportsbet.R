# Get points rebounds and assists markets---------------------------------------
points_unders <-
sportsbet_sgm |> 
  filter(type == "Unders") |> 
  filter(market_name == c("Player Points"))

rebounds_unders <- 
sportsbet_sgm |> 
  filter(type == "Unders") |> 
  filter(market_name == c("Player Rebounds"))

assists_unders <-
sportsbet_sgm |> 
  filter(type == "Unders") |> 
  filter(market_name == c("Player Assists"))

points_unders_lines <-
points_unders |> 
  select(player_name, point_line = line)

rebounds_unders_lines <-
rebounds_unders |>
  select(player_name, rebound_line = line)

assists_unders_lines <-
assists_unders |> 
  select(player_name, assist_line = line)

# Combine all

all_lines <-
points_unders_lines |>
  left_join(rebounds_unders_lines, by = "player_name") |> 
  left_join(assists_unders_lines, by = "player_name") |>
  filter(!is.na(rebound_line) & !is.na(point_line) & !is.na(assist_line))

empty_list <- list()

# Loop through
for (i in 1:length(all_lines$player_name)) {
  result <-
  call_sgm_sportsbet(
    sportsbet_sgm,
    player_names = c(all_lines$player_name[i], all_lines$player_name[i], all_lines$player_name[i]),
    prop_line = c(all_lines$point_line[i], all_lines$rebound_line[i], all_lines$assist_line[i]),
    prop_type = c("Player Points", "Player Rebounds", "Player Assists"),
    over_under = c("Unders", "Unders", "Unders")
  )
  
  empty_list[[i]] <- result
  
  print(paste("Finished", i, "of", length(all_lines$player_name)))
  
  Sys.sleep(1)
  
}

# Bind results
all_results <-
  empty_list |>
  bind_rows() |> 
  arrange(desc(Adjustment_Factor))



