# Libraries and functions-------------------------------------------------------
library(tidyverse)
library(future)
library(furrr)

# Set up parallel processing----------------------------------------------------
plan(multisession)

# Read in position data---------------------------------------------------------
player_positions_cleaning_the_glass <-
  read_csv("Data/player_positions_cleaning_the_glass.csv") |> 
  select(Player, Team, Pos)

# Fix player names to be consistent---------------------------------------------
player_positions_cleaning_the_glass <-
  player_positions_cleaning_the_glass |> 
  mutate(Player = case_when(
    Player == "Cameron Reddish" ~ "Cam Reddish",
    Player == "Kelly Oubre" ~ "Kelly Oubre Jr.",
    Player == "PJ Tucker" ~ "P.J. Tucker",
    Player == "Nah'Shon Hyland" ~ "Bones Hyland",
    Player == "TJ McConnell" ~ "T.J. McConnell",
    Player == "PJ Washington" ~ "P.J. Washington",
    Player == "Wendell Carter" ~ "Wendell Carter Jr.",
    Player == "Jaren Jackson" ~ "Jaren Jackson Jr.",
    Player == "Michael Porter" ~ "Michael Porter Jr.",
    Player == "Nicolas Claxton" ~ "Nic Claxton",
    Player == "Cameron Thomas" ~ "Cam Thomas",
    Player == "Lonnie Walker" ~ "Lonnie Walker IV",
    .default = Player
  ))

# Read in player stats----------------------------------------------------------
all_player_stats_2023_2024 <-
  read_csv("Data/all_player_stats_2023-2024.csv") |>
  mutate(team = paste(teamCity, teamName)) |> 
  mutate(opposition_team = ifelse(team == HOME_TEAM, AWAY_TEAM, HOME_TEAM))

# Select only the columns we need-----------------------------------------------
all_player_stats_2023_2024 <-
  all_player_stats_2023_2024 |> 
  transmute(Player = paste(firstName, familyName),
            date = GAME_DATE,
            game_id = gameId,
            Team = teamTricode,
            Opponent = opposition_team,
            minutes,
            Points = points,
            Assists = assists,
            Rebounds = reboundsTotal,
            Steals = steals,
            Blocks = blocks,
            Threes = threePointersMade,
            PRAs = points + reboundsTotal + assists) |>
  filter(!is.na(minutes))

# Join with positions-----------------------------------------------------------
all_player_stats_2023_2024 <-
  all_player_stats_2023_2024 |> 
  left_join(player_positions_cleaning_the_glass) |> 
  filter(!is.na(Pos)) |> 
  arrange(desc(date), game_id, Team, Opponent, desc(Points))

#===============================================================================
# Function to get DVP for a position
#===============================================================================

get_dvp <- function(team, stat, num_games = 15) {
  
  # Match IDs to keep
  match_ids <-
  all_player_stats_2023_2024 |> 
    filter(Opponent == team) |>
    arrange(desc(date)) |> 
    distinct(game_id, date, .keep_all = TRUE) |> 
    transmute(game_id, date, match_num = row_number()) |>
    slice_head(n = num_games)
    
  # Stats
  stats_table <-
    all_player_stats_2023_2024 |> 
    mutate(minutes = hms(minutes)) |> 
    mutate(mins = hour(minutes)) |> 
    mutate(seconds = minute(minutes)) |>
    mutate(minutes = mins + (seconds / 60)) |>
    filter(minutes >= 15) |> 
    transmute(Player,
              game_id,
              Team,
              Opponent,
              date,
              minutes,
              Points,
              Assists,
              Rebounds,
              Pos) |> 
    mutate(Points = 36 * (Points / minutes),
           Assists = 36 * (Assists / minutes),
           Rebounds = 36 * (Rebounds / minutes))
  
  # Vs team
  stats_table_vs_team <-
    stats_table |> 
    filter(Opponent == team) |>
    arrange(desc(date)) |>
    filter(game_id %in% match_ids$game_id)
  
  # Vs others
  stats_table_vs_others <-
    stats_table |> 
    filter(Opponent != team) |>
    arrange(Player, desc(date)) |> 
    group_by(Player) |> 
    mutate(match_number = dense_rank(date)) |> 
    mutate(games_played = max(match_number)) |>
    filter(games_played >= num_games) |>
    slice_head(n = num_games)
  
  # Get median vs team
  med_vs_team <-
    stats_table_vs_team |> 
    group_by(Player, Pos, Team, Opponent) |> 
    summarise(med_points_vs = median(Points, na.rm = TRUE),
              med_assists_vs = median(Assists, na.rm = TRUE),
              med_rebounds_vs = median(Rebounds, na.rm = TRUE))
  
  # Get median vs all other teams in period
  med_vs_others <-
    stats_table_vs_others |> 
    group_by(Player, Pos, Team) |> 
    summarise(med_points_others = median(Points, na.rm = TRUE),
              med_assists_others = median(Assists, na.rm = TRUE),
              med_rebounds_others = median(Rebounds, na.rm = TRUE))
  
  # Join Together
  dvp_data <-
    med_vs_team |>
    left_join(med_vs_others, by = c("Player", "Pos", "Team")) |> 
    transmute(Player,
              Pos,
              Team,
              Opponent,
              point_diff = med_points_vs - med_points_others,
              assist_diff = med_assists_vs - med_assists_others,
              rebound_diff = med_rebounds_vs - med_rebounds_others)
  
  # Get for desired stat
  if (stat == "points") {
    dvp_data |> 
      group_by(Pos, Opponent) |>
      summarise(games = n(),
                med_points = median(point_diff, na.rm = TRUE)) |> 
      arrange(desc(med_points))
  } else if (stat == "rebounds") {
    dvp_data |> 
      group_by(Pos, Opponent) |>
      summarise(games = n(),
                med_rebounds = median(rebound_diff, na.rm = TRUE)) |> 
      arrange(desc(med_rebounds))
  } else {
    dvp_data |> 
      group_by(Pos, Opponent) |>
      summarise(
        games = n(),
        med_assists = median(assist_diff, na.rm = TRUE)) |> 
      arrange(desc(med_assists))
  }
}

# Get team list
team_list <-
  all_player_stats_2023_2024 |> 
  pull(Opponent) |> 
  unique()

#===============================================================================
# Get DVP for each stat
#===============================================================================

# Get points DVP
points_dvp <-
  team_list |> 
  map_df(get_dvp, stat = "points") |> 
  bind_rows() |> 
  arrange(Pos, desc(med_points))

# Get rebounds DVP
rebounds_dvp <-
  team_list |> 
  map_df(get_dvp, stat = "rebounds") |> 
  arrange(Pos, desc(med_rebounds))

# Get assists DVP
assists_dvp <-
  team_list |> 
  map_df(get_dvp, stat = "assists") |> 
  arrange(Pos, desc(med_assists))

#===============================================================================
# Save Data
#===============================================================================

# DVP Data
points_dvp_combine <-
points_dvp |> 
  rename(dvp = med_points) |> 
  mutate(market_name = "Player Points")

rebounds_dvp_combine <-
rebounds_dvp |> 
  rename(dvp = med_rebounds) |> 
  mutate(market_name = "Player Rebounds")

assists_dvp_combine <-
assists_dvp |> 
  rename(dvp = med_assists) |> 
  mutate(market_name = "Player Assists")

dvp_data <-
  bind_rows(points_dvp_combine,
            rebounds_dvp_combine,
            assists_dvp_combine) |>
  arrange(market_name, Pos, desc(dvp))

# Write out
write_csv(dvp_data, "Data/dvp_data.csv")
