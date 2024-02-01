# Libraries and functions-------------------------------------------------------
library(tidyverse)

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
  arrange(Player, Team, desc(date))

#===============================================================================
# GET DVP
#===============================================================================





