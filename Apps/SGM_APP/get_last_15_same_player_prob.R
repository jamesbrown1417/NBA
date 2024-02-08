#===============================================================================
# Libraries and functions
#===============================================================================

library(tidyverse)
`%notin%` <- Negate(`%in%`)

#===============================================================================
# Read in season data
#===============================================================================

# Read in player stats----------------------------------------------------------
all_player_stats_2023_2024 <-
  read_csv("../../Data/all_player_stats_2023-2024.csv") |>
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

# Get last 15 games for each player
last_15_stats <-
  all_player_stats_2023_2024 |> 
  arrange(Player, desc(date)) |>
  filter(!is.na(minutes))

#===============================================================================
# Function to take selections string and get combined probability over last 15
#===============================================================================

get_sgm_emp_prob <- function(Selections, Markets, as_at_date = lubridate::today()) {
  # Get last 15 games dataset as of date
  last_15_stats <-
    last_15_stats |>
    group_by(Player) |> 
    filter(date < as_at_date) |> 
    slice_head(n = 15) |> 
    ungroup()
  
  # Parse selection string
  selection_1 <- str_split(Selections, ", ")[[1]][1]
  selection_2 <- str_split(Selections, ", ")[[1]][2]
  market_1 <- str_split(Markets, ", ")[[1]][1]
  market_2 <- str_split(Markets, ", ")[[1]][2]
  player_name <- str_extract_all(Selections, "^.*?(?=:)")
  line_1 <- str_extract(selection_1, "\\d+\\.\\d+") |> as.numeric()
  line_2 <- str_extract(selection_2, "\\d+\\.\\d+") |> as.numeric()
  
  # Get empirical probability of going under line for each selection individually
  emp_prob_1 <-
    last_15_stats |>
    filter(Player == player_name) |>
    summarise(emp_prob = sum(get(str_remove(market_1, "Player ")) < line_1) / n())
  
  emp_prob_2 <-
    last_15_stats |>
    filter(Player == player_name) |>
    summarise(emp_prob = sum(get(str_remove(market_2, "Player ")) < line_2) / n())
  
  # Get empirical probability of going under line for both selections combined
  emp_prob_both <-
    last_15_stats |>
    filter(Player == player_name) |>
    summarise(emp_prob = sum(get(str_remove(market_1, "Player ")) < line_1 & get(str_remove(market_2, "Player ")) < line_2) / n())
  
  # Return in a tibble
  tibble(
    Emp_Prob_1 = emp_prob_1[[1]],
    Emp_Prob_2 = emp_prob_2[[1]],
    Emp_Prob_Both = emp_prob_both[[1]]
  )
}
  