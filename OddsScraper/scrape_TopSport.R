# Libraries
library(tidyverse)
library(rvest)
library(httr2)
library(jsonlite)
library(future)
library(furrr)

plan(multisession)

# Get teams table
teams <-
    read_csv("Data/all_teams.csv")

# Get player names table
player_names_all <-
    read_csv("Data/all_rosters.csv") |>
    select(player_full_name = PLAYER, TeamID) |> 
    left_join(teams[, c("id", "full_name")], by = c("TeamID" = "id")) |> 
    mutate(first_initial = str_sub(player_full_name, 1, 1)) |>
    mutate(surname = str_extract(player_full_name, "(?<=\\s).*$")) |> 
    mutate(join_name = paste(first_initial, surname, sep = " ")) |> 
    rename(team_name = full_name)

# unique join names
player_names_unique <-
    player_names_all |>
    group_by(join_name) |> 
    filter(n() == 1) |> 
    ungroup()

# Non unique names (take first two letters of first name)
player_names_non_unique <-
    player_names_all |>
    group_by(join_name) |> 
    filter(n() > 1) |> 
    mutate(first_initial = str_sub(player_full_name, 1, 2)) |>
    mutate(join_name = paste(first_initial, surname, sep = " ")) |> 
    ungroup()

player_names <-
    bind_rows(player_names_unique, player_names_non_unique) |> 
    mutate(join_name = ifelse(player_full_name == "Keyontae Johnson", "Key Johnson", join_name)) |> 
    mutate(join_name = ifelse(player_full_name == "Miles Bridges", "Mil Bridges", join_name)) |> 
    mutate(join_name = ifelse(player_full_name == "Jaylin Williams", "Jay Williams", join_name))


# URL of website
topsport_url = "https://www.topsport.com.au/Sport/Basketball/NBA_Matches/Matches"

#===============================================================================
# Use rvest to get main market information-------------------------------------#
#===============================================================================

# Get data from main market page
main_markets <-
topsport_url |> 
    read_html() |>
    html_nodes(".marketTable") |> 
    html_table()

#===============================================================================
# Use rvest to get additional market information-------------------------------#
#===============================================================================

# Get links to other markets
topsport_other_markets <-
    topsport_url |>
    read_html() |>
    html_nodes("dd") |> 
    html_attr("data-compurl")

# Remove NA
topsport_other_markets <- topsport_other_markets[!is.na(topsport_other_markets)]

# Remove ?issubcomp=true
topsport_other_markets <- str_remove(topsport_other_markets, "\\?issubcomp=true")

# Add base url
topsport_other_markets <- paste0("https://www.topsport.com.au", topsport_other_markets)

#===============================================================================
# Head to Head markets---------------------------------------------------------#
#===============================================================================

head_to_head_main <- function() {

# Function to get head to head data--------------------------------------------#
get_h2h <- function(market_table) {
    
    # Home Team Data
    home_info <- market_table[3, 1:2]
    names(home_info) <- c("home_team", "home_win")
    
    # Away Team Data
    away_info <- market_table[2, 1:2]
    names(away_info) <- c("away_team", "away_win")
    
    # Match Start Time
    match_start_time <- market_table[1, 1]
    names(match_start_time) <- "start_time"
    
    # Combine together into one table
    bind_cols(home_info, away_info, match_start_time)
    
}

# Map function to main markets list
topsport_h2h <- future_map(main_markets, get_h2h) |> bind_rows()

# Fix names
topsport_h2h <-
topsport_h2h |> 
    # mutate(home_team = fix_team_names(home_team)) |>
    # mutate(away_team = fix_team_names(away_team)) |>
    mutate(match = paste(home_team, "v", away_team)) |> 
    mutate(market_name = "Head To Head") |>
    mutate(home_win = as.numeric(home_win)) |>
    mutate(away_win = as.numeric(away_win)) |>
    select(match, start_time, market_name, home_team, home_win, away_team, away_win) |> 
    mutate(margin = round((1/home_win + 1/away_win), digits = 3)) |> 
    mutate(agency = "TopSport") |> 
    mutate(start_time = dmy_hm(start_time)) |>
    filter(!is.na(home_win))

# Write to csv
write_csv(topsport_h2h, "Data/scraped_odds/topsport_h2h.csv")
}

#===============================================================================
# Player Points
#===============================================================================

# Function to read the html of a given url
read_topsport_html <- function(url) {
  
  # Get market name from url
  market_name <- str_extract(url, "(?<=Basketball/).*")
  market_name <- str_remove(market_name, "\\/.*$")
  
  # Get line from market name
  line <- str_extract(market_name, "\\d+\\.?\\d?")
  
  # Get match name from html
  match_name_html <-    
    url |> 
    read_html() |>
    html_nodes("h1") |>
    html_text() |> 
    paste(collapse = " ")
  
  # Get match name from extracted text
  match_name_html <- strsplit(match_name_html, split = " - ")
  match_name <- match_name_html[[1]][length(match_name_html[[1]])]
  player_name <- match_name_html[[1]][length(match_name_html[[1]]) - 1]
  
  # Get data from html
  result <-    
    url |> 
    read_html() |>
    html_nodes(".marketTable") |> 
    html_table()
  
  # Get tibble
  result[[1]] |>
    mutate(line = ifelse(!is.na(line), line, str_extract(Selection, "\\d+\\.?\\d?"))) |>
    mutate(line = as.numeric(line)) |>
    mutate(match = match_name) |>
    mutate(Selection = if_else(str_detect(Selection, "(Over)|(Under)"), paste(player_name, Selection), Selection))
}

# Get data for pick your own player points--------------------------------------

# Get URLs
pick_your_own_points_markets <- 
    topsport_other_markets[str_detect(topsport_other_markets, "Player_to_Have_[0-9]{1,2}_Points")]

# Map function
if (length(pick_your_own_points_markets) > 0) {
  
player_points_alternate <-
future_map(pick_your_own_points_markets, read_topsport_html) |> 
    bind_rows() |> 
    mutate(line = line - 0.5) |>
    rename(over_price = Win) |> 
    rename(player_name = Selection) |> 
    mutate(player_name = str_remove(player_name, " \\(.*\\)$")) |>
    mutate(
        player_name =
            case_when(
                player_name == "PJ Washington" ~ "P.J. Washington",
                player_name == "Lebron James" ~ "LeBron James",
                player_name == "Michael Porter Jr" ~ "Michael Porter Jr.",
                player_name == "Dangelo Russell" ~ "D'Angelo Russell",
                player_name == "Kentavious Caldwell Pope" ~ "Kentavious Caldwell-Pope",
                player_name == "Wendell Carter Jr" ~ "Wendell Carter Jr.",
                player_name == "Cj Mccollum" ~ "CJ McCollum",
                player_name ==  "Shai Gilgeous Alexander" ~ "Shai Gilgeous-Alexander",
                player_name == "Jaren Jackson Jr" ~ "Jaren Jackson Jr.",
                player_name == "Xavier Tillman Sr" ~ "Xavier Tillman",
                player_name == "Deandre Hunter" ~ "De'Andre Hunter",
                player_name == "DeAndre Ayton" ~ "Deandre Ayton",
                player_name == "Dereck Lively" ~ "Dereck Lively II",
                player_name == "Karl Anthony Towns" ~ "Karl-Anthony Towns",
                player_name == "Jimmy Butler III" ~ "Jimmy Butler",
                player_name == "Talen Horton Tucker" ~ "Talen Horton-Tucker",
                player_name == "Anthony Davis Jr" ~ "Anthony Davis",
                player_name == "De'andre Hunter" ~ "De'Andre Hunter",
                player_name == "Lamelo Ball" ~ "LaMelo Ball",
                player_name == "Rj Barrett" ~ "RJ Barrett",
                player_name == "Bruce Brown Jr" ~ "Bruce Brown",
                player_name == "Alperen Seng?n" ~ "Alperen Sengun",
                player_name == "Franz Wagnr" ~ "Franz Wagner",
                player_name == "Fred Vanvleet" ~ "Fred VanVleet",
                .default = player_name)) |>
    left_join(player_names[, c("player_full_name", "team_name")], by = c("player_name" = "player_full_name")) |>
    relocate(match, .before = player_name) |> 
    separate(match, into = c("away_team", "home_team"), sep = " v ", remove = FALSE) |> 
    rename(player_team = team_name) |> 
    mutate(opposition_team = if_else(player_team == home_team, away_team, home_team))
}

# Get data for player points over/under-----------------------------------------

# Get URLs
player_points_markets <- 
    topsport_other_markets[str_detect(topsport_other_markets, "Player_Points_.*\\(")]

# Only proceed if markets have been picked up above
if (length(player_points_markets) > 0) {

# future_map function
player_points_lines <-
    future_map(player_points_markets, read_topsport_html) |> 
    bind_rows()

# Get Overs
player_points_lines_overs <-
  player_points_lines |> 
  select(-line) |> 
  filter(str_detect(Selection, "Over")) |>
  rename(over_price = Win) |> 
  mutate(Selection = str_remove(Selection, "\\(.*\\) ")) |>
  separate(Selection, into = c("player_name", "line"), sep = " Over ") |> 
  mutate(line = as.numeric(line)) |>
    mutate(
        player_name =
          case_when(
            player_name == "PJ Washington" ~ "P.J. Washington",
            player_name == "Lebron James" ~ "LeBron James",
            player_name == "Michael Porter Jr" ~ "Michael Porter Jr.",
            player_name == "Dangelo Russell" ~ "D'Angelo Russell",
            player_name == "Kentavious Caldwell Pope" ~ "Kentavious Caldwell-Pope",
            player_name == "Wendell Carter Jr" ~ "Wendell Carter Jr.",
            player_name == "Cj Mccollum" ~ "CJ McCollum",
            player_name ==  "Shai Gilgeous Alexander" ~ "Shai Gilgeous-Alexander",
            player_name == "Jaren Jackson Jr" ~ "Jaren Jackson Jr.",
            player_name == "Xavier Tillman Sr" ~ "Xavier Tillman",
            player_name == "Deandre Hunter" ~ "De'Andre Hunter",
            player_name == "DeAndre Ayton" ~ "Deandre Ayton",
            player_name == "Dereck Lively" ~ "Dereck Lively II",
            player_name == "Karl Anthony Towns" ~ "Karl-Anthony Towns",
            player_name == "Jimmy Butler III" ~ "Jimmy Butler",
            player_name == "Talen Horton Tucker" ~ "Talen Horton-Tucker",
            player_name == "Anthony Davis Jr" ~ "Anthony Davis",
            player_name == "De'andre Hunter" ~ "De'Andre Hunter",
            player_name == "Lamelo Ball" ~ "LaMelo Ball",
            player_name == "Rj Barrett" ~ "RJ Barrett",
            player_name == "Bruce Brown Jr" ~ "Bruce Brown",
            player_name == "Alperen Seng?n" ~ "Alperen Sengun",
            player_name == "Franz Wagnr" ~ "Franz Wagner",
            player_name == "Fred Vanvleet" ~ "Fred VanVleet",
            .default = player_name)) |>
    left_join(player_names[, c("player_full_name", "team_name")], by = c("player_name" = "player_full_name")) |>
    relocate(match, .before = player_name) |> 
    separate(match, into = c("away_team", "home_team"), sep = " v ", remove = FALSE) |> 
    rename(player_team = team_name) |> 
    mutate(opposition_team = if_else(player_team == home_team, away_team, home_team))

# Get Unders
player_points_lines_unders <-
  player_points_lines |> 
  select(-line) |> 
  filter(str_detect(Selection, "Under")) |>
  mutate(Selection = str_remove(Selection, "\\(.*\\) ")) |> 
  separate(Selection, into = c("player_name", "line"), sep = " Under ") |> 
  rename(under_price = Win) |>
  mutate(line = as.numeric(line)) |>
    mutate(
        player_name =
          case_when(
            player_name == "PJ Washington" ~ "P.J. Washington",
            player_name == "Lebron James" ~ "LeBron James",
            player_name == "Michael Porter Jr" ~ "Michael Porter Jr.",
            player_name == "Dangelo Russell" ~ "D'Angelo Russell",
            player_name == "Kentavious Caldwell Pope" ~ "Kentavious Caldwell-Pope",
            player_name == "Wendell Carter Jr" ~ "Wendell Carter Jr.",
            player_name == "Cj Mccollum" ~ "CJ McCollum",
            player_name ==  "Shai Gilgeous Alexander" ~ "Shai Gilgeous-Alexander",
            player_name == "Jaren Jackson Jr" ~ "Jaren Jackson Jr.",
            player_name == "Xavier Tillman Sr" ~ "Xavier Tillman",
            player_name == "Deandre Hunter" ~ "De'Andre Hunter",
            player_name == "DeAndre Ayton" ~ "Deandre Ayton",
            player_name == "Dereck Lively" ~ "Dereck Lively II",
            player_name == "Karl Anthony Towns" ~ "Karl-Anthony Towns",
            player_name == "Jimmy Butler III" ~ "Jimmy Butler",
            player_name == "Talen Horton Tucker" ~ "Talen Horton-Tucker",
            player_name == "Anthony Davis Jr" ~ "Anthony Davis",
            player_name == "De'andre Hunter" ~ "De'Andre Hunter",
            player_name == "Lamelo Ball" ~ "LaMelo Ball",
            player_name == "Rj Barrett" ~ "RJ Barrett",
            player_name == "Bruce Brown Jr" ~ "Bruce Brown",
            player_name == "Alperen Seng?n" ~ "Alperen Sengun",
            player_name == "Franz Wagnr" ~ "Franz Wagner",
            player_name == "Fred Vanvleet" ~ "Fred VanVleet",
            .default = player_name)) |>
    left_join(player_names[, c("player_full_name", "team_name")], by = c("player_name" = "player_full_name")) |>
    relocate(match, .before = player_name) |> 
    separate(match, into = c("away_team", "home_team"), sep = " v ", remove = FALSE) |> 
    rename(player_team = team_name) |> 
    mutate(opposition_team = if_else(player_team == home_team, away_team, home_team))

# Combine
player_points_lines <- 
    player_points_lines_overs |> 
    left_join(player_points_lines_unders) |> 
    mutate(market_name = "Player Points") |> 
    mutate(agency = "TopSport") }

#===============================================================================
# Player Assists
#===============================================================================

# Get data for pick your own player assists--------------------------------------

# Get URLs
pick_your_own_assists_markets <- 
    topsport_other_markets[str_detect(topsport_other_markets, "Player_to_Have_[0-9]{1,2}_Assists")]

# future_map function
if (length(pick_your_own_assists_markets) > 0) {
  
player_assists_alternate <-
    future_map(pick_your_own_assists_markets, read_topsport_html) |> 
    bind_rows() |> 
    mutate(line = line - 0.5) |>
    rename(over_price = Win) |> 
    rename(player_name = Selection) |>
    mutate(player_name = str_remove(player_name, " \\(.*\\)$")) |> 
    mutate(
        player_name =
          case_when(
            player_name == "PJ Washington" ~ "P.J. Washington",
            player_name == "Lebron James" ~ "LeBron James",
            player_name == "Michael Porter Jr" ~ "Michael Porter Jr.",
            player_name == "Dangelo Russell" ~ "D'Angelo Russell",
            player_name == "Kentavious Caldwell Pope" ~ "Kentavious Caldwell-Pope",
            player_name == "Wendell Carter Jr" ~ "Wendell Carter Jr.",
            player_name == "Cj Mccollum" ~ "CJ McCollum",
            player_name ==  "Shai Gilgeous Alexander" ~ "Shai Gilgeous-Alexander",
            player_name == "Jaren Jackson Jr" ~ "Jaren Jackson Jr.",
            player_name == "Xavier Tillman Sr" ~ "Xavier Tillman",
            player_name == "Deandre Hunter" ~ "De'Andre Hunter",
            player_name == "DeAndre Ayton" ~ "Deandre Ayton",
            player_name == "Dereck Lively" ~ "Dereck Lively II",
            player_name == "Karl Anthony Towns" ~ "Karl-Anthony Towns",
            player_name == "Jimmy Butler III" ~ "Jimmy Butler",
            player_name == "Talen Horton Tucker" ~ "Talen Horton-Tucker",
            player_name == "Anthony Davis Jr" ~ "Anthony Davis",
            player_name == "De'andre Hunter" ~ "De'Andre Hunter",
            player_name == "Lamelo Ball" ~ "LaMelo Ball",
            player_name == "Rj Barrett" ~ "RJ Barrett",
            player_name == "Bruce Brown Jr" ~ "Bruce Brown",
            player_name == "Alperen Seng?n" ~ "Alperen Sengun",
            player_name == "Franz Wagnr" ~ "Franz Wagner",
            player_name == "Fred Vanvleet" ~ "Fred VanVleet",
            .default = player_name)) |>
    left_join(player_names[, c("player_full_name", "team_name")], by = c("player_name" = "player_full_name")) |>
    relocate(match, .before = player_name) |> 
    separate(match, into = c("away_team", "home_team"), sep = " v ", remove = FALSE) |> 
    rename(player_team = team_name) |> 
    mutate(opposition_team = if_else(player_team == home_team, away_team, home_team))
}

# Get data for player assists over/under-----------------------------------------

# Get URLs
player_assists_markets <- 
  topsport_other_markets[str_detect(topsport_other_markets, "Total_Assists|Player_Assists")]

# Only proceed if markets have been picked up above
if (length(player_assists_markets) > 0) {
    
  # Map function
  player_assists_lines <-
    map(player_assists_markets, read_topsport_html) |> 
    bind_rows()
    
    # Get Overs
    player_assists_lines_overs <-
        player_assists_lines |> 
        select(-line) |> 
        filter(str_detect(Selection, "Over")) |>
      mutate(Selection = str_remove(Selection, "\\(.*\\) ")) |> 
      rename(over_price = Win) |> 
        separate(Selection, into = c("player_name", "line"), sep = " Over ") |> 
        mutate(line = as.numeric(line)) |>
        mutate(
            player_name =
              case_when(
                player_name == "PJ Washington" ~ "P.J. Washington",
                player_name == "Lebron James" ~ "LeBron James",
                player_name == "Michael Porter Jr" ~ "Michael Porter Jr.",
                player_name == "Dangelo Russell" ~ "D'Angelo Russell",
                player_name == "Kentavious Caldwell Pope" ~ "Kentavious Caldwell-Pope",
                player_name == "Wendell Carter Jr" ~ "Wendell Carter Jr.",
                player_name == "Cj Mccollum" ~ "CJ McCollum",
                player_name ==  "Shai Gilgeous Alexander" ~ "Shai Gilgeous-Alexander",
                player_name == "Jaren Jackson Jr" ~ "Jaren Jackson Jr.",
                player_name == "Xavier Tillman Sr" ~ "Xavier Tillman",
                player_name == "Deandre Hunter" ~ "De'Andre Hunter",
                player_name == "DeAndre Ayton" ~ "Deandre Ayton",
                player_name == "Dereck Lively" ~ "Dereck Lively II",
                player_name == "Karl Anthony Towns" ~ "Karl-Anthony Towns",
                player_name == "Jimmy Butler III" ~ "Jimmy Butler",
                player_name == "Talen Horton Tucker" ~ "Talen Horton-Tucker",
                player_name == "Anthony Davis Jr" ~ "Anthony Davis",
                player_name == "De'andre Hunter" ~ "De'Andre Hunter",
                player_name == "Lamelo Ball" ~ "LaMelo Ball",
                player_name == "Rj Barrett" ~ "RJ Barrett",
                player_name == "Bruce Brown Jr" ~ "Bruce Brown",
                player_name == "Alperen Seng?n" ~ "Alperen Sengun",
                player_name == "Franz Wagnr" ~ "Franz Wagner",
                player_name == "Fred Vanvleet" ~ "Fred VanVleet",
                .default = player_name)) |>
        left_join(player_names[, c("player_full_name", "team_name")], by = c("player_name" = "player_full_name")) |>
        relocate(match, .before = player_name) |> 
        separate(match, into = c("away_team", "home_team"), sep = " v ", remove = FALSE) |> 
        rename(player_team = team_name) |> 
        mutate(opposition_team = if_else(player_team == home_team, away_team, home_team))
    
    # Get Unders
    player_assists_lines_unders <-
        player_assists_lines |> 
        select(-line) |> 
        filter(str_detect(Selection, "Under")) |>
      mutate(Selection = str_remove(Selection, "\\(.*\\) ")) |> 
        rename(under_price = Win) |> 
        separate(Selection, into = c("player_name", "line"), sep = " Under ") |> 
        mutate(line = as.numeric(line)) |>
        mutate(
            player_name =
              case_when(
                player_name == "PJ Washington" ~ "P.J. Washington",
                player_name == "Lebron James" ~ "LeBron James",
                player_name == "Michael Porter Jr" ~ "Michael Porter Jr.",
                player_name == "Dangelo Russell" ~ "D'Angelo Russell",
                player_name == "Kentavious Caldwell Pope" ~ "Kentavious Caldwell-Pope",
                player_name == "Wendell Carter Jr" ~ "Wendell Carter Jr.",
                player_name == "Cj Mccollum" ~ "CJ McCollum",
                player_name ==  "Shai Gilgeous Alexander" ~ "Shai Gilgeous-Alexander",
                player_name == "Jaren Jackson Jr" ~ "Jaren Jackson Jr.",
                player_name == "Xavier Tillman Sr" ~ "Xavier Tillman",
                player_name == "Deandre Hunter" ~ "De'Andre Hunter",
                player_name == "DeAndre Ayton" ~ "Deandre Ayton",
                player_name == "Dereck Lively" ~ "Dereck Lively II",
                player_name == "Karl Anthony Towns" ~ "Karl-Anthony Towns",
                player_name == "Jimmy Butler III" ~ "Jimmy Butler",
                player_name == "Talen Horton Tucker" ~ "Talen Horton-Tucker",
                player_name == "Anthony Davis Jr" ~ "Anthony Davis",
                player_name == "De'andre Hunter" ~ "De'Andre Hunter",
                player_name == "Lamelo Ball" ~ "LaMelo Ball",
                player_name == "Rj Barrett" ~ "RJ Barrett",
                player_name == "Bruce Brown Jr" ~ "Bruce Brown",
                player_name == "Alperen Seng?n" ~ "Alperen Sengun",
                player_name == "Franz Wagnr" ~ "Franz Wagner",
                player_name == "Fred Vanvleet" ~ "Fred VanVleet",
                .default = player_name)) |>
        left_join(player_names[, c("player_full_name", "team_name")], by = c("player_name" = "player_full_name")) |>
        relocate(match, .before = player_name) |> 
        separate(match, into = c("away_team", "home_team"), sep = " v ", remove = FALSE) |> 
        rename(player_team = team_name) |> 
        mutate(opposition_team = if_else(player_team == home_team, away_team, home_team))
    
    # Combine
    player_assists_lines <- 
        player_assists_lines_overs |> 
        left_join(player_assists_lines_unders) |> 
        mutate(market_name = "Player Assists") |> 
        mutate(agency = "TopSport") }

#===============================================================================
# Player Rebounds
#===============================================================================

# Get data for pick your own player rebounds------------------------------------

# Get URLs
pick_your_own_rebounds_markets <- 
    topsport_other_markets[str_detect(topsport_other_markets, "Player_to_Have_[0-9]{1,2}_Rebounds")]

# future_map function
if (length(pick_your_own_rebounds_markets) > 0) {

player_rebounds_alternate <-
    future_map(pick_your_own_rebounds_markets, read_topsport_html) |> 
    bind_rows() |> 
    mutate(line = line - 0.5) |>
    rename(over_price = Win) |> 
    rename(player_name = Selection) |>
    mutate(player_name = str_remove(player_name, " \\(.*\\)$")) |> 
    mutate(
        player_name =
          case_when(
            player_name == "PJ Washington" ~ "P.J. Washington",
            player_name == "Lebron James" ~ "LeBron James",
            player_name == "Michael Porter Jr" ~ "Michael Porter Jr.",
            player_name == "Dangelo Russell" ~ "D'Angelo Russell",
            player_name == "Kentavious Caldwell Pope" ~ "Kentavious Caldwell-Pope",
            player_name == "Wendell Carter Jr" ~ "Wendell Carter Jr.",
            player_name == "Cj Mccollum" ~ "CJ McCollum",
            player_name ==  "Shai Gilgeous Alexander" ~ "Shai Gilgeous-Alexander",
            player_name == "Jaren Jackson Jr" ~ "Jaren Jackson Jr.",
            player_name == "Xavier Tillman Sr" ~ "Xavier Tillman",
            player_name == "Deandre Hunter" ~ "De'Andre Hunter",
            player_name == "DeAndre Ayton" ~ "Deandre Ayton",
            player_name == "Dereck Lively" ~ "Dereck Lively II",
            player_name == "Karl Anthony Towns" ~ "Karl-Anthony Towns",
            player_name == "Jimmy Butler III" ~ "Jimmy Butler",
            player_name == "Talen Horton Tucker" ~ "Talen Horton-Tucker",
            player_name == "Anthony Davis Jr" ~ "Anthony Davis",
            player_name == "De'andre Hunter" ~ "De'Andre Hunter",
            player_name == "Lamelo Ball" ~ "LaMelo Ball",
            player_name == "Rj Barrett" ~ "RJ Barrett",
            player_name == "Bruce Brown Jr" ~ "Bruce Brown",
            player_name == "Alperen Seng?n" ~ "Alperen Sengun",
            player_name == "Franz Wagnr" ~ "Franz Wagner",
            player_name == "Fred Vanvleet" ~ "Fred VanVleet",
            .default = player_name)) |>
    left_join(player_names[, c("player_full_name", "team_name")], by = c("player_name" = "player_full_name")) |>
    relocate(match, .before = player_name) |> 
    separate(match, into = c("away_team", "home_team"), sep = " v ", remove = FALSE) |> 
    rename(player_team = team_name) |> 
    mutate(opposition_team = if_else(player_team == home_team, away_team, home_team))
}

# Get data for player rebounds over/under---------------------------------------

# Get URLs
player_rebounds_markets <- 
  topsport_other_markets[str_detect(topsport_other_markets, "Total_Rebounds|Player_Rebounds")]

# Only proceed if markets have been picked up above
if (length(player_rebounds_markets) > 0) {
    
    # future_map function
  player_rebounds_lines <-
    map(player_rebounds_markets, read_topsport_html) |> 
    bind_rows()
    
    # Get Overs
    player_rebounds_lines_overs <-
        player_rebounds_lines |> 
        select(-line) |> 
        filter(str_detect(Selection, "Over")) |>
      mutate(Selection = str_remove(Selection, "\\(.*\\) ")) |> 
      rename(over_price = Win) |>
        separate(Selection, into = c("player_name", "line"), sep = " Over ") |> 
        mutate(line = as.numeric(line)) |>
        mutate(
            player_name =
              case_when(
                player_name == "PJ Washington" ~ "P.J. Washington",
                player_name == "Lebron James" ~ "LeBron James",
                player_name == "Michael Porter Jr" ~ "Michael Porter Jr.",
                player_name == "Dangelo Russell" ~ "D'Angelo Russell",
                player_name == "Kentavious Caldwell Pope" ~ "Kentavious Caldwell-Pope",
                player_name == "Wendell Carter Jr" ~ "Wendell Carter Jr.",
                player_name == "Cj Mccollum" ~ "CJ McCollum",
                player_name ==  "Shai Gilgeous Alexander" ~ "Shai Gilgeous-Alexander",
                player_name == "Jaren Jackson Jr" ~ "Jaren Jackson Jr.",
                player_name == "Xavier Tillman Sr" ~ "Xavier Tillman",
                player_name == "Deandre Hunter" ~ "De'Andre Hunter",
                player_name == "DeAndre Ayton" ~ "Deandre Ayton",
                player_name == "Dereck Lively" ~ "Dereck Lively II",
                player_name == "Karl Anthony Towns" ~ "Karl-Anthony Towns",
                player_name == "Jimmy Butler III" ~ "Jimmy Butler",
                player_name == "Talen Horton Tucker" ~ "Talen Horton-Tucker",
                player_name == "Anthony Davis Jr" ~ "Anthony Davis",
                player_name == "De'andre Hunter" ~ "De'Andre Hunter",
                player_name == "Lamelo Ball" ~ "LaMelo Ball",
                player_name == "Rj Barrett" ~ "RJ Barrett",
                player_name == "Bruce Brown Jr" ~ "Bruce Brown",
                player_name == "Alperen Seng?n" ~ "Alperen Sengun",
                player_name == "Franz Wagnr" ~ "Franz Wagner",
                player_name == "Fred Vanvleet" ~ "Fred VanVleet",
                .default = player_name)) |>
        left_join(player_names[, c("player_full_name", "team_name")], by = c("player_name" = "player_full_name")) |>
        relocate(match, .before = player_name) |> 
        separate(match, into = c("away_team", "home_team"), sep = " v ", remove = FALSE) |> 
        rename(player_team = team_name) |> 
        mutate(opposition_team = if_else(player_team == home_team, away_team, home_team))
    
    # Get Unders
    player_rebounds_lines_unders <-
        player_rebounds_lines |> 
        select(-line) |> 
        filter(str_detect(Selection, "Under")) |>
        mutate(Selection = str_remove(Selection, "\\(.*\\) ")) |>
      rename(under_price = Win) |>
        rename(under_price = over_price) |> 
        separate(Selection, into = c("player_name", "line"), sep = " Under ") |> 
        mutate(line = as.numeric(line)) |>
        mutate(
            player_name =
              case_when(
                player_name == "PJ Washington" ~ "P.J. Washington",
                player_name == "Lebron James" ~ "LeBron James",
                player_name == "Michael Porter Jr" ~ "Michael Porter Jr.",
                player_name == "Dangelo Russell" ~ "D'Angelo Russell",
                player_name == "Kentavious Caldwell Pope" ~ "Kentavious Caldwell-Pope",
                player_name == "Wendell Carter Jr" ~ "Wendell Carter Jr.",
                player_name == "Cj Mccollum" ~ "CJ McCollum",
                player_name ==  "Shai Gilgeous Alexander" ~ "Shai Gilgeous-Alexander",
                player_name == "Jaren Jackson Jr" ~ "Jaren Jackson Jr.",
                player_name == "Xavier Tillman Sr" ~ "Xavier Tillman",
                player_name == "Deandre Hunter" ~ "De'Andre Hunter",
                player_name == "DeAndre Ayton" ~ "Deandre Ayton",
                player_name == "Dereck Lively" ~ "Dereck Lively II",
                player_name == "Karl Anthony Towns" ~ "Karl-Anthony Towns",
                player_name == "Jimmy Butler III" ~ "Jimmy Butler",
                player_name == "Talen Horton Tucker" ~ "Talen Horton-Tucker",
                player_name == "Anthony Davis Jr" ~ "Anthony Davis",
                player_name == "De'andre Hunter" ~ "De'Andre Hunter",
                player_name == "Lamelo Ball" ~ "LaMelo Ball",
                player_name == "Rj Barrett" ~ "RJ Barrett",
                player_name == "Bruce Brown Jr" ~ "Bruce Brown",
                player_name == "Alperen Seng?n" ~ "Alperen Sengun",
                player_name == "Franz Wagnr" ~ "Franz Wagner",
                player_name == "Fred Vanvleet" ~ "Fred VanVleet",
                .default = player_name)) |>
        left_join(player_names[, c("player_full_name", "team_name")], by = c("player_name" = "player_full_name")) |>
        relocate(match, .before = player_name) |> 
        separate(match, into = c("away_team", "home_team"), sep = " v ", remove = FALSE) |> 
        rename(player_team = team_name) |> 
        mutate(opposition_team = if_else(player_team == home_team, away_team, home_team))
    
    # Combine
    player_rebounds_lines <- 
        player_rebounds_lines_overs |> 
        left_join(player_rebounds_lines_unders) |> 
        mutate(market_name = "Player Rebounds") |> 
        mutate(agency = "TopSport") }

#===============================================================================
# Write to CSV
#===============================================================================

if (!exists("player_points_lines")) {
    player_points_lines <- tibble() } 

if (!exists("player_points_alternate")) {
  player_points_alternate <- tibble() } 

if (!exists("player_rebounds_lines")) {
    player_rebounds_lines <- tibble() }

if (!exists("player_rebounds_alternate")) {
  player_rebounds_alternate <- tibble() }

if (!exists("player_assists_lines")) {
    player_assists_lines <- tibble() }

if (!exists("player_assists_alternate")) {
  player_assists_alternate <- tibble() }

# Points
player_points_alternate |>
    bind_rows(player_points_lines) |>
    mutate(match = paste(home_team, away_team, sep = " v ")) |>
    select(any_of(
        c(
        "match",
        "home_team",
        "away_team",
        "market_name",
        "player_name",
        "player_team",
        "line",
        "over_price",
        "under_price",
        "agency",
        "opposition_team"))) |>
    mutate(market_name = "Player Points") |>
    mutate(agency = "TopSport") |> 
    write_csv("Data/scraped_odds/topsport_player_points.csv")

# Rebounds
player_rebounds_alternate |>
    bind_rows(player_rebounds_lines) |>
    mutate(match = paste(home_team, away_team, sep = " v ")) |>
    select(any_of(
        c(
            "match",
            "home_team",
            "away_team",
            "market_name",
            "player_name",
            "player_team",
            "line",
            "over_price",
            "under_price",
            "agency",
            "opposition_team"))) |>
    mutate(market_name = "Player Rebounds") |>
    mutate(agency = "TopSport") |> 
    write_csv("Data/scraped_odds/topsport_player_rebounds.csv")

# Assists
player_assists_alternate |>
    bind_rows(player_assists_lines) |>
    mutate(match = paste(home_team, away_team, sep = " v ")) |>
    select(any_of(
        c(
            "match",
            "home_team",
            "away_team",
            "market_name",
            "player_name",
            "player_team",
            "line",
            "over_price",
            "under_price",
            "agency",
            "opposition_team"))) |>
    mutate(market_name = "Player Assists") |>
    mutate(agency = "TopSport") |> 
    write_csv("Data/scraped_odds/topsport_player_assists.csv")

##%######################################################%##
#                                                          #
####                   Run functions                    ####
#                                                          #
##%######################################################%##

h2h_safe_topsport <- safely(head_to_head_main)

# Run functions
h2h_safe_topsport()
