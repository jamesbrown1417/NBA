# Libraries
library(tidyverse)
library(rvest)
library(httr2)
library(jsonlite)

# Load user functions
source("Scripts/04-helper-functions.R")

# Get player name and team data
player_names_teams <-
    read_csv("Data/supercoach-data.csv") |> 
    mutate(first_initial = str_sub(player_first_name, 1, 1)) |>
    select(player_first_name, first_initial, player_last_name, player_team) |> 
    mutate(player_name_initials = paste(first_initial, player_last_name, sep = " ")) |> 
    mutate(player_full_name = paste(player_first_name, player_last_name, sep = " "))

# Get schedule
nbl_schedule <- read_rds("Data/season_schedule_2023_2024.rds")

# Add current datetime
nbl_schedule <- nbl_schedule |> 
    mutate(current_timestamp = Sys.time()) |> 
    mutate(home_team = fix_team_names(home_team), away_team = fix_team_names(away_team)) |> 
    mutate(match = paste(home_team, "v", away_team, sep = " "))

# Get home teams and away teams next match
home_teams <-
    nbl_schedule |> 
    rename(team = home_team, opposition_team = away_team) 

away_teams <-
    nbl_schedule |> 
    rename(team = away_team, opposition_team = home_team)

# Get next match
next_match <-
    bind_rows(home_teams, away_teams) |>
    arrange(team, start_time) |> 
    filter(current_timestamp <= start_time) |> 
    group_by(team) |>
    slice_head(n = 1) |>
    mutate(team = fix_team_names(team), opposition_team = fix_team_names(opposition_team))

# URL of website
topsport_url = "https://www.topsport.com.au/Sport/Basketball/NBL_Matches/Matches"

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
    home_info <- market_table[2, 1:2]
    names(home_info) <- c("home_team", "home_win")
    
    # Away Team Data
    away_info <- market_table[3, 1:2]
    names(away_info) <- c("away_team", "away_win")
    
    # Match Start Time
    match_start_time <- market_table[1, 1]
    names(match_start_time) <- "start_time"
    
    # Combine together into one table
    bind_cols(home_info, away_info, match_start_time)
    
}

# Map function to main markets list
topsport_h2h <- map(main_markets, get_h2h) |> bind_rows()

# Fix names
topsport_h2h <-
topsport_h2h |> 
    mutate(home_team = fix_team_names(home_team)) |>
    mutate(away_team = fix_team_names(away_team)) |>
    mutate(match = paste(home_team, "v", away_team)) |> 
    mutate(market_name = "Head To Head") |>
    mutate(home_win = as.numeric(home_win)) |>
    mutate(away_win = as.numeric(away_win)) |>
    select(match, start_time, market_name, home_team, home_win, away_team, away_win) |> 
    mutate(margin = round((1/home_win + 1/away_win), digits = 3)) |> 
    mutate(agency = "TopSport") |> 
    mutate(start_time = dmy_hm(start_time))

# Write to csv
write_csv(topsport_h2h, "Data/scraped_odds/topsport_h2h.csv")
}

player_props_main <- function() {

#===============================================================================
# Player Points
#===============================================================================

# Function to read the html of a given url
read_topsport_html <- function(url) {
    
    # Get market name from url
    market_name <- str_extract(url, "(?<=Basketball/).*")
    market_name <- str_remove(market_name, "\\/.*$")
    
    # Get line from market name
    line <- str_extract(market_name, "\\d+")
    
    # Get data from html
    result <-    
    url |> 
        read_html() |>
        html_nodes(".marketTable") |> 
        html_table()
    
    # Get tibble
    result[[1]] |> mutate(line = as.numeric(line))
}

# Get data for pick your own player points--------------------------------------

# Get URLs
pick_your_own_points_markets <- 
    topsport_other_markets[str_detect(topsport_other_markets, "Player_to_Score")]

# Map function
player_points_alternate <-
map(pick_your_own_points_markets, read_topsport_html) |> 
    bind_rows() |> 
    mutate(Selection = str_replace_all(Selection, "Mcveigh", "McVeigh")) |>
    mutate(Selection = str_replace_all(Selection, "Jordon", "Jordan")) |>
    mutate(Selection = str_replace_all(Selection, "D.J.", "DJ")) |>
    mutate(line = line - 0.5) |>
    rename(over_price = Win) |> 
    left_join(player_names_teams[c("player_name_initials", "player_last_name", "player_first_name", "player_team")], by = c("Selection" = "player_name_initials")) |> 
    rename(player_name = Selection) |> 
    left_join(next_match[, c("team", "opposition_team", "match")], by = c("player_team" = "team")) |> 
    relocate(match, .before = player_name) |> 
    separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |> 
    mutate(player_name = paste(player_first_name, player_last_name)) |> 
    select(-player_first_name, -player_last_name)

# Get data for player points over/under-----------------------------------------

# Get URLs
player_points_markets <- 
    topsport_other_markets[str_detect(topsport_other_markets, "Total_Points_.*\\(")]

# Map function
player_points_lines <-
    map(player_points_markets, read_topsport_html) |> 
    bind_rows() |> 
    mutate(Selection = str_replace_all(Selection, "Mcveigh", "McVeigh")) |>
    mutate(Selection = str_replace_all(Selection, "Jordon", "Jordan")) |>
    mutate(Selection = str_replace_all(Selection, "D.J.", "DJ")) |>
    mutate(line = line - 0.5) |>
    rename(over_price = Win)

# Get Overs
player_points_lines_overs <-
    player_points_lines |> 
    select(-line) |> 
    filter(str_detect(Selection, "Over")) |>
    separate(Selection, into = c("player_name", "line"), sep = " Over ") |> 
    mutate(line = as.numeric(line)) |>
    mutate(player_name = str_replace(player_name, "Matt", "Matthew")) |> 
    left_join(player_names_teams[,c("player_full_name", "player_team")], by = c("player_name" = "player_full_name")) |> 
    left_join(next_match[, c("team", "opposition_team", "match")], by = c("player_team" = "team")) |> 
    relocate(match, .before = player_name) |> 
    separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE)

# Get Unders
player_points_lines_unders <-
    player_points_lines |> 
    select(-line) |> 
    filter(str_detect(Selection, "Under")) |>
    rename(under_price = over_price) |> 
    separate(Selection, into = c("player_name", "line"), sep = " Under ") |> 
    mutate(line = as.numeric(line)) |>
    mutate(player_name = str_replace(player_name, "Matt", "Matthew")) |> 
    left_join(player_names_teams[,c("player_full_name", "player_team")], by = c("player_name" = "player_full_name")) |> 
    left_join(next_match[, c("team", "opposition_team", "match")], by = c("player_team" = "team")) |> 
    relocate(match, .before = player_name) |> 
    separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE)

# Combine
player_points_lines <- 
    player_points_lines_overs |> 
    left_join(player_points_lines_unders) |> 
    mutate(market_name = "Player Points") |> 
    mutate(agency = "TopSport")

#===============================================================================
# Player Assists
#===============================================================================

# Get data for player assists over/under-----------------------------------------

# Get URLs
player_assists_markets <- 
    topsport_other_markets[str_detect(topsport_other_markets, "Assists")]

# Map function
player_assists_lines <-
    map(player_assists_markets, read_topsport_html) |>
    bind_rows() |>
    mutate(Selection = str_replace_all(Selection, "Mcveigh", "McVeigh")) |>
    mutate(Selection = str_replace_all(Selection, "Jordon", "Jordan")) |>
    mutate(Selection = str_replace_all(Selection, "D.J.", "DJ")) |>
    mutate(line = line - 0.5) |>
    rename(over_price = Win)

# Get Overs
player_assists_lines_overs <-
    player_assists_lines |> 
    select(-line) |> 
    filter(str_detect(Selection, "Over")) |>
    separate(Selection, into = c("player_name", "line"), sep = " Over ") |> 
    mutate(line = as.numeric(line)) |>
    mutate(player_name = str_replace(player_name, "Matt", "Matthew")) |> 
    left_join(player_names_teams[,c("player_full_name", "player_team")], by = c("player_name" = "player_full_name")) |> 
    left_join(next_match[, c("team", "opposition_team", "match")], by = c("player_team" = "team")) |> 
    relocate(match, .before = player_name) |> 
    separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE)
    
# Get Unders
player_assists_lines_unders <-
    player_assists_lines |> 
    select(-line) |> 
    filter(str_detect(Selection, "Under")) |>
    rename(under_price = over_price) |> 
    separate(Selection, into = c("player_name", "line"), sep = " Under ") |> 
    mutate(line = as.numeric(line)) |>
    mutate(player_name = str_replace(player_name, "Matt", "Matthew")) |> 
    left_join(player_names_teams[,c("player_full_name", "player_team")], by = c("player_name" = "player_full_name")) |> 
    left_join(next_match[, c("team", "opposition_team", "match")], by = c("player_team" = "team")) |> 
    relocate(match, .before = player_name) |> 
    separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE)

# Combine
player_assists_lines <- 
    player_assists_lines_overs |> 
    left_join(player_assists_lines_unders) |> 
    mutate(market_name = "Player Points") |> 
    mutate(agency = "TopSport")

#===============================================================================
# Player Rebounds
#===============================================================================

# Get data for player rebounds over/under-----------------------------------------

# Get URLs
player_rebounds_markets <- 
    topsport_other_markets[str_detect(topsport_other_markets, "Rebounds")]

# Map function
player_rebounds_lines <-
    map(player_rebounds_markets, read_topsport_html) |> 
    bind_rows() |> 
    mutate(Selection = str_replace_all(Selection, "Mcveigh", "McVeigh")) |>
    mutate(Selection = str_replace_all(Selection, "Jordon", "Jordan")) |>
    mutate(Selection = str_replace_all(Selection, "D.J.", "DJ")) |>
    mutate(line = line - 0.5) |>
    rename(over_price = Win)

# Get Overs
player_rebounds_lines_overs <-
    player_rebounds_lines |> 
    select(-line) |> 
    filter(str_detect(Selection, "Over")) |>
    separate(Selection, into = c("player_name", "line"), sep = " Over ") |> 
    mutate(line = as.numeric(line)) |>
    mutate(player_name = str_replace(player_name, "Matt", "Matthew")) |> 
    left_join(player_names_teams[,c("player_full_name", "player_team")], by = c("player_name" = "player_full_name")) |> 
    left_join(next_match[, c("team", "opposition_team", "match")], by = c("player_team" = "team")) |> 
    relocate(match, .before = player_name) |> 
    separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE)

# Get Unders
player_rebounds_lines_unders <-
    player_rebounds_lines |> 
    select(-line) |> 
    filter(str_detect(Selection, "Under")) |>
    rename(under_price = over_price) |> 
    separate(Selection, into = c("player_name", "line"), sep = " Under ") |> 
    mutate(line = as.numeric(line)) |>
    mutate(player_name = str_replace(player_name, "Matt", "Matthew")) |> 
    left_join(player_names_teams[,c("player_full_name", "player_team")], by = c("player_name" = "player_full_name")) |> 
    left_join(next_match[, c("team", "opposition_team", "match")], by = c("player_team" = "team")) |> 
    relocate(match, .before = player_name) |> 
    separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE)

# Combine
player_rebounds_lines <- 
    player_rebounds_lines_overs |> 
    left_join(player_rebounds_lines_unders) |> 
    mutate(market_name = "Player Points") |> 
    mutate(agency = "TopSport")

#===============================================================================
# Write to CSV
#===============================================================================

# Points
player_points_lines |>
    bind_rows(player_points_alternate) |>
    select(
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
        "opposition_team"
    ) |>
    mutate(market_name = "Player Points") |>
    mutate(agency = "TopSport") |> 
    write_csv("Data/scraped_odds/topsport_player_points.csv")

# Rebounds
player_rebounds_lines |>
    select(
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
        "opposition_team"
    ) |>
    write_csv("Data/scraped_odds/topsport_player_rebounds.csv")

# Assists
player_assists_lines |>
    select(
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
        "opposition_team"
    ) |>
    write_csv("Data/scraped_odds/topsport_player_assists.csv") }

##%######################################################%##
#                                                          #
####                   Run functions                    ####
#                                                          #
##%######################################################%##

h2h_safe_topsport <- safely(head_to_head_main)
player_props_safe_topsport <- safely(player_props_main)

# Run functions
h2h_safe_topsport()
player_props_safe_topsport()
