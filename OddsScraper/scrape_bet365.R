# Libraries
library(tidyverse)
library(rvest)
library(httr2)
library(jsonlite)
library(glue)

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

# Fix team names function
source("Scripts/fix_team_names.R")

#===============================================================================
# Use rvest to get main market information-------------------------------------#
#===============================================================================

get_head_to_head <- function() {

# Read scraped HTML from the BET365_HTML Folder
scraped_file <- list.files("Data/BET365_HTML", full.names = TRUE, pattern = "h2h")[[1]]

# Get Teams
bet365_teams <-
    read_html(scraped_file) |> 
    html_nodes(".scb-ParticipantFixtureDetailsHigherBasketball_TeamWrapper") |> 
    html_text()

# Get H2H Odds
bet365_h2h_odds <-
    read_html(scraped_file) |> 
    html_nodes(".sac-ParticipantOddsOnly50OTB") |> 
    html_text()

# Get Start Time
bet365_start_time <-
    read_html(scraped_file) |> 
    html_nodes(".sgl-MarketFixtureDetailsLabel") |>
    html_nodes(".rcl-MarketHeaderLabel-isdate, .scb-ParticipantFixtureDetailsHigherBasketball_TeamWrapper") |> 
    html_text()

# Get indices that contain a number surrounded by spaces
start_time_indices <- which(str_detect(bet365_start_time, " \\d+ "))

# Empty list
result_list <- list()

# Split into chunks from each index to the next and from the last to the end of the vector
for (i in 1:length(start_time_indices)) {
    if (i != length(start_time_indices)) {
        result = (bet365_start_time[start_time_indices[i]:start_time_indices[i + 1]])
        result = result[-length(result)]
    } else {
        result = (bet365_start_time[start_time_indices[i]:length(bet365_start_time)])
    }
    result_list[[i]] <- result
}

# Turn each vector into a tibble
start_dates <- map(result_list, ~ expand_grid(.x[1], .x[2:length(.x)])) |> bind_rows()
names(start_dates) <- c("start_date", "team")

start_dates <- 
    start_dates |> 
    distinct(team, .keep_all = TRUE)

#===============================================================================
# Create head to head table----------------------------------------------------#
#===============================================================================

# Get Home teams - Odd elements
home_teams <- bet365_teams[seq(2, length(bet365_teams), 2)]
home_odds <- bet365_h2h_odds[seq(2, length(bet365_h2h_odds), 2)]

# Get only positions of elements of home odds that are numeric
home_odds_to_keep <- which(str_detect(home_odds, "^\\d+\\.\\d+$"))

home_teams <- home_teams[home_odds_to_keep]
home_odds <- home_odds[home_odds_to_keep]


home_h2h <- tibble(home_teams, home_odds) |>
    left_join(start_dates, by = c("home_teams" = "team"))

# Get Away teams - Even elements
away_teams <- bet365_teams[seq(1, length(bet365_teams), 2)]
away_odds <- bet365_h2h_odds[seq(1, length(bet365_h2h_odds), 2)]

# Get only positions of elements of away odds that are numeric
away_odds_to_keep <- which(str_detect(away_odds, "^\\d+\\.\\d+$"))

away_teams <- away_teams[away_odds_to_keep]
away_odds <- away_odds[away_odds_to_keep]

away_h2h <- tibble(away_teams, away_odds) |>
    left_join(start_dates, by = c("away_teams" = "team"))

# Combine together into one table
bet365_h2h <-
    bind_cols(home_h2h, away_h2h) |>
    mutate(home_teams = fix_team_names(home_teams),
           away_teams = fix_team_names(away_teams)) |>
    transmute(match = paste(home_teams, away_teams, sep = " v "),
              start_time = `start_date...3`,
              market_name = "Head To Head",
              home_team = home_teams,
              home_win = as.numeric(home_odds),
              away_team = away_teams,
              away_win = as.numeric(away_odds)) |>
    mutate(start_time = dmy(paste(start_time, "2023"))) |> 
    mutate(start_time = if_else(month(start_time) < 9, start_time + years(1), start_time)) |> 
    mutate(margin = round((1/home_win + 1/away_win), digits = 3)) |> 
    mutate(agency = "Bet365")

# Write to csv
bet365_h2h
}

# Run function
bet365_h2h <- get_head_to_head()

# Write to csv
write_csv(bet365_h2h, "Data/scraped_odds/bet365_h2h.csv")

##%######################################################%##
#                                                          #
####                    Player Props                    ####
#                                                          #
##%######################################################%##

# Read scraped HTML from the BET365_HTML Folder
scraped_files_player <- list.files("Data/BET365_HTML", full.names = TRUE, pattern = "player")

get_player_props <- function() {

# Main Function
main <- function(scraped_file) {

# Get Markets
bet365_player_markets <-
    read_html(scraped_file) |> 
    html_nodes(".gl-MarketGroup ")

# Functions to get the information from each section----------------------------

#===============================================================================
# Player Points----------------------------------------------------------------#
#===============================================================================

# Function to get player points
get_player_points <- function(html_market_node) {
    # Market Name
    market_name <-
        html_market_node |> 
        html_elements(".gl-MarketGroupButton_Text ") |> 
        html_text()
    
    # Player Names
    player_names <-
        html_market_node |> 
        html_elements(".srb-ParticipantLabelWithTeam_Name ") |> 
        html_text()
    
    # Team Names
    team_names <-
        html_market_node |> 
        html_elements(".srb-ParticipantLabelWithTeam_Team ") |> 
        html_text()
    
    # Get column list
    column_list <-
        html_market_node |> 
        html_elements(".gl-Market_General-columnheader")
    
    # Function to get odds and handicap from each column header-----------------
    get_handicaps_and_odds <- function(column) {
        # Name
        col_name <-
        column |>
            html_elements(".gl-MarketColumnHeader ") |>
            html_text()
        
        # Handicaps
        col_handicaps <-
        column |>
            html_elements(".gl-ParticipantCenteredStacked_Handicap") |>
            html_text()
        
        # Odds
        col_odds <-
        column |>
            html_elements(".gl-ParticipantCenteredStacked_Odds") |>
            html_text()
        
        # Combine into tibble
        output <- tibble(col_handicaps, col_odds)
        
        # Append column name to tibble columns
        names(output) <- paste(col_name, names(output), sep = "_")
        
        # Return tibble
        return(output)
    }
    
    # Get odds and handicaps for each column
    handicaps_and_odds <-
        map(column_list, get_handicaps_and_odds) |>
        map(~ if (nrow(.) == 0) NULL else .) |>
        bind_cols()
    
    # Combine into one tibble
        tibble(market_name, team_names, player_names) |>
        bind_cols(handicaps_and_odds)
}

# Get player points node
all_nodes_names <-  
    bet365_player_markets |> 
    html_elements(".gl-MarketGroupButton_Text ") |> 
    html_text()

player_points_index <- which(str_detect(all_nodes_names, "^Player Points$"))

# Get player points data
bet365_player_points <- get_player_points(bet365_player_markets[player_points_index])

# Tidy
bet365_player_points <-
    bet365_player_points |> 
    filter(Over_col_handicaps == Under_col_handicaps) |> 
    mutate(team_names = fix_team_names(team_names)) |>
    transmute(player_name = player_names,
              team_name = team_names,
              market_name,
              line = as.numeric(Over_col_handicaps),
              over_price = as.numeric(Over_col_odds),
              under_price = as.numeric(Under_col_odds)) |>
    mutate(margin = round((1/over_price + 1/under_price), digits = 3)) |>
    mutate(agency = "Bet365")

#===============================================================================
# Player Points Milestones-----------------------------------------------------#
#===============================================================================

# Function to get player points milestones
get_player_points_milestones <- function(html_market_node) {
    # Market Name
    market_name <-
        html_market_node |> 
        html_elements(".gl-MarketGroupButton_Text ") |> 
        html_text()
    
    # Player Names
    player_names <-
        html_market_node |> 
        html_elements(".srb-ParticipantLabelWithTeam_Name ") |> 
        html_text()
    
    # Team Names
    team_names <-
        html_market_node |> 
        html_elements(".srb-ParticipantLabelWithTeam_Team ") |> 
        html_text()
    
    # Get column list
    column_list <-
        html_market_node |> 
        html_elements(".srb-HScrollPlaceColumnMarket")
    
    # Function to get odds and handicap from each column header-----------------
    get_handicaps_and_odds <- function(column) {
        # Name
        col_name <-
            column |>
            html_elements(".srb-HScrollPlaceHeader ") |>
            html_text()
        
        # Odds
        col_odds <-
            column |>
            html_elements(".gl-ParticipantOddsOnly_Odds") |>
            html_text()
        
        # Combine into tibble
        output <- tibble(col_odds)
        
        # Append column name to tibble columns
        names(output) <- paste(col_name, names(output), sep = "_")
        
        # Return tibble
        return(output)
    }
    
    # Get odds and handicaps for each column
    handicaps_and_odds <-
        map(column_list, get_handicaps_and_odds) |>
        map(~ if (nrow(.) == 0) NULL else .) |>
        bind_cols()
    
    # Combine into one tibble
    tibble(market_name, team_names, player_names) |>
        bind_cols(handicaps_and_odds)
}

# Get player points milestones node
player_points_index <- which(str_detect(all_nodes_names, "^Player Points Milestones$"))

# Get player points data
bet365_player_points_milestones <- get_player_points_milestones(bet365_player_markets[player_points_index])

# Tidy
bet365_player_points_milestones <-
    bet365_player_points_milestones |> 
    pivot_longer(cols = -c(market_name, team_names, player_names), names_to = "line", values_to = "price") |> 
    mutate(line = str_extract(line, "\\d+")) |> 
    filter(price != "") |> 
    mutate(team_names = fix_team_names(team_names)) |>
    transmute(player_name = player_names,
              team_name = team_names,
              market_name = "Player Points",
              line = as.numeric(line),
              over_price = as.numeric(price)) |> 
    mutate(line = line - 0.5)

#===============================================================================
# Player Points Low------------------------------------------------------------#
#===============================================================================

# Get player points node
all_nodes_names <-  
    bet365_player_markets |> 
    html_elements(".gl-MarketGroupButton_Text ") |> 
    html_text()

player_points_low_index <- which(str_detect(all_nodes_names, "^Player Points Low$"))

# Get player points data
bet365_player_points_low <- get_player_points(bet365_player_markets[player_points_low_index])

# Tidy
bet365_player_points_low <-
    bet365_player_points_low |> 
    filter(Over_col_handicaps == Under_col_handicaps) |> 
    mutate(team_names = fix_team_names(team_names)) |>
    transmute(player_name = player_names,
              team_name = team_names,
              market_name,
              line = as.numeric(Over_col_handicaps),
              over_price = as.numeric(Over_col_odds),
              under_price = as.numeric(Under_col_odds)) |>
    mutate(margin = round((1/over_price + 1/under_price), digits = 3)) |>
    mutate(agency = "Bet365")

#===============================================================================
# Player Points High-----------------------------------------------------------#
#===============================================================================

# Get player points node
all_nodes_names <-  
    bet365_player_markets |> 
    html_elements(".gl-MarketGroupButton_Text ") |> 
    html_text()

player_points_high_index <- which(str_detect(all_nodes_names, "^Player Points High$"))

# Get player points data
bet365_player_points_high <- get_player_points(bet365_player_markets[player_points_high_index])

# Tidy
bet365_player_points_high <-
    bet365_player_points_high |> 
    filter(Over_col_handicaps == Under_col_handicaps) |> 
    mutate(team_names = fix_team_names(team_names)) |>
    transmute(player_name = player_names,
              team_name = team_names,
              market_name,
              line = as.numeric(Over_col_handicaps),
              over_price = as.numeric(Over_col_odds),
              under_price = as.numeric(Under_col_odds)) |>
    mutate(margin = round((1/over_price + 1/under_price), digits = 3)) |>
    mutate(agency = "Bet365")

#===============================================================================
# Player Assists---------------------------------------------------------------#
#===============================================================================

# Function to get player assists
get_player_assists <- function(html_market_node) {
    # Market Name
    market_name <-
        html_market_node |> 
        html_elements(".gl-MarketGroupButton_Text ") |> 
        html_text()
    
    # Player Names
    player_names <-
        html_market_node |> 
        html_elements(".srb-ParticipantLabelWithTeam_Name ") |> 
        html_text()
    
    # Team Names
    team_names <-
        html_market_node |> 
        html_elements(".srb-ParticipantLabelWithTeam_Team ") |> 
        html_text()
    
    # Get column list
    column_list <-
        html_market_node |> 
        html_elements(".gl-Market_General-columnheader")
    
    # Function to get odds and handicap from each column header-----------------
    get_handicaps_and_odds <- function(column) {
        # Name
        col_name <-
            column |>
            html_elements(".gl-MarketColumnHeader ") |>
            html_text()
        
        # Handicaps
        col_handicaps <-
            column |>
            html_elements(".gl-ParticipantCenteredStacked_Handicap") |>
            html_text()
        
        # Odds
        col_odds <-
            column |>
            html_elements(".gl-ParticipantCenteredStacked_Odds") |>
            html_text()
        
        # Combine into tibble
        output <- tibble(col_handicaps, col_odds)
        
        # Append column name to tibble columns
        names(output) <- paste(col_name, names(output), sep = "_")
        
        # Return tibble
        return(output)
    }
    
    # Get odds and handicaps for each column
    handicaps_and_odds <-
        map(column_list, get_handicaps_and_odds) |>
        map(~ if (nrow(.) == 0) NULL else .) |>
        bind_cols()
    
    # Combine into one tibble
    tibble(market_name, team_names, player_names) |>
        bind_cols(handicaps_and_odds)
}

# Get player assists node
all_nodes_names <-  
    bet365_player_markets |> 
    html_elements(".gl-MarketGroupButton_Text ") |> 
    html_text()

player_assists_index <- which(str_detect(all_nodes_names, "^Player Assists$"))

# Get player assists data
bet365_player_assists <- get_player_assists(bet365_player_markets[player_assists_index])

# Tidy
bet365_player_assists <-
    bet365_player_assists |> 
    filter(Over_col_handicaps == Under_col_handicaps) |> 
    mutate(team_names = fix_team_names(team_names)) |>
    transmute(player_name = player_names,
              team_name = team_names,
              market_name,
              line = as.numeric(Over_col_handicaps),
              over_price = as.numeric(Over_col_odds),
              under_price = as.numeric(Under_col_odds)) |>
    mutate(margin = round((1/over_price + 1/under_price), digits = 3)) |>
    mutate(agency = "Bet365")

#===============================================================================
# Player Assists Milestones----------------------------------------------------#
#===============================================================================

# Function to get player assists milestones
get_player_assists_milestones <- function(html_market_node) {
    # Market Name
    market_name <-
        html_market_node |> 
        html_elements(".gl-MarketGroupButton_Text ") |> 
        html_text()
    
    # Player Names
    player_names <-
        html_market_node |> 
        html_elements(".srb-ParticipantLabelWithTeam_Name ") |> 
        html_text()
    
    # Team Names
    team_names <-
        html_market_node |> 
        html_elements(".srb-ParticipantLabelWithTeam_Team ") |> 
        html_text()
    
    # Get column list
    column_list <-
        html_market_node |> 
        html_elements(".srb-HScrollPlaceColumnMarket")
    
    # Function to get odds and handicap from each column header-----------------
    get_handicaps_and_odds <- function(column) {
        # Name
        col_name <-
            column |>
            html_elements(".srb-HScrollPlaceHeader ") |>
            html_text()
        
        # Odds
        col_odds <-
            column |>
            html_elements(".gl-ParticipantOddsOnly_Odds") |>
            html_text()
        
        # Combine into tibble
        output <- tibble(col_odds)
        
        # Append column name to tibble columns
        names(output) <- paste(col_name, names(output), sep = "_")
        
        # Return tibble
        return(output)
    }
    
    # Get odds and handicaps for each column
    handicaps_and_odds <-
        map(column_list, get_handicaps_and_odds) |>
        map(~ if (nrow(.) == 0) NULL else .) |>
        bind_cols()
    
    # Combine into one tibble
    tibble(market_name, team_names, player_names) |>
        bind_cols(handicaps_and_odds)
}

# Get player assists milestones node
player_assists_index <- which(str_detect(all_nodes_names, "^Player Assists Milestones$"))

# Get player assists data
bet365_player_assists_milestones <- get_player_assists_milestones(bet365_player_markets[player_assists_index])

# Tidy
bet365_player_assists_milestones <-
    bet365_player_assists_milestones |> 
    pivot_longer(cols = -c(market_name, team_names, player_names), names_to = "line", values_to = "price") |> 
    mutate(line = str_extract(line, "\\d+")) |> 
    filter(price != "") |> 
    mutate(team_names = fix_team_names(team_names)) |>
    transmute(player_name = player_names,
              team_name = team_names,
              market_name = "Player Assists",
              line = as.numeric(line),
              over_price = as.numeric(price)) |> 
    mutate(line = line - 0.5)

#===============================================================================
# Player Rebounds--------------------------------------------------------------#
#===============================================================================

# Function to get player rebounds
get_player_rebounds <- function(html_market_node) {
    # Market Name
    market_name <-
        html_market_node |> 
        html_elements(".gl-MarketGroupButton_Text ") |> 
        html_text()
    
    # Player Names
    player_names <-
        html_market_node |> 
        html_elements(".srb-ParticipantLabelWithTeam_Name ") |> 
        html_text()
    
    # Team Names
    team_names <-
        html_market_node |> 
        html_elements(".srb-ParticipantLabelWithTeam_Team ") |> 
        html_text()
    
    # Get column list
    column_list <-
        html_market_node |> 
        html_elements(".gl-Market_General-columnheader")
    
    # Function to get odds and handicap from each column header-----------------
    get_handicaps_and_odds <- function(column) {
        # Name
        col_name <-
            column |>
            html_elements(".gl-MarketColumnHeader ") |>
            html_text()
        
        # Handicaps
        col_handicaps <-
            column |>
            html_elements(".gl-ParticipantCenteredStacked_Handicap") |>
            html_text()
        
        # Odds
        col_odds <-
            column |>
            html_elements(".gl-ParticipantCenteredStacked_Odds") |>
            html_text()
        
        # Combine into tibble
        output <- tibble(col_handicaps, col_odds)
        
        # Append column name to tibble columns
        names(output) <- paste(col_name, names(output), sep = "_")
        
        # Return tibble
        return(output)
    }
    
    # Get odds and handicaps for each column
    handicaps_and_odds <-
        map(column_list, get_handicaps_and_odds) |>
        map(~ if (nrow(.) == 0) NULL else .) |>
        bind_cols()
    
    # Combine into one tibble
    tibble(market_name, team_names, player_names) |>
        bind_cols(handicaps_and_odds)
}

# Get player rebounds node
all_nodes_names <-  
    bet365_player_markets |> 
    html_elements(".gl-MarketGroupButton_Text ") |> 
    html_text()

player_rebounds_index <- which(str_detect(all_nodes_names, "^Player Rebounds$"))

# Get player rebounds data
bet365_player_rebounds <- get_player_rebounds(bet365_player_markets[player_rebounds_index])

# Tidy
if (nrow(bet365_player_rebounds) > 0) {
bet365_player_rebounds <-
    bet365_player_rebounds |> 
    filter(Over_col_handicaps == Under_col_handicaps) |> 
    mutate(team_names = fix_team_names(team_names)) |>
    transmute(player_name = player_names,
              team_name = team_names,
              market_name,
              line = as.numeric(Over_col_handicaps),
              over_price = as.numeric(Over_col_odds),
              under_price = as.numeric(Under_col_odds)) |>
    mutate(margin = round((1/over_price + 1/under_price), digits = 3)) |>
    mutate(agency = "Bet365")}

if (nrow(bet365_player_rebounds) == 0) {
    bet365_player_rebounds <- tibble(player_name = character(),
                                     team_name = character(),
                                     market_name = character(),
                                     line = numeric(),
                                     over_price = numeric(),
                                     under_price = numeric(),
                                     margin = numeric(),
                                     agency = "Bet365")
}

#===============================================================================
# Player Rebounds Milestones---------------------------------------------------#
#===============================================================================

# Function to get player rebounds milestones
get_player_rebounds_milestones <- function(html_market_node) {
    # Market Name
    market_name <-
        html_market_node |> 
        html_elements(".gl-MarketGroupButton_Text ") |> 
        html_text()
    
    # Player Names
    player_names <-
        html_market_node |> 
        html_elements(".srb-ParticipantLabelWithTeam_Name ") |> 
        html_text()
    
    # Team Names
    team_names <-
        html_market_node |> 
        html_elements(".srb-ParticipantLabelWithTeam_Team ") |> 
        html_text()
    
    # Get column list
    column_list <-
        html_market_node |> 
        html_elements(".srb-HScrollPlaceColumnMarket")
    
    # Function to get odds and handicap from each column header-----------------
    get_handicaps_and_odds <- function(column) {
        # Name
        col_name <-
            column |>
            html_elements(".srb-HScrollPlaceHeader ") |>
            html_text()
        
        # Odds
        col_odds <-
            column |>
            html_elements(".gl-ParticipantOddsOnly_Odds") |>
            html_text()
        
        # Combine into tibble
        output <- tibble(col_odds)
        
        # Append column name to tibble columns
        names(output) <- paste(col_name, names(output), sep = "_")
        
        # Return tibble
        return(output)
    }
    
    # Get odds and handicaps for each column
    handicaps_and_odds <-
        map(column_list, get_handicaps_and_odds) |>
        map(~ if (nrow(.) == 0) NULL else .) |>
        bind_cols()
    
    # Combine into one tibble
    tibble(market_name, team_names, player_names) |>
        bind_cols(handicaps_and_odds)
}

# Get player rebounds milestones node
player_rebounds_index <- which(str_detect(all_nodes_names, "^Player Rebounds Milestones$"))

# Get player rebounds data
bet365_player_rebounds_milestones <- get_player_rebounds_milestones(bet365_player_markets[player_rebounds_index])

# Tidy
if (nrow(bet365_player_rebounds_milestones) > 0) {
bet365_player_rebounds_milestones <-
    bet365_player_rebounds_milestones |> 
    pivot_longer(cols = -c(market_name, team_names, player_names), names_to = "line", values_to = "price") |> 
    mutate(line = str_extract(line, "\\d+")) |> 
    filter(price != "") |> 
    mutate(team_names = fix_team_names(team_names)) |>
    transmute(player_name = player_names,
              team_name = team_names,
              market_name = "Player rebounds",
              line = as.numeric(line),
              over_price = as.numeric(price)) |> 
    mutate(line = line - 0.5)}

if (nrow(bet365_player_rebounds_milestones) == 0) {
    bet365_player_rebounds_milestones <- tibble(player_name = character(),
                                                team_name = character(),
                                                market_name = character(),
                                                line = numeric(),
                                                over_price = numeric())}

#===============================================================================
# Player Threes Made-----------------------------------------------------------#
#===============================================================================

# Function to get player threes
get_player_threes <- function(html_market_node) {
    # Market Name
    market_name <-
        html_market_node |> 
        html_elements(".gl-MarketGroupButton_Text ") |>
        html_text()
    
    # Player Names
    player_names <-
        html_market_node |> 
        html_elements(".srb-ParticipantLabelWithTeam_Name ") |> 
        html_text()
    
    # Team Names
    team_names <-
        html_market_node |> 
        html_elements(".srb-ParticipantLabelWithTeam_Team ") |> 
        html_text()
    
    # Get column list
    column_list <-
        html_market_node |> 
        html_elements(".gl-Market_General-columnheader")
    
    # Function to get odds and handicap from each column header-----------------
    get_handicaps_and_odds <- function(column) {
        # Name
        col_name <-
            column |>
            html_elements(".gl-MarketColumnHeader ") |>
            html_text()
        
        # Handicaps
        col_handicaps <-
            column |>
            html_elements(".gl-ParticipantCenteredStacked_Handicap") |>
            html_text()
        
        # Odds
        col_odds <-
            column |>
            html_elements(".gl-ParticipantCenteredStacked_Odds") |>
            html_text()
        
        # Combine into tibble
        output <- tibble(col_handicaps, col_odds)
        
        # Append column name to tibble columns
        names(output) <- paste(col_name, names(output), sep = "_")
        
        # Return tibble
        return(output)
    }
    
    # Get odds and handicaps for each column
    handicaps_and_odds <-
        map(column_list, get_handicaps_and_odds) |>
        map(~ if (nrow(.) == 0) NULL else .) |>
        bind_cols()
    
    # Combine into one tibble
    tibble(market_name, team_names, player_names) |>
        bind_cols(handicaps_and_odds)
}

# Get player threes node
all_nodes_names <-  
    bet365_player_markets |> 
    html_elements(".gl-MarketGroupButton_Text ") |> 
    html_text()

player_threes_index <- which(str_detect(all_nodes_names, "^Player Threes Made$"))

# Get player threes data
bet365_player_threes <- get_player_threes(bet365_player_markets[player_threes_index])

# Tidy
bet365_player_threes <-
    bet365_player_threes |> 
    filter(Over_col_handicaps == Under_col_handicaps) |> 
    mutate(team_names = fix_team_names(team_names)) |>
    transmute(player_name = player_names,
              team_name = team_names,
              market_name = "Player Threes",
              line = as.numeric(Over_col_handicaps),
              over_price = as.numeric(Over_col_odds),
              under_price = as.numeric(Under_col_odds)) |>
    mutate(margin = round((1/over_price + 1/under_price), digits = 3)) |>
    mutate(agency = "Bet365")

#===============================================================================
# Player Threes Made Milestones---------------------------------------------------#
#===============================================================================

# Function to get player threes_made milestones
get_player_threes_made_milestones <- function(html_market_node) {
    # Market Name
    market_name <-
        html_market_node |> 
        html_elements(".gl-MarketGroupButton_Text ") |> 
        html_text()
    
    # Player Names
    player_names <-
        html_market_node |> 
        html_elements(".srb-ParticipantLabelWithTeam_Name ") |> 
        html_text()
    
    # Team Names
    team_names <-
        html_market_node |> 
        html_elements(".srb-ParticipantLabelWithTeam_Team ") |> 
        html_text()
    
    # Get column list
    column_list <-
        html_market_node |> 
        html_elements(".srb-HScrollPlaceColumnMarket")
    
    # Function to get odds and handicap from each column header-----------------
    get_handicaps_and_odds <- function(column) {
        # Name
        col_name <-
            column |>
            html_elements(".srb-HScrollPlaceHeader ") |>
            html_text()
        
        # Odds
        col_odds <-
            column |>
            html_elements(".gl-ParticipantOddsOnly_Odds") |>
            html_text()
        
        # Combine into tibble
        output <- tibble(col_odds)
        
        # Append column name to tibble columns
        names(output) <- paste(col_name, names(output), sep = "_")
        
        # Return tibble
        return(output)
    }
    
    # Get odds and handicaps for each column
    handicaps_and_odds <-
        map(column_list, get_handicaps_and_odds) |>
        map(~ if (nrow(.) == 0) NULL else .) |>
        bind_cols()
    
    # Combine into one tibble
    tibble(market_name, team_names, player_names) |>
        bind_cols(handicaps_and_odds)
}

# Get player threes_made milestones node
player_threes_made_index <- which(str_detect(all_nodes_names, "^Player Threes Made Milestones$"))

# Get player threes_made data
bet365_player_threes_made_milestones <- get_player_threes_made_milestones(bet365_player_markets[player_threes_made_index])

# Tidy
bet365_player_threes_made_milestones <-
    bet365_player_threes_made_milestones |> 
    pivot_longer(cols = -c(market_name, team_names, player_names), names_to = "line", values_to = "price") |> 
    mutate(line = str_extract(line, "\\d+")) |> 
    filter(price != "") |> 
    mutate(team_names = fix_team_names(team_names)) |>
    transmute(player_name = player_names,
              team_name = team_names,
              market_name = "Player Threes",
              line = as.numeric(line),
              over_price = as.numeric(price)) |> 
    mutate(line = line - 0.5)

#===============================================================================
# Player Steals----------------------------------------------------------------#
#===============================================================================

# Function to get player steals
get_player_steals <- function(html_market_node) {
    # Market Name
    market_name <-
        html_market_node |> 
        html_elements(".gl-MarketGroupButton_Text ") |> 
        html_text()
    
    # Player Names
    player_names <-
        html_market_node |> 
        html_elements(".srb-ParticipantLabelWithTeam_Name ") |> 
        html_text()
    
    # Team Names
    team_names <-
        html_market_node |> 
        html_elements(".srb-ParticipantLabelWithTeam_Team ") |> 
        html_text()
    
    # Get column list
    column_list <-
        html_market_node |> 
        html_elements(".gl-Market_General-columnheader")
    
    # Function to get odds and handicap from each column header-----------------
    get_handicaps_and_odds <- function(column) {
        # Name
        col_name <-
            column |>
            html_elements(".gl-MarketColumnHeader ") |>
            html_text()
        
        # Handicaps
        col_handicaps <-
            column |>
            html_elements(".gl-ParticipantCenteredStacked_Handicap") |>
            html_text()
        
        # Odds
        col_odds <-
            column |>
            html_elements(".gl-ParticipantCenteredStacked_Odds") |>
            html_text()
        
        # Combine into tibble
        output <- tibble(col_handicaps, col_odds)
        
        # Append column name to tibble columns
        names(output) <- paste(col_name, names(output), sep = "_")
        
        # Return tibble
        return(output)
    }
    
    # Get odds and handicaps for each column
    handicaps_and_odds <-
        map(column_list, get_handicaps_and_odds) |>
        map(~ if (nrow(.) == 0) NULL else .) |>
        bind_cols()
    
    # Combine into one tibble
    tibble(market_name, team_names, player_names) |>
        bind_cols(handicaps_and_odds)
}

# Get player steals node
all_nodes_names <-  
    bet365_player_markets |> 
    html_elements(".gl-MarketGroupButton_Text ") |> 
    html_text()

player_steals_index <- which(str_detect(all_nodes_names, "^Player Steals$"))

# Get player steals data
bet365_player_steals <- get_player_steals(bet365_player_markets[player_steals_index])

# Tidy
bet365_player_steals <-
    bet365_player_steals |> 
    filter(Over_col_handicaps == Under_col_handicaps) |> 
    mutate(team_names = fix_team_names(team_names)) |>
    transmute(player_name = player_names,
              team_name = team_names,
              market_name = "Player Steals",
              line = as.numeric(Over_col_handicaps),
              over_price = as.numeric(Over_col_odds),
              under_price = as.numeric(Under_col_odds)) |>
    mutate(margin = round((1/over_price + 1/under_price), digits = 3)) |>
    mutate(agency = "Bet365")

#===============================================================================
# Player Blocks----------------------------------------------------------------#
#===============================================================================

# Function to get player blocks
get_player_blocks <- function(html_market_node) {
    # Market Name
    market_name <-
        html_market_node |> 
        html_elements(".gl-MarketGroupButton_Text ") |> 
        html_text()
    
    # Player Names
    player_names <-
        html_market_node |> 
        html_elements(".srb-ParticipantLabelWithTeam_Name ") |> 
        html_text()
    
    # Team Names
    team_names <-
        html_market_node |> 
        html_elements(".srb-ParticipantLabelWithTeam_Team ") |> 
        html_text()
    
    # Get column list
    column_list <-
        html_market_node |> 
        html_elements(".gl-Market_General-columnheader")
    
    # Function to get odds and handicap from each column header-----------------
    get_handicaps_and_odds <- function(column) {
        # Name
        col_name <-
            column |>
            html_elements(".gl-MarketColumnHeader ") |>
            html_text()
        
        # Handicaps
        col_handicaps <-
            column |>
            html_elements(".gl-ParticipantCenteredStacked_Handicap") |>
            html_text()
        
        # Odds
        col_odds <-
            column |>
            html_elements(".gl-ParticipantCenteredStacked_Odds") |>
            html_text()
        
        # Combine into tibble
        output <- tibble(col_handicaps, col_odds)
        
        # Append column name to tibble columns
        names(output) <- paste(col_name, names(output), sep = "_")
        
        # Return tibble
        return(output)
    }
    
    # Get odds and handicaps for each column
    handicaps_and_odds <-
        map(column_list, get_handicaps_and_odds) |>
        map(~ if (nrow(.) == 0) NULL else .) |>
        bind_cols()
    
    # Combine into one tibble
    tibble(market_name, team_names, player_names) |>
        bind_cols(handicaps_and_odds)
}

# Get player blocks node
all_nodes_names <-  
    bet365_player_markets |> 
    html_elements(".gl-MarketGroupButton_Text ") |> 
    html_text()

player_blocks_index <- which(str_detect(all_nodes_names, "^Player Blocks$"))

# Get player blocks data
bet365_player_blocks <- get_player_blocks(bet365_player_markets[player_blocks_index])

# Tidy
bet365_player_blocks <-
    bet365_player_blocks |> 
    filter(Over_col_handicaps == Under_col_handicaps) |> 
    mutate(team_names = fix_team_names(team_names)) |>
    transmute(player_name = player_names,
              team_name = team_names,
              market_name = "Player Blocks",
              line = as.numeric(Over_col_handicaps),
              over_price = as.numeric(Over_col_odds),
              under_price = as.numeric(Under_col_odds)) |>
    mutate(margin = round((1/over_price + 1/under_price), digits = 3)) |>
    mutate(agency = "Bet365")

#===============================================================================
# Player Points, Assists and Rebounds------------------------------------------#
#===============================================================================

# Function to get player points_assists_rebounds
get_player_points_assists_rebounds <- function(html_market_node) {
    # Market Name
    market_name <-
        html_market_node |> 
        html_elements(".gl-MarketGroupButton_Text ") |> 
        html_text()
    
    # Player Names
    player_names <-
        html_market_node |> 
        html_elements(".srb-ParticipantLabelWithTeam_Name ") |> 
        html_text()
    
    # Team Names
    team_names <-
        html_market_node |> 
        html_elements(".srb-ParticipantLabelWithTeam_Team ") |> 
        html_text()
    
    # Get column list
    column_list <-
        html_market_node |> 
        html_elements(".gl-Market_General-columnheader")
    
    # Function to get odds and handicap from each column header-----------------
    get_handicaps_and_odds <- function(column) {
        # Name
        col_name <-
            column |>
            html_elements(".gl-MarketColumnHeader ") |>
            html_text()
        
        # Handicaps
        col_handicaps <-
            column |>
            html_elements(".gl-ParticipantCenteredStacked_Handicap") |>
            html_text()
        
        # Odds
        col_odds <-
            column |>
            html_elements(".gl-ParticipantCenteredStacked_Odds") |>
            html_text()
        
        # Combine into tibble
        output <- tibble(col_handicaps, col_odds)
        
        # Append column name to tibble columns
        names(output) <- paste(col_name, names(output), sep = "_")
        
        # Return tibble
        return(output)
    }
    
    # Get odds and handicaps for each column
    handicaps_and_odds <-
        map(column_list, get_handicaps_and_odds) |>
        map(~ if (nrow(.) == 0) NULL else .) |>
        bind_cols()
    
    # Combine into one tibble
    tibble(market_name, team_names, player_names) |>
        bind_cols(handicaps_and_odds)
}

# Get player points_assists_rebounds node
all_nodes_names <-  
    bet365_player_markets |> 
    html_elements(".gl-MarketGroupButton_Text ") |> 
    html_text()

player_points_assists_rebounds_index <- which(str_detect(all_nodes_names, "^Player Points, Assists and Rebounds$"))

# Get player points_assists_rebounds data
bet365_player_points_assists_rebounds <- get_player_points_assists_rebounds(bet365_player_markets[player_points_assists_rebounds_index])

# Tidy
if (nrow(bet365_player_points_assists_rebounds) > 0) {
  
bet365_player_points_assists_rebounds <-
    bet365_player_points_assists_rebounds |> 
    filter(Over_col_handicaps == Under_col_handicaps) |> 
    mutate(team_names = fix_team_names(team_names)) |>
    transmute(player_name = player_names,
              team_name = team_names,
              market_name = "Player PRAs",
              line = as.numeric(Over_col_handicaps),
              over_price = as.numeric(Over_col_odds),
              under_price = as.numeric(Under_col_odds)) |>
    mutate(margin = round((1/over_price + 1/under_price), digits = 3)) |>
    mutate(agency = "Bet365")}

if (nrow(bet365_player_points_assists_rebounds) == 0) {
  bet365_player_points_assists_rebounds <- tibble(player_name = character(),
                                                  team_name = character(),
                                                  market_name = character(),
                                                  line = numeric(),
                                                  over_price = numeric(),
                                                  under_price = numeric(),
                                                  margin = numeric(),
                                                  agency = character())
}

#===============================================================================
# Get list of derived dataframes and return
#===============================================================================

list(
    "Player Points" = bet365_player_points,
    "Player Points Low" = bet365_player_points_low,
    "Player Points High" = bet365_player_points_high,
    "Player Points Milestones" = bet365_player_points_milestones,
    "Player Assists" = bet365_player_assists,
    "Player Assists Milestones" = bet365_player_assists_milestones,
    "Player Rebounds" = bet365_player_rebounds,
    "Player Rebounds Milestones" = bet365_player_rebounds_milestones,
    "Player Threes Made" = bet365_player_threes,
    "Player Threes Made Milestones" = bet365_player_threes_made_milestones,
    "Player PRAs" = bet365_player_points_assists_rebounds,
    "Player Steals" = bet365_player_steals,
    "Player Blocks" = bet365_player_blocks)
}

##%######################################################%##
#                                                          #
####            Map to files and output data            ####
#                                                          #
##%######################################################%##

# Create safe version of main function
main_safe <- safely(main)

# Get matches df to join
match_info <- bet365_h2h |> select(match, home_team, away_team)

# Get list of data
list_of_scraped_data <-
    map(scraped_files_player, main_safe) |> 
    map("result") |>
    keep(~ !is.null(.))

# Map over list and get dataframes for player points
player_points <- 
    list_of_scraped_data |>
    map(~.x[c("Player Points", "Player Points Low", "Player Points High", "Player Points Milestones")]) |>
    map_df(bind_rows) |> 
    mutate(agency = "Bet365") |> 
    left_join(match_info, by = c("team_name" = "home_team")) |>
    left_join(match_info, by = c("team_name" = "away_team")) |>
    mutate(match = coalesce(match.x, match.y)) |>
    separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |> 
    rename(player_team = team_name) |> 
    mutate(opposition_team = if_else(player_team == home_team, away_team, home_team)) |>
    select(match, home_team, away_team, market_name, player_name, player_team, opposition_team, line, over_price, under_price, agency)

# Map over list and get dataframes for player assists
player_assists <- 
    list_of_scraped_data |>
    map(~.x[c("Player Assists", "Player Assists Milestones")]) |>
    map_df(bind_rows) |> 
    mutate(agency = "Bet365") |> 
    left_join(match_info, by = c("team_name" = "home_team")) |>
    left_join(match_info, by = c("team_name" = "away_team")) |>
    mutate(match = coalesce(match.x, match.y)) |>
    separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
    rename(player_team = team_name) |>
    mutate(opposition_team = if_else(player_team == home_team, away_team, home_team)) |>
    select(match, home_team, away_team, market_name, player_name, player_team, opposition_team, line, over_price, under_price, agency)

# Map over list and get dataframes for player rebounds
player_rebounds <- 
    list_of_scraped_data |>
    map(~.x[c("Player Rebounds", "Player Rebounds Milestones")]) |>
    map_df(bind_rows) |> 
    mutate(agency = "Bet365") |> 
    left_join(match_info, by = c("team_name" = "home_team")) |>
    left_join(match_info, by = c("team_name" = "away_team")) |>
    mutate(match = coalesce(match.x, match.y)) |>
    separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
    rename(player_team = team_name) |>
    mutate(opposition_team = if_else(player_team == home_team, away_team, home_team)) |>
    select(match, home_team, away_team, market_name, player_name, player_team, opposition_team, line, over_price, under_price, agency)

# Map over list and get dataframes for player Threes
player_threes <- 
    list_of_scraped_data |>
    map(~.x[c("Player Threes Made", "Player Threes Made Milestones")]) |>
    map_df(bind_rows) |> 
    mutate(agency = "Bet365") |> 
    left_join(match_info, by = c("team_name" = "home_team")) |>
    left_join(match_info, by = c("team_name" = "away_team")) |>
    mutate(match = coalesce(match.x, match.y)) |>
    separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
    rename(player_team = team_name) |>
    mutate(opposition_team = if_else(player_team == home_team, away_team, home_team)) |>
    select(match, home_team, away_team, market_name, player_name, player_team, opposition_team, line, over_price, under_price, agency)

# Map over list and get dataframes for player PRAs
player_PRAs <- 
    list_of_scraped_data |>
    map(~.x["Player PRAs"]) |>
    map_df(bind_rows) |> 
    mutate(agency = "Bet365") |> 
    left_join(match_info, by = c("team_name" = "home_team")) |>
    left_join(match_info, by = c("team_name" = "away_team")) |>
    mutate(match = coalesce(match.x, match.y)) |>
    separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
    rename(player_team = team_name) |>
    mutate(opposition_team = if_else(player_team == home_team, away_team, home_team)) |>
    select(match, home_team, away_team, market_name, player_name, player_team, opposition_team, line, over_price, under_price, agency)

# Map over list and get dataframes for player steals
player_steals <- 
    list_of_scraped_data |>
    map(~.x["Player Steals"]) |>
    map_df(bind_rows) |> 
    mutate(agency = "Bet365") |> 
    left_join(match_info, by = c("team_name" = "home_team")) |>
    left_join(match_info, by = c("team_name" = "away_team")) |>
    mutate(match = coalesce(match.x, match.y)) |>
    separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
    rename(player_team = team_name) |>
    mutate(opposition_team = if_else(player_team == home_team, away_team, home_team)) |>
    select(match, home_team, away_team, market_name, player_name, player_team, opposition_team, line, over_price, under_price, agency)

# Map over list and get dataframes for player blocks
player_blocks <- 
    list_of_scraped_data |>
    map(~.x["Player Blocks"]) |>
    map_df(bind_rows) |> 
    mutate(agency = "Bet365") |> 
    left_join(match_info, by = c("team_name" = "home_team")) |>
    left_join(match_info, by = c("team_name" = "away_team")) |>
    mutate(match = coalesce(match.x, match.y)) |>
    separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
    rename(player_team = team_name) |>
    mutate(opposition_team = if_else(player_team == home_team, away_team, home_team)) |>
    select(match, home_team, away_team, market_name, player_name, player_team, opposition_team, line, over_price, under_price, agency)

# Write to CSV------------------------------------------------------------------

player_points |> write_csv("Data/scraped_odds/bet365_player_points.csv")
player_assists |> write_csv("Data/scraped_odds/bet365_player_assists.csv")
player_rebounds |> write_csv("Data/scraped_odds/bet365_player_rebounds.csv")
player_threes |> write_csv("Data/scraped_odds/bet365_player_threes.csv")
player_PRAs |> write_csv("Data/scraped_odds/bet365_player_pras.csv")
player_steals |> write_csv("Data/scraped_odds/bet365_player_steals.csv")
player_blocks |> write_csv("Data/scraped_odds/bet365_player_blocks.csv")
}

# Create safe version of functions-----------------------------------------------
get_player_props_safe <- safely(get_player_props, otherwise = NULL)

# Run function------------------------------------------------------------------
tryCatch(get_player_props(), error = function(e) print("Error in get_player_props()"))
