# Libraries
library(tidyverse)
library(rvest)
library(httr2)
library(jsonlite)
library(glue)

# Fix team names function
source("Scripts/fix_team_names.R")

# Read scraped HTML from the BET365_HTML Folder
scraped_files_player <- list.files("Data/BET365_HTML/", full.names = TRUE, pattern = "player")

# Main Function
get_player_props <- function(scraped_file) {
  # Get Markets
  bet365_player_markets <-
    read_html(scraped_file) |>
    html_nodes(".gl-MarketGroupPod")
  
  # Market Names
  market_names <-
    bet365_player_markets |>
    html_elements(".cm-MarketGroupWithIconsButton_Text, .sc-MarketGroupButtonWithStats_Text") |>
    html_text()
  
  #=============================================================================
  # Player Points Over / Under
  #=============================================================================
  
  # Get index for node with text "Player Points Over/Under"
  points_over_under_index <- which(market_names == "Points O/U")
  
  # Get Player Names from node
  points_players <-
    bet365_player_markets[[points_over_under_index]] |>
    html_elements(".srb-ParticipantLabelWithTeam_Name") |>
    html_text()
  
  # # Get Player Teams from node
  # points_teams <-
  #     bet365_player_markets[[points_over_under_index]] |>
  #     html_elements(".srb-ParticipantLabelWithTeam_Team") |>
  #     html_text()
  
  # Get Over Node Index
  points_cols <-
    bet365_player_markets[[points_over_under_index]] |>
    html_elements(".gl-Market_General")
    
  points_over_index <- which(str_detect(points_cols |> html_text(), "Over"))
  
  # Get Over Lines
  points_over_lines <-
    points_cols[[points_over_index]] |>
    html_elements(".gl-ParticipantCenteredStacked_Handicap") |>
    html_text()
  
  # Get Over Odds
  points_over_odds <-
    points_cols[[points_over_index]] |>
    html_elements(".gl-ParticipantCenteredStacked_Odds") |>
    html_text()
  
  # Get Under Node Index
  points_under_index <- which(str_detect(points_cols |> html_text(), "Under"))
  
  # Get Under Odds
  points_under_odds <-
    points_cols[[points_under_index]] |>
    html_elements(".gl-ParticipantCenteredStacked_Odds") |>
    html_text()
  
  # Create Player Points Table
  player_points <-
    tibble(player = points_players,
           # team = points_teams,
           line = as.numeric(points_over_lines),
           over_price = as.numeric(points_over_odds),
           under_price = as.numeric(points_under_odds)) |>
    mutate(market_name = "Player Points Over/Under") |>
    mutate(agency = "Bet365")
  
  #=============================================================================
  # Alternate Player Points
  #=============================================================================
  
  # Get index for node with text "Player Points Milestones"
  alternate_points_index <- which(market_names == "Points")
  
  # Get Player Names from node
  alternate_points_players <-
    bet365_player_markets[[alternate_points_index]] |>
    html_elements(".srb-ParticipantLabelWithTeam_Name") |>
    html_text()
  
  # # Get Player Teams from node
  # alternate_points_teams <-
  #     bet365_player_markets[[alternate_points_index]] |>
  #     html_elements(".srb-ParticipantLabelWithTeam_Team") |>
  #     html_text()
  
  # Get Points Node Indexes for 5 to 40 points
  alternate_points_cols <-
    bet365_player_markets[[alternate_points_index]] |>
    html_elements(".gl-Market_General")
  
  alternate_points_5_index <- which(str_detect(alternate_points_cols |> html_node(".srb-HScrollPlaceHeader ") |> html_text(), "^5$"))
  alternate_points_10_index <- which(str_detect(alternate_points_cols |> html_node(".srb-HScrollPlaceHeader ") |> html_text(), "10"))
  alternate_points_15_index <- which(str_detect(alternate_points_cols |> html_node(".srb-HScrollPlaceHeader ") |> html_text(), "15"))
  alternate_points_20_index <- which(str_detect(alternate_points_cols |> html_node(".srb-HScrollPlaceHeader ") |> html_text(), "20"))
  alternate_points_25_index <- which(str_detect(alternate_points_cols |> html_node(".srb-HScrollPlaceHeader ") |> html_text(), "25"))
  alternate_points_30_index <- which(str_detect(alternate_points_cols |> html_node(".srb-HScrollPlaceHeader ") |> html_text(), "30"))
  alternate_points_35_index <- which(str_detect(alternate_points_cols |> html_node(".srb-HScrollPlaceHeader ") |> html_text(), "35"))
  alternate_points_40_index <- which(str_detect(alternate_points_cols |> html_node(".srb-HScrollPlaceHeader ") |> html_text(), "40"))
  
  # Get Odds for each points range
  alternate_points_5_odds <-
    alternate_points_cols[[alternate_points_5_index]] |>
    html_elements(".gl-ParticipantOddsOnly_Odds") |>
    html_text()
  
  alternate_points_10_odds <-
    alternate_points_cols[[alternate_points_10_index]] |>
    html_elements(".gl-ParticipantOddsOnly_Odds") |>
    html_text()
  
  alternate_points_15_odds <-
    alternate_points_cols[[alternate_points_15_index]] |>
    html_elements(".gl-ParticipantOddsOnly_Odds") |>
    html_text()
  
  alternate_points_20_odds <-
    alternate_points_cols[[alternate_points_20_index]] |>
    html_elements(".gl-ParticipantOddsOnly_Odds") |>
    html_text()
  
  alternate_points_25_odds <-
    alternate_points_cols[[alternate_points_25_index]] |>
    html_elements(".gl-ParticipantOddsOnly_Odds") |>
    html_text()
  
  alternate_points_30_odds <-
    alternate_points_cols[[alternate_points_30_index]] |>
    html_elements(".gl-ParticipantOddsOnly_Odds") |>
    html_text()
  
  alternate_points_35_odds <-
    alternate_points_cols[[alternate_points_35_index]] |>
    html_elements(".gl-ParticipantOddsOnly_Odds") |>
    html_text()
  
  alternate_points_40_odds <-
    alternate_points_cols[[alternate_points_40_index]] |>
    html_elements(".gl-ParticipantOddsOnly_Odds") |>
    html_text()
  
  # Create Alternate Player Points Tables
  alternate_points_5 <-
    tibble(player = alternate_points_players,
           # team = alternate_points_teams,
           line = 5,
           over_price = as.numeric(alternate_points_5_odds)) |>
    mutate(market_name = "Alternate Player Points") |>
    mutate(agency = "Bet365")
  
  alternate_points_10 <-
    tibble(player = alternate_points_players,
           # team = alternate_points_teams,
           line = 10,
           over_price = as.numeric(alternate_points_10_odds)) |>
    mutate(market_name = "Alternate Player Points") |>
    mutate(agency = "Bet365")
  
  alternate_points_15 <-
    tibble(player = alternate_points_players,
           # team = alternate_points_teams,
           line = 15,
           over_price = as.numeric(alternate_points_15_odds)) |>
    mutate(market_name = "Alternate Player Points") |>
    mutate(agency = "Bet365")
  
  alternate_points_20 <-
    tibble(player = alternate_points_players,
           # team = alternate_points_teams,
           line = 20,
           over_price = as.numeric(alternate_points_20_odds)) |>
    mutate(market_name = "Alternate Player Points") |>
    mutate(agency = "Bet365")
  
  alternate_points_25 <-
    tibble(player = alternate_points_players,
           # team = alternate_points_teams,
           line = 25,
           over_price = as.numeric(alternate_points_25_odds)) |>
    mutate(market_name = "Alternate Player Points") |>
    mutate(agency = "Bet365")
  
  alternate_points_30 <-
    tibble(player = alternate_points_players,
           # team = alternate_points_teams,
           line = 30,
           over_price = as.numeric(alternate_points_30_odds)) |>
    mutate(market_name = "Alternate Player Points") |>
    mutate(agency = "Bet365")
  
  alternate_points_35 <-
    tibble(player = alternate_points_players,
           # team = alternate_points_teams,
           line = 35,
           over_price = as.numeric(alternate_points_35_odds)) |>
    mutate(market_name = "Alternate Player Points") |>
    mutate(agency = "Bet365")
  
  alternate_points_40 <-
    tibble(player = alternate_points_players,
           # team = alternate_points_teams,
           line = 40,
           over_price = as.numeric(alternate_points_40_odds)) |>
    mutate(market_name = "Alternate Player Points") |>
    mutate(agency = "Bet365")
  
  # Combine
  alternate_player_points <-
    bind_rows(alternate_points_5, alternate_points_10, alternate_points_15, alternate_points_20,
              alternate_points_25, alternate_points_30, alternate_points_35, alternate_points_40) |> 
    filter(!is.na(over_price))
  
  
  #=============================================================================
  # Combine Lines and milestones for Player Points
  #=============================================================================
  
  # Get teams
  team_names <-
    scraped_file |> 
    read_html() |> 
    html_nodes(".sph-FixturePodHeader_TeamName ") |> 
    html_text()
  
  team_names <- fix_team_names(team_names)
  
  # Get Match Name
  match_name <- paste(team_names, collapse = " v ")
  
  # Combine all tables
  player_points_all <-
    bind_rows(player_points,alternate_player_points) |> 
    arrange(player, line, over_price) |> 
    mutate(market_name = "Player Points") |> 
    mutate(match = match_name) |> 
    relocate(match, .before = player)
  
  #=============================================================================
  # Player Rebounds Over / Under
  #=============================================================================
  
  # Get index for node with text "Player Rebounds Over/Under"
  rebounds_over_under_index <- which(market_names == "Rebounds O/U")
  
  # Get Player Names from node
  rebounds_players <-
    bet365_player_markets[[rebounds_over_under_index]] |>
    html_elements(".srb-ParticipantLabelWithTeam_Name") |>
    html_text()
  
  # # Get Player Teams from node
  # rebounds_teams <-
  #     bet365_player_markets[[rebounds_over_under_index]] |>
  #     html_elements(".srb-ParticipantLabelWithTeam_Team") |>
  #     html_text()
  
  # Get Over Node Index
  rebounds_cols <-
    bet365_player_markets[[rebounds_over_under_index]] |>
    html_elements(".gl-Market_General")
  
  rebounds_over_index <- which(str_detect(rebounds_cols |> html_text(), "Over"))
  
  # Get Over Lines
  rebounds_over_lines <-
    rebounds_cols[[rebounds_over_index]] |>
    html_elements(".gl-ParticipantCenteredStacked_Handicap") |>
    html_text()
  
  # Get Over Odds
  rebounds_over_odds <-
    rebounds_cols[[rebounds_over_index]] |>
    html_elements(".gl-ParticipantCenteredStacked_Odds") |>
    html_text()
  
  # Get Under Node Index
  rebounds_under_index <- which(str_detect(rebounds_cols |> html_text(), "Under"))
  
  # Get Under Odds
  rebounds_under_odds <-
    rebounds_cols[[rebounds_under_index]] |>
    html_elements(".gl-ParticipantCenteredStacked_Odds") |>
    html_text()
  
  # Create Player Rebounds Table
  player_rebounds <-
    tibble(player = rebounds_players,
           # team = rebounds_teams,
           line = as.numeric(rebounds_over_lines),
           over_price = as.numeric(rebounds_over_odds),
           under_price = as.numeric(rebounds_under_odds)) |>
    mutate(market_name = "Player Rebounds Over/Under") |>
    mutate(agency = "Bet365")
  
  #=============================================================================
  # Alternate Player Rebounds
  #=============================================================================
  
  # Get index for node with text "Player Rebounds Milestones"
  alternate_rebounds_index <- which(market_names == "Rebounds")
  
  # Get Player Names from node
  alternate_rebounds_players <-
    bet365_player_markets[[alternate_rebounds_index]] |>
    html_elements(".srb-ParticipantLabelWithTeam_Name") |>
    html_text()
  
  # # Get Player Teams from node
  # alternate_rebounds_teams <-
  #     bet365_player_markets[[alternate_rebounds_index]] |>
  #     html_elements(".srb-ParticipantLabelWithTeam_Team") |>
  #     html_text()
  
  # Get Rebounds Node Indexes for 3 to 17 rebounds
  alternate_rebounds_cols <-
    bet365_player_markets[[alternate_rebounds_index]] |>
    html_elements(".gl-Market_General")
  
  alternate_rebounds_3_index <- which(str_detect(alternate_rebounds_cols |> html_node(".srb-HScrollPlaceHeader ") |> html_text(), "^3$"))
  alternate_rebounds_5_index <- which(str_detect(alternate_rebounds_cols |> html_node(".srb-HScrollPlaceHeader ") |> html_text(), "^5$"))
  alternate_rebounds_7_index <- which(str_detect(alternate_rebounds_cols |> html_node(".srb-HScrollPlaceHeader ") |> html_text(), "^7$"))
  alternate_rebounds_10_index <- which(str_detect(alternate_rebounds_cols |> html_node(".srb-HScrollPlaceHeader ") |> html_text(), "10"))
  alternate_rebounds_13_index <- which(str_detect(alternate_rebounds_cols |> html_node(".srb-HScrollPlaceHeader ") |> html_text(), "13"))
  alternate_rebounds_15_index <- which(str_detect(alternate_rebounds_cols |> html_node(".srb-HScrollPlaceHeader ") |> html_text(), "15"))
  alternate_rebounds_17_index <- which(str_detect(alternate_rebounds_cols |> html_node(".srb-HScrollPlaceHeader ") |> html_text(), "17"))
  
  # Get Odds for each rebounds range
  alternate_rebounds_3_odds <-
    alternate_rebounds_cols[[alternate_rebounds_3_index]] |>
    html_elements(".gl-ParticipantOddsOnly_Odds") |>
    html_text()
  
  alternate_rebounds_5_odds <-
    alternate_rebounds_cols[[alternate_rebounds_5_index]] |>
    html_elements(".gl-ParticipantOddsOnly_Odds") |>
    html_text()
  
  alternate_rebounds_7_odds <-
    alternate_rebounds_cols[[alternate_rebounds_7_index]] |>
    html_elements(".gl-ParticipantOddsOnly_Odds") |>
    html_text()
  
  alternate_rebounds_10_odds <-
    alternate_rebounds_cols[[alternate_rebounds_10_index]] |>
    html_elements(".gl-ParticipantOddsOnly_Odds") |>
    html_text()
  
  alternate_rebounds_13_odds <-
    alternate_rebounds_cols[[alternate_rebounds_13_index]] |>
    html_elements(".gl-ParticipantOddsOnly_Odds") |>
    html_text()
  
  alternate_rebounds_15_odds <-
    alternate_rebounds_cols[[alternate_rebounds_15_index]] |>
    html_elements(".gl-ParticipantOddsOnly_Odds") |>
    html_text()
  
  alternate_rebounds_17_odds <-
    alternate_rebounds_cols[[alternate_rebounds_17_index]] |>
    html_elements(".gl-ParticipantOddsOnly_Odds") |>
    html_text()
  
  # Create Alternate Player Rebounds Tables
  alternate_rebounds_3 <-
    tibble(player = alternate_rebounds_players,
           # team = alternate_rebounds_teams,
           line = 3,
           over_price = as.numeric(alternate_rebounds_3_odds)) |>
    mutate(market_name = "Alternate Player Rebounds") |>
    mutate(agency = "Bet365")
  
  alternate_rebounds_5 <-
    tibble(player = alternate_rebounds_players,
           # team = alternate_rebounds_teams,
           line = 5,
           over_price = as.numeric(alternate_rebounds_5_odds)) |>
    mutate(market_name = "Alternate Player Rebounds") |>
    mutate(agency = "Bet365")
  
  alternate_rebounds_7 <-
    tibble(player = alternate_rebounds_players,
           # team = alternate_rebounds_teams,
           line = 7,
           over_price = as.numeric(alternate_rebounds_7_odds)) |>
    mutate(market_name = "Alternate Player Rebounds") |>
    mutate(agency = "Bet365")
  
  alternate_rebounds_10 <-
    tibble(player = alternate_rebounds_players,
           # team = alternate_rebounds_teams,
           line = 10,
           over_price = as.numeric(alternate_rebounds_10_odds)) |>
    mutate(market_name = "Alternate Player Rebounds") |>
    mutate(agency = "Bet365")
  
  alternate_rebounds_13 <-
    tibble(player = alternate_rebounds_players,
           # team = alternate_rebounds_teams,
           line = 13,
           over_price = as.numeric(alternate_rebounds_13_odds)) |>
    mutate(market_name = "Alternate Player Rebounds") |>
    mutate(agency = "Bet365")
  
  alternate_rebounds_15 <-
    tibble(player = alternate_rebounds_players,
           # team = alternate_rebounds_teams,
           line = 15,
           over_price = as.numeric(alternate_rebounds_15_odds)) |>
    mutate(market_name = "Alternate Player Rebounds") |>
    mutate(agency = "Bet365")
  
  alternate_rebounds_17 <-
    tibble(player = alternate_rebounds_players,
           # team = alternate_rebounds_teams,
           line = 17,
           over_price = as.numeric(alternate_rebounds_17_odds)) |>
    mutate(market_name = "Alternate Player Rebounds") |>
    mutate(agency = "Bet365")
  
  # Combine
  alternate_player_rebounds <-
    bind_rows(alternate_rebounds_3, alternate_rebounds_5, alternate_rebounds_7, alternate_rebounds_10,
              alternate_rebounds_13, alternate_rebounds_15, alternate_rebounds_17) |> 
    filter(!is.na(over_price))
  
  #=============================================================================
  # Combine Lines and Milestones for Player Rebounds
  #=============================================================================
  
  # Get teams
  team_names <-
    scraped_file |> 
    read_html() |> 
    html_nodes(".sph-FixturePodHeader_TeamName ") |> 
    html_text()
  
  team_names <- fix_team_names(team_names)
  
  # Get Match Name
  match_name <- paste(team_names, collapse = " v ")
  
  # Combine all tables
  player_rebounds_all <-
    bind_rows(player_rebounds, alternate_player_rebounds) |> 
    arrange(player, line, over_price) |> 
    mutate(market_name = "Player Rebounds") |> 
    mutate(match = match_name) 
  
  #=============================================================================
  # Player Assists Over / Under
  #=============================================================================
  
  # Get index for node with text "Player Assists Over/Under"
  assists_over_under_index <- which(market_names == "Assists O/U")
  
  # Get Player Names from node
  assists_players <-
    bet365_player_markets[[assists_over_under_index]] |>
    html_elements(".srb-ParticipantLabelWithTeam_Name") |>
    html_text()
  
  # # Get Player Teams from node
  # assists_teams <-
  #     bet365_player_markets[[assists_over_under_index]] |>
  #     html_elements(".srb-ParticipantLabelWithTeam_Team") |>
  #     html_text()
  
  # Get Over Node Index
  assists_cols <-
    bet365_player_markets[[assists_over_under_index]] |>
    html_elements(".gl-Market_General")
  
  assists_over_index <- which(str_detect(assists_cols |> html_text(), "Over"))
  
  # Get Over Lines
  assists_over_lines <-
    assists_cols[[assists_over_index]] |>
    html_elements(".gl-ParticipantCenteredStacked_Handicap") |>
    html_text()
  
  # Get Over Odds
  assists_over_odds <-
    assists_cols[[assists_over_index]] |>
    html_elements(".gl-ParticipantCenteredStacked_Odds") |>
    html_text()
  
  # Get Under Node Index
  assists_under_index <- which(str_detect(assists_cols |> html_text(), "Under"))
  
  # Get Under Odds
  assists_under_odds <-
    assists_cols[[assists_under_index]] |>
    html_elements(".gl-ParticipantCenteredStacked_Odds") |>
    html_text()
  
  # Create Player Assists Table
  player_assists <-
    tibble(player = assists_players,
           # team = assists_teams,
           line = as.numeric(assists_over_lines),
           over_price = as.numeric(assists_over_odds),
           under_price = as.numeric(assists_under_odds)) |>
    mutate(market_name = "Player Assists Over/Under") |>
    mutate(agency = "Bet365")
  
  #=============================================================================
  # Alternate Player Assists
  #=============================================================================
  
  # Get index for node with text "Player Assists Milestones"
  alternate_assists_index <- which(market_names == "Assists")
  
  # Get Player Names from node
  alternate_assists_players <-
    bet365_player_markets[[alternate_assists_index]] |>
    html_elements(".srb-ParticipantLabelWithTeam_Name") |>
    html_text()
  
  # # Get Player Teams from node
  # alternate_assists_teams <-
  #     bet365_player_markets[[alternate_assists_index]] |>
  #     html_elements(".srb-ParticipantLabelWithTeam_Team") |>
  #     html_text()
  
  # Get Assists Node Indexes for 3 to 10 assists
  alternate_assists_cols <-
    bet365_player_markets[[alternate_assists_index]] |>
    html_elements(".gl-Market_General")
  
  alternate_assists_3_index <- which(str_detect(alternate_assists_cols |> html_node(".srb-HScrollPlaceHeader ") |> html_text(), "^3$"))
  alternate_assists_5_index <- which(str_detect(alternate_assists_cols |> html_node(".srb-HScrollPlaceHeader ") |> html_text(), "^5$"))
  alternate_assists_7_index <- which(str_detect(alternate_assists_cols |> html_node(".srb-HScrollPlaceHeader ") |> html_text(), "^7$"))
  alternate_assists_10_index <- which(str_detect(alternate_assists_cols |> html_node(".srb-HScrollPlaceHeader ") |> html_text(), "10"))
  
  # Get Odds for each assists range
  alternate_assists_3_odds <-
    alternate_assists_cols[[alternate_assists_3_index]] |>
    html_elements(".gl-ParticipantOddsOnly_Odds") |>
    html_text()
  
  alternate_assists_5_odds <-
    alternate_assists_cols[[alternate_assists_5_index]] |>
    html_elements(".gl-ParticipantOddsOnly_Odds") |>
    html_text()
  
  alternate_assists_7_odds <-
    alternate_assists_cols[[alternate_assists_7_index]] |>
    html_elements(".gl-ParticipantOddsOnly_Odds") |>
    html_text()
  
  alternate_assists_10_odds <-
    alternate_assists_cols[[alternate_assists_10_index]] |>
    html_elements(".gl-ParticipantOddsOnly_Odds") |>
    html_text()
  
  # Create Alternate Player Assists Tables
  alternate_assists_3 <-
    tibble(player = alternate_assists_players,
           # team = alternate_assists_teams,
           line = 3,
           over_price = as.numeric(alternate_assists_3_odds)) |>
    mutate(market_name = "Alternate Player Assists") |>
    mutate(agency = "Bet365")
  
  alternate_assists_5 <-
    tibble(player = alternate_assists_players,
           # team = alternate_assists_teams,
           line = 5,
           over_price = as.numeric(alternate_assists_5_odds)) |>
    mutate(market_name = "Alternate Player Assists") |>
    mutate(agency = "Bet365")
  
  alternate_assists_7 <-
    tibble(player = alternate_assists_players,
           # team = alternate_assists_teams,
           line = 7,
           over_price = as.numeric(alternate_assists_7_odds)) |>
    mutate(market_name = "Alternate Player Assists") |>
    mutate(agency = "Bet365")
  
  alternate_assists_10 <-
    tibble(player = alternate_assists_players,
           # team = alternate_assists_teams,
           line = 10,
           over_price = as.numeric(alternate_assists_10_odds)) |>
    mutate(market_name = "Alternate Player Assists") |>
    mutate(agency = "Bet365")
  
  # Combine
  alternate_player_assists <-
    bind_rows(alternate_assists_3, alternate_assists_5, alternate_assists_7, alternate_assists_10) |> 
    filter(!is.na(over_price))
  
  
  #=============================================================================
  # Combine Lines and Milestones for Player Assists
  #=============================================================================
  
  # Get teams
  team_names <-
    scraped_file |> 
    read_html() |> 
    html_nodes(".sph-FixturePodHeader_TeamName ") |> 
    html_text()
  
  team_names <- fix_team_names(team_names)
  
  # Get Match Name
  match_name <- paste(team_names, collapse = " v ")
  
  # Combine all tables
  player_assists_all <-
    bind_rows(player_assists, alternate_player_assists) |> 
    arrange(player, line, over_price) |> 
    mutate(market_name = "Player Assists") |> 
    mutate(match = match_name) |> 
    relocate(match, .before = player)
  
  #=============================================================================
  # Player Threes Made Over / Under
  #=============================================================================
  
  # Get index for node with text "Player Threes Made Over/Under"
  threes_over_under_index <- which(market_names == "Threes Made O/U")
  
  # Get Player Names from node
  threes_players <-
    bet365_player_markets[[threes_over_under_index]] |>
    html_elements(".srb-ParticipantLabelWithTeam_Name") |>
    html_text()
  
  # # Get Player Teams from node
  # threes_teams <-
  #     bet365_player_markets[[threes_over_under_index]] |>
  #     html_elements(".srb-ParticipantLabelWithTeam_Team") |>
  #     html_text()
  
  # Get Over Node Index
  threes_cols <-
    bet365_player_markets[[threes_over_under_index]] |>
    html_elements(".gl-Market_General")
  
  threes_over_index <- which(str_detect(threes_cols |> html_text(), "Over"))
  
  # Get Over Lines
  threes_over_lines <-
    threes_cols[[threes_over_index]] |>
    html_elements(".gl-ParticipantCenteredStacked_Handicap") |>
    html_text()
  
  # Get Over Odds
  threes_over_odds <-
    threes_cols[[threes_over_index]] |>
    html_elements(".gl-ParticipantCenteredStacked_Odds") |>
    html_text()
  
  # Get Under Node Index
  threes_under_index <- which(str_detect(threes_cols |> html_text(), "Under"))
  
  # Get Under Odds
  threes_under_odds <-
    threes_cols[[threes_under_index]] |>
    html_elements(".gl-ParticipantCenteredStacked_Odds") |>
    html_text()
  
  # Create Player Threes Made Table
  player_threes_made <-
    tibble(player = threes_players,
           # team = threes_teams,
           line = as.numeric(threes_over_lines),
           over_price = as.numeric(threes_over_odds),
           under_price = as.numeric(threes_under_odds)) |>
    mutate(market_name = "Player Threes Made Over/Under") |>
    mutate(agency = "Bet365")
  
  #=============================================================================
  # Alternate Player Threes Made
  #=============================================================================
  
  # Get index for node with text "Player Threes Made Milestones"
  alternate_threes_index <- which(market_names == "Threes Made")
  
  # Get Player Names from node
  alternate_threes_players <-
    bet365_player_markets[[alternate_threes_index]] |>
    html_elements(".srb-ParticipantLabelWithTeam_Name") |>
    html_text()
  
  # # Get Player Teams from node
  # alternate_threes_teams <-
  #     bet365_player_markets[[alternate_threes_index]] |>
  #     html_elements(".srb-ParticipantLabelWithTeam_Team") |>
  #     html_text()
  
  # Get Threes Made Node Indexes for 1 to 5 threes
  alternate_threes_cols <-
    bet365_player_markets[[alternate_threes_index]] |>
    html_elements(".gl-Market_General")
  
  # Define indexes for each milestone (1 to 5 threes)
  alternate_threes_indexes <- list()
  for(i in 1:5) {
    alternate_threes_indexes[[i]] <- which(str_detect(alternate_threes_cols |> html_node(".srb-HScrollPlaceHeader ") |> html_text(), paste0("^", i, "$")))
  }
  
  # Get Odds for each threes range
  alternate_threes_odds <- list()
  for(i in 1:5) {
    alternate_threes_odds[[i]] <- alternate_threes_cols[[alternate_threes_indexes[[i]]]] |>
      html_elements(".gl-ParticipantOddsOnly_Odds") |>
      html_text()
  }
  
  # Create Alternate Player Threes Made Tables
  alternate_threes_tables <- list()
  for(i in 1:5) {
    alternate_threes_tables[[i]] <-
      tibble(player = alternate_threes_players,
             # team = alternate_threes_teams,
             line = i,
             over_price = as.numeric(alternate_threes_odds[[i]])) |>
      mutate(market_name = "Alternate Player Threes Made") |>
      mutate(agency = "Bet365")
  }
  
  # Combine all alternate tables into one
  alternate_player_threes_made <-
    bind_rows(alternate_threes_tables) |> 
    filter(!is.na(over_price))
  
  
  #=============================================================================
  # Combine Lines and Milestones for Player Threes Made
  #=============================================================================
  
  # Get teams
  team_names <-
    scraped_file |> 
    read_html() |> 
    html_nodes(".sph-FixturePodHeader_TeamName ") |> 
    html_text()
  
  team_names <- fix_team_names(team_names)
  
  # Get Match Name
  match_name <- paste(team_names, collapse = " v ")
  
  # Combine all tables
  player_threes_made_all <-
    bind_rows(player_threes_made, alternate_player_threes_made) |> 
    arrange(player, line, over_price) |> 
    mutate(market_name = "Player Threes Made") |> 
    mutate(match = match_name) |> 
    relocate(match, .before = player)
  
  #===============================================================================
  # Combine markets together
  #===============================================================================
  
  return(
    player_points_all |> 
      bind_rows(player_rebounds_all) |>
      bind_rows(player_assists_all) |>
      bind_rows(player_threes_made_all))
}

# Create safe version of function
get_player_props_safe <- safely(get_player_props)

# Map Over all html files
list_of_player_props <- map(scraped_files_player, get_player_props_safe)

# Keep only successful results
list_of_player_props <-
  list_of_player_props |>
  # Keep if the error is null
  keep(~is.null(.x$error)) |>
  # Extract the result
  map_dfr("result")

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

# Combine into a df
all_player_props <-
  list_of_player_props |> 
  mutate(player = ifelse(player == "Derrick Walton Jr.", "Derrick Walton Jr", player)) |>
  left_join(player_names[,c("player_full_name", "team_name")], by = c("player" = "player_full_name")) |>
  rename(player_name = player) |> 
  mutate(player_team = fix_team_names(team_name)) |> 
  separate(match, into = c("away_team", "home_team"), sep = " v ", remove = FALSE) |> 
  mutate(match = paste(home_team, away_team, sep = " v ")) |>
  mutate(opposition_team = if_else(player_team == home_team, away_team, home_team)) |>
  # If line ends with .0 subtract 0.5
  mutate(line = if_else(line %% 1 == 0, line - 0.5, line)) |> 
  select(-team_name)

# Separate into points, rebounds, assists, threes
player_points <- all_player_props |> filter(market_name == "Player Points")
player_rebounds <- all_player_props |> filter(market_name == "Player Rebounds")
player_assists <- all_player_props |> filter(market_name == "Player Assists")
player_threes <- all_player_props |> filter(market_name == "Player Threes Made") |> mutate(market_name = "Player Threes")

# Write out
write_csv(player_points, "Data/scraped_odds/bet365_player_points.csv")
write_csv(player_rebounds, "Data/scraped_odds/bet365_player_rebounds.csv")
write_csv(player_assists, "Data/scraped_odds/bet365_player_assists.csv")
write_csv(player_threes, "Data/scraped_odds/bet365_player_threes.csv")
