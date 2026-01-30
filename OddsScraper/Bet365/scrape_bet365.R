# Libraries
library(tidyverse)
library(rvest)
library(httr2)
library(jsonlite)
library(glue)

# Fix team names and players functions
source("Scripts/fix_team_names.R")
source("Scripts/fix_player_names.R")

# Get teams table
teams <- read_csv("Data/all_teams.csv")

# Get players
player_names_all <-
  read_csv("Data/all_rosters.csv") |>
  select(player_full_name = PLAYER, TeamID) |> 
  left_join(teams[, c("id", "full_name")], by = c("TeamID" = "id")) |> 
  mutate(first_initial = str_sub(player_full_name, 1, 1)) |>
  
  mutate(surname = str_extract(player_full_name, "(?<=\\s).*$")) |> 
  mutate(join_name = paste(first_initial, surname, sep = " ")) |> 
  rename(team_name = full_name)

# Read scraped HTML files by category
html_folder <- "OddsScraper/Bet365/HTML"

files_points   <- list.files(html_folder, full.names = TRUE, pattern = "players_match_\\d+_points\\.txt$")
files_threes   <- list.files(html_folder, full.names = TRUE, pattern = "players_match_\\d+_threes\\.txt$")
files_assists  <- list.files(html_folder, full.names = TRUE, pattern = "players_match_\\d+_assists\\.txt$")
files_rebounds <- list.files(html_folder, full.names = TRUE, pattern = "players_match_\\d+_rebounds\\.txt$")
files_combos   <- list.files(html_folder, full.names = TRUE, pattern = "players_match_\\d+_combos\\.txt$")
files_defence  <- list.files(html_folder, full.names = TRUE, pattern = "players_match_\\d+_defence\\.txt$")

#===============================================================================
# Helper functions
#===============================================================================

# Safe empty tibble
safe_tbl <- function() {
  
  tibble(
    match = character(),
    player = character(),
    line = numeric(),
    over_price = numeric(),
    under_price = numeric(),
    market_name = character(),
    agency = character()
  )
}

# Safe section wrapper
safe_section <- function(expr) tryCatch(expr, error = function(e) safe_tbl())

# Get match name from HTML file
get_match_name <- function(scraped_file) {
  team_names <-
    scraped_file |>
    read_html() |>
    html_nodes(".sph-FixturePodHeader_TeamName ") |>
    html_text()
  team_names <- fix_team_names(team_names)
  paste(team_names, collapse = " @ ")
}

# Get suspended elements from a column node
get_suspended_indices <- function(col_node) {
  child_nodes <- html_children(col_node)
  as.character(child_nodes) |>
    as_tibble() |>
    filter(str_detect(value, "ParticipantCenteredStacked")) |>
    mutate(row_num = row_number()) |>
    filter(str_detect(value, "Suspended")) |>
    pull(row_num)
}

#===============================================================================
# Points Parser (I43)
#===============================================================================

get_player_points <- function(scraped_file) {
  
  bet365_player_markets <-
    read_html(scraped_file) |>
    html_nodes(".gl-MarketGroupPod")
  
  market_names <-
    bet365_player_markets |>
    html_elements(".cm-MarketGroupWithIconsButton_Text, .sc-MarketGroupButtonWithStats_Text") |>
    html_text()
  
  match_name <- get_match_name(scraped_file)
  
  player_points_all <- safe_section({
    
    parse_points_ou_market <- function(market_label, out_market_name) {
      market_index <- which(market_names == market_label)
      if (length(market_index) == 0) return(NULL)
      
      points_players <-
        bet365_player_markets[[market_index]] |>
        html_elements(".srb-ParticipantLabelWithTeam_Name") |>
        html_text()
      
      points_cols <-
        bet365_player_markets[[market_index]] |>
        html_elements(".gl-Market_General")
      
      points_over_index <- which(str_detect(points_cols |> html_text(), "Over"))
      points_under_index <- which(str_detect(points_cols |> html_text(), "Under"))
      if (length(points_over_index) == 0 || length(points_under_index) == 0) return(NULL)
      
      points_over_lines <-
        points_cols[[points_over_index]] |>
        html_elements(".gl-ParticipantCenteredStacked_Handicap") |>
        html_text()
      
      points_over_odds <-
        points_cols[[points_over_index]] |>
        html_elements(".gl-ParticipantCenteredStacked_Odds") |>
        html_text()
      
      points_under_odds <-
        points_cols[[points_under_index]] |>
        html_elements(".gl-ParticipantCenteredStacked_Odds") |>
        html_text()
      
      suspended_elements <- get_suspended_indices(points_cols[[points_over_index]])
      
      player_points <-
        tibble(
          player = points_players,
          line = as.numeric(points_over_lines),
          over_price = as.numeric(points_over_odds),
          under_price = as.numeric(points_under_odds)
        ) |>
        mutate(market_name = out_market_name, agency = "Bet365")
      
      if (length(suspended_elements) > 0) {
        player_points <- player_points |> slice(-suspended_elements)
      }
      
      return(player_points)
    }
    
    # Points O/U + Points High/Low (same structure as O/U)
    player_points <- bind_rows(
      parse_points_ou_market("Points O/U", "Player Points Over/Under"),
      parse_points_ou_market("Points High", "Player Points High"),
      parse_points_ou_market("Points Low", "Player Points Low")
    )
    
    # Alternate Points (Milestones)
    alternate_points_index <- which(market_names == "Points")
    
    alternate_points_players <-
      bet365_player_markets[[alternate_points_index]] |>
      html_elements(".srb-ParticipantLabelWithTeam_Name") |>
      html_text()
    
    alternate_points_cols <-
      bet365_player_markets[[alternate_points_index]] |>
      html_elements(".gl-Market_General")
    
    milestone_values <- c(5, 10, 15, 20, 25, 30, 35, 40)
    
    alternate_player_points <- map_dfr(milestone_values, function(val) {
      pattern <- if (val < 10) paste0("^", val, "$") else as.character(val)
      idx <- which(str_detect(
        alternate_points_cols |> html_node(".srb-HScrollPlaceHeader ") |> html_text(),
        pattern
      ))
      if (length(idx) == 0) return(NULL)
      
      odds <- alternate_points_cols[[idx]] |>
        html_elements(".gl-ParticipantOddsOnly_Odds") |>
        html_text()
      
      tibble(
        player = alternate_points_players,
        line = val,
        over_price = as.numeric(odds)
      ) |>
        mutate(market_name = "Alternate Player Points", agency = "Bet365")
    }) |>
      filter(!is.na(over_price))
    
    bind_rows(player_points, alternate_player_points) |>
      arrange(player, line, over_price) |>
      mutate(market_name = "Player Points", match = match_name) |>
      relocate(match, .before = player)
  })
  
  return(player_points_all)
}

#===============================================================================
# Threes Parser (I45)
#===============================================================================

get_player_threes <- function(scraped_file) {
  
  bet365_player_markets <-
    read_html(scraped_file) |>
    html_nodes(".gl-MarketGroupPod")
  
  market_names <-
    bet365_player_markets |>
    html_elements(".cm-MarketGroupWithIconsButton_Text, .sc-MarketGroupButtonWithStats_Text") |>
    html_text()
  
  match_name <- get_match_name(scraped_file)
  
  player_threes_all <- safe_section({
    
    # Threes O/U
    threes_over_under_index <- which(market_names == "Threes Made O/U")
    
    threes_players <-
      bet365_player_markets[[threes_over_under_index]] |>
      html_elements(".srb-ParticipantLabelWithTeam_Name") |>
      html_text()
    
    threes_cols <-
      bet365_player_markets[[threes_over_under_index]] |>
      html_elements(".gl-Market_General")
    
    threes_over_index <- which(str_detect(threes_cols |> html_text(), "Over"))
    threes_under_index <- which(str_detect(threes_cols |> html_text(), "Under"))
    
    threes_over_lines <-
      threes_cols[[threes_over_index]] |>
      html_elements(".gl-ParticipantCenteredStacked_Handicap") |>
      html_text()
    
    threes_over_odds <-
      threes_cols[[threes_over_index]] |>
      html_elements(".gl-ParticipantCenteredStacked_Odds") |>
      html_text()
    
    threes_under_odds <-
      threes_cols[[threes_under_index]] |>
      html_elements(".gl-ParticipantCenteredStacked_Odds") |>
      html_text()
    
    suspended_elements <- get_suspended_indices(threes_cols[[threes_over_index]])
    
    player_threes <-
      tibble(
        player = threes_players,
        line = as.numeric(threes_over_lines),
        over_price = as.numeric(threes_over_odds),
        under_price = as.numeric(threes_under_odds)
      ) |>
      mutate(market_name = "Player Threes Made Over/Under", agency = "Bet365")
    
    if (length(suspended_elements) > 0) {
      player_threes <- player_threes |> slice(-suspended_elements)
    }
    
    # Alternate Threes (Milestones)
    alternate_threes_index <- which(market_names == "Threes Made")
    
    alternate_threes_players <-
      bet365_player_markets[[alternate_threes_index]] |>
      html_elements(".srb-ParticipantLabelWithTeam_Name") |>
      html_text()
    
    alternate_threes_cols <-
      bet365_player_markets[[alternate_threes_index]] |>
      html_elements(".gl-Market_General")
    
    alternate_player_threes <- map_dfr(1:5, function(val) {
      idx <- which(str_detect(
        alternate_threes_cols |> html_node(".srb-HScrollPlaceHeader ") |> html_text(),
        paste0("^", val, "$")
      ))
      if (length(idx) == 0) return(NULL)
      
      odds <- alternate_threes_cols[[idx]] |>
        html_elements(".gl-ParticipantOddsOnly_Odds") |>
        html_text()
      
      tibble(
        player = alternate_threes_players,
        line = val,
        over_price = as.numeric(odds)
      ) |>
        mutate(market_name = "Alternate Player Threes Made", agency = "Bet365")
    }) |>
      filter(!is.na(over_price))
    
    bind_rows(player_threes, alternate_player_threes) |>
      arrange(player, line, over_price) |>
      mutate(market_name = "Player Threes Made", match = match_name) |>
      relocate(match, .before = player)
  })
  
  return(player_threes_all)
}

#===============================================================================
# Assists Parser (I46)
#===============================================================================

get_player_assists <- function(scraped_file) {
  
  bet365_player_markets <-
    read_html(scraped_file) |>
    html_nodes(".gl-MarketGroupPod")
  
  market_names <-
    bet365_player_markets |>
    html_elements(".cm-MarketGroupWithIconsButton_Text, .sc-MarketGroupButtonWithStats_Text") |>
    html_text()
  
  match_name <- get_match_name(scraped_file)
  
  player_assists_all <- safe_section({
    
    # Assists O/U
    assists_over_under_index <- which(market_names == "Assists O/U")
    
    assists_players <-
      bet365_player_markets[[assists_over_under_index]] |>
      html_elements(".srb-ParticipantLabelWithTeam_Name") |>
      html_text()
    
    assists_cols <-
      bet365_player_markets[[assists_over_under_index]] |>
      html_elements(".gl-Market_General")
    
    assists_over_index <- which(str_detect(assists_cols |> html_text(), "Over"))
    assists_under_index <- which(str_detect(assists_cols |> html_text(), "Under"))
    
    assists_over_lines <-
      assists_cols[[assists_over_index]] |>
      html_elements(".gl-ParticipantCenteredStacked_Handicap") |>
      html_text()
    
    assists_over_odds <-
      assists_cols[[assists_over_index]] |>
      html_elements(".gl-ParticipantCenteredStacked_Odds") |>
      html_text()
    
    assists_under_odds <-
      assists_cols[[assists_under_index]] |>
      html_elements(".gl-ParticipantCenteredStacked_Odds") |>
      html_text()
    
    suspended_elements <- get_suspended_indices(assists_cols[[assists_over_index]])
    
    player_assists <-
      tibble(
        player = assists_players,
        line = as.numeric(assists_over_lines),
        over_price = as.numeric(assists_over_odds),
        under_price = as.numeric(assists_under_odds)
      ) |>
      mutate(market_name = "Player Assists Over/Under", agency = "Bet365")
    
    if (length(suspended_elements) > 0) {
      player_assists <- player_assists |> slice(-suspended_elements)
    }
    
    # Alternate Assists (Milestones)
    alternate_assists_index <- which(market_names == "Assists")
    
    alternate_assists_players <-
      bet365_player_markets[[alternate_assists_index]] |>
      html_elements(".srb-ParticipantLabelWithTeam_Name") |>
      html_text()
    
    alternate_assists_cols <-
      bet365_player_markets[[alternate_assists_index]] |>
      html_elements(".gl-Market_General")
    
    milestone_values <- c(3, 5, 7, 10)
    
    alternate_player_assists <- map_dfr(milestone_values, function(val) {
      pattern <- if (val < 10) paste0("^", val, "$") else as.character(val)
      idx <- which(str_detect(
        alternate_assists_cols |> html_node(".srb-HScrollPlaceHeader ") |> html_text(),
        pattern
      ))
      if (length(idx) == 0) return(NULL)
      
      odds <- alternate_assists_cols[[idx]] |>
        html_elements(".gl-ParticipantOddsOnly_Odds") |>
        html_text()
      
      tibble(
        player = alternate_assists_players,
        line = val,
        over_price = as.numeric(odds)
      ) |>
        mutate(market_name = "Alternate Player Assists", agency = "Bet365")
    }) |>
      filter(!is.na(over_price))
    
    bind_rows(player_assists, alternate_player_assists) |>
      arrange(player, line, over_price) |>
      mutate(market_name = "Player Assists", match = match_name) |>
      relocate(match, .before = player)
  })
  
  return(player_assists_all)
}

#===============================================================================
# Rebounds Parser (I47)
#===============================================================================

get_player_rebounds <- function(scraped_file) {
  
  bet365_player_markets <-
    read_html(scraped_file) |>
    html_nodes(".gl-MarketGroupPod")
  
  market_names <-
    bet365_player_markets |>
    html_elements(".cm-MarketGroupWithIconsButton_Text, .sc-MarketGroupButtonWithStats_Text") |>
    html_text()
  
  match_name <- get_match_name(scraped_file)
  
  player_rebounds_all <- safe_section({
    
    # Rebounds O/U
    rebounds_over_under_index <- which(market_names == "Rebounds O/U")
    
    rebounds_players <-
      bet365_player_markets[[rebounds_over_under_index]] |>
      html_elements(".srb-ParticipantLabelWithTeam_Name") |>
      html_text()
    
    rebounds_cols <-
      bet365_player_markets[[rebounds_over_under_index]] |>
      html_elements(".gl-Market_General")
    
    rebounds_over_index <- which(str_detect(rebounds_cols |> html_text(), "Over"))
    rebounds_under_index <- which(str_detect(rebounds_cols |> html_text(), "Under"))
    
    rebounds_over_lines <-
      rebounds_cols[[rebounds_over_index]] |>
      html_elements(".gl-ParticipantCenteredStacked_Handicap") |>
      html_text()
    
    rebounds_over_odds <-
      rebounds_cols[[rebounds_over_index]] |>
      html_elements(".gl-ParticipantCenteredStacked_Odds") |>
      html_text()
    
    rebounds_under_odds <-
      rebounds_cols[[rebounds_under_index]] |>
      html_elements(".gl-ParticipantCenteredStacked_Odds") |>
      html_text()
    
    suspended_elements <- get_suspended_indices(rebounds_cols[[rebounds_over_index]])
    
    player_rebounds <-
      tibble(
        player = rebounds_players,
        line = as.numeric(rebounds_over_lines),
        over_price = as.numeric(rebounds_over_odds),
        under_price = as.numeric(rebounds_under_odds)
      ) |>
      mutate(market_name = "Player Rebounds Over/Under", agency = "Bet365")
    
    if (length(suspended_elements) > 0) {
      player_rebounds <- player_rebounds |> slice(-suspended_elements)
    }
    
    # Alternate Rebounds (Milestones)
    alternate_rebounds_index <- which(market_names == "Rebounds")
    
    alternate_rebounds_players <-
      bet365_player_markets[[alternate_rebounds_index]] |>
      html_elements(".srb-ParticipantLabelWithTeam_Name") |>
      html_text()
    
    alternate_rebounds_cols <-
      bet365_player_markets[[alternate_rebounds_index]] |>
      html_elements(".gl-Market_General")
    
    milestone_values <- c(5, 7, 10)
    
    alternate_player_rebounds <- map_dfr(milestone_values, function(val) {
      pattern <- if (val < 10) paste0("^", val, "$") else as.character(val)
      idx <- which(str_detect(
        alternate_rebounds_cols |> html_node(".srb-HScrollPlaceHeader ") |> html_text(),
        pattern
      ))
      if (length(idx) == 0) return(NULL)
      
      odds <- alternate_rebounds_cols[[idx]] |>
        html_elements(".gl-ParticipantOddsOnly_Odds") |>
        html_text()
      
      tibble(
        player = alternate_rebounds_players,
        line = val,
        over_price = as.numeric(odds)
      ) |>
        mutate(market_name = "Alternate Player Rebounds", agency = "Bet365")
    }) |>
      filter(!is.na(over_price))
    
    bind_rows(player_rebounds, alternate_player_rebounds) |>
      arrange(player, line, over_price) |>
      mutate(market_name = "Player Rebounds", match = match_name) |>
      relocate(match, .before = player)
  })
  
  return(player_rebounds_all)
}

#===============================================================================
# Combos Parser (I48) - Double Double, Triple Double, PRAs
#===============================================================================

get_player_combos <- function(scraped_file) {
  
  bet365_player_markets <-
    read_html(scraped_file) |>
    html_nodes(".gl-MarketGroupPod")
  
  market_names <-
    bet365_player_markets |>
    html_elements(".cm-MarketGroupWithIconsButton_Text, .sc-MarketGroupButtonWithStats_Text") |>
    html_text()
  
  match_name <- get_match_name(scraped_file)
  
  # Double Double
  player_double_double_all <- safe_section({
    double_double_index <- which(market_names == "Double Double")
    
    double_double_players <-
      bet365_player_markets[[double_double_index]] |>
      html_elements(".srb-ParticipantLabelWithTeam_Name") |>
      html_text()
    
    double_double_cols <-
      bet365_player_markets[[double_double_index]] |>
      html_elements(".gl-Market_General")
    
    double_double_over_index <- which(str_detect(double_double_cols |> html_text(), "Yes"))
    double_double_under_index <- which(str_detect(double_double_cols |> html_text(), "No"))
    
    double_double_over_odds <-
      double_double_cols[[double_double_over_index]] |>
      html_elements(".gl-ParticipantOddsOnly_Odds") |>
      html_text()
    
    double_double_under_odds <-
      double_double_cols[[double_double_under_index]] |>
      html_elements(".gl-ParticipantOddsOnly_Odds") |>
      html_text()
    
    tibble(
      match = match_name,
      player = double_double_players,
      line = 0.5,
      over_price = as.numeric(double_double_over_odds),
      under_price = as.numeric(double_double_under_odds),
      market_name = "Player Double Double",
      agency = "Bet365"
    )
  })
  
  # Triple Double
  player_triple_double_all <- safe_section({
    triple_double_index <- which(market_names == "Triple Double")
    
    triple_double_players <-
      bet365_player_markets[[triple_double_index]] |>
      html_elements(".srb-ParticipantLabelWithTeam_Name") |>
      html_text()
    
    triple_double_cols <-
      bet365_player_markets[[triple_double_index]] |>
      html_elements(".gl-Market_General")
    
    triple_double_over_index <- which(str_detect(triple_double_cols |> html_text(), "Yes"))
    triple_double_under_index <- which(str_detect(triple_double_cols |> html_text(), "No"))
    
    triple_double_over_odds <-
      triple_double_cols[[triple_double_over_index]] |>
      html_elements(".gl-ParticipantOddsOnly_Odds") |>
      html_text()
    
    triple_double_under_odds <-
      triple_double_cols[[triple_double_under_index]] |>
      html_elements(".gl-ParticipantOddsOnly_Odds") |>
      html_text()
    
    tibble(
      match = match_name,
      player = triple_double_players,
      line = 0.5,
      over_price = as.numeric(triple_double_over_odds),
      under_price = as.numeric(triple_double_under_odds),
      market_name = "Player Triple Double",
      agency = "Bet365"
    )
  })
  
  # TODO: Add PRA combo markets here if needed (Pts + Reb + Ast, etc.)
  # You'll need to identify the exact market names on the page
  
  bind_rows(player_double_double_all, player_triple_double_all)
}

#===============================================================================
# Defence Parser (I49) - Steals, Blocks
#===============================================================================

get_player_defence <- function(scraped_file) {
  
  bet365_player_markets <-
    read_html(scraped_file) |>
    html_nodes(".gl-MarketGroupPod")
  
  market_names <-
    bet365_player_markets |>
    html_elements(".cm-MarketGroupWithIconsButton_Text, .sc-MarketGroupButtonWithStats_Text") |>
    html_text()
  
  match_name <- get_match_name(scraped_file)
  
  # Steals
  player_steals_all <- safe_section({
    steals_over_under_index <- which(market_names == "Steals O/U")
    
    steals_players <-
      bet365_player_markets[[steals_over_under_index]] |>
      html_elements(".srb-ParticipantLabelWithTeam_Name") |>
      html_text()
    
    steals_cols <-
      bet365_player_markets[[steals_over_under_index]] |>
      html_elements(".gl-Market_General")
    
    steals_over_index <- which(str_detect(steals_cols |> html_text(), "Over"))
    steals_under_index <- which(str_detect(steals_cols |> html_text(), "Under"))
    
    steals_over_lines <-
      steals_cols[[steals_over_index]] |>
      html_elements(".gl-ParticipantCenteredStacked_Handicap") |>
      html_text()
    
    steals_over_odds <-
      steals_cols[[steals_over_index]] |>
      html_elements(".gl-ParticipantCenteredStacked_Odds") |>
      html_text()
    
    steals_under_odds <-
      steals_cols[[steals_under_index]] |>
      html_elements(".gl-ParticipantCenteredStacked_Odds") |>
      html_text()
    
    suspended_elements <- get_suspended_indices(steals_cols[[steals_over_index]])
    
    player_steals <-
      tibble(
        match = match_name,
        player = steals_players,
        line = as.numeric(steals_over_lines),
        over_price = as.numeric(steals_over_odds),
        under_price = as.numeric(steals_under_odds),
        market_name = "Player Steals",
        agency = "Bet365"
      )
    
    if (length(suspended_elements) > 0) {
      player_steals <- player_steals |> slice(-suspended_elements)
    }
    
    player_steals
  })
  
  # Blocks
  player_blocks_all <- safe_section({
    blocks_over_under_index <- which(market_names == "Blocks O/U")
    
    blocks_players <-
      bet365_player_markets[[blocks_over_under_index]] |>
      html_elements(".srb-ParticipantLabelWithTeam_Name") |>
      html_text()
    
    blocks_cols <-
      bet365_player_markets[[blocks_over_under_index]] |>
      html_elements(".gl-Market_General")
    
    blocks_over_index <- which(str_detect(blocks_cols |> html_text(), "Over"))
    blocks_under_index <- which(str_detect(blocks_cols |> html_text(), "Under"))
    
    blocks_over_lines <-
      blocks_cols[[blocks_over_index]] |>
      html_elements(".gl-ParticipantCenteredStacked_Handicap") |>
      html_text()
    
    blocks_over_odds <-
      blocks_cols[[blocks_over_index]] |>
      html_elements(".gl-ParticipantCenteredStacked_Odds") |>
      html_text()
    
    blocks_under_odds <-
      blocks_cols[[blocks_under_index]] |>
      html_elements(".gl-ParticipantCenteredStacked_Odds") |>
      html_text()
    
    suspended_elements <- get_suspended_indices(blocks_cols[[blocks_over_index]])
    
    player_blocks <-
      tibble(
        match = match_name,
        player = blocks_players,
        line = as.numeric(blocks_over_lines),
        over_price = as.numeric(blocks_over_odds),
        under_price = as.numeric(blocks_under_odds),
        market_name = "Player Blocks",
        agency = "Bet365"
      )
    
    if (length(suspended_elements) > 0) {
      player_blocks <- player_blocks |> slice(-suspended_elements)
    }
    
    player_blocks
  })
  
  bind_rows(player_steals_all, player_blocks_all)
}

#===============================================================================
# Process all files by category
#===============================================================================

# Create safe versions
get_player_points_safe   <- safely(get_player_points)
get_player_threes_safe   <- safely(get_player_threes)
get_player_assists_safe  <- safely(get_player_assists)
get_player_rebounds_safe <- safely(get_player_rebounds)
get_player_combos_safe   <- safely(get_player_combos)
get_player_defence_safe  <- safely(get_player_defence)

# Process each category
process_category <- function(files, parser_fn) {
  if (length(files) == 0) return(tibble())
  
  results <- map(files, parser_fn)
  
  results |>
    keep(~is.null(.x$error)) |>
    map_dfr("result")
}

points_data   <- process_category(files_points, get_player_points_safe)
threes_data   <- process_category(files_threes, get_player_threes_safe)
assists_data  <- process_category(files_assists, get_player_assists_safe)
rebounds_data <- process_category(files_rebounds, get_player_rebounds_safe)
combos_data   <- process_category(files_combos, get_player_combos_safe)
defence_data  <- process_category(files_defence, get_player_defence_safe)

#===============================================================================
# Combine and post-process
#===============================================================================

all_player_props <-
  bind_rows(
    points_data,
    threes_data,
    assists_data,
    rebounds_data,
    combos_data,
    defence_data
  ) |>
  mutate(player = fix_player_names(player)) |>
  left_join(player_names_all[, c("player_full_name", "team_name")], by = c("player" = "player_full_name")) |>
  rename(player_name = player) |>
  mutate(player_team = fix_team_names(team_name)) |>
  select(-team_name) |>
  separate(match, into = c("away_team", "home_team"), sep = " @ ", remove = FALSE) |>
  mutate(
    home_team = fix_team_names(home_team),
    away_team = fix_team_names(away_team)
  ) |>
  mutate(opposition_team = if_else(player_team == home_team, away_team, home_team)) |>
  mutate(match = paste(home_team, away_team, sep = " v ")) |>
  mutate(line = if_else(line %% 1 == 0, line - 0.5, line))

# Separate by market
player_points        <- all_player_props |> filter(market_name == "Player Points")
player_rebounds      <- all_player_props |> filter(market_name == "Player Rebounds")
player_assists       <- all_player_props |> filter(market_name == "Player Assists")
player_threes        <- all_player_props |> filter(market_name == "Player Threes Made") |> mutate(market_name = "Player Threes")
player_blocks        <- all_player_props |> filter(market_name == "Player Blocks")
player_steals        <- all_player_props |> filter(market_name == "Player Steals")
player_double_double <- all_player_props |> filter(market_name == "Player Double Double")
player_triple_double <- all_player_props |> filter(market_name == "Player Triple Double")

# Write out
write_csv(player_points, "Data/scraped_odds/bet365_player_points.csv")
write_csv(player_rebounds, "Data/scraped_odds/bet365_player_rebounds.csv")
write_csv(player_assists, "Data/scraped_odds/bet365_player_assists.csv")
write_csv(player_threes, "Data/scraped_odds/bet365_player_threes.csv")
write_csv(player_blocks, "Data/scraped_odds/bet365_player_blocks.csv")
write_csv(player_steals, "Data/scraped_odds/bet365_player_steals.csv")
write_csv(player_double_double, "Data/scraped_odds/bet365_player_double_double.csv")
write_csv(player_triple_double, "Data/scraped_odds/bet365_player_triple_double.csv")
