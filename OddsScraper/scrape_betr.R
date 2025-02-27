# Libraries
library(tidyverse)
library(rvest)
library(httr2)
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
    mutate(join_name = ifelse(player_full_name == "Jaylin Williams", "Jay Williams", join_name)) |> 
    mutate(join_name = ifelse(player_full_name == "Bojan Bogdanovic", "Boj Bogdanovic", join_name))

# Fix team names function
source("Scripts/fix_team_names.R")

# URL to get responses
bluebet_url = "https://web20-api.bluebet.com.au/SportsCategory?CategoryId=39251&format=json"

# Make request and get response
bluebet_response <-
    request(bluebet_url) |>
    req_perform() |> 
    resp_body_json()

# Get matches
matches <- bluebet_response$MasterCategories[[1]]$Categories[[1]]$MasterEvents

# Function to extract market info from response---------------------------------
get_market_info <- function(market) {
    
    # Market info
    markets_name = market$EventName
    market_propositions = market$OutcomeName
    market_prices = market$Price
    
    # Output Tibble
    tibble(market = markets_name,
           propositions = market_propositions,
           prices = market_prices)
}


# Function to extract match info from response----------------------------------
get_match_info <- function(matches) {
    # Match info
    match_name = matches$MasterEventName
    match_start_time = matches$MinAdvertisedStartTime
    match_id = matches$MasterEventId
    
    # Market info
    market_info = map(matches$Markets, get_market_info) |> bind_rows()
    
    # Output Tibble
    tibble(
        match = match_name,
        match_id = match_id,
        start_time = match_start_time,
        market_name = market_info$market,
        propositions = market_info$propositions,
        prices = market_info$prices
    )
}

# Map functions to data
all_bluebet_markets <-
    map(matches, get_match_info) |> bind_rows()

#===============================================================================
# Head to head markets
#===============================================================================

# Home teams
home_teams <-
    all_bluebet_markets |>
    separate(match, into = c("away_team", "home_team"), sep = " @ ", remove = FALSE) |>
    filter(str_detect(market_name, "Money Line")) |> 
    mutate(market_name = "Head To Head") |> 
    group_by(match) |> 
    filter(row_number() == 1) |> 
    rename(home_win = prices) |> 
    select(-propositions)

# Away teams
away_teams <-
    all_bluebet_markets |>
    separate(match, into = c("away_team", "home_team"), sep = " @ ", remove = FALSE) |>
    filter(str_detect(market_name, "Money Line")) |> 
    mutate(market_name = "Head To Head") |>
    group_by(match) |> 
    filter(row_number() == 2) |> 
    rename(away_win = prices) |> 
    select(-propositions)

# Combine
bluebet_head_to_head_markets <-
    home_teams |>
    left_join(away_teams) |> 
    select(match, start_time, market_name, home_team, home_win, away_team, away_win) |> 
    mutate(margin = round((1/home_win + 1/away_win), digits = 3)) |> 
    mutate(agency = "Betr")

# Fix team names
bluebet_head_to_head_markets <-
    bluebet_head_to_head_markets |> 
    mutate(match = paste(home_team, "v", away_team))

# Write to csv
write_csv(bluebet_head_to_head_markets, "Data/scraped_odds/bluebet_h2h.csv")

#===============================================================================
# Player Props
#===============================================================================

# Get API URL for each market type----------------------------------------------

# Player Points
player_points_links <-
    glue("https://web20-api.bluebet.com.au/MasterEvent?GroupTypeCode=G199&MasterEventId={unique(all_bluebet_markets$match_id)}&format=json")

# Player Points Props
player_points_props_links <-
    glue("https://web20-api.bluebet.com.au/MasterEvent?GroupTypeCode=G382&MasterEventId={unique(all_bluebet_markets$match_id)}&format=json")

# Player Assists Links
player_assists_links <-
    glue("https://web20-api.bluebet.com.au/MasterEvent?GroupTypeCode=G198&MasterEventId={unique(all_bluebet_markets$match_id)}&format=json")

# Player Assists Props Links
player_assists_props_links <-
    glue("https://web20-api.bluebet.com.au/MasterEvent?GroupTypeCode=G383&MasterEventId={unique(all_bluebet_markets$match_id)}&format=json")

# Player Rebounds Links
player_rebounds_links <-
    glue("https://web20-api.bluebet.com.au/MasterEvent?GroupTypeCode=G200&MasterEventId={unique(all_bluebet_markets$match_id)}&format=json")

# Player Rebounds Props Links
player_rebounds_props_links <-
    glue("https://web20-api.bluebet.com.au/MasterEvent?GroupTypeCode=G384&MasterEventId={unique(all_bluebet_markets$match_id)}&format=json")

# Player 3 Pointers Links
player_3_pointers_links <-
    glue("https://web20-api.bluebet.com.au/MasterEvent?GroupTypeCode=G202&MasterEventId={unique(all_bluebet_markets$match_id)}&format=json")

# Player 3 Pointers Props Links
player_3_pointers_props_links <-
    glue("https://web20-api.bluebet.com.au/MasterEvent?GroupTypeCode=G386&MasterEventId={unique(all_bluebet_markets$match_id)}&format=json")

# Player Steals Links
player_steals_links <-
    glue("https://web20-api.bluebet.com.au/MasterEvent?GroupTypeCode=G338&MasterEventId={unique(all_bluebet_markets$match_id)}&format=json")

# Player Steals Props Links
player_steals_props_links <-
    glue("https://web20-api.bluebet.com.au/MasterEvent?GroupTypeCode=G389&MasterEventId={unique(all_bluebet_markets$match_id)}&format=json")

# Player Blocks Links
player_blocks_links <-
    glue("https://web20-api.bluebet.com.au/MasterEvent?GroupTypeCode=G337&MasterEventId={unique(all_bluebet_markets$match_id)}&format=json")

# Player Blocks Props Links
player_blocks_props_links <-
    glue("https://web20-api.bluebet.com.au/MasterEvent?GroupTypeCode=G390&MasterEventId={unique(all_bluebet_markets$match_id)}&format=json")

# Player PRAs Links
player_pras_links <-
    glue("https://web20-api.bluebet.com.au/MasterEvent?GroupTypeCode=G340&MasterEventId={unique(all_bluebet_markets$match_id)}&format=json")

# Player PRAs Props Links
player_pras_props_links <-
    glue("https://web20-api.bluebet.com.au/MasterEvent?GroupTypeCode=G385&MasterEventId={unique(all_bluebet_markets$match_id)}&format=json")

# Player Props Links
player_props_links <-
    glue("https://web20-api.bluebet.com.au/MasterEvent?GroupTypeCode=G106&MasterEventId={unique(all_bluebet_markets$match_id)}&format=json")


# Function to extract prop data from links--------------------------------------

get_prop_data <- function(link) {
    
    # Get response
    response <-
        request(link) |>
        req_perform() |> 
        resp_body_json()
    
    # Empty vectors to append to
    event_name <- c()
    outcome_title <- c()
    outcome_name <- c()
    price <- c()
    
    for (event in response$Events) {
        for (outcome in event$Outcomes) {
            event_name <- c(event_name, event$EventName)
            outcome_title <- c(outcome_title, outcome$EventName)
            outcome_name <- c(outcome_name, outcome$OutcomeName)
            price <- c(price, outcome$Price)
        }
    }
    
    # Output Tibble
    tibble(
        event_name = event_name,
        outcome_title = outcome_title,
        outcome_name = outcome_name,
        price = price
    )
}

# Safe version of function
safe_get_prop_data <- safely(get_prop_data)

# Get Player Props Data (Lines)-------------------------------------------------
bluebet_player_points_props <-
map(player_points_props_links, safe_get_prop_data) |> 
    map("result") |> 
    bind_rows()

bluebet_player_assists_props <-
map(player_assists_props_links, safe_get_prop_data) |> 
    map("result") |> 
    bind_rows()

bluebet_player_rebounds_props <-
map(player_rebounds_props_links, safe_get_prop_data) |> 
    map("result") |> 
    bind_rows()

bluebet_player_3_pointers_props <-
map(player_3_pointers_props_links, safe_get_prop_data) |> 
    map("result") |> 
    bind_rows()

bluebet_player_steals_props <-
map(player_steals_props_links, safe_get_prop_data) |> 
    map("result") |> 
    bind_rows()

bluebet_player_blocks_props <-
map(player_blocks_props_links, safe_get_prop_data) |> 
    map("result") |> 
    bind_rows()

bluebet_player_pras_props <-
map(player_pras_props_links, safe_get_prop_data) |> 
    map("result") |> 
    bind_rows()

# Get player points data--------------------------------------------------------

# Alternate Points
bluebet_player_points <-
map(player_points_links, safe_get_prop_data) |> 
    map("result") |>
    bind_rows() |>
    separate(outcome_name, into = c("market_name", "player_name"), sep = " - ") |>
    mutate(line = as.numeric(str_extract(player_name, "\\d+")) - 0.5) |>
    mutate(match = str_extract(event_name, "\\(.*\\)")) |>
    mutate(match = str_remove_all(match, "\\(|\\)")) |>
    separate(match, into = c("away_team", "home_team"), sep = " @ ", remove = FALSE) |>
    mutate(match = paste(home_team, "v", away_team)) |>
    mutate(player_name = str_remove_all(player_name, "\\(.*\\)")) |>
    mutate(player_name = str_remove_all(player_name, " \\d+\\+ ")) |> 
    mutate(first_initial = str_sub(player_name, 1, 1)) |>
    mutate(surname = str_extract(player_name, "(?<=\\s).*$")) |>
    mutate(join_name = paste(first_initial, surname, sep = " ")) |>
    select(-first_initial,-surname) |>
    mutate(join_name = case_when(
        str_detect(player_name, "D. Mitchell") ~ "Do Mitchell",
        str_detect(player_name, "Bogdan Bogdanovic") ~ "Bo Bogdanovic",
        str_detect(player_name, "C-Pope") ~ "K Caldwell-Pope",
        str_detect(player_name, "M Porter") ~ "M Porter Jr.",
        str_detect(player_name, "St.* Curry$") ~ "St Curry",
        str_detect(player_name, "D. Schröder$") ~ "D Schroder",
        str_detect(player_name, "A. Sengün$") ~ "A Sengun",
        str_detect(player_name, "A-kounmpo") ~ "G Antetokounmpo",
        str_detect(player_name, "K.* Johnson$") ~ "Ke Johnson",
        str_detect(player_name, "Ke.* Murray$") ~ "Ke Murray",
        str_detect(player_name, "J.* Butler$") ~ "Ji Butler",
        str_detect(player_name, "An.* Wiggins$") ~ "An Wiggins",
        str_detect(player_name, "Tr.* Young$") ~ "Tr Young",
        str_detect(player_name, "Jr.* Holiday$") ~ "Jr Holiday",
        str_detect(player_name, "Ja.* Green$") ~ "Ja Green",
        str_detect(player_name, "Je.* Green$") ~ "Je Green",
        str_detect(player_name, "Jal.* Williams$") ~ "Ja Williams",
        str_detect(player_name, "Jal.* Wiliams$") ~ "Ja Williams",
        str_detect(player_name, "Z.* Wiliamson$") ~ "Z Williamson",
        str_detect(player_name, "C.* Cuni.*$") ~ "C Cunningham",
        str_detect(player_name, "Au.* Thompson$") ~ "Au Thompson",
        str_detect(player_name, "Ty.* Jones$") ~ "Ty Jones",
        str_detect(player_name, "Valanciunas") ~ "J Valanciunas",
        str_detect(player_name, "F.* Wagnr$") ~ "F Wagner",
        str_detect(player_name, "Haliburto") ~ "T Haliburton",
        str_detect(player_name, "P.* Wshington$") ~ "P Washington",
        str_detect(player_name, "W.* Carter.*$") ~ "W Carter Jr.",
        str_detect(player_name, "J.* Jackson.*$") ~ "J Jackson Jr.",
        str_detect(player_name, "B Brown Jr") ~ "B Brown",
        str_detect(player_name, "La.* Ball$") ~ "La Ball",
        str_detect(player_name, "Te.* Mann$") ~ "Te Mann",
        str_detect(player_name, "Mik.* Bridges$") ~ "Mi Bridges",
        str_detect(player_name, "G-Alexander") ~ "S Gilgeous-Alexander",
        .default = join_name
    )) |>
    left_join(player_names[, c("player_full_name", "team_name", "join_name")], by = c("join_name")) |>
    mutate(opposition_team = if_else(home_team == team_name, away_team, home_team)) |>
    mutate(agency = "Betr") |>
    select(
        "match",
        "home_team",
        "away_team",
        "market_name",
        "player_name" = "player_full_name",
        "player_team" = "team_name",
        "line",
        "over_price" = "price",
        "agency",
        "opposition_team"
    )

# Lines
bluebet_player_points_lines <-
  bluebet_player_points_props |> 
    filter(str_detect(outcome_title, "points over under")) |> 
    separate(outcome_name, into = c("market_name", "player_name"), sep = " - ", remove = FALSE) |>
    mutate(line = as.numeric(str_extract(player_name, "\\d+\\.\\d+"))) |>
    mutate(match = str_extract(event_name, "\\(.*\\)")) |>
    mutate(match = str_remove_all(match, "\\(|\\)")) |>
    separate(match, into = c("away_team", "home_team"), sep = " @ ", remove = FALSE) |>
    mutate(match = paste(home_team, "v", away_team)) |>
    mutate(player_name = str_remove_all(player_name, " over")) |>
    mutate(player_name = str_remove_all(player_name, " under")) |>
    mutate(player_name = str_remove_all(player_name, "\\(.*\\)")) |>
    mutate(player_name = str_remove_all(player_name, " \\d+\\.\\d+ ")) |> 
    mutate(first_initial = str_sub(player_name, 1, 1)) |>
    mutate(surname = str_extract(player_name, "(?<=\\s).*$")) |>
    mutate(join_name = paste(first_initial, surname, sep = " ")) |>
    select(-first_initial,-surname) |>
    mutate(join_name = case_when(
        str_detect(player_name, "D. Mitchell") ~ "Do Mitchell",
        str_detect(player_name, "Bogdan Bogdanovic") ~ "Bo Bogdanovic",
        str_detect(player_name, "C-Pope") ~ "K Caldwell-Pope",
        str_detect(player_name, "M Porter") ~ "M Porter Jr.",
        str_detect(player_name, "St.* Curry$") ~ "St Curry",
        str_detect(player_name, "D. Schröder$") ~ "D Schroder",
        str_detect(player_name, "A. Sengün$") ~ "A Sengun",
        str_detect(player_name, "A-kounmpo") ~ "G Antetokounmpo",
        str_detect(player_name, "K.* Johnson$") ~ "Ke Johnson",
        str_detect(player_name, "Ke.* Murray$") ~ "Ke Murray",
        str_detect(player_name, "J.* Butler$") ~ "Ji Butler",
        str_detect(player_name, "An.* Wiggins$") ~ "An Wiggins",
        str_detect(player_name, "Tr.* Young$") ~ "Tr Young",
        str_detect(player_name, "Jr.* Holiday$") ~ "Jr Holiday",
        str_detect(player_name, "Ja.* Green$") ~ "Ja Green",
        str_detect(player_name, "Je.* Green$") ~ "Je Green",
        str_detect(player_name, "Jal.* Williams$") ~ "Ja Williams",
        str_detect(player_name, "Jal.* Wiliams$") ~ "Ja Williams",
        str_detect(player_name, "Z.* Wiliamson$") ~ "Z Williamson",
        str_detect(player_name, "C.* Cuni.*$") ~ "C Cunningham",
        str_detect(player_name, "Au.* Thompson$") ~ "Au Thompson",
        str_detect(player_name, "Ty.* Jones$") ~ "Ty Jones",
        str_detect(player_name, "Valanciunas") ~ "J Valanciunas",
        str_detect(player_name, "F.* Wagnr$") ~ "F Wagner",
        str_detect(player_name, "Haliburto") ~ "T Haliburton",
        str_detect(player_name, "P.* Wshington$") ~ "P Washington",
        str_detect(player_name, "W.* Carter.*$") ~ "W Carter Jr.",
        str_detect(player_name, "J.* Jackson.*$") ~ "J Jackson Jr.",
        str_detect(player_name, "B Brown Jr") ~ "B Brown",
        str_detect(player_name, "La.* Ball$") ~ "La Ball",
        str_detect(player_name, "Te.* Mann$") ~ "Te Mann",
        str_detect(player_name, "Mik.* Bridges$") ~ "Mi Bridges",
        str_detect(player_name, "G-Alexander") ~ "S Gilgeous-Alexander",
        .default = join_name
    )) |>
    left_join(player_names[, c("player_full_name", "team_name", "join_name")], by = c("join_name")) |>
    mutate(opposition_team = if_else(home_team == team_name, away_team, home_team))

    bluebet_points_overs_lines <-
    bluebet_player_points_lines |> 
    filter(str_detect(outcome_name, "over")) |>
    mutate(agency = "Betr") |>
    select(
        "match",
        "home_team",
        "away_team",
        "market_name",
        "player_name" = "player_full_name",
        "player_team" = "team_name",
        "line",
        "over_price" = "price",
        "agency",
        "opposition_team")
    
bluebet_points_unders_lines <-
    bluebet_player_points_lines |> 
    filter(str_detect(outcome_name, "under")) |>
    mutate(agency = "Betr") |>
    select(
        "match",
        "home_team",
        "away_team",
        "market_name",
        "player_name" = "player_full_name",
        "player_team" = "team_name",
        "line",
        "under_price" = "price",
        "agency",
        "opposition_team")

bluebet_player_points_lines <-
    bluebet_points_overs_lines |> 
    left_join(bluebet_points_unders_lines)
    
# Combine
bluebet_player_points <-
    bluebet_player_points |> 
    bind_rows(bluebet_player_points_lines) |>
    mutate(home_team = fix_team_names(home_team)) |>
    mutate(away_team = fix_team_names(away_team)) |>
    mutate(match = paste(home_team, "v", away_team)) |>
    mutate(market_name = "Player Points")

# Get player assists data-------------------------------------------------------
# Alternate Assists
bluebet_player_assists_alt <-
  map(player_assists_links, safe_get_prop_data) |> 
  map("result") |>
  bind_rows() |>
  separate(outcome_name, into = c("market_name", "player_name"), sep = " - ") |>
  mutate(line = as.numeric(str_extract(player_name, "\\d+")) - 0.5) |>
  mutate(match = str_extract(event_name, "\\(.*\\)")) |>
  mutate(match = str_remove_all(match, "\\(|\\)")) |>
  separate(match, into = c("away_team", "home_team"), sep = " @ ", remove = FALSE) |>
  mutate(match = paste(home_team, "v", away_team)) |>
  mutate(player_name = str_remove_all(player_name, "\\(.*\\)")) |>
  mutate(player_name = str_remove_all(player_name, " \\d+\\+ ")) |> 
  mutate(first_initial = str_sub(player_name, 1, 1)) |>
  mutate(surname = str_extract(player_name, "(?<=\\s).*$")) |>
  mutate(join_name = paste(first_initial, surname, sep = " ")) |>
  select(-first_initial,-surname) |>
  mutate(join_name = case_when(
    str_detect(player_name, "D. Mitchell") ~ "Do Mitchell",
    str_detect(player_name, "Bogdan Bogdanovic") ~ "Bo Bogdanovic",
    str_detect(player_name, "C-Pope") ~ "K Caldwell-Pope",
    str_detect(player_name, "M Porter") ~ "M Porter Jr.",
    str_detect(player_name, "St.* Curry$") ~ "St Curry",
    str_detect(player_name, "D. Schröder$") ~ "D Schroder",
    str_detect(player_name, "A. Sengün$") ~ "A Sengun",
    str_detect(player_name, "A-kounmpo") ~ "G Antetokounmpo",
    str_detect(player_name, "K.* Johnson$") ~ "Ke Johnson",
    str_detect(player_name, "Ke.* Murray$") ~ "Ke Murray",
    str_detect(player_name, "J.* Butler$") ~ "Ji Butler",
    str_detect(player_name, "An.* Wiggins$") ~ "An Wiggins",
    str_detect(player_name, "Tr.* Young$") ~ "Tr Young",
    str_detect(player_name, "Jr.* Holiday$") ~ "Jr Holiday",
    str_detect(player_name, "Ja.* Green$") ~ "Ja Green",
    str_detect(player_name, "Je.* Green$") ~ "Je Green",
    str_detect(player_name, "Jal.* Williams$") ~ "Ja Williams",
    str_detect(player_name, "Jal.* Wiliams$") ~ "Ja Williams",
    str_detect(player_name, "Z.* Wiliamson$") ~ "Z Williamson",
    str_detect(player_name, "C.* Cuni.*$") ~ "C Cunningham",
    str_detect(player_name, "Au.* Thompson$") ~ "Au Thompson",
    str_detect(player_name, "Ty.* Jones$") ~ "Ty Jones",
    str_detect(player_name, "Valanciunas") ~ "J Valanciunas",
    str_detect(player_name, "F.* Wagnr$") ~ "F Wagner",
    str_detect(player_name, "Haliburto") ~ "T Haliburton",
    str_detect(player_name, "P.* Wshington$") ~ "P Washington",
    str_detect(player_name, "W.* Carter.*$") ~ "W Carter Jr.",
    str_detect(player_name, "J.* Jackson.*$") ~ "J Jackson Jr.",
    str_detect(player_name, "B Brown Jr") ~ "B Brown",
    str_detect(player_name, "La.* Ball$") ~ "La Ball",
    str_detect(player_name, "Te.* Mann$") ~ "Te Mann",
    str_detect(player_name, "Mik.* Bridges$") ~ "Mi Bridges",
    str_detect(player_name, "G-Alexander") ~ "S Gilgeous-Alexander",
    .default = join_name
  )) |>
  left_join(player_names[, c("player_full_name", "team_name", "join_name")], by = c("join_name")) |>
  mutate(opposition_team = if_else(home_team == team_name, away_team, home_team)) |>
  mutate(agency = "Betr") |>
  select(
    "match",
    "home_team",
    "away_team",
    "market_name",
    "player_name" = "player_full_name",
    "player_team" = "team_name",
    "line",
    "over_price" = "price",
    "agency",
    "opposition_team"
  )

# Lines for assists
bluebet_player_assists_lines <-
  bluebet_player_assists_props |> 
  filter(str_detect(outcome_title, "assists over under")) |> 
  separate(outcome_name, into = c("market_name", "player_name"), sep = " - ", remove = FALSE) |>
  mutate(line = as.numeric(str_extract(player_name, "\\d+\\.\\d+"))) |>
  mutate(match = str_extract(event_name, "\\(.*\\)")) |>
  mutate(match = str_remove_all(match, "\\(|\\)")) |>
  separate(match, into = c("away_team", "home_team"), sep = " @ ", remove = FALSE) |>
  mutate(match = paste(home_team, "v", away_team)) |>
  mutate(player_name = str_remove_all(player_name, " over")) |>
  mutate(player_name = str_remove_all(player_name, " under")) |>
  mutate(player_name = str_remove_all(player_name, "\\(.*\\)")) |>
  mutate(player_name = str_remove_all(player_name, " \\d+\\.\\d+ ")) |> 
  mutate(first_initial = str_sub(player_name, 1, 1)) |>
  mutate(surname = str_extract(player_name, "(?<=\\s).*$")) |>
  mutate(join_name = paste(first_initial, surname, sep = " ")) |>
  select(-first_initial,-surname) |>
  mutate(join_name = case_when(
    str_detect(player_name, "D. Mitchell") ~ "Do Mitchell",
    str_detect(player_name, "Bogdan Bogdanovic") ~ "Bo Bogdanovic",
    str_detect(player_name, "C-Pope") ~ "K Caldwell-Pope",
    str_detect(player_name, "M Porter") ~ "M Porter Jr.",
    str_detect(player_name, "St.* Curry$") ~ "St Curry",
    str_detect(player_name, "D. Schröder$") ~ "D Schroder",
    str_detect(player_name, "A. Sengün$") ~ "A Sengun",
    str_detect(player_name, "A-kounmpo") ~ "G Antetokounmpo",
    str_detect(player_name, "K.* Johnson$") ~ "Ke Johnson",
    str_detect(player_name, "Ke.* Murray$") ~ "Ke Murray",
    str_detect(player_name, "J.* Butler$") ~ "Ji Butler",
    str_detect(player_name, "An.* Wiggins$") ~ "An Wiggins",
    str_detect(player_name, "Tr.* Young$") ~ "Tr Young",
    str_detect(player_name, "Jr.* Holiday$") ~ "Jr Holiday",
    str_detect(player_name, "Ja.* Green$") ~ "Ja Green",
    str_detect(player_name, "Je.* Green$") ~ "Je Green",
    str_detect(player_name, "Jal.* Williams$") ~ "Ja Williams",
    str_detect(player_name, "Jal.* Wiliams$") ~ "Ja Williams",
    str_detect(player_name, "Z.* Wiliamson$") ~ "Z Williamson",
    str_detect(player_name, "C.* Cuni.*$") ~ "C Cunningham",
    str_detect(player_name, "Au.* Thompson$") ~ "Au Thompson",
    str_detect(player_name, "Ty.* Jones$") ~ "Ty Jones",
    str_detect(player_name, "Valanciunas") ~ "J Valanciunas",
    str_detect(player_name, "F.* Wagnr$") ~ "F Wagner",
    str_detect(player_name, "Haliburto") ~ "T Haliburton",
    str_detect(player_name, "P.* Wshington$") ~ "P Washington",
    str_detect(player_name, "W.* Carter.*$") ~ "W Carter Jr.",
    str_detect(player_name, "J.* Jackson.*$") ~ "J Jackson Jr.",
    str_detect(player_name, "B Brown Jr") ~ "B Brown",
    str_detect(player_name, "La.* Ball$") ~ "La Ball",
    str_detect(player_name, "Te.* Mann$") ~ "Te Mann",
    str_detect(player_name, "Mik.* Bridges$") ~ "Mi Bridges",
    str_detect(player_name, "G-Alexander") ~ "S Gilgeous-Alexander",
    .default = join_name
  )) |>
  left_join(player_names[, c("player_full_name", "team_name", "join_name")], by = c("join_name")) |>
  mutate(opposition_team = if_else(home_team == team_name, away_team, home_team))

bluebet_assists_overs_lines <-
  bluebet_player_assists_lines |> 
  filter(str_detect(outcome_name, "over")) |>
  mutate(agency = "Betr") |>
  select(
    "match",
    "home_team",
    "away_team",
    "market_name",
    "player_name" = "player_full_name",
    "player_team" = "team_name",
    "line",
    "over_price" = "price",
    "agency",
    "opposition_team")

bluebet_assists_unders_lines <-
  bluebet_player_assists_lines |> 
  filter(str_detect(outcome_name, "under")) |>
  mutate(agency = "Betr") |>
  select(
    "match",
    "home_team",
    "away_team",
    "market_name",
    "player_name" = "player_full_name",
    "player_team" = "team_name",
    "line",
    "under_price" = "price",
    "agency",
    "opposition_team")

bluebet_player_assists_lines <-
  bluebet_assists_overs_lines |> 
  left_join(bluebet_assists_unders_lines)

# Combine assists
bluebet_player_assists <-
  bluebet_player_assists_alt |> 
  bind_rows(bluebet_player_assists_lines) |>
  mutate(home_team = fix_team_names(home_team)) |>
  mutate(away_team = fix_team_names(away_team)) |>
  mutate(match = paste(home_team, "v", away_team)) |>
  mutate(market_name = "Player Assists")

# Get player rebounds data------------------------------------------------------
bluebet_player_rebounds <-
    map(player_rebounds_links, safe_get_prop_data) |> 
    map("result") |>
    bind_rows() |>
    separate(outcome_name, into = c("market_name", "player_name"), sep = " - ") |>
    mutate(line = as.numeric(str_extract(player_name, "\\d+")) - 0.5) |>
    mutate(match = str_extract(event_name, "\\(.*\\)")) |>
    mutate(match = str_remove_all(match, "\\(|\\)")) |>
    separate(match, into = c("away_team", "home_team"), sep = " @ ", remove = FALSE) |>
    mutate(match = paste(home_team, "v", away_team)) |>
    mutate(player_name = str_remove_all(player_name, "\\(.*\\)")) |>
    mutate(player_name = str_remove_all(player_name, " \\d+\\+ ")) |> 
    mutate(first_initial = str_sub(player_name, 1, 1)) |>
    mutate(surname = str_extract(player_name, "(?<=\\s).*$")) |>
    mutate(join_name = paste(first_initial, surname, sep = " ")) |>
    select(-first_initial,-surname) |>
    mutate(join_name = case_when(
        str_detect(player_name, "D. Mitchell") ~ "Do Mitchell",
        str_detect(player_name, "Bogdan Bogdanovic") ~ "Bo Bogdanovic",
        str_detect(player_name, "C-Pope") ~ "K Caldwell-Pope",
        str_detect(player_name, "M Porter") ~ "M Porter Jr.",
        str_detect(player_name, "St.* Curry$") ~ "St Curry",
        str_detect(player_name, "D. Schröder$") ~ "D Schroder",
        str_detect(player_name, "A. Sengün$") ~ "A Sengun",
        str_detect(player_name, "A-kounmpo") ~ "G Antetokounmpo",
        str_detect(player_name, "K.* Johnson$") ~ "Ke Johnson",
        str_detect(player_name, "Ke.* Murray$") ~ "Ke Murray",
        str_detect(player_name, "J.* Butler$") ~ "Ji Butler",
        str_detect(player_name, "An.* Wiggins$") ~ "An Wiggins",
        str_detect(player_name, "Tr.* Young$") ~ "Tr Young",
        str_detect(player_name, "Jr.* Holiday$") ~ "Jr Holiday",
        str_detect(player_name, "Ja.* Green$") ~ "Ja Green",
        str_detect(player_name, "Je.* Green$") ~ "Je Green",
        str_detect(player_name, "Jal.* Williams$") ~ "Ja Williams",
        str_detect(player_name, "Jal.* Wiliams$") ~ "Ja Williams",
        str_detect(player_name, "Z.* Wiliamson$") ~ "Z Williamson",
        str_detect(player_name, "C.* Cuni.*$") ~ "C Cunningham",
        str_detect(player_name, "Au.* Thompson$") ~ "Au Thompson",
        str_detect(player_name, "Ty.* Jones$") ~ "Ty Jones",
        str_detect(player_name, "Valanciunas") ~ "J Valanciunas",
        str_detect(player_name, "F.* Wagnr$") ~ "F Wagner",
        str_detect(player_name, "Haliburto") ~ "T Haliburton",
        str_detect(player_name, "P.* Wshington$") ~ "P Washington",
        str_detect(player_name, "W.* Carter.*$") ~ "W Carter Jr.",
        str_detect(player_name, "J.* Jackson.*$") ~ "J Jackson Jr.",
        str_detect(player_name, "B Brown Jr") ~ "B Brown",
        str_detect(player_name, "La.* Ball$") ~ "La Ball",
        str_detect(player_name, "Te.* Mann$") ~ "Te Mann",
        str_detect(player_name, "Mik.* Bridges$") ~ "Mi Bridges",
        str_detect(player_name, "G-Alexander") ~ "S Gilgeous-Alexander",
        .default = join_name
    )) |>
    left_join(player_names[, c("player_full_name", "team_name", "join_name")], by = c("join_name")) |>
    mutate(opposition_team = if_else(home_team == team_name, away_team, home_team)) |>
    mutate(agency = "Betr") |>
    select(
        "match",
        "home_team",
        "away_team",
        "market_name",
        "player_name" = "player_full_name",
        "player_team" = "team_name",
        "line",
        "over_price" = "price",
        "agency",
        "opposition_team"
    )

# Lines for rebounds
bluebet_player_rebounds_lines <-
  bluebet_player_rebounds_props |> 
  filter(str_detect(outcome_title, "rebounds over under")) |> 
  separate(outcome_name, into = c("market_name", "player_name"), sep = " - ", remove = FALSE) |>
  mutate(line = as.numeric(str_extract(player_name, "\\d+\\.\\d+"))) |>
  mutate(match = str_extract(event_name, "\\(.*\\)")) |>
  mutate(match = str_remove_all(match, "\\(|\\)")) |>
  separate(match, into = c("away_team", "home_team"), sep = " @ ", remove = FALSE) |>
  mutate(match = paste(home_team, "v", away_team)) |>
  mutate(player_name = str_remove_all(player_name, " over")) |>
  mutate(player_name = str_remove_all(player_name, " under")) |>
  mutate(player_name = str_remove_all(player_name, "\\(.*\\)")) |>
  mutate(player_name = str_remove_all(player_name, " \\d+\\.\\d+ ")) |> 
  mutate(first_initial = str_sub(player_name, 1, 1)) |>
  mutate(surname = str_extract(player_name, "(?<=\\s).*$")) |>
  mutate(join_name = paste(first_initial, surname, sep = " ")) |>
  select(-first_initial,-surname) |>
  mutate(join_name = case_when(
    str_detect(player_name, "D. Mitchell") ~ "Do Mitchell",
    str_detect(player_name, "Bogdan Bogdanovic") ~ "Bo Bogdanovic",
    str_detect(player_name, "C-Pope") ~ "K Caldwell-Pope",
    str_detect(player_name, "M Porter") ~ "M Porter Jr.",
    str_detect(player_name, "St.* Curry$") ~ "St Curry",
    str_detect(player_name, "D. Schröder$") ~ "D Schroder",
    str_detect(player_name, "A. Sengün$") ~ "A Sengun",
    str_detect(player_name, "A-kounmpo") ~ "G Antetokounmpo",
    str_detect(player_name, "K.* Johnson$") ~ "Ke Johnson",
    str_detect(player_name, "Ke.* Murray$") ~ "Ke Murray",
    str_detect(player_name, "J.* Butler$") ~ "Ji Butler",
    str_detect(player_name, "An.* Wiggins$") ~ "An Wiggins",
    str_detect(player_name, "Tr.* Young$") ~ "Tr Young",
    str_detect(player_name, "Jr.* Holiday$") ~ "Jr Holiday",
    str_detect(player_name, "Ja.* Green$") ~ "Ja Green",
    str_detect(player_name, "Je.* Green$") ~ "Je Green",
    str_detect(player_name, "Jal.* Williams$") ~ "Ja Williams",
    str_detect(player_name, "Jal.* Wiliams$") ~ "Ja Williams",
    str_detect(player_name, "Z.* Wiliamson$") ~ "Z Williamson",
    str_detect(player_name, "C.* Cuni.*$") ~ "C Cunningham",
    str_detect(player_name, "Au.* Thompson$") ~ "Au Thompson",
    str_detect(player_name, "Ty.* Jones$") ~ "Ty Jones",
    str_detect(player_name, "Valanciunas") ~ "J Valanciunas",
    str_detect(player_name, "F.* Wagnr$") ~ "F Wagner",
    str_detect(player_name, "Haliburto") ~ "T Haliburton",
    str_detect(player_name, "P.* Wshington$") ~ "P Washington",
    str_detect(player_name, "W.* Carter.*$") ~ "W Carter Jr.",
    str_detect(player_name, "J.* Jackson.*$") ~ "J Jackson Jr.",
    str_detect(player_name, "B Brown Jr") ~ "B Brown",
    str_detect(player_name, "La.* Ball$") ~ "La Ball",
    str_detect(player_name, "Te.* Mann$") ~ "Te Mann",
    str_detect(player_name, "Mik.* Bridges$") ~ "Mi Bridges",
    str_detect(player_name, "G-Alexander") ~ "S Gilgeous-Alexander",
    .default = join_name
  )) |>
  left_join(player_names[, c("player_full_name", "team_name", "join_name")], by = c("join_name")) |>
  mutate(opposition_team = if_else(home_team == team_name, away_team, home_team))

bluebet_rebounds_overs_lines <-
  bluebet_player_rebounds_lines |> 
  filter(str_detect(outcome_name, "over")) |>
  mutate(agency = "Betr") |>
  select(
    "match",
    "home_team",
    "away_team",
    "market_name",
    "player_name" = "player_full_name",
    "player_team" = "team_name",
    "line",
    "over_price" = "price",
    "agency",
    "opposition_team")

bluebet_rebounds_unders_lines <-
  bluebet_player_rebounds_lines |> 
  filter(str_detect(outcome_name, "under")) |>
  mutate(agency = "Betr") |>
  select(
    "match",
    "home_team",
    "away_team",
    "market_name",
    "player_name" = "player_full_name",
    "player_team" = "team_name",
    "line",
    "under_price" = "price",
    "agency",
    "opposition_team")

bluebet_player_rebounds_lines <-
  bluebet_rebounds_overs_lines |> 
  left_join(bluebet_rebounds_unders_lines)

# Combine rebounds
bluebet_player_rebounds <-
  bluebet_player_rebounds |> 
  bind_rows(bluebet_player_rebounds_lines) |>
  mutate(home_team = fix_team_names(home_team)) |>
  mutate(away_team = fix_team_names(away_team)) |>
  mutate(match = paste(home_team, "v", away_team)) |>
  mutate(market_name = "Player Rebounds")

# Get player 3-pointers data----------------------------------------------------
# Alternate 3-Pointers
bluebet_player_3_pointers_alt <-
  map(player_3_pointers_links, safe_get_prop_data) |> 
  map("result") |>
  bind_rows() |>
  separate(outcome_name, into = c("market_name", "player_name"), sep = " - ") |>
  mutate(line = as.numeric(str_extract(player_name, "\\d+")) - 0.5) |>
  mutate(match = str_extract(event_name, "\\(.*\\)")) |>
  mutate(match = str_remove_all(match, "\\(|\\)")) |>
  separate(match, into = c("away_team", "home_team"), sep = " @ ", remove = FALSE) |>
  mutate(match = paste(home_team, "v", away_team)) |>
  mutate(player_name = str_remove_all(player_name, "\\(.*\\)")) |>
  mutate(player_name = str_remove_all(player_name, " \\d+\\+ ")) |> 
  mutate(first_initial = str_sub(player_name, 1, 1)) |>
  mutate(surname = str_extract(player_name, "(?<=\\s).*$")) |>
  mutate(join_name = paste(first_initial, surname, sep = " ")) |>
  select(-first_initial,-surname) |>
  mutate(join_name = case_when(
    str_detect(player_name, "D. Mitchell") ~ "Do Mitchell",
    str_detect(player_name, "Bogdan Bogdanovic") ~ "Bo Bogdanovic",
    str_detect(player_name, "C-Pope") ~ "K Caldwell-Pope",
    str_detect(player_name, "M Porter") ~ "M Porter Jr.",
    str_detect(player_name, "St.* Curry$") ~ "St Curry",
    str_detect(player_name, "D. Schröder$") ~ "D Schroder",
    str_detect(player_name, "A. Sengün$") ~ "A Sengun",
    str_detect(player_name, "A-kounmpo") ~ "G Antetokounmpo",
    str_detect(player_name, "K.* Johnson$") ~ "Ke Johnson",
    str_detect(player_name, "Ke.* Murray$") ~ "Ke Murray",
    str_detect(player_name, "J.* Butler$") ~ "Ji Butler",
    str_detect(player_name, "An.* Wiggins$") ~ "An Wiggins",
    str_detect(player_name, "Tr.* Young$") ~ "Tr Young",
    str_detect(player_name, "Jr.* Holiday$") ~ "Jr Holiday",
    str_detect(player_name, "Ja.* Green$") ~ "Ja Green",
    str_detect(player_name, "Je.* Green$") ~ "Je Green",
    str_detect(player_name, "Jal.* Williams$") ~ "Ja Williams",
    str_detect(player_name, "Jal.* Wiliams$") ~ "Ja Williams",
    str_detect(player_name, "Z.* Wiliamson$") ~ "Z Williamson",
    str_detect(player_name, "C.* Cuni.*$") ~ "C Cunningham",
    str_detect(player_name, "Au.* Thompson$") ~ "Au Thompson",
    str_detect(player_name, "Ty.* Jones$") ~ "Ty Jones",
    str_detect(player_name, "Valanciunas") ~ "J Valanciunas",
    str_detect(player_name, "F.* Wagnr$") ~ "F Wagner",
    str_detect(player_name, "Haliburto") ~ "T Haliburton",
    str_detect(player_name, "P.* Wshington$") ~ "P Washington",
    str_detect(player_name, "W.* Carter.*$") ~ "W Carter Jr.",
    str_detect(player_name, "J.* Jackson.*$") ~ "J Jackson Jr.",
    str_detect(player_name, "B Brown Jr") ~ "B Brown",
    str_detect(player_name, "La.* Ball$") ~ "La Ball",
    str_detect(player_name, "Te.* Mann$") ~ "Te Mann",
    str_detect(player_name, "Mik.* Bridges$") ~ "Mi Bridges",
    str_detect(player_name, "G-Alexander") ~ "S Gilgeous-Alexander",
    .default = join_name
  )) |>
  left_join(player_names[, c("player_full_name", "team_name", "join_name")], by = c("join_name")) |>
  mutate(opposition_team = if_else(home_team == team_name, away_team, home_team)) |>
  mutate(agency = "Betr") |>
  select(
    "match",
    "home_team",
    "away_team",
    "market_name",
    "player_name" = "player_full_name",
    "player_team" = "team_name",
    "line",
    "over_price" = "price",
    "agency",
    "opposition_team"
  )

# Lines for 3-pointers
bluebet_player_3_pointers_lines <-
  bluebet_player_3_pointers_props |> 
  filter(str_detect(outcome_title, "three pointers over under")) |> 
  separate(outcome_name, into = c("market_name", "player_name"), sep = " - ", remove = FALSE) |>
  mutate(line = as.numeric(str_extract(player_name, "\\d+\\.\\d+"))) |>
  mutate(match = str_extract(event_name, "\\(.*\\)")) |>
  mutate(match = str_remove_all(match, "\\(|\\)")) |>
  separate(match, into = c("away_team", "home_team"), sep = " @ ", remove = FALSE) |>
  mutate(match = paste(home_team, "v", away_team)) |>
  mutate(player_name = str_remove_all(player_name, " over")) |>
  mutate(player_name = str_remove_all(player_name, " under")) |>
  mutate(player_name = str_remove_all(player_name, "\\(.*\\)")) |>
  mutate(player_name = str_remove_all(player_name, " \\d+\\.\\d+ ")) |> 
  mutate(first_initial = str_sub(player_name, 1, 1)) |>
  mutate(surname = str_extract(player_name, "(?<=\\s).*$")) |>
  mutate(join_name = paste(first_initial, surname, sep = " ")) |>
  select(-first_initial,-surname) |>
  mutate(join_name = case_when(
    str_detect(player_name, "D. Mitchell") ~ "Do Mitchell",
    str_detect(player_name, "Bogdan Bogdanovic") ~ "Bo Bogdanovic",
    str_detect(player_name, "C-Pope") ~ "K Caldwell-Pope",
    str_detect(player_name, "M Porter") ~ "M Porter Jr.",
    str_detect(player_name, "St.* Curry$") ~ "St Curry",
    str_detect(player_name, "D. Schröder$") ~ "D Schroder",
    str_detect(player_name, "A. Sengün$") ~ "A Sengun",
    str_detect(player_name, "A-kounmpo") ~ "G Antetokounmpo",
    str_detect(player_name, "K.* Johnson$") ~ "Ke Johnson",
    str_detect(player_name, "Ke.* Murray$") ~ "Ke Murray",
    str_detect(player_name, "J.* Butler$") ~ "Ji Butler",
    str_detect(player_name, "An.* Wiggins$") ~ "An Wiggins",
    str_detect(player_name, "Tr.* Young$") ~ "Tr Young",
    str_detect(player_name, "Jr.* Holiday$") ~ "Jr Holiday",
    str_detect(player_name, "Ja.* Green$") ~ "Ja Green",
    str_detect(player_name, "Je.* Green$") ~ "Je Green",
    str_detect(player_name, "Jal.* Williams$") ~ "Ja Williams",
    str_detect(player_name, "Jal.* Wiliams$") ~ "Ja Williams",
    str_detect(player_name, "Z.* Wiliamson$") ~ "Z Williamson",
    str_detect(player_name, "C.* Cuni.*$") ~ "C Cunningham",
    str_detect(player_name, "Au.* Thompson$") ~ "Au Thompson",
    str_detect(player_name, "Ty.* Jones$") ~ "Ty Jones",
    str_detect(player_name, "Valanciunas") ~ "J Valanciunas",
    str_detect(player_name, "F.* Wagnr$") ~ "F Wagner",
    str_detect(player_name, "Haliburto") ~ "T Haliburton",
    str_detect(player_name, "P.* Wshington$") ~ "P Washington",
    str_detect(player_name, "W.* Carter.*$") ~ "W Carter Jr.",
    str_detect(player_name, "J.* Jackson.*$") ~ "J Jackson Jr.",
    str_detect(player_name, "B Brown Jr") ~ "B Brown",
    str_detect(player_name, "La.* Ball$") ~ "La Ball",
    str_detect(player_name, "Te.* Mann$") ~ "Te Mann",
    str_detect(player_name, "Mik.* Bridges$") ~ "Mi Bridges",
    str_detect(player_name, "G-Alexander") ~ "S Gilgeous-Alexander",
    .default = join_name
  )) |>
  left_join(player_names[, c("player_full_name", "team_name", "join_name")], by = c("join_name")) |>
  mutate(opposition_team = if_else(home_team == team_name, away_team, home_team))

bluebet_3_pointers_overs_lines <-
  bluebet_player_3_pointers_lines |> 
  filter(str_detect(outcome_name, "over")) |>
  mutate(agency = "Betr") |>
  select(
    "match",
    "home_team",
    "away_team",
    "market_name",
    "player_name" = "player_full_name",
    "player_team" = "team_name",
    "line",
    "over_price" = "price",
    "agency",
    "opposition_team")

bluebet_3_pointers_unders_lines <-
  bluebet_player_3_pointers_lines |> 
  filter(str_detect(outcome_name, "under")) |>
  mutate(agency = "Betr") |>
  select(
    "match",
    "home_team",
    "away_team",
    "market_name",
    "player_name" = "player_full_name",
    "player_team" = "team_name",
    "line",
    "under_price" = "price",
    "agency",
    "opposition_team")

bluebet_player_3_pointers_lines <-
  bluebet_3_pointers_overs_lines |> 
  left_join(bluebet_3_pointers_unders_lines)

# Combine 3-pointers
bluebet_player_3_pointers <-
  bluebet_player_3_pointers_alt |> 
  bind_rows(bluebet_player_3_pointers_lines) |>
  mutate(home_team = fix_team_names(home_team)) |>
  mutate(away_team = fix_team_names(away_team)) |>
  mutate(match = paste(home_team, "v", away_team)) |>
  mutate(market_name = "Player Threes")

# Get player steals data--------------------------------------------------------
# Alternate Steals
bluebet_player_steals_alt <-
  map(player_steals_links, safe_get_prop_data) |> 
  map("result") |>
  bind_rows() |>
  separate(outcome_name, into = c("market_name", "player_name"), sep = " - ") |>
  mutate(line = as.numeric(str_extract(player_name, "\\d+")) - 0.5) |>
  mutate(match = str_extract(event_name, "\\(.*\\)")) |>
  mutate(match = str_remove_all(match, "\\(|\\)")) |>
  separate(match, into = c("away_team", "home_team"), sep = " @ ", remove = FALSE) |>
  mutate(match = paste(home_team, "v", away_team)) |>
  mutate(player_name = str_remove_all(player_name, "\\(.*\\)")) |>
  mutate(player_name = str_remove_all(player_name, " \\d+\\+ ")) |> 
  mutate(first_initial = str_sub(player_name, 1, 1)) |>
  mutate(surname = str_extract(player_name, "(?<=\\s).*$")) |>
  mutate(join_name = paste(first_initial, surname, sep = " ")) |>
  select(-first_initial,-surname) |>
  mutate(join_name = case_when(
    str_detect(player_name, "D. Mitchell") ~ "Do Mitchell",
    str_detect(player_name, "Bogdan Bogdanovic") ~ "Bo Bogdanovic",
    str_detect(player_name, "C-Pope") ~ "K Caldwell-Pope",
    str_detect(player_name, "M Porter") ~ "M Porter Jr.",
    str_detect(player_name, "St.* Curry$") ~ "St Curry",
    str_detect(player_name, "D. Schröder$") ~ "D Schroder",
    str_detect(player_name, "A. Sengün$") ~ "A Sengun",
    str_detect(player_name, "A-kounmpo") ~ "G Antetokounmpo",
    str_detect(player_name, "K.* Johnson$") ~ "Ke Johnson",
    str_detect(player_name, "Ke.* Murray$") ~ "Ke Murray",
    str_detect(player_name, "J.* Butler$") ~ "Ji Butler",
    str_detect(player_name, "An.* Wiggins$") ~ "An Wiggins",
    str_detect(player_name, "Tr.* Young$") ~ "Tr Young",
    str_detect(player_name, "Jr.* Holiday$") ~ "Jr Holiday",
    str_detect(player_name, "Ja.* Green$") ~ "Ja Green",
    str_detect(player_name, "Je.* Green$") ~ "Je Green",
    str_detect(player_name, "Jal.* Williams$") ~ "Ja Williams",
    str_detect(player_name, "Jal.* Wiliams$") ~ "Ja Williams",
    str_detect(player_name, "Z.* Wiliamson$") ~ "Z Williamson",
    str_detect(player_name, "C.* Cuni.*$") ~ "C Cunningham",
    str_detect(player_name, "Au.* Thompson$") ~ "Au Thompson",
    str_detect(player_name, "Ty.* Jones$") ~ "Ty Jones",
    str_detect(player_name, "Valanciunas") ~ "J Valanciunas",
    str_detect(player_name, "F.* Wagnr$") ~ "F Wagner",
    str_detect(player_name, "Haliburto") ~ "T Haliburton",
    str_detect(player_name, "P.* Wshington$") ~ "P Washington",
    str_detect(player_name, "W.* Carter.*$") ~ "W Carter Jr.",
    str_detect(player_name, "J.* Jackson.*$") ~ "J Jackson Jr.",
    str_detect(player_name, "B Brown Jr") ~ "B Brown",
    str_detect(player_name, "La.* Ball$") ~ "La Ball",
    str_detect(player_name, "Te.* Mann$") ~ "Te Mann",
    str_detect(player_name, "Mik.* Bridges$") ~ "Mi Bridges",
    str_detect(player_name, "G-Alexander") ~ "S Gilgeous-Alexander",
    .default = join_name
  )) |>
  left_join(player_names[, c("player_full_name", "team_name", "join_name")], by = c("join_name")) |>
  mutate(opposition_team = if_else(home_team == team_name, away_team, home_team)) |>
  mutate(agency = "Betr") |>
  select(
    "match",
    "home_team",
    "away_team",
    "market_name",
    "player_name" = "player_full_name",
    "player_team" = "team_name",
    "line",
    "over_price" = "price",
    "agency",
    "opposition_team"
  )

# Lines for steals
bluebet_player_steals_lines <-
  bluebet_player_steals_props |> 
  filter(str_detect(outcome_title, "steals over under")) |> 
  separate(outcome_name, into = c("market_name", "player_name"), sep = " - ", remove = FALSE) |>
  mutate(line = as.numeric(str_extract(player_name, "\\d+\\.\\d+"))) |>
  mutate(match = str_extract(event_name, "\\(.*\\)")) |>
  mutate(match = str_remove_all(match, "\\(|\\)")) |>
  separate(match, into = c("away_team", "home_team"), sep = " @ ", remove = FALSE) |>
  mutate(match = paste(home_team, "v", away_team)) |>
  mutate(player_name = str_remove_all(player_name, " over")) |>
  mutate(player_name = str_remove_all(player_name, " under")) |>
  mutate(player_name = str_remove_all(player_name, "\\(.*\\)")) |>
  mutate(player_name = str_remove_all(player_name, " \\d+\\.\\d+ ")) |> 
  mutate(first_initial = str_sub(player_name, 1, 1)) |>
  mutate(surname = str_extract(player_name, "(?<=\\s).*$")) |>
  mutate(join_name = paste(first_initial, surname, sep = " ")) |>
  select(-first_initial,-surname) |>
  mutate(join_name = case_when(
    str_detect(player_name, "D. Mitchell") ~ "Do Mitchell",
    str_detect(player_name, "Bogdan Bogdanovic") ~ "Bo Bogdanovic",
    str_detect(player_name, "C-Pope") ~ "K Caldwell-Pope",
    str_detect(player_name, "M Porter") ~ "M Porter Jr.",
    str_detect(player_name, "St.* Curry$") ~ "St Curry",
    str_detect(player_name, "D. Schröder$") ~ "D Schroder",
    str_detect(player_name, "A. Sengün$") ~ "A Sengun",
    str_detect(player_name, "A-kounmpo") ~ "G Antetokounmpo",
    str_detect(player_name, "K.* Johnson$") ~ "Ke Johnson",
    str_detect(player_name, "Ke.* Murray$") ~ "Ke Murray",
    str_detect(player_name, "J.* Butler$") ~ "Ji Butler",
    str_detect(player_name, "An.* Wiggins$") ~ "An Wiggins",
    str_detect(player_name, "Tr.* Young$") ~ "Tr Young",
    str_detect(player_name, "Jr.* Holiday$") ~ "Jr Holiday",
    str_detect(player_name, "Ja.* Green$") ~ "Ja Green",
    str_detect(player_name, "Je.* Green$") ~ "Je Green",
    str_detect(player_name, "Jal.* Williams$") ~ "Ja Williams",
    str_detect(player_name, "Jal.* Wiliams$") ~ "Ja Williams",
    str_detect(player_name, "Z.* Wiliamson$") ~ "Z Williamson",
    str_detect(player_name, "C.* Cuni.*$") ~ "C Cunningham",
    str_detect(player_name, "Au.* Thompson$") ~ "Au Thompson",
    str_detect(player_name, "Ty.* Jones$") ~ "Ty Jones",
    str_detect(player_name, "Valanciunas") ~ "J Valanciunas",
    str_detect(player_name, "F.* Wagnr$") ~ "F Wagner",
    str_detect(player_name, "Haliburto") ~ "T Haliburton",
    str_detect(player_name, "P.* Wshington$") ~ "P Washington",
    str_detect(player_name, "W.* Carter.*$") ~ "W Carter Jr.",
    str_detect(player_name, "J.* Jackson.*$") ~ "J Jackson Jr.",
    str_detect(player_name, "B Brown Jr") ~ "B Brown",
    str_detect(player_name, "La.* Ball$") ~ "La Ball",
    str_detect(player_name, "Te.* Mann$") ~ "Te Mann",
    str_detect(player_name, "Mik.* Bridges$") ~ "Mi Bridges",
    str_detect(player_name, "G-Alexander") ~ "S Gilgeous-Alexander",
    .default = join_name
  )) |>
  left_join(player_names[, c("player_full_name", "team_name", "join_name")], by = c("join_name")) |>
  mutate(opposition_team = if_else(home_team == team_name, away_team, home_team))

bluebet_steals_overs_lines <-
  bluebet_player_steals_lines |> 
  filter(str_detect(outcome_name, "over")) |>
  mutate(agency = "Betr") |>
  select(
    "match",
    "home_team",
    "away_team",
    "market_name",
    "player_name" = "player_full_name",
    "player_team" = "team_name",
    "line",
    "over_price" = "price",
    "agency",
    "opposition_team")

bluebet_steals_unders_lines <-
  bluebet_player_steals_lines |> 
  filter(str_detect(outcome_name, "under")) |>
  mutate(agency = "Betr") |>
  select(
    "match",
    "home_team",
    "away_team",
    "market_name",
    "player_name" = "player_full_name",
    "player_team" = "team_name",
    "line",
    "under_price" = "price",
    "agency",
    "opposition_team")

bluebet_player_steals_lines <-
  bluebet_steals_overs_lines |> 
  left_join(bluebet_steals_unders_lines)

# Combine steals
bluebet_player_steals <-
  bluebet_player_steals_alt |> 
  bind_rows(bluebet_player_steals_lines) |>
  mutate(home_team = fix_team_names(home_team)) |>
  mutate(away_team = fix_team_names(away_team)) |>
  mutate(match = paste(home_team, "v", away_team)) |>
  mutate(market_name = "Player Steals")

# Get player blocks data--------------------------------------------------------
# Alternate Blocks
bluebet_player_blocks_alt <-
  map(player_blocks_links, safe_get_prop_data) |> 
  map("result") |>
  bind_rows() |>
  separate(outcome_name, into = c("market_name", "player_name"), sep = " - ") |>
  mutate(line = as.numeric(str_extract(player_name, "\\d+")) - 0.5) |>
  mutate(match = str_extract(event_name, "\\(.*\\)")) |>
  mutate(match = str_remove_all(match, "\\(|\\)")) |>
  separate(match, into = c("away_team", "home_team"), sep = " @ ", remove = FALSE) |>
  mutate(match = paste(home_team, "v", away_team)) |>
  mutate(player_name = str_remove_all(player_name, "\\(.*\\)")) |>
  mutate(player_name = str_remove_all(player_name, " \\d+\\+ ")) |> 
  mutate(first_initial = str_sub(player_name, 1, 1)) |>
  mutate(surname = str_extract(player_name, "(?<=\\s).*$")) |>
  mutate(join_name = paste(first_initial, surname, sep = " ")) |>
  select(-first_initial,-surname) |>
  mutate(join_name = case_when(
    str_detect(player_name, "D. Mitchell") ~ "Do Mitchell",
    str_detect(player_name, "Bogdan Bogdanovic") ~ "Bo Bogdanovic",
    str_detect(player_name, "C-Pope") ~ "K Caldwell-Pope",
    str_detect(player_name, "M Porter") ~ "M Porter Jr.",
    str_detect(player_name, "St.* Curry$") ~ "St Curry",
    str_detect(player_name, "D. Schröder$") ~ "D Schroder",
    str_detect(player_name, "A. Sengün$") ~ "A Sengun",
    str_detect(player_name, "A-kounmpo") ~ "G Antetokounmpo",
    str_detect(player_name, "K.* Johnson$") ~ "Ke Johnson",
    str_detect(player_name, "Ke.* Murray$") ~ "Ke Murray",
    str_detect(player_name, "J.* Butler$") ~ "Ji Butler",
    str_detect(player_name, "An.* Wiggins$") ~ "An Wiggins",
    str_detect(player_name, "Tr.* Young$") ~ "Tr Young",
    str_detect(player_name, "Jr.* Holiday$") ~ "Jr Holiday",
    str_detect(player_name, "Ja.* Green$") ~ "Ja Green",
    str_detect(player_name, "Je.* Green$") ~ "Je Green",
    str_detect(player_name, "Jal.* Williams$") ~ "Ja Williams",
    str_detect(player_name, "Jal.* Wiliams$") ~ "Ja Williams",
    str_detect(player_name, "Z.* Wiliamson$") ~ "Z Williamson",
    str_detect(player_name, "C.* Cuni.*$") ~ "C Cunningham",
    str_detect(player_name, "Au.* Thompson$") ~ "Au Thompson",
    str_detect(player_name, "Ty.* Jones$") ~ "Ty Jones",
    str_detect(player_name, "Valanciunas") ~ "J Valanciunas",
    str_detect(player_name, "F.* Wagnr$") ~ "F Wagner",
    str_detect(player_name, "Haliburto") ~ "T Haliburton",
    str_detect(player_name, "P.* Wshington$") ~ "P Washington",
    str_detect(player_name, "W.* Carter.*$") ~ "W Carter Jr.",
    str_detect(player_name, "J.* Jackson.*$") ~ "J Jackson Jr.",
    str_detect(player_name, "B Brown Jr") ~ "B Brown",
    str_detect(player_name, "La.* Ball$") ~ "La Ball",
    str_detect(player_name, "Te.* Mann$") ~ "Te Mann",
    str_detect(player_name, "Mik.* Bridges$") ~ "Mi Bridges",
    str_detect(player_name, "G-Alexander") ~ "S Gilgeous-Alexander",
    .default = join_name
  )) |>
  left_join(player_names[, c("player_full_name", "team_name", "join_name")], by = c("join_name")) |>
  mutate(opposition_team = if_else(home_team == team_name, away_team, home_team)) |>
  mutate(agency = "Betr") |>
  select(
    "match",
    "home_team",
    "away_team",
    "market_name",
    "player_name" = "player_full_name",
    "player_team" = "team_name",
    "line",
    "over_price" = "price",
    "agency",
    "opposition_team"
  )

# Lines for blocks
bluebet_player_blocks_lines <-
  bluebet_player_blocks_props |> 
  filter(str_detect(outcome_title, "blocks over under")) |> 
  separate(outcome_name, into = c("market_name", "player_name"), sep = " - ", remove = FALSE) |>
  mutate(line = as.numeric(str_extract(player_name, "\\d+\\.\\d+"))) |>
  mutate(match = str_extract(event_name, "\\(.*\\)")) |>
  mutate(match = str_remove_all(match, "\\(|\\)")) |>
  separate(match, into = c("away_team", "home_team"), sep = " @ ", remove = FALSE) |>
  mutate(match = paste(home_team, "v", away_team)) |>
  mutate(player_name = str_remove_all(player_name, " over")) |>
  mutate(player_name = str_remove_all(player_name, " under")) |>
  mutate(player_name = str_remove_all(player_name, "\\(.*\\)")) |>
  mutate(player_name = str_remove_all(player_name, " \\d+\\.\\d+ ")) |> 
  mutate(first_initial = str_sub(player_name, 1, 1)) |>
  mutate(surname = str_extract(player_name, "(?<=\\s).*$")) |>
  mutate(join_name = paste(first_initial, surname, sep = " ")) |>
  select(-first_initial,-surname) |>
  mutate(join_name = case_when(
    str_detect(player_name, "D. Mitchell") ~ "Do Mitchell",
    str_detect(player_name, "Bogdan Bogdanovic") ~ "Bo Bogdanovic",
    str_detect(player_name, "C-Pope") ~ "K Caldwell-Pope",
    str_detect(player_name, "M Porter") ~ "M Porter Jr.",
    str_detect(player_name, "St.* Curry$") ~ "St Curry",
    str_detect(player_name, "D. Schröder$") ~ "D Schroder",
    str_detect(player_name, "A. Sengün$") ~ "A Sengun",
    str_detect(player_name, "A-kounmpo") ~ "G Antetokounmpo",
    str_detect(player_name, "K.* Johnson$") ~ "Ke Johnson",
    str_detect(player_name, "Ke.* Murray$") ~ "Ke Murray",
    str_detect(player_name, "J.* Butler$") ~ "Ji Butler",
    str_detect(player_name, "An.* Wiggins$") ~ "An Wiggins",
    str_detect(player_name, "Tr.* Young$") ~ "Tr Young",
    str_detect(player_name, "Jr.* Holiday$") ~ "Jr Holiday",
    str_detect(player_name, "Ja.* Green$") ~ "Ja Green",
    str_detect(player_name, "Je.* Green$") ~ "Je Green",
    str_detect(player_name, "Jal.* Williams$") ~ "Ja Williams",
    str_detect(player_name, "Jal.* Wiliams$") ~ "Ja Williams",
    str_detect(player_name, "Z.* Wiliamson$") ~ "Z Williamson",
    str_detect(player_name, "C.* Cuni.*$") ~ "C Cunningham",
    str_detect(player_name, "Au.* Thompson$") ~ "Au Thompson",
    str_detect(player_name, "Ty.* Jones$") ~ "Ty Jones",
    str_detect(player_name, "Valanciunas") ~ "J Valanciunas",
    str_detect(player_name, "F.* Wagnr$") ~ "F Wagner",
    str_detect(player_name, "Haliburto") ~ "T Haliburton",
    str_detect(player_name, "P.* Wshington$") ~ "P Washington",
    str_detect(player_name, "W.* Carter.*$") ~ "W Carter Jr.",
    str_detect(player_name, "J.* Jackson.*$") ~ "J Jackson Jr.",
    str_detect(player_name, "B Brown Jr") ~ "B Brown",
    str_detect(player_name, "La.* Ball$") ~ "La Ball",
    str_detect(player_name, "Te.* Mann$") ~ "Te Mann",
    str_detect(player_name, "Mik.* Bridges$") ~ "Mi Bridges",
    str_detect(player_name, "G-Alexander") ~ "S Gilgeous-Alexander",
    .default = join_name
  )) |>
  left_join(player_names[, c("player_full_name", "team_name", "join_name")], by = c("join_name")) |>
  mutate(opposition_team = if_else(home_team == team_name, away_team, home_team))

bluebet_blocks_overs_lines <-
  bluebet_player_blocks_lines |> 
  filter(str_detect(outcome_name, "over")) |>
  mutate(agency = "Betr") |>
  select(
    "match",
    "home_team",
    "away_team",
    "market_name",
    "player_name" = "player_full_name",
    "player_team" = "team_name",
    "line",
    "over_price" = "price",
    "agency",
    "opposition_team")

bluebet_blocks_unders_lines <-
  bluebet_player_blocks_lines |> 
  filter(str_detect(outcome_name, "under")) |>
  mutate(agency = "Betr") |>
  select(
    "match",
    "home_team",
    "away_team",
    "market_name",
    "player_name" = "player_full_name",
    "player_team" = "team_name",
    "line",
    "under_price" = "price",
    "agency",
    "opposition_team")

bluebet_player_blocks_lines <-
  bluebet_blocks_overs_lines |> 
  left_join(bluebet_blocks_unders_lines)

# Combine blocks
bluebet_player_blocks <-
  bluebet_player_blocks_alt |> 
  bind_rows(bluebet_player_blocks_lines) |>
  mutate(home_team = fix_team_names(home_team)) |>
  mutate(away_team = fix_team_names(away_team)) |>
  mutate(match = paste(home_team, "v", away_team)) |>
  mutate(market_name = "Player Blocks")
  
# Get player PRAs (Points+Rebounds+Assists) data--------------------------------
# Alternate PRAs
bluebet_player_pras_alt <-
  map(player_pras_links, safe_get_prop_data) |> 
  map("result") |>
  bind_rows() |>
  separate(outcome_name, into = c("market_name", "player_name"), sep = " - ") |>
  mutate(line = as.numeric(str_extract(player_name, "\\d+")) - 0.5) |>
  mutate(match = str_extract(event_name, "\\(.*\\)")) |>
  mutate(match = str_remove_all(match, "\\(|\\)")) |>
  separate(match, into = c("away_team", "home_team"), sep = " @ ", remove = FALSE) |>
  mutate(match = paste(home_team, "v", away_team)) |>
  mutate(player_name = str_remove_all(player_name, "\\(.*\\)")) |>
  mutate(player_name = str_remove_all(player_name, " \\d+\\+ ")) |> 
  mutate(first_initial = str_sub(player_name, 1, 1)) |>
  mutate(surname = str_extract(player_name, "(?<=\\s).*$")) |>
  mutate(join_name = paste(first_initial, surname, sep = " ")) |>
  select(-first_initial,-surname) |>
  mutate(join_name = case_when(
    str_detect(player_name, "D. Mitchell") ~ "Do Mitchell",
    str_detect(player_name, "Bogdan Bogdanovic") ~ "Bo Bogdanovic",
    str_detect(player_name, "C-Pope") ~ "K Caldwell-Pope",
    str_detect(player_name, "M Porter") ~ "M Porter Jr.",
    str_detect(player_name, "St.* Curry$") ~ "St Curry",
    str_detect(player_name, "D. Schröder$") ~ "D Schroder",
    str_detect(player_name, "A. Sengün$") ~ "A Sengun",
    str_detect(player_name, "A-kounmpo") ~ "G Antetokounmpo",
    str_detect(player_name, "K.* Johnson$") ~ "Ke Johnson",
    str_detect(player_name, "Ke.* Murray$") ~ "Ke Murray",
    str_detect(player_name, "J.* Butler$") ~ "Ji Butler",
    str_detect(player_name, "An.* Wiggins$") ~ "An Wiggins",
    str_detect(player_name, "Tr.* Young$") ~ "Tr Young",
    str_detect(player_name, "Jr.* Holiday$") ~ "Jr Holiday",
    str_detect(player_name, "Ja.* Green$") ~ "Ja Green",
    str_detect(player_name, "Je.* Green$") ~ "Je Green",
    str_detect(player_name, "Jal.* Williams$") ~ "Ja Williams",
    str_detect(player_name, "Jal.* Wiliams$") ~ "Ja Williams",
    str_detect(player_name, "Z.* Wiliamson$") ~ "Z Williamson",
    str_detect(player_name, "C.* Cuni.*$") ~ "C Cunningham",
    str_detect(player_name, "Au.* Thompson$") ~ "Au Thompson",
    str_detect(player_name, "Ty.* Jones$") ~ "Ty Jones",
    str_detect(player_name, "Valanciunas") ~ "J Valanciunas",
    str_detect(player_name, "F.* Wagnr$") ~ "F Wagner",
    str_detect(player_name, "Haliburto") ~ "T Haliburton",
    str_detect(player_name, "P.* Wshington$") ~ "P Washington",
    str_detect(player_name, "W.* Carter.*$") ~ "W Carter Jr.",
    str_detect(player_name, "J.* Jackson.*$") ~ "J Jackson Jr.",
    str_detect(player_name, "B Brown Jr") ~ "B Brown",
    str_detect(player_name, "La.* Ball$") ~ "La Ball",
    str_detect(player_name, "Te.* Mann$") ~ "Te Mann",
    str_detect(player_name, "Mik.* Bridges$") ~ "Mi Bridges",
    str_detect(player_name, "G-Alexander") ~ "S Gilgeous-Alexander",
    .default = join_name
  )) |>
  left_join(player_names[, c("player_full_name", "team_name", "join_name")], by = c("join_name")) |>
  mutate(opposition_team = if_else(home_team == team_name, away_team, home_team)) |>
  mutate(agency = "Betr") |>
  select(
    "match",
    "home_team",
    "away_team",
    "market_name",
    "player_name" = "player_full_name",
    "player_team" = "team_name",
    "line",
    "over_price" = "price",
    "agency",
    "opposition_team"
  )

# Lines for PRAs
bluebet_player_pras_lines <-
  bluebet_player_pras_props |> 
  filter(str_detect(outcome_title, "points \\+ assists \\+ rebounds over under")) |> 
  separate(outcome_name, into = c("market_name", "player_name"), sep = " - ", remove = FALSE) |>
  mutate(line = as.numeric(str_extract(player_name, "\\d+\\.\\d+"))) |>
  mutate(match = str_extract(event_name, "\\(.*\\)")) |>
  mutate(match = str_remove_all(match, "\\(|\\)")) |>
  separate(match, into = c("away_team", "home_team"), sep = " @ ", remove = FALSE) |>
  mutate(match = paste(home_team, "v", away_team)) |>
  mutate(player_name = str_remove_all(player_name, " over")) |>
  mutate(player_name = str_remove_all(player_name, " under")) |>
  mutate(player_name = str_remove_all(player_name, "\\(.*\\)")) |>
  mutate(player_name = str_remove_all(player_name, " \\d+\\.\\d+ ")) |> 
  mutate(first_initial = str_sub(player_name, 1, 1)) |>
  mutate(surname = str_extract(player_name, "(?<=\\s).*$")) |>
  mutate(join_name = paste(first_initial, surname, sep = " ")) |>
  select(-first_initial,-surname) |>
  mutate(join_name = case_when(
    str_detect(player_name, "D. Mitchell") ~ "Do Mitchell",
    str_detect(player_name, "Bogdan Bogdanovic") ~ "Bo Bogdanovic",
    str_detect(player_name, "C-Pope") ~ "K Caldwell-Pope",
    str_detect(player_name, "M Porter") ~ "M Porter Jr.",
    str_detect(player_name, "St.* Curry$") ~ "St Curry",
    str_detect(player_name, "D. Schröder$") ~ "D Schroder",
    str_detect(player_name, "A. Sengün$") ~ "A Sengun",
    str_detect(player_name, "A-kounmpo") ~ "G Antetokounmpo",
    str_detect(player_name, "K.* Johnson$") ~ "Ke Johnson",
    str_detect(player_name, "Ke.* Murray$") ~ "Ke Murray",
    str_detect(player_name, "J.* Butler$") ~ "Ji Butler",
    str_detect(player_name, "An.* Wiggins$") ~ "An Wiggins",
    str_detect(player_name, "Tr.* Young$") ~ "Tr Young",
    str_detect(player_name, "Jr.* Holiday$") ~ "Jr Holiday",
    str_detect(player_name, "Ja.* Green$") ~ "Ja Green",
    str_detect(player_name, "Je.* Green$") ~ "Je Green",
    str_detect(player_name, "Jal.* Williams$") ~ "Ja Williams",
    str_detect(player_name, "Jal.* Wiliams$") ~ "Ja Williams",
    str_detect(player_name, "Z.* Wiliamson$") ~ "Z Williamson",
    str_detect(player_name, "C.* Cuni.*$") ~ "C Cunningham",
    str_detect(player_name, "Au.* Thompson$") ~ "Au Thompson",
    str_detect(player_name, "Ty.* Jones$") ~ "Ty Jones",
    str_detect(player_name, "Valanciunas") ~ "J Valanciunas",
    str_detect(player_name, "F.* Wagnr$") ~ "F Wagner",
    str_detect(player_name, "Haliburto") ~ "T Haliburton",
    str_detect(player_name, "P.* Wshington$") ~ "P Washington",
    str_detect(player_name, "W.* Carter.*$") ~ "W Carter Jr.",
    str_detect(player_name, "J.* Jackson.*$") ~ "J Jackson Jr.",
    str_detect(player_name, "B Brown Jr") ~ "B Brown",
    str_detect(player_name, "La.* Ball$") ~ "La Ball",
    str_detect(player_name, "Te.* Mann$") ~ "Te Mann",
    str_detect(player_name, "Mik.* Bridges$") ~ "Mi Bridges",
    str_detect(player_name, "G-Alexander") ~ "S Gilgeous-Alexander",
    .default = join_name
  )) |>
  left_join(player_names[, c("player_full_name", "team_name", "join_name")], by = c("join_name")) |>
  mutate(opposition_team = if_else(home_team == team_name, away_team, home_team))

bluebet_pras_overs_lines <-
  bluebet_player_pras_lines |> 
  filter(str_detect(outcome_name, "over")) |>
  mutate(agency = "Betr") |>
  select(
    "match",
    "home_team",
    "away_team",
    "market_name",
    "player_name" = "player_full_name",
    "player_team" = "team_name",
    "line",
    "over_price" = "price",
    "agency",
    "opposition_team")

bluebet_pras_unders_lines <-
  bluebet_player_pras_lines |> 
  filter(str_detect(outcome_name, "under")) |>
  mutate(agency = "Betr") |>
  select(
    "match",
    "home_team",
    "away_team",
    "market_name",
    "player_name" = "player_full_name",
    "player_team" = "team_name",
    "line",
    "under_price" = "price",
    "agency",
    "opposition_team")

bluebet_player_pras_lines <-
  bluebet_pras_overs_lines |> 
  left_join(bluebet_pras_unders_lines)

# Combine PRAs
bluebet_player_pras <-
  bluebet_player_pras_alt |> 
  bind_rows(bluebet_player_pras_lines) |>
  mutate(home_team = fix_team_names(home_team)) |>
  mutate(away_team = fix_team_names(away_team)) |>
  mutate(match = paste(home_team, "v", away_team)) |>
  mutate(market_name = "Player PRAs")

#===============================================================================
# Write to CSV
#===============================================================================

bluebet_player_points |> write_csv("Data/scraped_odds/bluebet_player_points.csv")
bluebet_player_assists |> write_csv("Data/scraped_odds/bluebet_player_assists.csv")
bluebet_player_rebounds |> write_csv("Data/scraped_odds/bluebet_player_rebounds.csv")
bluebet_player_3_pointers |> write_csv("Data/scraped_odds/bluebet_player_threes.csv")
bluebet_player_steals |> write_csv("Data/scraped_odds/bluebet_player_steals.csv")
bluebet_player_blocks |> write_csv("Data/scraped_odds/bluebet_player_blocks.csv")
bluebet_player_pras |> write_csv("Data/scraped_odds/bluebet_player_pras.csv")
