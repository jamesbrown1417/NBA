# Libraries
library(tidyverse)
library(rvest)
library(httr2)

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

# URL to get responses
neds_url = "https://api.neds.com.au/v2/sport/event-request?category_ids=%5B%223c34d075-dc14-436d-bfc4-9272a49c2b39%22%5D&include_any_team_vs_any_team_events=true"

# Make request and get response
neds_response <-
    request(neds_url) |>
    req_perform() |> 
    resp_body_json()

# Initialize empty lists to store data
event_name <- character()
event_id <- character()
competition_name <- character()

# Extract event IDs and names from JSON response
for (value in neds_response$events) {
    event_name <- c(event_name, value$name)
    event_id <- c(event_id, value$id)
    competition_name <- c(competition_name, value$competition$name)
}

# Create a data frame from the vectors
df <- data.frame(event_name, event_id, competition_name)

# Filter the data frame to only include matches with ' vs ' in the event name
df <- df |> filter(str_detect(event_name, ' vs '))

# Only get NBL Games
df <- df |> filter(str_detect(competition_name, 'NBA'))

#===============================================================================
# Get event card data for each match
#===============================================================================

# Base URL for event card
event_url <- "https://api.neds.com.au/v2/sport/event-card?id="

# List of event URLs
event_json <- paste0(event_url, df$event_id)

# Initialize an empty list to store event JSON data
event_json_list <- list()

# Loop through each event URL and get the event card JSON data
for (url in event_json) {
    tryCatch({
        response2 <- request(url) |>
            req_perform() |>
            resp_body_json()
        event_json_list <- append(event_json_list, list(response2))
    }, error = function(e) {
        cat("Error:", url, "\n")
    })
}

#===============================================================================
# Get the market information for each match
#===============================================================================

# Initialize empty vectors to store the market names and IDs for mapping
market_lookup_name <- character()
market_lookup_id <- character()

# Initialize empty vectors to store data
entrants <- character()
market_id <- character()
match_names <- character()
handicaps <- numeric()
prices <- numeric()

# Loop through the entrants
for (i in seq_along(event_json_list)) {
    match_name <- df$event_name[i]
    match <- event_json_list[[i]]
    
    for (entrant in match$entrants) {
        entrants <- c(entrants, entrant$name)
        market_id <- c(market_id, entrant$market_id)
        match_names <- c(match_names, match_name)
    }
    
    # Loop through the markets
    for (market in match$markets) {
        market_lookup_name <- c(market_lookup_name, market$name)
        market_lookup_id <- c(market_lookup_id, market$id)
        
        if (is.null(market$handicap)) {
            handicaps <- c(handicaps, NA)
        } else {
            handicaps <- c(handicaps, market$handicap)
        }
    }
    
    # Loop through the prices
    for (price in match$prices) {
        fractional_odds <- price$odds$numerator / price$odds$denominator
        decimal_odds <- fractional_odds + 1
        prices <- c(prices, decimal_odds)
    }
}

# Create market lookup dataframe
market_lookup_df <- data.frame(market_id = market_lookup_id, market_name = market_lookup_name, handicaps = handicaps)

# Create market dataframe
market_df <- data.frame(match_name = match_names, market_id = market_id, entrants = entrants, price = prices)

# Merge market lookup dataframe with market dataframe
market_df <- merge(market_df, market_lookup_df, by = 'market_id', all.x = TRUE)

# Reorder columns in market_df
market_df <- market_df |> select(match_name, market_name, entrants, handicaps, price)

##%######################################################%##
#                                                          #
####               Get Head to Head Data                ####
#                                                          #
##%######################################################%##

# Filter to only include head to head markets
h2h_data <-
market_df |> 
    filter(market_name == "Head To Head") |> 
    select(-market_name)

# Home teams
home_teams <-
    h2h_data |> 
    separate(match_name, c("home_team", "away_team"), sep = " vs ", remove = FALSE) |> 
    filter(entrants == home_team) |> 
    select(match = match_name, home_team, home_win = price)

# Away teams
away_teams <-
    h2h_data |> 
    separate(match_name, c("home_team", "away_team"), sep = " vs ", remove = FALSE) |> 
    filter(entrants == away_team) |> 
    select(match = match_name, away_team, away_win = price)

# Merge home and away teams
h2h_data <-
    home_teams |> 
    left_join(away_teams, by = c("match")) |> 
    mutate(margin = round(1 / home_win + 1 / away_win, digits = 2)) |>
    mutate(agency = "Neds") |>
    select(match, home_team, away_team, home_win, away_win, margin, agency) |> 
  mutate(match = str_replace(match, " vs ", " v "))

##%######################################################%##
#                                                          #
####                 Player Points Data                 ####
#                                                          #
##%######################################################%##

# Filter to only include player points markets
player_points_data <-
market_df |> 
    filter(str_detect(market_name, "(Player Points O/U)|(To Score)"))

# Overs
points_overs <-
    player_points_data |>
    filter(str_detect(entrants, "Over") |
               str_detect(market_name, "To Score")) |>
    mutate(handicap_1 = as.numeric(str_extract(market_name, "\\d+")) - 0.5) |>
    mutate(handicap = coalesce(handicaps, handicap_1)) |>
    mutate(player_name_1 = str_extract(entrants, pattern <-
                                           ".*(?= \\()")) |>
    mutate(player_name_2 = str_extract(market_name, "(?<= \\- ).*")) |> 
    mutate(player_name = coalesce(player_name_1, player_name_2)) |>
    transmute(
        match = match_name,
        market_name = "Player Points",
        player_name,
        line = handicap,
        over_price = price,
        agency = "Neds"
    )

# Unders
points_unders <-
    player_points_data |>
    filter(str_detect(entrants, "Under")) |>
    mutate(handicap_1 = as.numeric(str_extract(market_name, "\\d+")) - 0.5) |>
    mutate(handicap = coalesce(handicaps, handicap_1)) |>
    mutate(player_name_1 = str_extract(entrants, pattern <-
                                           ".*(?= \\()")) |>
    mutate(player_name_2 = str_extract(market_name, "(?<= \\- ).*")) |>
    mutate(player_name = coalesce(player_name_1, player_name_2)) |>
    transmute(
        match = match_name,
        market_name = "Player Points",
        player_name,
        line = handicap,
        under_price = price,
        agency = "Neds"
    )

# Merge overs and unders
player_points_data <-
    points_overs |> 
    full_join(points_unders, by = c("match", "player_name", "line", "agency", "market_name")) |> 
    select(match, market_name, player_name, line, over_price, under_price, agency) |> 
    mutate(player_name = case_when(player_name == "PJ Washington" ~ "P.J. Washington",
                                   player_name == "Kelly Oubre" ~ "Kelly Oubre Jr.",
                                   player_name == "Derrick Jones" ~ "Derrick Jones Jr.",
                                   player_name == "Jabari Smith Jr" ~ "Jabari Smith Jr.",
                                   .default = player_name)) |> 
    left_join(player_names[, c("player_full_name", "team_name")], by = c("player_name" = "player_full_name")) |> 
    rename(player_team = team_name) |>
    mutate(match = str_replace(match, " vs ", " v ")) |>
    separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |> 
    mutate(opposition_team = case_when(player_team == home_team ~ away_team,
                                       player_team == away_team ~ home_team)) |>
    relocate(player_team, opposition_team, .after = player_name)

##%######################################################%##
#                                                          #
####                Player Assists Data                 ####
#                                                          #
##%######################################################%##

# Filter to only include player assists markets
player_assists_data <-
    market_df |> 
    filter(str_detect(market_name, "(Player Assists O/U)|(^To Have .* Assists$)"))

# Overs
assists_overs <-
    player_assists_data |>
    filter(str_detect(entrants, "Over") |
               str_detect(market_name, "To Have")) |>
    mutate(handicap_1 = as.numeric(str_extract(market_name, "\\d+")) - 0.5) |>
    mutate(handicap = coalesce(handicaps, handicap_1)) |>
    mutate(player_name_1 = str_extract(entrants, pattern <-
                                           ".*(?= \\()")) |>
    mutate(player_name_2 = str_extract(market_name, "(?<= \\- ).*")) |>
    mutate(player_name = coalesce(player_name_1, player_name_2)) |>
    transmute(
        match = match_name,
        market_name = "Player Assists",
        player_name,
        line = handicap,
        over_price = price,
        agency = "Neds"
    )

# Unders
assists_unders <-
    player_assists_data |>
    filter(str_detect(entrants, "Under")) |>
    mutate(handicap_1 = as.numeric(str_extract(market_name, "\\d+")) - 0.5) |>
    mutate(handicap = coalesce(handicaps, handicap_1)) |>
    mutate(player_name_1 = str_extract(entrants, pattern <-
                                           ".*(?= \\()")) |>
    mutate(player_name_2 = str_extract(market_name, "(?<= \\- ).*")) |>
    mutate(player_name = coalesce(player_name_1, player_name_2)) |>
    transmute(
        match = match_name,
        market_name = "Player Assists",
        player_name,
        line = handicap,
        under_price = price,
        agency = "Neds"
    )

# Merge overs and unders
player_assists_data <-
    assists_overs |> 
    full_join(assists_unders, by = c("match", "player_name", "line", "agency", "market_name")) |> 
    select(match, market_name, player_name, line, over_price, under_price, agency) |> 
  mutate(player_name = case_when(player_name == "PJ Washington" ~ "P.J. Washington",
                                 player_name == "Kelly Oubre" ~ "Kelly Oubre Jr.",
                                 player_name == "Derrick Jones" ~ "Derrick Jones Jr.",
                                 player_name == "Jabari Smith Jr" ~ "Jabari Smith Jr.",
                                 .default = player_name)) |> 
  left_join(player_names[, c("player_full_name", "team_name")], by = c("player_name" = "player_full_name")) |> 
  rename(player_team = team_name) |>
  mutate(match = str_replace(match, " vs ", " v ")) |>
  separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |> 
  mutate(opposition_team = case_when(player_team == home_team ~ away_team,
                                     player_team == away_team ~ home_team)) |>
  relocate(player_team, opposition_team, .after = player_name)

##%######################################################%##
#                                                          #
####                Player Rebound Data                 ####
#                                                          #
##%######################################################%##

# Filter to only include player rebounds markets
player_rebounds_data <-
    market_df |> 
  filter(str_detect(market_name, "(Player Rebounds O/U)|(^To Have .* Rebounds$)"))

# Overs
rebounds_overs <-
    player_rebounds_data |>
    filter(str_detect(entrants, "Over") |
               str_detect(market_name, "To Have")) |>
    mutate(handicap_1 = as.numeric(str_extract(market_name, "\\d+")) - 0.5) |>
    mutate(handicap = coalesce(handicaps, handicap_1)) |>
    mutate(player_name_1 = str_extract(entrants, pattern <-
                                           ".*(?= \\()")) |>
    mutate(player_name_2 = str_extract(market_name, "(?<= \\- ).*")) |>
    mutate(player_name = coalesce(player_name_1, player_name_2)) |>
    transmute(
        match = match_name,
        market_name = "Player Rebounds",
        player_name,
        line = handicap,
        over_price = price,
        agency = "Neds"
    )

# Unders
rebounds_unders <-
    player_rebounds_data |>
    filter(str_detect(entrants, "Under")) |>
    mutate(handicap_1 = as.numeric(str_extract(market_name, "\\d+")) - 0.5) |>
    mutate(handicap = coalesce(handicaps, handicap_1)) |>
    mutate(player_name_1 = str_extract(entrants, pattern <-
                                           ".*(?= \\()")) |>
    mutate(player_name_2 = str_extract(market_name, "(?<= \\- ).*")) |>
    mutate(player_name = coalesce(player_name_1, player_name_2)) |>
    transmute(
        match = match_name,
        market_name = "Player Rebounds",
        player_name,
        line = handicap,
        under_price = price,
        agency = "Neds"
    )

# Merge overs and unders
player_rebounds_data <-
    rebounds_overs |> 
    full_join(rebounds_unders, by = c("match", "player_name", "line", "agency", "market_name")) |> 
    select(match, market_name, player_name, line, over_price, under_price, agency) |> 
  mutate(player_name = case_when(player_name == "PJ Washington" ~ "P.J. Washington",
                                 player_name == "Kelly Oubre" ~ "Kelly Oubre Jr.",
                                 player_name == "Derrick Jones" ~ "Derrick Jones Jr.",
                                 player_name == "Jabari Smith Jr" ~ "Jabari Smith Jr.",
                                 .default = player_name)) |> 
  left_join(player_names[, c("player_full_name", "team_name")], by = c("player_name" = "player_full_name")) |> 
  rename(player_team = team_name) |>
  mutate(match = str_replace(match, " vs ", " v ")) |>
  separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |> 
  mutate(opposition_team = case_when(player_team == home_team ~ away_team,
                                     player_team == away_team ~ home_team)) |>
  relocate(player_team, opposition_team, .after = player_name)

##%######################################################%##
#                                                          #
####                Player Threes Data                  ####
#                                                          #
##%######################################################%##

# Filter to only include player pras markets
player_pras_data <-
  market_df |> 
  filter(str_detect(market_name, "Threes"))

# Overs
pras_overs <-
  player_pras_data |>
  filter(str_detect(entrants, "Over") |
           str_detect(market_name, "Threes")) |>
  mutate(handicap_1 = as.numeric(str_extract(market_name, "\\d+")) - 0.5) |>
  mutate(handicap = coalesce(handicaps, handicap_1)) |>
  mutate(player_name_1 = str_extract(entrants, pattern <-
                                       ".*(?= \\()")) |>
  mutate(player_name_2 = str_extract(market_name, "(?<= \\- ).*")) |>
  mutate(player_name = coalesce(player_name_1, player_name_2)) |>
  transmute(
    match = match_name,
    market_name = "Player Threess",
    player_name,
    line = handicap,
    over_price = price,
    agency = "Neds"
  )

# Unders
pras_unders <-
  player_pras_data |>
  filter(str_detect(entrants, "Under")) |>
  mutate(handicap_1 = as.numeric(str_extract(market_name, "\\d+")) - 0.5) |>
  mutate(handicap = coalesce(handicaps, handicap_1)) |>
  mutate(player_name_1 = str_extract(entrants, pattern <-
                                       ".*(?= \\()")) |>
  mutate(player_name_2 = str_extract(market_name, "(?<= \\- ).*")) |>
  mutate(player_name = coalesce(player_name_1, player_name_2)) |>
  transmute(
    match = match_name,
    market_name = "Player Threess",
    player_name,
    line = handicap,
    under_price = price,
    agency = "Neds"
  )

# Merge overs and unders
player_pras_data <-
  pras_overs |> 
  full_join(pras_unders, by = c("match", "player_name", "line", "agency", "market_name")) |> 
  select(match, market_name, player_name, line, over_price, under_price, agency) |> 
  mutate(player_name = case_when(player_name == "PJ Washington" ~ "P.J. Washington",
                                 player_name == "Kelly Oubre" ~ "Kelly Oubre Jr.",
                                 player_name == "Derrick Jones" ~ "Derrick Jones Jr.",
                                 player_name == "Jabari Smith Jr" ~ "Jabari Smith Jr.",
                                 .default = player_name)) |> 
  left_join(player_names[, c("player_full_name", "team_name")], by = c("player_name" = "player_full_name")) |> 
  rename(player_team = team_name) |>
  mutate(match = str_replace(match, " vs ", " v ")) |>
  separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |> 
  mutate(opposition_team = case_when(player_team == home_team ~ away_team,
                                     player_team == away_team ~ home_team)) |>
  relocate(player_team, opposition_team, .after = player_name)

##%######################################################%##
#                                                          #
####                 Player Threes Data                 ####
#                                                          #
##%######################################################%##

# Filter to only include player threes markets
player_threes_data <-
  market_df |> 
  filter(str_detect(market_name, "Three Point FG"))

# Overs
threes_overs <-
  player_threes_data |>
  filter(str_detect(entrants, "Over") |
           str_detect(market_name, "Three")) |>
  mutate(handicap_1 = as.numeric(str_extract(market_name, "\\d+")) - 0.5) |>
  mutate(handicap = coalesce(handicaps, handicap_1)) |>
  mutate(player_name_1 = str_extract(entrants, pattern <-
                                       ".*(?= \\()")) |>
  mutate(player_name_2 = str_extract(market_name, "(?<= \\- ).*")) |>
  mutate(player_name = coalesce(player_name_1, player_name_2)) |>
  transmute(
    match = match_name,
    market_name = "Player Threes",
    player_name,
    line = handicap,
    over_price = price,
    agency = "Neds"
  )

# Unders
threes_unders <-
  player_threes_data |>
  filter(str_detect(entrants, "Under")) |>
  mutate(handicap_1 = as.numeric(str_extract(market_name, "\\d+")) - 0.5) |>
  mutate(handicap = coalesce(handicaps, handicap_1)) |>
  mutate(player_name_1 = str_extract(entrants, pattern <-
                                       ".*(?= \\()")) |>
  mutate(player_name_2 = str_extract(market_name, "(?<= \\- ).*")) |>
  mutate(player_name = coalesce(player_name_1, player_name_2)) |>
  transmute(
    match = match_name,
    market_name = "Player Threes",
    player_name,
    line = handicap,
    under_price = price,
    agency = "Neds"
  )

# Merge overs and unders
player_threes_data <-
  threes_overs |> 
  full_join(threes_unders, by = c("match", "player_name", "line", "agency", "market_name")) |> 
  select(match, market_name, player_name, line, over_price, under_price, agency) |> 
  mutate(player_name = case_when(player_name == "PJ Washington" ~ "P.J. Washington",
                                 player_name == "Kelly Oubre" ~ "Kelly Oubre Jr.",
                                 player_name == "Derrick Jones" ~ "Derrick Jones Jr.",
                                 player_name == "Jabari Smith Jr" ~ "Jabari Smith Jr.",
                                 .default = player_name)) |> 
  left_join(player_names[, c("player_full_name", "team_name")], by = c("player_name" = "player_full_name")) |> 
  rename(player_team = team_name) |>
  mutate(match = str_replace(match, " vs ", " v ")) |>
  separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |> 
  mutate(opposition_team = case_when(player_team == home_team ~ away_team,
                                     player_team == away_team ~ home_team)) |>
  relocate(player_team, opposition_team, .after = player_name)

##%######################################################%##
#                                                          #
####                 Player Steals Data                 ####
#                                                          #
##%######################################################%##

##%######################################################%##
#                                                          #
####                 Player Blocks Data                 ####
#                                                          #
##%######################################################%##
  

##%######################################################%##
#                                                          #
####                  Write out as CSV                  ####
#                                                          #
##%######################################################%##

h2h_data |> write_csv("Data/scraped_odds/neds_h2h.csv")
player_points_data |> write_csv("Data/scraped_odds/neds_player_points.csv")
player_assists_data |> write_csv("Data/scraped_odds/neds_player_assists.csv")
player_rebounds_data |> write_csv("Data/scraped_odds/tab_player_rebounds.csv")
player_pras_data |> write_csv("Data/scraped_odds/neds_player_pras.csv")
player_threes_data |> write_csv("Data/scraped_odds/neds_player_threes.csv")
