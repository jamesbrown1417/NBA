# Libraries and functions
library(tidyverse)
library(jsonlite)
library(httr)

# Function to get bookmaker edge percentage
get_edge <- function(x) round(100 * (x - 1), 3)

# Team / Player Information-----------------------------------------------------

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

# Wrap everything in try catch:
tryCatch(
  {
    # Get URL to make requests from
    game_id_url = "https://www.unibet.com.au/sportsbook-feeds/views/filter/basketball/nba/all/matches?includeParticipants=true&useCombined=true&ncid=1699768968"
    
    # Make request
    game_id_response = GET(game_id_url)
    
    # Convert response to JSON
    game_id_json = fromJSON(content(game_id_response, "text"))
    
    # Get event information and IDs
    event_info <-
      game_id_json$layout$sections$widgets[[2]]$matches$events[[1]]$event |> 
      tibble()
    
    # Get event IDs and names
    event_info <-
      event_info |>
      select(event_id = id, match = name) |>
      mutate(match = str_replace(match, " - ", " v "))
    
    # Add all markets url
    event_info <-
      event_info |>
      mutate(all_markets_url =
               paste0("https://oc-offering-api.kambicdn.com/offering/v2018/ubau/betoffer/event/" ,
                      event_id,
                      ".json?lang=en_AU&market=AU&ncid=1691104490"))
    
    # Create a function that takes the url and returns a json of the data
    get_unibet <- function(match, url) {
      response = GET(url)
      json = fromJSON(content(response, "text"))
      output <- tibble(json)
      output <- output$json[[1]] |> tibble()
      output <- unnest(output, cols = c("criterion") , names_repair = "universal")
      output <- unnest(output, cols = c("outcomes") , names_repair = "universal")
      output <- unnest(output, cols = c("tags") , names_repair = "universal")
      output$match <- match
      
      return(output)
    }
    
    #===================================================================================================
    # Get all market data
    #===================================================================================================
    
    all_markets <-
      event_info |>
      select(match, url = all_markets_url) |>
      pmap(get_unibet) |>
      bind_rows()
    
    #===================================================================================================
    # Head to Head
    #===================================================================================================
    
    # All head to head
    all_head_to_head <-
      all_markets |> 
      filter(label...4 == "Head To Head - Including Overtime") |> 
      select(match, date = closed, participant, oddsAmerican) |>
      mutate(oddsAmerican = as.numeric(oddsAmerican)) |>
      mutate(price = ifelse(oddsAmerican < 0, (100 / abs(oddsAmerican)) + 1, (oddsAmerican / 100) + 1)) |>
      distinct(match, date, participant, price) |> 
      separate(match, c("home_team", "away_team"), sep = " v ", remove = FALSE) |> 
      mutate(date = as_date(ymd_hms(date)))
    
    # All home teams
    home_head_to_head <-
      all_head_to_head |>
      filter(home_team == participant) |>
      select(match, date, home_team = participant, home_win = price)
    
    # All away teams
    away_head_to_head <-
      all_head_to_head |>
      filter(away_team == participant) |>
      select(match, date, away_team = participant, away_win = price)
    
    # Combine
    head_to_head_combined <-
      home_head_to_head |> 
      full_join(away_head_to_head)
    
    #===================================================================================================
    # Points
    #===================================================================================================
    
    # All Player Points
    all_player_points <-
      all_markets |> 
      filter(label...4 == "Points scored by the player - Including Overtime") |> 
      select(match, date = closed, participant, oddsAmerican, line, type) |>
      mutate(line = line / 1000) |> 
      mutate(oddsAmerican = as.numeric(oddsAmerican)) |>
      mutate(price = ifelse(oddsAmerican < 0, (100 / abs(oddsAmerican)) + 1, (oddsAmerican / 100) + 1)) |>
      distinct(match, date, participant, price, line, type) |> 
      separate(match, c("home_team", "away_team"), sep = " v ", remove = FALSE) |> 
      mutate(date = as_date(ymd_hms(date))) |> 
      separate(participant, c("last_name", "first_name"), sep = ", ", remove = TRUE) |> 
      mutate(player_name = paste(first_name, last_name, sep = " ")) |>
      select(-first_name, -last_name)
    
    # Overs
    player_points_overs <- 
      all_player_points |>
      filter(type == "OT_OVER") |>
      select(match, date, line, player_name, over_price = price)
    
    # Unders
    player_points_unders <- 
      all_player_points |>
      filter(type == "OT_UNDER") |>
      select(match, date, line, player_name, under_price = price)
    
    # Combine
    player_points_combined <-
      player_points_overs |>
      full_join(player_points_unders) |>
      mutate(agency = "Unibet") |>
      arrange(match, player_name, line) |>
      mutate(over_price = round(over_price, 2),
             under_price = round(under_price, 2)) |> 
      left_join(player_names[,c("player_full_name", "team_name")], by = c("player_name" = "player_full_name")) |>
      rename(player_team = team_name) |> 
      separate(match, c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
      mutate(opposition_team = ifelse(player_team == home_team, away_team, home_team)) |> 
      relocate(player_team, opposition_team, .after = player_name)
    
    #===================================================================================================
    # Rebounds
    #===================================================================================================
    
    # All Player Rebounds
    all_player_rebounds <-
      all_markets |> 
      filter(label...4 == "Rebounds by the player - Including Overtime") |> 
      select(match, date = closed, participant, oddsAmerican, line, type) |>
      mutate(line = line / 1000) |> 
      mutate(oddsAmerican = as.numeric(oddsAmerican)) |>
      mutate(price = ifelse(oddsAmerican < 0, (100 / abs(oddsAmerican)) + 1, (oddsAmerican / 100) + 1)) |>
      distinct(match, date, participant, price, line, type) |> 
      separate(match, c("home_team", "away_team"), sep = " v ", remove = FALSE) |> 
      mutate(date = as_date(ymd_hms(date))) |> 
      separate(participant, c("last_name", "first_name"), sep = ", ", remove = TRUE) |> 
      mutate(player_name = paste(first_name, last_name, sep = " ")) |>
      select(-first_name, -last_name)
    
    # Overs
    player_rebounds_overs <- 
      all_player_rebounds |>
      filter(type == "OT_OVER") |>
      select(match, date, line, player_name, over_price = price)
    
    # Unders
    player_rebounds_unders <- 
      all_player_rebounds |>
      filter(type == "OT_UNDER") |>
      select(match, date, line, player_name, under_price = price)
    
    # Combine
    player_rebounds_combined <-
      player_rebounds_overs |>
      full_join(player_rebounds_unders) |>
      mutate(agency = "Unibet") |>
      arrange(match, player_name, line) |>
      mutate(over_price = round(over_price, 2),
             under_price = round(under_price, 2)) |> 
      left_join(player_names[,c("player_full_name", "team_name")], by = c("player_name" = "player_full_name")) |>
      rename(player_team = team_name) |> 
      separate(match, c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
      mutate(opposition_team = ifelse(player_team == home_team, away_team, home_team)) |> 
      relocate(player_team, opposition_team, .after = player_name)
    
    #===================================================================================================
    # Assists
    #===================================================================================================
    
    # All Player Assists
    all_player_assists <-
      all_markets |> 
      filter(label...4 == "Assists by the player - Including Overtime") |> 
      select(match, date = closed, participant, oddsAmerican, line, type) |>
      mutate(line = line / 1000) |> 
      mutate(oddsAmerican = as.numeric(oddsAmerican)) |>
      mutate(price = ifelse(oddsAmerican < 0, (100 / abs(oddsAmerican)) + 1, (oddsAmerican / 100) + 1)) |>
      distinct(match, date, participant, price, line, type) |> 
      separate(match, c("home_team", "away_team"), sep = " v ", remove = FALSE) |> 
      mutate(date = as_date(ymd_hms(date))) |> 
      separate(participant, c("last_name", "first_name"), sep = ", ", remove = TRUE) |> 
      mutate(player_name = paste(first_name, last_name, sep = " ")) |>
      select(-first_name, -last_name)
    
    # Overs
    player_assists_overs <- 
      all_player_assists |>
      filter(type == "OT_OVER") |>
      select(match, date, line, player_name, over_price = price)
    
    # Unders
    player_assists_unders <- 
      all_player_assists |>
      filter(type == "OT_UNDER") |>
      select(match, date, line, player_name, under_price = price)
    
    # Combine
    player_assists_combined <-
      player_assists_overs |>
      full_join(player_assists_unders) |>
      mutate(agency = "Unibet") |>
      arrange(match, player_name, line) |>
      mutate(over_price = round(over_price, 2),
             under_price = round(under_price, 2)) |> 
      left_join(player_names[,c("player_full_name", "team_name")], by = c("player_name" = "player_full_name")) |>
      rename(player_team = team_name) |> 
      separate(match, c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
      mutate(opposition_team = ifelse(player_team == home_team, away_team, home_team)) |> 
      relocate(player_team, opposition_team, .after = player_name)
    
    #===================================================================================================
    # Threes
    #===================================================================================================
    
    # All Player Threes
    all_player_threes <-
      all_markets |> 
      filter(label...4 == "3-point field goals made by the player - Including Overtime") |> 
      select(match, date = closed, participant, oddsAmerican, line, type) |>
      mutate(line = line / 1000) |> 
      mutate(oddsAmerican = as.numeric(oddsAmerican)) |>
      mutate(price = ifelse(oddsAmerican < 0, (100 / abs(oddsAmerican)) + 1, (oddsAmerican / 100) + 1)) |>
      distinct(match, date, participant, price, line, type) |> 
      separate(match, c("home_team", "away_team"), sep = " v ", remove = FALSE) |> 
      mutate(date = as_date(ymd_hms(date))) |> 
      separate(participant, c("last_name", "first_name"), sep = ", ", remove = TRUE) |> 
      mutate(player_name = paste(first_name, last_name, sep = " ")) |>
      select(-first_name, -last_name)
    
    # Overs
    player_threes_overs <- 
      all_player_threes |>
      filter(type == "OT_OVER") |>
      select(match, date, line, player_name, over_price = price)
    
    # Unders
    player_threes_unders <- 
      all_player_threes |>
      filter(type == "OT_UNDER") |>
      select(match, date, line, player_name, under_price = price)
    
    # Combine
    player_threes_combined <-
      player_threes_overs |>
      full_join(player_threes_unders) |>
      mutate(agency = "Unibet") |>
      arrange(match, player_name, line) |>
      mutate(over_price = round(over_price, 2),
             under_price = round(under_price, 2)) |> 
      left_join(player_names[,c("player_full_name", "team_name")], by = c("player_name" = "player_full_name")) |>
      rename(player_team = team_name) |> 
      separate(match, c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
      mutate(opposition_team = ifelse(player_team == home_team, away_team, home_team)) |> 
      relocate(player_team, opposition_team, .after = player_name)
    
    #===================================================================================================
    # Steals
    #===================================================================================================
    
    # All Player Steals
    all_player_steals <-
      all_markets |> 
      filter(label...4 == "Steals by the player - Including Overtime") |> 
      select(match, date = closed, participant, oddsAmerican, line, type) |>
      mutate(line = line / 1000) |> 
      mutate(oddsAmerican = as.numeric(oddsAmerican)) |>
      mutate(price = ifelse(oddsAmerican < 0, (100 / abs(oddsAmerican)) + 1, (oddsAmerican / 100) + 1)) |>
      distinct(match, date, participant, price, line, type) |> 
      separate(match, c("home_team", "away_team"), sep = " v ", remove = FALSE) |> 
      mutate(date = as_date(ymd_hms(date))) |> 
      separate(participant, c("last_name", "first_name"), sep = ", ", remove = TRUE) |> 
      mutate(player_name = paste(first_name, last_name, sep = " ")) |>
      select(-first_name, -last_name)
    
    # Overs
    player_steals_overs <- 
      all_player_steals |>
      filter(type == "OT_OVER") |>
      select(match, date, line, player_name, over_price = price)
    
    # Unders
    player_steals_unders <- 
      all_player_steals |>
      filter(type == "OT_UNDER") |>
      select(match, date, line, player_name, under_price = price)
    
    # Combine
    player_steals_combined <-
      player_steals_overs |>
      full_join(player_steals_unders) |>
      mutate(agency = "Unibet") |>
      arrange(match, player_name, line) |>
      mutate(over_price = round(over_price, 2),
             under_price = round(under_price, 2)) |> 
      left_join(player_names[,c("player_full_name", "team_name")], by = c("player_name" = "player_full_name")) |>
      rename(player_team = team_name) |> 
      separate(match, c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
      mutate(opposition_team = ifelse(player_team == home_team, away_team, home_team)) |> 
      relocate(player_team, opposition_team, .after = player_name)
    
    #===================================================================================================
    # Blocks
    #===================================================================================================
    
    # All Player Blocks
    all_player_blocks <-
      all_markets |> 
      filter(label...4 == "Blocks by the player - Including Overtime") |> 
      select(match, date = closed, participant, oddsAmerican, line, type) |>
      mutate(line = line / 1000) |> 
      mutate(oddsAmerican = as.numeric(oddsAmerican)) |>
      mutate(price = ifelse(oddsAmerican < 0, (100 / abs(oddsAmerican)) + 1, (oddsAmerican / 100) + 1)) |>
      distinct(match, date, participant, price, line, type) |> 
      separate(match, c("home_team", "away_team"), sep = " v ", remove = FALSE) |> 
      mutate(date = as_date(ymd_hms(date))) |> 
      separate(participant, c("last_name", "first_name"), sep = ", ", remove = TRUE) |> 
      mutate(player_name = paste(first_name, last_name, sep = " ")) |>
      select(-first_name, -last_name)
    
    # Overs
    player_blocks_overs <- 
      all_player_blocks |>
      filter(type == "OT_OVER") |>
      select(match, date, line, player_name, over_price = price)
    
    # Unders
    player_blocks_unders <- 
      all_player_blocks |>
      filter(type == "OT_UNDER") |>
      select(match, date, line, player_name, under_price = price)
    
    # Combine
    player_blocks_combined <-
      player_blocks_overs |>
      full_join(player_blocks_unders) |>
      mutate(agency = "Unibet") |>
      arrange(match, player_name, line) |>
      mutate(over_price = round(over_price, 2),
             under_price = round(under_price, 2)) |> 
      left_join(player_names[,c("player_full_name", "team_name")], by = c("player_name" = "player_full_name")) |>
      rename(player_team = team_name) |> 
      separate(match, c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
      mutate(opposition_team = ifelse(player_team == home_team, away_team, home_team)) |> 
      relocate(player_team, opposition_team, .after = player_name)
    
    #===================================================================================================
    # PRAs
    #===================================================================================================
    
    # All Player PRAs
    all_player_pras <-
      all_markets |> 
      filter(label...4 == "Points, rebounds & assists by the player - Including Overtime") |> 
      select(match, date = closed, participant, oddsAmerican, line, type) |>
      mutate(line = line / 1000) |> 
      mutate(oddsAmerican = as.numeric(oddsAmerican)) |>
      mutate(price = ifelse(oddsAmerican < 0, (100 / abs(oddsAmerican)) + 1, (oddsAmerican / 100) + 1)) |>
      distinct(match, date, participant, price, line, type) |> 
      separate(match, c("home_team", "away_team"), sep = " v ", remove = FALSE) |> 
      mutate(date = as_date(ymd_hms(date))) |> 
      separate(participant, c("last_name", "first_name"), sep = ", ", remove = TRUE) |> 
      mutate(player_name = paste(first_name, last_name, sep = " ")) |>
      select(-first_name, -last_name)
    
    # Overs
    player_pras_overs <- 
      all_player_pras |>
      filter(type == "OT_OVER") |>
      select(match, date, line, player_name, over_price = price)
    
    # Unders
    player_pras_unders <- 
      all_player_pras |>
      filter(type == "OT_UNDER") |>
      select(match, date, line, player_name, under_price = price)
    
    # Combine
    player_pras_combined <-
      player_pras_overs |>
      full_join(player_pras_unders) |>
      mutate(agency = "Unibet") |>
      arrange(match, player_name, line) |>
      mutate(over_price = round(over_price, 2),
             under_price = round(under_price, 2)) |> 
      left_join(player_names[,c("player_full_name", "team_name")], by = c("player_name" = "player_full_name")) |>
      rename(player_team = team_name) |> 
      separate(match, c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
      mutate(opposition_team = ifelse(player_team == home_team, away_team, home_team)) |> 
      relocate(player_team, opposition_team, .after = player_name)
    
    #===================================================================================================
    # Write out data
    #===================================================================================================
    
    # Points
    player_points_combined |> 
      mutate(market_name = "Player Points") |> 
      write_csv("Data/scraped_odds/unibet_player_points.csv")
    
    # Rebounds
    player_rebounds_combined |>
      mutate(market_name = "Player Rebounds") |>
      write_csv("Data/scraped_odds/unibet_player_rebounds.csv")
    
    # Assists
    player_assists_combined |>
      mutate(market_name = "Player Assists") |>
      write_csv("Data/scraped_odds/unibet_player_assists.csv")
    
    # Threes
    player_threes_combined |>
      mutate(market_name = "Player Threes") |>
      write_csv("Data/scraped_odds/unibet_player_threes.csv")
    
    # Steals
    player_steals_combined |>
      mutate(market_name = "Player Steals") |>
      write_csv("Data/scraped_odds/unibet_player_steals.csv")
    
    # Blocks
    player_blocks_combined |>
      mutate(market_name = "Player Blocks") |>
      write_csv("Data/scraped_odds/unibet_player_blocks.csv")
    
    # PRAs
    player_pras_combined |>
      mutate(market_name = "Player PRAs") |>
      write_csv("Data/scraped_odds/unibet_player_pras.csv")
    
  },
  error = function(e) {
    message("Error in scraping: ", e)
  })
