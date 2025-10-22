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
source("Scripts/fix_player_names.R")

# User agent pool for rotation
user_agents <- c(
  "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124.0.0.0 Safari/537.36",
  "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/123.0.0.0 Safari/537.36",
  "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124.0.0.0 Safari/537.36",
  "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/123.0.0.0 Safari/537.36",
  "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124.0.0.0 Safari/537.36",
  "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:124.0) Gecko/20100101 Firefox/124.0",
  "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/17.4 Safari/605.1.15",
  "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124.0.0.0 Safari/537.36 Edg/124.0.0.0"
)

# Polite request helper with backoff to avoid rate limits
perform_request_json <- function(url, max_tries = 2, base_delay = 1, jitter = 0.05, verbose = TRUE, throttle = 1) {
  attempt <- 1
  repeat {
    # small throttle to smooth bursts
    Sys.sleep(throttle)
    if (isTRUE(verbose)) cat(sprintf("[%s] GET %s (attempt %d)\n", format(Sys.time(), "%H:%M:%S"), url, attempt))
    # Randomly select user agent from pool
    selected_ua <- sample(user_agents, 1)
    req <- request(url) |> req_user_agent(selected_ua)
    resp <- tryCatch(req_perform(req), error = function(e) e)
    
    # Handle transport errors (no HTTP response)
    if (inherits(resp, "error")) {
      if (isTRUE(verbose)) cat(sprintf("[%s] Transport error: %s\n", format(Sys.time(), "%H:%M:%S"), resp$message))
      if (attempt >= max_tries) stop(resp)
      delay <- base_delay * (2^(attempt - 1)) + runif(1, 0, jitter)
      if (isTRUE(verbose)) cat(sprintf("[%s] Backing off %.2fs (attempt %d/%d)\n", format(Sys.time(), "%H:%M:%S"), delay, attempt, max_tries))
      Sys.sleep(delay)
      attempt <- attempt + 1
      next
    }
    
    status <- httr2::resp_status(resp)
    if (status == 429) {
      # Respect Retry-After when provided
      ra <- suppressWarnings(as.numeric(httr2::resp_header(resp, "Retry-After")))
      delay <- if (!is.na(ra)) ra else base_delay * (2^(attempt - 1)) + runif(1, 0, jitter)
      if (isTRUE(verbose)) cat(sprintf("[%s] 429 Too Many Requests. Retry-After=%s. Backing off %.2fs (attempt %d/%d)\n", format(Sys.time(), "%H:%M:%S"), ifelse(is.na(ra), "NA", ra), delay, attempt, max_tries))
      if (attempt >= max_tries) stop("HTTP 429 Too Many Requests after retries")
      Sys.sleep(delay)
      attempt <- attempt + 1
      next
    }
    if (status >= 500 && status < 600) {
      if (isTRUE(verbose)) cat(sprintf("[%s] Server error %d. Retrying with backoff.\n", format(Sys.time(), "%H:%M:%S"), status))
      if (attempt >= max_tries) stop(paste0("HTTP ", status, " after retries"))
      delay <- base_delay * (2^(attempt - 1)) + runif(1, 0, jitter)
      if (isTRUE(verbose)) cat(sprintf("[%s] Backing off %.2fs (attempt %d/%d)\n", format(Sys.time(), "%H:%M:%S"), delay, attempt, max_tries))
      Sys.sleep(delay)
      attempt <- attempt + 1
      next
    }
    
    # Success
    if (isTRUE(verbose)) cat(sprintf("[%s] OK %d\n", format(Sys.time(), "%H:%M:%S"), status))
    return(httr2::resp_body_json(resp))
  }
}

# Standardize to initials (TAB approach)
standardize_to_initials <- function(player_names) {
  trimmed_names <- str_trim(player_names)
  case_when(
    # Already initials format (1-2 letters + space + lastname)
    str_detect(trimmed_names, "^[A-Za-z]{1,2}\\s+[A-Za-z]+") ~ trimmed_names,
    # Full name -> initial + lastname
    str_detect(trimmed_names, "^[A-Za-z]{3,}\\s+[A-Za-z]+") ~ {
      first_initial <- str_extract(trimmed_names, "^[A-Za-z]")
      last_name <- str_extract(trimmed_names, "\\s+([A-Za-z]+)$") |> str_trim()
      paste(first_initial, last_name)
    },
    # Fallback
    .default = trimmed_names
  )
}

# Function to standardize player names
standardize_player_name <- function(df) {
  df |>
    mutate(player_name = str_remove_all(player_name, "\\(.*\\)")) |>
    mutate(player_name = str_remove_all(player_name, " \\d+\\+ ")) |> 
    mutate(player_name = str_remove_all(player_name, " over")) |>
    mutate(player_name = str_remove_all(player_name, " under")) |>
    mutate(player_name = str_remove_all(player_name, " \\d+\\.\\d+ ")) |>
    mutate(player_name = str_squish(player_name)) |>
    mutate(player_name_raw = player_name) |>
    mutate(player_name = str_remove_all(player_name, "\\.")) |>
    mutate(player_name = standardize_to_initials(player_name)) |>
    # Disambiguate duplicate surnames using full first-name cues
    mutate(player_name = case_when(
      # Thompsons
      str_detect(player_name_raw, "^Amen Thompson$") ~ "Am Thompson",
      str_detect(player_name_raw, "^Ausar Thompson$") ~ "Au Thompson",
      # Williams variants
      str_detect(player_name_raw, "^Jalen Williams$") ~ "Ja Williams",
      str_detect(player_name_raw, "^Jaylin Williams$") ~ "Jay Williams",
      # Greens
      str_detect(player_name_raw, "^Jae?len Green$|^Jalen Green$") ~ "Ja Green",
      str_detect(player_name_raw, "^Jeff Green$") ~ "Je Green",
      # Butler
      str_detect(player_name_raw, "^Jimmy Butler$") ~ "Ji Butler",
      # Johnson common
      str_detect(player_name_raw, "^Ke.* Johnson$") ~ "Ke Johnson",
      # Murray
      str_detect(player_name_raw, "^Ke.* Murray$") ~ "Ke Murray",
      # Young
      str_detect(player_name_raw, "^Trae Young$") ~ "T Young",
      # Holiday
      str_detect(player_name_raw, "^Jrue Holiday$") ~ "J Holiday",
      # Wiggins
      str_detect(player_name_raw, "^Aaron Wiggins$") ~ "Aa Wiggins",
      str_detect(player_name_raw, "^Andrew Wiggins$") ~ "An Wiggins",
      # Carter Jr and Jackson Jr fixes
      str_detect(player_name_raw, "^Wendell Carter( Jr\\.?)?$") ~ "W Carter Jr.",
      str_detect(player_name_raw, "^Jaren Jackson( Jr\\.?)?$") ~ "J Jackson Jr.",
      # Edge typos/patterns from feed
      str_detect(player_name_raw, "C.* Cuni.*$") ~ "C Cunningham",
      str_detect(player_name_raw, "F.* Wagnr$") ~ "F Wagner",
      str_detect(player_name_raw, "Haliburto") ~ "T Haliburton",
      str_detect(player_name_raw, "P.* Wshington$") ~ "P Washington",
      TRUE ~ player_name
    )) |>
    mutate(player_name = fix_player_names_tab_initials(player_name)) |>
    left_join(player_names[, c("player_full_name", "team_name", "join_name")], by = c("player_name" = "join_name")) |>
    select(-player_name_raw) |>
    mutate(opposition_team = if_else(home_team == team_name, away_team, home_team))
}

# URL to get responses
betr_url = "https://web20-api.bluebet.com.au/SportsCategory?CategoryId=39251&format=json"

# Make request and get response
betr_response <- perform_request_json(betr_url)

# Get matches
matches <- betr_response$MasterCategories[[1]]$Categories[[1]]$MasterEvents

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
all_betr_markets <-
  map(matches, get_match_info) |> bind_rows()

#===============================================================================
# Head to head markets
#===============================================================================

# Home teams
home_teams <-
  all_betr_markets |>
  separate(match, into = c("away_team", "home_team"), sep = " @ ", remove = FALSE) |>
  filter(str_detect(market_name, "Money Line")) |> 
  mutate(market_name = "Head To Head") |> 
  group_by(match) |> 
  filter(row_number() == 1) |> 
  rename(home_win = prices) |> 
  select(-propositions)

# Away teams
away_teams <-
  all_betr_markets |>
  separate(match, into = c("away_team", "home_team"), sep = " @ ", remove = FALSE) |>
  filter(str_detect(market_name, "Money Line")) |> 
  mutate(market_name = "Head To Head") |>
  group_by(match) |> 
  filter(row_number() == 2) |> 
  rename(away_win = prices) |> 
  select(-propositions)

# Combine
betr_head_to_head_markets <-
  home_teams |>
  left_join(away_teams) |> 
  select(match, start_time, market_name, home_team, home_win, away_team, away_win) |> 
  mutate(margin = round((1/home_win + 1/away_win), digits = 3)) |> 
  mutate(agency = "Betr")

# Fix team names
betr_head_to_head_markets <-
  betr_head_to_head_markets |> 
  mutate(match = paste(home_team, "v", away_team))

# Write to csv
write_csv(betr_head_to_head_markets, "Data/scraped_odds/betr_h2h.csv")

# Get only games from next gameday or if today is a gameday only after this point in time
all_betr_markets <-
all_betr_markets |> 
  select(match, match_id, start_time) |> 
  mutate(start_time = lubridate::ymd_hms(start_time, tz = "Australia/Adelaide")) |>
  mutate(current_datetime = lubridate::now(tzone = "Australia/Adelaide")) |> 
  filter(start_time >= current_datetime) |>
  filter(day(start_time) == min(day(start_time)))

#===============================================================================
# Player Props
#===============================================================================

# Get API URL for each market type----------------------------------------------

# Player Points
player_points_links <-
  glue("https://web20-api.bluebet.com.au/MasterEvent?GroupTypeCode=G199&MasterEventId={unique(all_betr_markets$match_id)}&format=json")

# Player Points Props
player_points_props_links <-
  glue("https://web20-api.bluebet.com.au/MasterEvent?GroupTypeCode=G382&MasterEventId={unique(all_betr_markets$match_id)}&format=json")

# Player Assists Links
player_assists_links <-
  glue("https://web20-api.bluebet.com.au/MasterEvent?GroupTypeCode=G198&MasterEventId={unique(all_betr_markets$match_id)}&format=json")

# Player Assists Props Links
player_assists_props_links <-
  glue("https://web20-api.bluebet.com.au/MasterEvent?GroupTypeCode=G383&MasterEventId={unique(all_betr_markets$match_id)}&format=json")

# Player Rebounds Links
player_rebounds_links <-
  glue("https://web20-api.bluebet.com.au/MasterEvent?GroupTypeCode=G200&MasterEventId={unique(all_betr_markets$match_id)}&format=json")

# Player Rebounds Props Links
player_rebounds_props_links <-
  glue("https://web20-api.bluebet.com.au/MasterEvent?GroupTypeCode=G384&MasterEventId={unique(all_betr_markets$match_id)}&format=json")

# Player 3 Pointers Links
player_3_pointers_links <-
  glue("https://web20-api.bluebet.com.au/MasterEvent?GroupTypeCode=G202&MasterEventId={unique(all_betr_markets$match_id)}&format=json")

# Player 3 Pointers Props Links
player_3_pointers_props_links <-
  glue("https://web20-api.bluebet.com.au/MasterEvent?GroupTypeCode=G386&MasterEventId={unique(all_betr_markets$match_id)}&format=json")

# Player Steals Links
player_steals_links <-
  glue("https://web20-api.bluebet.com.au/MasterEvent?GroupTypeCode=G338&MasterEventId={unique(all_betr_markets$match_id)}&format=json")

# Player Steals Props Links
player_steals_props_links <-
  glue("https://web20-api.bluebet.com.au/MasterEvent?GroupTypeCode=G389&MasterEventId={unique(all_betr_markets$match_id)}&format=json")

# Player Blocks Links
player_blocks_links <-
  glue("https://web20-api.bluebet.com.au/MasterEvent?GroupTypeCode=G337&MasterEventId={unique(all_betr_markets$match_id)}&format=json")

# Player Blocks Props Links
player_blocks_props_links <-
  glue("https://web20-api.bluebet.com.au/MasterEvent?GroupTypeCode=G390&MasterEventId={unique(all_betr_markets$match_id)}&format=json")

# Player PRAs Links
player_pras_links <-
  glue("https://web20-api.bluebet.com.au/MasterEvent?GroupTypeCode=G340&MasterEventId={unique(all_betr_markets$match_id)}&format=json")

# Player PRAs Props Links
player_pras_props_links <-
  glue("https://web20-api.bluebet.com.au/MasterEvent?GroupTypeCode=G385&MasterEventId={unique(all_betr_markets$match_id)}&format=json")

# Player Props Links
player_props_links <-
  glue("https://web20-api.bluebet.com.au/MasterEvent?GroupTypeCode=G106&MasterEventId={unique(all_betr_markets$match_id)}&format=json")

# Function to extract prop data from links--------------------------------------

get_prop_data <- function(link) {
    # Polite delay and backoff
    response <- perform_request_json(link)
    
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

# Helper functions to process player props data
process_alternate_stats <- function(links, safe_get_prop_data_fn = safe_get_prop_data) {
  map(links, safe_get_prop_data_fn) |> 
    map("result") |>
    bind_rows() |>
    separate(outcome_name, into = c("market_name", "player_name"), sep = " - ", remove = FALSE) |>
    mutate(line = as.numeric(str_extract(player_name, "\\d+")) - 0.5) |>
    mutate(match = str_extract(event_name, "\\(.*\\)")) |>
    mutate(match = str_remove_all(match, "\\(|\\)")) |>
    separate(match, into = c("away_team", "home_team"), sep = " @ ", remove = FALSE) |>
    mutate(match = paste(home_team, "v", away_team)) |>
    standardize_player_name() |>
    mutate(agency = "Betr") |>
    select(
      "match",
      "home_team",
      "away_team",
      "market_name",
      "player_name" = "player_full_name",
      "outcome_name",
      "player_team" = "team_name",
      "line",
      "over_price" = "price",
      "agency",
      "opposition_team"
    )
}

process_lines_data <- function(props_data, outcome_title_pattern) {
  props_data |> 
    filter(str_detect(outcome_title, outcome_title_pattern)) |> 
    separate(outcome_name, into = c("market_name", "player_name"), sep = " - ", remove = FALSE) |>
    mutate(line = as.numeric(str_extract(player_name, "\\d+\\.\\d+"))) |>
    mutate(match = str_extract(event_name, "\\(.*\\)")) |>
    mutate(match = str_remove_all(match, "\\(|\\)")) |>
    separate(match, into = c("away_team", "home_team"), sep = " @ ", remove = FALSE) |>
    mutate(match = paste(home_team, "v", away_team)) |>
    standardize_player_name()
}

create_overs_unders_dataset <- function(lines_data) {
  # Overs
  overs_lines <- 
    lines_data |> 
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
  
  # Unders
  unders_lines <- 
    lines_data |> 
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
  
  # Join together
  overs_lines |> 
    left_join(unders_lines)
}

# Get player points data--------------------------------------------------------

betr_player_points_props <-
  map(player_points_props_links, safe_get_prop_data) |> 
  map("result") |> 
  bind_rows()

# Alternate Points
betr_player_points <- process_alternate_stats(player_points_links)

# Lines for points
betr_player_points_lines <- 
  process_lines_data(betr_player_points_props, "points over under") |>
  create_overs_unders_dataset()

# Combine
betr_player_points <-
  betr_player_points |> 
  bind_rows(betr_player_points_lines) |>
  mutate(home_team = fix_team_names(home_team)) |>
  mutate(away_team = fix_team_names(away_team)) |>
  mutate(match = paste(home_team, "v", away_team)) |>
  mutate(market_name = "Player Points")

# Get player assists data-------------------------------------------------------
# Get props data for assists
betr_player_assists_props <-
  map(player_assists_props_links, safe_get_prop_data) |> 
  map("result") |> 
  bind_rows()

# Alternate Assists
betr_player_assists_alt <- process_alternate_stats(player_assists_links)

# Lines for assists
betr_player_assists_lines <- 
  process_lines_data(betr_player_assists_props, "assists over under") |>
  create_overs_unders_dataset()

# Combine assists
betr_player_assists <-
  betr_player_assists_alt |> 
  bind_rows(betr_player_assists_lines) |>
  mutate(home_team = fix_team_names(home_team)) |>
  mutate(away_team = fix_team_names(away_team)) |>
  mutate(match = paste(home_team, "v", away_team)) |>
  mutate(market_name = "Player Assists")

# Get player rebounds data------------------------------------------------------
# Get props data for rebounds
betr_player_rebounds_props <-
  map(player_rebounds_props_links, safe_get_prop_data) |> 
  map("result") |> 
  bind_rows()

# Alternate Rebounds
betr_player_rebounds <- process_alternate_stats(player_rebounds_links)

# Lines for rebounds
betr_player_rebounds_lines <- 
  process_lines_data(betr_player_rebounds_props, "rebounds over under") |>
  create_overs_unders_dataset()

# Combine rebounds
betr_player_rebounds <-
  betr_player_rebounds |> 
  bind_rows(betr_player_rebounds_lines) |>
  mutate(home_team = fix_team_names(home_team)) |>
  mutate(away_team = fix_team_names(away_team)) |>
  mutate(match = paste(home_team, "v", away_team)) |>
  mutate(market_name = "Player Rebounds")
# Get player 3-pointers data----------------------------------------------------
# Get props data for 3-pointers
betr_player_3_pointers_props <-
  map(player_3_pointers_props_links, safe_get_prop_data) |> 
  map("result") |> 
  bind_rows()

# Alternate 3-Pointers
betr_player_3_pointers_alt <- process_alternate_stats(player_3_pointers_links)

# Lines for 3-pointers
betr_player_3_pointers_lines <- 
  process_lines_data(betr_player_3_pointers_props, "three pointers over under") |>
  create_overs_unders_dataset()

# Combine 3-pointers
betr_player_3_pointers <-
  betr_player_3_pointers_alt |> 
  bind_rows(betr_player_3_pointers_lines) |>
  mutate(home_team = fix_team_names(home_team)) |>
  mutate(away_team = fix_team_names(away_team)) |>
  mutate(match = paste(home_team, "v", away_team)) |>
  mutate(market_name = "Player Threes")
# Get player steals data--------------------------------------------------------
# Get props data for steals
betr_player_steals_props <-
  map(player_steals_props_links, safe_get_prop_data) |> 
  map("result") |> 
  bind_rows()

# Alternate Steals
betr_player_steals_alt <- process_alternate_stats(player_steals_links)

# Lines for steals
betr_player_steals_lines <- 
  process_lines_data(betr_player_steals_props, "steals over under") |>
  create_overs_unders_dataset()

# Combine steals
betr_player_steals <-
  betr_player_steals_alt |> 
  bind_rows(betr_player_steals_lines) |>
  mutate(home_team = fix_team_names(home_team)) |>
  mutate(away_team = fix_team_names(away_team)) |>
  mutate(match = paste(home_team, "v", away_team)) |>
  mutate(market_name = "Player Steals")

# Get player blocks data--------------------------------------------------------
# Get props data for blocks
betr_player_blocks_props <-
  map(player_blocks_props_links, safe_get_prop_data) |> 
  map("result") |> 
  bind_rows()

# Alternate Blocks
betr_player_blocks_alt <- process_alternate_stats(player_blocks_links)

# Lines for blocks
betr_player_blocks_lines <- 
  process_lines_data(betr_player_blocks_props, "blocks over under") |>
  create_overs_unders_dataset()

# Combine blocks
betr_player_blocks <-
  betr_player_blocks_alt |> 
  bind_rows(betr_player_blocks_lines) |>
  mutate(home_team = fix_team_names(home_team)) |>
  mutate(away_team = fix_team_names(away_team)) |>
  mutate(match = paste(home_team, "v", away_team)) |>
  mutate(market_name = "Player Blocks")

# Get player PRAs (Points+Rebounds+Assists) data--------------------------------
# Get props data for PRAs
betr_player_pras_props <-
  map(player_pras_props_links, safe_get_prop_data) |> 
  map("result") |> 
  bind_rows()

# Alternate PRAs
betr_player_pras_alt <- process_alternate_stats(player_pras_links)

# Lines for PRAs
betr_player_pras_lines <- 
  process_lines_data(betr_player_pras_props, "points \\+ assists \\+ rebounds over under") |>
  create_overs_unders_dataset()

# Combine PRAs
betr_player_pras <-
  betr_player_pras_alt |> 
  bind_rows(betr_player_pras_lines) |>
  mutate(home_team = fix_team_names(home_team)) |>
  mutate(away_team = fix_team_names(away_team)) |>
  mutate(match = paste(home_team, "v", away_team)) |>
  mutate(market_name = "Player PRAs")

#===============================================================================
# Write to CSV
#===============================================================================

betr_player_points |> filter(!is.na(player_name)) |> write_csv("Data/scraped_odds/betr_player_points.csv")
betr_player_assists |> filter(!is.na(player_name)) |> write_csv("Data/scraped_odds/betr_player_assists.csv")
betr_player_rebounds |> filter(!is.na(player_name)) |> write_csv("Data/scraped_odds/betr_player_rebounds.csv")
betr_player_3_pointers |> filter(!is.na(player_name)) |> write_csv("Data/scraped_odds/betr_player_threes.csv")
betr_player_steals |> filter(!is.na(player_name)) |> write_csv("Data/scraped_odds/betr_player_steals.csv")
betr_player_blocks |> filter(!is.na(player_name)) |> write_csv("Data/scraped_odds/betr_player_blocks.csv")
betr_player_pras |> filter(!is.na(player_name)) |> write_csv("Data/scraped_odds/betr_player_pras.csv")
