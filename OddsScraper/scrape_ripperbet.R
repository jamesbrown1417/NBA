# Ripperbet Markets Scraper
# Fetches player markets and SGM data from Ripperbet's public API.

library(httr2)
library(purrr)
library(tidyverse)
library(cli)

# Fix team names function
source("Scripts/fix_team_names.R")

# Configuration ----------------------------------------------------------------

BASE_URL <- "https://api.public.ripperbet.au"

HEADERS <- c(
  
  "Accept" = "application/json",
  "Accept-Encoding" = "gzip, deflate, br, zstd",
  "Accept-Language" = "en-GB,en;q=0.7",
  "Content-Type" = "application/json",
  "Origin" = "https://www.ripperbet.au",
  "Referer" = "https://www.ripperbet.au/",
  "Sec-Ch-Ua" = '"Brave";v="143", "Chromium";v="143", "Not A(Brand";v="24"',
  "Sec-Ch-Ua-Mobile" = "?0",
  "Sec-Ch-Ua-Platform" = '"macOS"',
  "Sec-Fetch-Dest" = "empty",
  "Sec-Fetch-Mode" = "cors",
  "Sec-Fetch-Site" = "same-site",
  "User-Agent" = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/143.0.0.0 Safari/537.36"
)

# Core functions ---------------------------------------------------------------

#' Get NBA events as a tibble
#'
#' @param url API endpoint URL
#' @return A tibble with event_key, name, and start_time
get_nba_events <- function(url = "https://api.public.ripperbet.au/api-events/public/master-events/basketball/nba") {
  data <- request(url) |>
    req_headers(!!!HEADERS) |>
    req_perform() |>
    resp_body_json()
  
  events <- data$MasterEvents |>
    map("Events") |>
    list_flatten()
  
  tibble(
    event_key = map_chr(events, "EventKey"),
    name = map_chr(events, "Name"),
    start_time = as.POSIXct(as.numeric(map_chr(events, "StartTime")) / 1000, origin = "1970-01-01")
  )
}

#' Create a base request with common configuration
#'
#' @param rate_limit Requests per second (default 2 = 0.5s between requests
#' @return An httr2 request object
create_base_request <- function(rate_limit = 2) {
  request(BASE_URL) |>
    req_headers(!!!HEADERS) |>
    req_retry(max_tries = 3, backoff = ~ 2) |>
    req_throttle(rate = rate_limit) |>
    req_timeout(30)
}

#' Build the payload for a markets request
#'
#' @param event_key The event identifier
#' @param group Market group (default "Player Markets")
#' @param market_type Market type (default "same-game-multi")
#' @return A named list for the JSON payload
build_payload <- function(
    event_key,
    group = "Player Markets",
    market_type = "same-game-multi"
) {
  list(
    EventKey = event_key,
    Group = group,
    MarketType = market_type
  )
}

#' Fetch markets for a single event
#'
#' @param event_key The event identifier
#' @param group Market group
#' @param market_type Market type
#' @param rate_limit Requests per second
#' @return Parsed JSON response as a list
fetch_markets <- function(
    event_key,
    group = "Player Markets",
    market_type = "same-game-multi",
    rate_limit = 2
) {
  payload <- build_payload(event_key, group, market_type)
  
  create_base_request(rate_limit) |>
    req_url_path_append("api-events", "public", "markets") |>
    req_body_json(payload) |>
    req_perform() |>
    resp_body_json()
}

#' Safely fetch markets (returns NULL on error instead of stopping
#'
#' @inheritParams fetch_markets
#' @return Parsed JSON response or NULL on error
fetch_markets_safely <- function(
    event_key,
    group = "Player Markets",
    market_type = "same-game-multi",
    rate_limit = 2
) {
  tryCatch(
    {
      fetch_markets(event_key, group, market_type, rate_limit)
    },
    error = function(e) {
      cli_alert_warning("Error fetching event {event_key}: {e$message}")
      NULL
    }
  )
}

#' Fetch markets for multiple events
#'
#' @param event_keys Character vector of event identifiers
#' @param group Market group
#' @param market_type Market type
#' @param rate_limit Requests per second
#' @param progress Show progress bar (default TRUE)
#' @return Named list mapping event_key -> market data (or NULL on error)
fetch_multiple_events <- function(
    event_keys,
    group = "Player Markets",
    market_type = "same-game-multi",
    rate_limit = 2,
    progress = TRUE
) {
  fetch_fn <- function(event_key) {
    fetch_markets_safely(event_key, group, market_type, rate_limit)
  }
  
  if (progress) {
    results <- map(cli_progress_along(event_keys, "Fetching markets"), function(i) {
      fetch_fn(event_keys[i])
    })
  } else {
    results <- map(event_keys, fetch_fn)
  }
  
  set_names(results, event_keys)
}

# get events ----------------------------------------------------------------

# Get all events
all_events <-
  get_nba_events()

today_events <-
  all_events |> 
  filter(start_time > Sys.time()) |> 
  # Make sure only from today
  filter(as.Date(start_time) == Sys.Date()) |> 
  rename(match = name)

# Multiple events (sequential with rate limiting)
event_keys <- today_events$event_key
results <- fetch_multiple_events(event_keys)
  
#===============================================================================
# Process scraped odds
#===============================================================================

#' Extract markets by description
#'
#' @param results Results from fetch_multiple_events
#' @param description Market description to filter (e.g., "Player Points")
#' @return A tibble with event_key, name, win_price, and reference fields
extract_markets <- function(results, description) {
  results |>
    map("Markets") |>
    list_flatten() |>
    keep(\(m) m$Description == description) |>
    map(function(market) {
      event_key <- market$EventKey
      map(market$Outcomes, function(o) {
        ref <- jsonlite::fromJSON(o$Reference)
        tibble(
          event_key = event_key,
          name = o$Name,
          win_price = o$Prices[[1]]$WinPrice,
          player_id = ref$ID,
          player_name = ref$Name,
          first_name = ref$First,
          last_name = ref$Last,
          team = ref$Team
        )
      }) |> list_rbind()
    }) |>
    list_rbind()
}

#' Extract markets by description with over/under prices
#'
#' @param results Results from fetch_multiple_events
#' @param description Market description to filter (e.g., "Player Points")
#' @return A tibble with one row per player/line, with over_price and under_price columns
extract_markets_ou <- function(results, description) {
  results |>
    map("Markets") |>
    list_flatten() |>
    keep(\(m) m$Description == description) |>
    map(function(market) {
      event_key <- market$EventKey
      map(market$Outcomes, function(o) {
        ref <- jsonlite::fromJSON(o$Reference)
        tibble(
          event_key = event_key,
          name = o$Name,
          win_price = o$Prices[[1]]$WinPrice,
          player_id = ref$ID,
          player_name = ref$Name,
          first_name = ref$First,
          last_name = ref$Last,
          team = ref$Team
        )
      }) |> list_rbind()
    }) |>
    list_rbind() |>
    mutate(
      line = as.numeric(str_extract(name, "[0-9]+\\.?[0-9]*")),
      type = if_else(str_detect(name, "Over"), "over_price", "under_price")
    ) |>
    select(-name) |>
    pivot_wider(names_from = type, values_from = win_price, values_fn = first)
}

#' Process alt lines market
#'
#' @param results Results from fetch_multiple_events
#' @param events Events tibble with event_key and match columns
#' @param description Market description to filter from API (e.g., "Player Points")
#' @param market_name Market name for output column (e.g., "Player Points")
#' @return A processed tibble ready for analysis
process_alt_lines <- function(results, events, description, market_name) {
  tryCatch(
    {
      extract_markets(results, description) |>
        filter(win_price > 0) |>
        mutate(
          player_name = paste(first_name, last_name),
          market_name = market_name
        ) |>
        left_join(events, by = "event_key") |>
        mutate(line = as.numeric(str_extract(name, "[-+]?[0-9]*\\.?[0-9]+")) - 0.5) |>
        separate(match, into = c("home_team", "away_team"), sep = " v ") |>
        mutate(
          home_team = fix_team_names(home_team),
          away_team = fix_team_names(away_team)
        ) |>
        mutate(
          player_team = case_when(team == "H" ~ home_team, team == "A" ~ away_team, TRUE ~ NA_character_),
          opposition_team = case_when(team == "H" ~ away_team, team == "A" ~ home_team, TRUE ~ NA_character_)
        ) |>
        mutate(
          match = paste(home_team, "v", away_team),
          over_price = win_price,
          agency = "BetMakers"
        ) |>
        select(
          match, home_team, away_team, market_name, player_name,
          player_team, line, over_price, agency, opposition_team
        )
    },
    error = function(e) {
      cli_alert_warning("Error processing alt lines ({description}): {e$message}")
      tibble(
        match = character(),
        home_team = character(),
        away_team = character(),
        market_name = character(),
        player_name = character(),
        player_team = character(),
        line = numeric(),
        over_price = numeric(),
        agency = character(),
        opposition_team = character()
      )
    }
  )
}

#' Process lines market with over/under prices
#'
#' @param results Results from fetch_multiple_events
#' @param events Events tibble with event_key and match columns
#' @param description Market description to filter from API (e.g., "Player Points")
#' @param market_name Market name for output column (e.g., "Player Points")
#' @return A processed tibble with over_price and under_price columns
process_lines_ou <- function(results, events, description, market_name) {
  tryCatch(
    {
      extract_markets_ou(results, description) |>
        filter(over_price > 0 | under_price > 0) |>
        mutate(
          player_name = paste(first_name, last_name),
          market_name = market_name
        ) |>
        left_join(events, by = "event_key") |>
        separate(match, into = c("home_team", "away_team"), sep = " v ") |>
        mutate(
          home_team = fix_team_names(home_team),
          away_team = fix_team_names(away_team)
        ) |>
        mutate(
          player_team = case_when(team == "H" ~ home_team, team == "A" ~ away_team, TRUE ~ NA_character_),
          opposition_team = case_when(team == "H" ~ away_team, team == "A" ~ home_team, TRUE ~ NA_character_)
        ) |>
        mutate(
          match = paste(home_team, "v", away_team),
          agency = "BetMakers"
        ) |>
        select(
          match, home_team, away_team, market_name, player_name,
          player_team, line, over_price, under_price, agency, opposition_team
        )
    },
    error = function(e) {
      cli_alert_warning("Error processing lines OU ({description}): {e$message}")
      tibble(
        match = character(),
        home_team = character(),
        away_team = character(),
        market_name = character(),
        player_name = character(),
        player_team = character(),
        line = numeric(),
        over_price = numeric(),
        under_price = numeric(),
        agency = character(),
        opposition_team = character()
      )
    }
  )
}

# Player Points-----------------------------------------------------------------

# Alt lines
player_points_alternate <-
  process_alt_lines(
    results,
    today_events,
    description = "Player Points",
    market_name = "Player Points"
  )

# Over / Under
player_points_over_under <-
  process_lines_ou(
    results,
    today_events,
    "Player Points Over Under",
    "Player Points")

# Combine
ripperbet_player_points <-
  bind_rows(player_points_alternate, player_points_over_under) |> 
  arrange(match, player_name, market_name, line, desc(over_price)) |> 
  relocate(under_price, .after = over_price)

# Player Rebounds --------------------------------------------------------------
player_rebounds_alternate <-
  process_alt_lines(
    results,
    today_events,
    description = "Player Rebounds",
    market_name = "Player Rebounds"
  )

player_rebounds_over_under <-
  process_lines_ou(
    results,
    today_events,
    "Player Rebounds Over Under",
    "Player Rebounds"
  )

ripperbet_player_rebounds <-
  bind_rows(player_rebounds_alternate, player_rebounds_over_under) |>
  arrange(match, player_name, market_name, line, desc(over_price)) |> 
  relocate(under_price, .after = over_price)

# Player Assists ---------------------------------------------------------------
player_assists_alternate <-
  process_alt_lines(
    results,
    today_events,
    description = "Player Assists",
    market_name = "Player Assists"
  )

player_assists_over_under <-
  process_lines_ou(
    results,
    today_events,
    "Player Assists Over Under",
    "Player Assists"
  )

ripperbet_player_assists <-
  bind_rows(player_assists_alternate, player_assists_over_under) |>
  arrange(match, player_name, market_name, line, desc(over_price)) |> 
  relocate(under_price, .after = over_price)

# Player Steals ----------------------------------------------------------------
ripperbet_player_steals <-
  process_alt_lines(
    results,
    today_events,
    description = "Player Steals",
    market_name = "Player Steals"
  ) |>
  arrange(match, player_name, market_name, line, desc(over_price))

# Player Blocks ----------------------------------------------------------------
ripperbet_player_blocks <-
  process_alt_lines(
    results,
    today_events,
    description = "Player Blocks",
    market_name = "Player Blocks"
  ) |>
  arrange(match, player_name, market_name, line, desc(over_price))

# Player Threes ----------------------------------------------------------------
ripperbet_player_threes <-
  process_alt_lines(
    results,
    today_events,
    description = "Player Three Pointers",
    market_name = "Player Threes"
  ) |>
  arrange(match, player_name, market_name, line, desc(over_price))

# Player PRAs ------------------------------------------------------------------
ripperbet_player_pras <-
  process_alt_lines(
    results,
    today_events,
    description = "Player Points + Assists + Rebounds",
    market_name = "Player PRAs"
  ) |>
  arrange(match, player_name, market_name, line, desc(over_price))

# Player Assists + Rebounds ----------------------------------------------------
ripperbet_player_assists_rebounds <-
  process_alt_lines(
    results,
    today_events,
    description = "Player Assists + Rebounds",
    market_name = "Player Assists + Rebounds"
  ) |>
  arrange(match, player_name, market_name, line, desc(over_price))

# Player Blocks + Steals -------------------------------------------------------
ripperbet_player_blocks_steals <-
  process_alt_lines(
    results,
    today_events,
    description = "Player Blocks + Steals",
    market_name = "Player Blocks + Steals"
  ) |>
  arrange(match, player_name, market_name, line, desc(over_price))

# Write outputs ----------------------------------------------------------------

ripperbet_player_points |> write_csv("Data/scraped_odds/ripperbet_player_points.csv")
ripperbet_player_rebounds |> write_csv("Data/scraped_odds/ripperbet_player_rebounds.csv")
ripperbet_player_assists |> write_csv("Data/scraped_odds/ripperbet_player_assists.csv")
ripperbet_player_steals |> write_csv("Data/scraped_odds/ripperbet_player_steals.csv")
ripperbet_player_blocks |> write_csv("Data/scraped_odds/ripperbet_player_blocks.csv")
ripperbet_player_threes |> write_csv("Data/scraped_odds/ripperbet_player_threes.csv")
ripperbet_player_pras |> write_csv("Data/scraped_odds/ripperbet_player_pras.csv")
ripperbet_player_assists_rebounds |> write_csv("Data/scraped_odds/ripperbet_player_assists_rebounds.csv")
ripperbet_player_blocks_steals |> write_csv("Data/scraped_odds/ripperbet_player_blocks_steals.csv")
