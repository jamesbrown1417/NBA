# Ripperbet Markets Scraper
# Fetches player markets and SGM data from Ripperbet's public API.

library(httr2)
library(purrr)
library(tibble)
library(cli)

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
    cli_progress_bar("Fetching markets", total = length(event_keys))
  }
  
  results <- map(event_keys, function(ek) {
    if (progress) cli_progress_update()
    fetch_fn(ek)
  })
  
  if (progress) cli_progress_done()
  
  set_names(results, event_keys)
}

# Example usage ----------------------------------------------------------------

if (FALSE) {
  # Single event
  data <- fetch_markets("12842335")
  str(data, max.level = 2)
  
  # Multiple events (sequential with rate limiting)
  event_keys <- c("12842335", "12842336", "12842337")
  results <- fetch_multiple_events(event_keys)
  
  # Multiple events (parallel - faster but be careful with rate limits)
  results_parallel <- fetch_multiple_parallel(event_keys, max_concurrent = 3)
  
  # Different market group
  match_markets <- fetch_markets(
    event_key = "12842335",
    group = "Match Markets",
    market_type = "same-game-multi"
  )
}