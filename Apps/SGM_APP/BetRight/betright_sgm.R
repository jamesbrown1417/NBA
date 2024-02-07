library(httr)
library(jsonlite)
library(dplyr)
library(purrr)


# BetRight SGM-----------------------------------------------------------------
# Read in all odds
all_files <-
  list.files("../../Data/scraped_odds", pattern = "betright_player")

# Read in a loop
betright_sgm <-
  map(all_files, function(x) {
    read_csv(paste0("../../Data/scraped_odds/", x))
  }) |> 
  bind_rows()

# Split overs and unders into separate rows
betright_sgm_overs <-
  betright_sgm |> 
  filter(!is.na(over_price)) |> 
  select(-outcome_name_under,
         -outcome_id_under,
         -fixed_market_id_under) |> 
  rename(price = over_price) |> 
  mutate(type = "Overs") |> 
  select(-under_price) |> 
  distinct(player_name, market_name, line, type, .keep_all = TRUE)

betright_sgm_unders <-
  betright_sgm |> 
  filter(!is.na(under_price)) |> 
  select(-outcome_name,
         -outcome_id,
         -fixed_market_id) |> 
  rename(outcome_name = outcome_name_under,
         outcome_id = outcome_id_under,
         fixed_market_id = fixed_market_id_under) |>
  rename(price = under_price) |> 
  mutate(type = "Unders") |> 
  select(-over_price) |> 
  distinct(player_name, market_name, line, type, .keep_all = TRUE)

if (nrow(betright_sgm_unders) > 0) {
  betright_sgm <-
    bind_rows(betright_sgm_overs, betright_sgm_unders)
} else {
  betright_sgm <- betright_sgm_overs
}

#===============================================================================
# Function to get SGM data
#=-=============================================================================

# Function to get SGM data
get_sgm_betright <- function(data, player_names, prop_line, prop_type, over_under) {
  
  if (length(player_names) != length(prop_line)) {
    stop("Both lists should have the same length")
  }
  
  filtered_df <- data.frame()
  for (i in 1:length(player_names)) {
    temp_df <- data[data$player_name == player_names[i] & 
                      data$line == prop_line[i] &
                      data$market_name == prop_type[i] &
                      data$type == over_under[i], ]
    if (nrow(temp_df) == 0) {
      stop(paste("No data found for", player_names[i], "with", prop_line[i], prop_type[i], over_under, "."))
    }
    filtered_df <- rbind(filtered_df, temp_df)
  }
  
  header <- filtered_df$group_by_header
  event_id <- filtered_df$event_id
  outcome_name <- filtered_df$outcome_name
  outcome_id <- filtered_df$outcome_id
  fixed_market_id <- filtered_df$fixed_market_id
  points <- "0"
  fixed_win <- filtered_df$price
  
  payload <- lapply(1:length(player_names), function(i) {
    list(
      eventId = unlist(event_id[i]),
      outcomeId = unlist(outcome_id[i]),
      marketType = "WIN",
      fixedWin = unlist(fixed_win[i]),
      fixedMarketId = unlist(fixed_market_id[i]),
      marketTypeDesc = "Win",
      groupByHeader = header[i],
      points = points,
      outcomeName = outcome_name[i]
    )
  })
  
  return(payload)
}


#==============================================================================
# Make Post Request
#==============================================================================

# Make POST request
call_sgm_betright <- function(data, player_names, prop_line, prop_type, over_under) {
  if (length(player_names) != length(prop_line)) {
    stop("Both lists should have the same length")
  }
  
  filtered_df <- data.frame()
  for (i in 1:length(player_names)) {
    temp_df <- data[data$player_name == player_names[i] & 
                      data$line == prop_line[i] &
                      data$market_name == prop_type[i] &
                      data$type == over_under[i], ]
    if (nrow(temp_df) == 0) {
      stop(paste("No data found for", player_names[i], "with", prop_line[i], prop_type[i], over_under, "."))
    }
    filtered_df <- rbind(filtered_df, temp_df)
  }
  
  unadjusted_price <- prod(filtered_df$price)
  
  payload <- get_sgm_betright(data, player_names, prop_line, prop_type, over_under)
  
  url <- "https://sgm-api.betright.com.au/Pricing/SgmPrice?"
  
  headers <- add_headers('User-Agent' = 'Mozilla/5.0 (Linux; Android 6.0; Nexus 5 Build/MRA58N) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/115.0.0.0 Mobile Safari/537.36',
                         'Content-Type' = 'application/json;charset=UTF-8',
                         'Origin' = 'https://betright.com.au',
                         'Referer' = 'https://betright.com.au/')
  
  response <- POST(url, headers, body = toJSON(payload, auto_unbox = TRUE))
  
  if (http_error(response)) {
    stop("HTTP error occurred while calling API.")
  }
  
  response_content <- fromJSON(content(response, "text"))
  
  if (!"price" %in% names(response_content)) {
    stop("No price information found in the API response.")
  }
  
  adjusted_price <- as.numeric(response_content$price)
  adjustment_factor <- adjusted_price / unadjusted_price
  player_string <- paste(paste(player_names, prop_line, sep = ": "), collapse = ", ")
  
  output_data <- data.frame(
    Selections = player_string,
    Markets = paste(prop_type, sep = ": ", collapse = ", "),
    Unadjusted_Price = unadjusted_price,
    Adjusted_Price = adjusted_price,
    Adjustment_Factor = adjustment_factor,
    Agency = 'Betright'
  )
  
  return(output_data)
}

# 
# call_sgm_betright(
#   data = betright_sgm,
#   player_names = c("Mikal Bridges", "Mikal Bridges"),
#   prop_line = c("2.5", "5.5"),
#   prop_type = c("Player Assists", "Player Rebounds"),
#   over_under = c("Overs", "Overs")
# )
