library(httr)
library(jsonlite)
library(tidyverse)
library(purrr)

# Pointsbet SGM-----------------------------------------------------------------
# Read in all odds
all_files <-
  list.files("../../Data/scraped_odds", pattern = "pointsbet_player")

# Read in a loop
pointsbet_sgm <-
  map(all_files, function(x) {
    read_csv(paste0("../../Data/scraped_odds/", x))
  }) |> 
  bind_rows()

# Split overs and unders into separate rows
pointsbet_sgm_overs <-
  pointsbet_sgm |> 
  filter(!is.na(over_price)) |> 
  select(-OutcomeKey_unders) |> 
  rename(price = over_price) |> 
  mutate(type = "Overs") |> 
  select(-under_price) |> 
  distinct(player_name, market_name, line, type, .keep_all = TRUE)

pointsbet_sgm_unders <-
  pointsbet_sgm |> 
  filter(!is.na(under_price)) |> 
  select(-OutcomeKey) |> 
  rename(OutcomeKey = OutcomeKey_unders) |> 
  rename(price = under_price) |> 
  mutate(type = "Unders") |> 
  select(-over_price) |> 
  distinct(player_name, market_name, line, type, .keep_all = TRUE)

pointsbet_sgm <-
  bind_rows(pointsbet_sgm_overs, pointsbet_sgm_unders)

#===============================================================================
# Function to get SGM data
#=-=============================================================================

# Create function to call the API
get_sgm_pointsbet <- function(data, player_names, prop_line, prop_type, over_under) {
  if (length(player_names) != length(prop_line)) {
    stop("Both lists should have the same length")
  }
  
  filtered_df <- data.frame()
  for (i in seq_along(player_names)) {
    temp_df <- data %>%
      filter(player_name == player_names[i],
             market_name == prop_type[i],
             type == over_under[i],
             line == prop_line[i])
    filtered_df <- bind_rows(filtered_df, temp_df)
  }
  
  id_list <- as.character(filtered_df$OutcomeKey)
  MarketKey_list <- as.character(filtered_df$MarketKey)
  event_key <- as.character(filtered_df$EventKey[1])
  
  selected_outcomes <- lapply(1:length(id_list), function(i) 
    list(MarketKey = unbox(MarketKey_list[i]), OutcomeKey = unbox(id_list[i]))
  )
  
  payload <- list(
    EventKey = unbox(event_key),
    SelectedOutcomes = selected_outcomes
  )
  
  return(payload)
}

#==============================================================================
# Make Post Request
#==============================================================================

call_sgm_pointsbet <- function(data, player_names, prop_line, prop_type, over_under) {
  if (length(player_names) != length(prop_line)) {
    stop("Both lists should have the same length")
  }
  
  filtered_df <- data.frame()
  for (i in seq_along(player_names)) {
    temp_df <- data %>%
      filter(player_name == player_names[i],
             market_name == prop_type[i],
             type == over_under[i],
             line == prop_line[i])
    filtered_df <- bind_rows(filtered_df, temp_df)
  }
  
  unadjusted_price <- prod(filtered_df$price)
  
  payload <- get_sgm_pointsbet(data, player_names, prop_line, prop_type, over_under)
  
  url <- 'https://api.au.pointsbet.com/api/v2/sgm/price'
  
  headers <- c('User-Agent' = 'Mozilla/5.0 (Linux; Android 6.0; Nexus 5 Build/MRA58N) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/115.0.0.0 Mobile Safari/537.36',
               'Content-Type' = 'application/json;charset=UTF-8',
               'Origin' = 'https://pointsbet.com.au',
               'Referer' = 'https://pointsbet.com.au/')
  
  # Error handling for the POST request
  tryCatch({
    response <- POST(url, body = toJSON(payload), add_headers(.headers = headers), encode = "json")
  }, error = function(e) {
    message("Error in POST request: ", e)
    return(NULL)
  })
  
  # If there is no response, return NULL
  if (is.null(response)) {
    return(NULL)
  }
  
  response_content <- content(response, "parsed")
  adjusted_price <- as.numeric(response_content$price)
  adjustment_factor <- adjusted_price / unadjusted_price
  combined_list <- paste(player_names, prop_line, sep = ": ")
  player_string <- paste(combined_list, collapse = ", ")
  
  output_data <- data.frame(
    Selections = player_string,
    Markets = paste(prop_type, sep = ": ", collapse = ", "),
    Unadjusted_Price = unadjusted_price,
    Adjusted_Price = adjusted_price,
    Adjustment_Factor = adjustment_factor,
    Agency = 'Pointsbet'
  )
  
  return(output_data)
}

# call_sgm_pointsbet(data = pointsbet_sgm, player_names = c("Paul George", "Paul George"), prop_line = c("24.5", "5.5"), prop_type = c("Player Points", "Player Rebounds"), over_under = c("Unders", "Unders"))

