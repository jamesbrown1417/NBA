library(httr)
library(jsonlite)
library(dplyr)
library(purrr)
library(mongolite)
uri <- Sys.getenv("mongodb_connection_string")

# Pointsbet SGM-----------------------------------------------------------------
pbsgm_con <- mongo(collection = "Pointsbet-SGM", db = "Odds", url = uri)
pointsbet_sgm <- pbsgm_con$find('{}') |> tibble()

#===============================================================================
# Function to get SGM data
#=-=============================================================================

# Create function to call the API
get_sgm_pointsbet <- function(data, player_names, disposal_counts) {
  if (length(player_names) != length(disposal_counts)) {
    stop("Both lists should have the same length")
  }
  
  filtered_df <- data.frame()
  for (i in seq_along(player_names)) {
    temp_df <- data %>% 
      filter(player_name == player_names[[i]] &
             number_of_disposals == disposal_counts[i])
    filtered_df <- bind_rows(filtered_df, temp_df)
  }
  
  id_list <- as.character(filtered_df$outcome_id)
  market_id_list <- as.character(filtered_df$market_id)
  event_key <- as.character(filtered_df$match_id[1])
  
  selected_outcomes <- lapply(1:length(id_list), function(i) 
    list(MarketKey = unbox(market_id_list[i]), OutcomeKey = unbox(id_list[i]))
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

call_sgm_pointsbet <- function(data, player_names, disposal_counts) {
  if (length(player_names) != length(disposal_counts)) {
    stop("Both lists should have the same length")
  }
  
  filtered_df <- data.frame()
  for (i in seq_along(player_names)) {
    temp_df <- data %>% 
      filter(player_name == player_names[i],
             number_of_disposals == disposal_counts[i])
    filtered_df <- bind_rows(filtered_df, temp_df)
  }
  
  unadjusted_price <- prod(filtered_df$price)
  
  payload <- get_sgm_pointsbet(data, player_names, disposal_counts)
  
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
  combined_list <- paste(player_names, disposal_counts, sep = ": ")
  player_string <- paste(combined_list, collapse = ", ")
  
  output_data <- data.frame(
    Selections = player_string,
    Unadjusted_Price = unadjusted_price,
    Adjusted_Price = adjusted_price,
    Adjustment_Factor = adjustment_factor,
    Agency = 'Pointsbet'
  )
  
  return(output_data)
}

data = pointsbet_sgm
player_names = c("Jack Sinclair", "Sam Docherty")
disposal_counts = c("25+", "25+")
