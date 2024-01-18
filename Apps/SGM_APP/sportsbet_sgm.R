library(httr)
library(jsonlite)
library(tidyverse)
library(purrr)


# Sportsbet SGM-----------------------------------------------------------------
# Read in all odds
all_files <-
  list.files("../../Data/scraped_odds", pattern = "sportsbet_player")

# Read in a loop
sportsbet_sgm <-
  map(all_files, function(x) {
    read_csv(paste0("../../Data/scraped_odds/", x))
  }) |> 
  bind_rows()

# Split overs and unders into separate rows
sportsbet_sgm_overs <-
  sportsbet_sgm |> 
  filter(!is.na(over_price)) |> 
  select(-player_id_unders) |> 
  rename(price = over_price) |> 
  mutate(type = "Overs") |> 
  select(-under_price) |> 
  distinct(player_name, market_name, line, type, .keep_all = TRUE)

sportsbet_sgm_unders <-
  sportsbet_sgm |> 
  filter(!is.na(under_price)) |> 
  select(-player_id) |> 
  rename(player_id = player_id_unders) |> 
  rename(price = under_price) |> 
  mutate(type = "Unders") |> 
  select(-over_price) |> 
  distinct(player_name, market_name, line, type, .keep_all = TRUE)

sportsbet_sgm <-
  bind_rows(sportsbet_sgm_overs, sportsbet_sgm_unders)

sportsbet_sgm <-
  rename(
    sportsbet_sgm,
    eventExternalId = event_external_id ,
    competitionExternalId = competition_external_id,
    classExternalId = class_external_id ,
    marketExternalId = market_id,
    outcomeExternalId = player_id
  )


#==============================================================================
# Function to get SGM data
#=-=============================================================================

get_sgm_sportsbet <- function(data, player_names, prop_line, prop_type, over_under) {
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
  
  outcomes_list <- lapply(1:nrow(filtered_df), function(i) {
    list(marketExternalId = as.integer(filtered_df$marketExternalId[i]),
         outcomeExternalId = as.integer(filtered_df$outcomeExternalId[i]))
  })
  
  payload <- list(
    classExternalId = as.integer(filtered_df$classExternalId[1]),
    competitionExternalId = as.integer(filtered_df$competitionExternalId[1]),
    eventExternalId = as.integer(filtered_df$eventExternalId[1]),
    outcomesExternalIds = outcomes_list
  )
  
  return(payload)
}

#==============================================================================
# Make Post Request
#==============================================================================

call_sgm_sportsbet <- function(data, player_names, prop_line, prop_type, over_under) {
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
  
  payload <- get_sgm_sportsbet(data, player_names, prop_line, prop_type, over_under)
  
  url <- 'https://www.sportsbet.com.au/apigw/multi-pricer/combinations/price'
  
  headers <- c('User-Agent' = 'Mozilla/5.0 (Linux; Android 6.0; Nexus 5 Build/MRA58N) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/115.0.0.0 Mobile Safari/537.36',
               'Content-Type' = 'application/json;charset=UTF-8')
  
  response <- POST(url, body = toJSON(payload, auto_unbox = TRUE), add_headers(.headers = headers), encode = "json")
  
  # Check if the request was successful
  if (http_error(response)) {
    stop("API request failed: ", content(response, "text"))
  }
  
  response_content <- content(response, "parsed")
  
  # Check if the response contains the expected data
  if (!"price" %in% names(response_content)) {
    stop("Unexpected API response: 'price' not found")
  }
  
  adjusted_price <- 1 + (response_content$price$numerator / response_content$price$denominator)
  adjustment_factor <- adjusted_price / unadjusted_price
  
  combined_list <- paste(player_names, prop_line, sep = ": ")
  player_string <- paste(combined_list, collapse = ", ")
  
  output_data <- data.frame(
    Selections = player_string,
    Markets = paste(prop_type, sep = ": ", collapse = ", "),
    Unadjusted_Price = unadjusted_price,
    Adjusted_Price = adjusted_price,
    Adjustment_Factor = adjustment_factor,
    Agency = 'Sportsbet'
  )
  
  return(output_data)
}

# call_sgm_sportsbet(data = sportsbet_sgm, player_names = c("Zach LaVine", "Zach LaVine"), prop_line = c("19.5", "5.5"), prop_type = c("Player Points", "Player Rebounds"), over_under = c("Overs", "Overs"))
