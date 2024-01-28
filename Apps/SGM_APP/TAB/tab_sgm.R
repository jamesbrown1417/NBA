library(httr)
library(jsonlite)
library(tidyverse)
library(purrr)

# TAB SGM-----------------------------------------------------------------------
# Read in all odds
all_files <-
  list.files("../../Data/scraped_odds", pattern = "tab_player")

# Read in a loop
tab_sgm <-
  map(all_files, function(x) {
    read_csv(paste0("../../Data/scraped_odds/", x))
  }) |> 
  bind_rows()

# Split overs and unders into separate rows
tab_sgm_overs <-
  tab_sgm |> 
  filter(!is.na(over_price)) |> 
  select(-under_prop_id) |> 
  rename(id = prop_id) |> 
  rename(price = over_price) |> 
  mutate(type = "Overs") |> 
  select(-under_price) |> 
  distinct(player_name, market_name, line, type, .keep_all = TRUE)

tab_sgm_unders <-
  tab_sgm |> 
  filter(!is.na(under_price)) |> 
  select(-prop_id) |> 
  rename(id = under_prop_id) |> 
  rename(price = under_price) |> 
  mutate(type = "Unders") |> 
  select(-over_price) |> 
  distinct(player_name, market_name, line, type, .keep_all = TRUE)

tab_sgm <-
  bind_rows(tab_sgm_overs, tab_sgm_unders)

#==============================================================================
# Function to get SGM data
#===============================================================================

# Function to get SGM data
get_sgm_tab <- function(data, player_names, prop_line, prop_type, over_under) {
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
  
  # Get the 'id' column as a list
  id_list <- filtered_df$id
  
  # Create the propositions list using the id_list
  propositions <- lapply(id_list, function(id) list(type = unbox("WIN"), propositionId = unbox(id)))
  
  return(propositions)
}

#==============================================================================
# Make Post Request
#==============================================================================

# Make Post Request
call_sgm_tab <- function(data, player_names, prop_line, prop_type, over_under) {
  tryCatch({
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
    
    # Unadjusted price
    unadjusted_price <- prod(filtered_df$price)
    
    # Get propositions
    propositions <- get_sgm_tab(data, player_names, prop_line, prop_type, over_under)
    
    url <- "https://api.beta.tab.com.au/v1/pricing-service/enquiry"
    
    headers <- c("Content-Type" = "application/json")
    
    payload <- list(
      clientDetails = list(jurisdiction = unbox("SA"), channel = unbox("web")),
      bets = list(
        list(
          type = unbox("FIXED_ODDS"),
          legs = list(
            list(
              type = unbox("SAME_GAME_MULTI"),
              propositions = propositions
            )
          )
        )
      )
    )
    
    response <- POST(url, body = toJSON(payload), add_headers(.headers = headers), encode = "json")
    
    if (http_error(response)) {
      stop("HTTP request failed. Please check your URL or network connection.")
    }
    
    response_content <- content(response, "parsed")
    adjusted_price <- as.numeric(response_content$bets[[1]]$legs[[1]]$odds$decimal)
    adjustment_factor <- adjusted_price / unadjusted_price
    combined_list <- paste(player_names, prop_line, sep = ": ")
    player_string <- paste(combined_list, collapse = ", ")
    
    output_data <- tryCatch({
      data.frame(
        Selections = player_string,
        Markets = paste(prop_type, sep = ": ", collapse = ", "),
        Unadjusted_Price = unadjusted_price,
        Adjusted_Price = adjusted_price,
        Adjustment_Factor = adjustment_factor,
        Agency = 'TAB'
      )
    }, error = function(e) {
      data.frame(
        Selections = NA_character_,
        Markets = NA_character_,
        Unadjusted_Price = NA_real_,
        Adjusted_Price = NA_real_,
        Adjustment_Factor = NA_real_,
        Agency = NA_character_
      )
    })
    
    return(output_data)
    
  }, error = function(e) {
    print(paste("Error: ", e))
  })
}

# call_sgm_tab(data = tab_sgm, player_names = c("Zach LaVine", "Zach LaVine"), prop_line = c("19.5", "5.5"), prop_type = c("Player Points", "Player Rebounds"), over_under = c("Overs", "Overs"))
