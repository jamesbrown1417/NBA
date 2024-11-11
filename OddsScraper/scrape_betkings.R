# Load necessary libraries
library(httr)
library(jsonlite)

# Define URLs with meaningful names
player_points_url <- "https://site-gateway.betkings.com.au/sportsbook/event/5554386?includeMarketGroups=PLAYER_POINTS&locale=ENG"
player_rebounds_url <- "https://site-gateway.betkings.com.au/sportsbook/event/5554386?includeMarketGroups=PLAYER_REBOUNDS&locale=ENG"
player_assists_url <- "https://site-gateway.betkings.com.au/sportsbook/event/5554386?includeMarketGroups=PLAYER_ASSISTS&locale=ENG"
player_blocks_url <- "https://site-gateway.betkings.com.au/sportsbook/event/5554386?includeMarketGroups=PLAYER_BLOCKS&locale=ENG"
player_steals_url <- "https://site-gateway.betkings.com.au/sportsbook/event/5554386?includeMarketGroups=PLAYER_STEALS&locale=ENG"
player_three_pointers_url <- "https://site-gateway.betkings.com.au/sportsbook/event/5554386?includeMarketGroups=PLAYER_THREE_POINTERS&locale=ENG"
player_par_url <- "https://site-gateway.betkings.com.au/sportsbook/event/5554386?includeMarketGroups=PLAYER_POINTS_ASSISTS_AND_REBOUNDS&locale=ENG"

# List of URLs with descriptive names
urls <- list(
  player_points = player_points_url,
  player_rebounds = player_rebounds_url,
  player_assists = player_assists_url,
  player_blocks = player_blocks_url,
  player_steals = player_steals_url,
  player_three_pointers = player_three_pointers_url,
  player_par = player_par_url
)

# Function to retrieve JSON data and save it to a named list
get_json_data <- function(url) {
  response <- GET(
    url,
    add_headers(
      `accept` = "application/json",
      `accept-encoding` = "gzip, deflate, br, zstd",
      `accept-language` = "en-US,en;q=0.9",
      `content-type` = "application/json",
      `device` = "desktop",
      `origin` = "https://www.betkings.com.au",
      `priority` = "u=1, i",
      `referer` = "https://www.betkings.com.au/",
      `sec-ch-ua` = '"Chromium";v="130", "Google Chrome";v="130", "Not?A_Brand";v="99"',
      `sec-ch-ua-mobile` = "?0",
      `sec-ch-ua-platform` = '"Windows"',
      `sec-fetch-dest` = "empty",
      `sec-fetch-mode` = "cors",
      `sec-fetch-site` = "same-site",
      `user-agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/130.0.0.0 Safari/537.36",
      `version` = "3.41.2",
      `x-local-language` = "en-US",
      `x-local-platform` = "Browser",
      `x-local-time-zone` = "Australia/Adelaide",
      `x-project-id` = "2"
    )
  )
  
  if (status_code(response) == 200) {
    data <- content(response, as = "text")
    json_data <- fromJSON(data, flatten = TRUE)
    return(json_data)
  } else {
    cat("Failed to retrieve data. Status code:", status_code(response), "\n")
    return(NULL)
  }
}

# Retrieve and save data for each URL in a named list for further processing
results <- lapply(urls, get_json_data)
names(results) <- names(urls) # Assigns names to each dataset in the results list

