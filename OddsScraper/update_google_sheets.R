# Libraries and functions-------------------------------------------------------
library(tidyverse)
library(googlesheets4)
library(googledrive)
library(future)
library(furrr)

# Google sheets authentification -----------------------------------------------
options(gargle_oauth_cache = ".secrets")
drive_auth(cache = ".secrets", email = "cuzzy.punting@gmail.com")
gs4_auth(token = drive_token())

sheet <- gs4_find("NBA Data")

# Function to retry sheet writing incase of failure-----------------------------
write_to_sheet_with_backoff <- function(sheet, data, sheet_name, max_retries = 5) {
  base_delay <- 1 # Base delay in seconds
  attempt <- 1
  
  while(attempt <= max_retries) {
    tryCatch({
      # Attempt to write data to the sheet
      sheet_write(sheet, data = data, sheet = sheet_name)
      message("Data written successfully on attempt ", attempt)
      break # Break the loop if operation is successful
    }, error = function(e) {
      # Log the error message
      message("Attempt ", attempt, " failed: ", e$message)
      if (attempt == max_retries) {
        stop("Maximum retries reached. Operation failed.")
      } else {
        # Calculate delay with exponential backoff
        delay <- base_delay * (2^(attempt - 1))
        message("Retrying in ", delay, " seconds...")
        Sys.sleep(delay) # Wait for the specified delay
        attempt <- attempt + 1
      }
    })
  }
}

#===============================================================================
# Player Points
#===============================================================================

# read RDS
all_player_points <- read_rds("Data/processed_odds/all_player_points.rds")

# Add to google sheets
# sheet_write(sheet, data = all_player_points, sheet = "Player Points")
write_to_sheet_with_backoff(sheet, all_player_points, "Player Points")

#===============================================================================
# Player Rebounds
#===============================================================================

# read RDS
all_player_rebounds <- read_rds("Data/processed_odds/all_player_rebounds.rds")

# Add to google sheets
# sheet_write(sheet, data = all_player_rebounds, sheet = "Player Rebounds")
write_to_sheet_with_backoff(sheet, all_player_rebounds, "Player Rebounds")

#===============================================================================
# Player Assists
#===============================================================================

# read RDS
all_player_assists <- read_rds("Data/processed_odds/all_player_assists.rds")

# Add to google sheets
# sheet_write(sheet, data = all_player_assists, sheet = "Player Assists")
write_to_sheet_with_backoff(sheet, all_player_assists, "Player Assists")

#===============================================================================
# Player PRAS
#===============================================================================

# read RDS
all_player_pras <- read_rds("Data/processed_odds/all_player_pras.rds")

# Add to google sheets
# sheet_write(sheet, data = all_player_pras, sheet = "Player PRAs")
write_to_sheet_with_backoff(sheet, all_player_pras, "Player PRAs")

#===============================================================================
# Steals
#===============================================================================

# read RDS
all_player_steals <- read_rds("Data/processed_odds/all_player_steals.rds")

# Add to google sheets
# sheet_write(sheet, data = all_player_steals, sheet = "Player Steals")
write_to_sheet_with_backoff(sheet, all_player_steals, "Player Steals")

#===============================================================================
# Blocks
#===============================================================================

# read RDS
all_player_blocks <- read_rds("Data/processed_odds/all_player_blocks.rds")

# Add to google sheets
# sheet_write(sheet, data = all_player_blocks, sheet = "Player Blocks")
write_to_sheet_with_backoff(sheet, all_player_blocks, "Player Blocks")

#===============================================================================
# Threes
#===============================================================================

# read RDS
all_player_threes <- read_rds("Data/processed_odds/all_player_threes.rds")

# Add to google sheets
# sheet_write(sheet, data = all_player_threes, sheet = "Player Threes")
write_to_sheet_with_backoff(sheet, all_player_threes, "Player Threes")

