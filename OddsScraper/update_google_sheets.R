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

#===============================================================================
# Player Points
#===============================================================================

# read RDS
all_player_points <- read_rds("Data/processed_odds/all_player_points.rds")

# Add to google sheets
sheet_write(sheet, data = all_player_points, sheet = "Player Points")

#===============================================================================
# Player Rebounds
#===============================================================================

# read RDS
all_player_rebounds <- read_rds("Data/processed_odds/all_player_rebounds.rds")

# Add to google sheets
sheet_write(sheet, data = all_player_rebounds, sheet = "Player Rebounds")

#===============================================================================
# Player Assists
#===============================================================================

# read RDS
all_player_assists <- read_rds("Data/processed_odds/all_player_assists.rds")

# Add to google sheets
sheet_write(sheet, data = all_player_assists, sheet = "Player Assists")

#===============================================================================
# Player PRAS
#===============================================================================

# read RDS
all_player_pras <- read_rds("Data/processed_odds/all_player_pras.rds")

# Add to google sheets
sheet_write(sheet, data = all_player_pras, sheet = "Player PRAs")


#===============================================================================
# Steals
#===============================================================================

# read RDS
all_player_steals <- read_rds("Data/processed_odds/all_player_steals.rds")

# Add to google sheets
sheet_write(sheet, data = all_player_steals, sheet = "Player Steals")

#===============================================================================
# Blocks
#===============================================================================

# read RDS
all_player_blocks <- read_rds("Data/processed_odds/all_player_blocks.rds")

# Add to google sheets
sheet_write(sheet, data = all_player_blocks, sheet = "Player Blocks")

#===============================================================================
# Threes
#===============================================================================

# read RDS
all_player_threes <- read_rds("Data/processed_odds/all_player_threes.rds")

# Add to google sheets
sheet_write(sheet, data = all_player_threes, sheet = "Player Threes")
