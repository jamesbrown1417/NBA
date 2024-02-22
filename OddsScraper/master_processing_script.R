# Libraries and functions-------------------------------------------------------
library(tidyverse)
library(googlesheets4)
library(googledrive)
library(future)
library(furrr)

# Set up parallel processing
plan(multisession)

# Get empirical probability function
source("Scripts/get_empirical_probabilities.R")

# Google sheets authentification -----------------------------------------------
options(gargle_oauth_cache = ".secrets")
drive_auth(cache = ".secrets", email = "cuzzy.punting@gmail.com")
gs4_auth(token = drive_token())

# # Run all odds scraping scripts-----------------------------------------------
run_scraping <- function(script_name) {
  tryCatch({
    source(script_name)
  }, error = function(e) {
    cat("Odds not released yet for:", script_name, "\n")
  })
}

# Run all odds scraping scripts
# run_scraping("OddsScraper/scrape_betr.R")
run_scraping("OddsScraper/scrape_BetRight.R")
# run_scraping("OddsScraper/scrape_Palmerbet.R")
run_scraping("OddsScraper/scrape_pointsbet.R")
run_scraping("OddsScraper/scrape_sportsbet.R")
run_scraping("OddsScraper/scrape_TAB.R")
run_scraping("OddsScraper/scrape_TopSport.R")
# run_scraping("OddsScraper/scrape_bet365.R")
run_scraping("OddsScraper/scrape_bluebet.R")
run_scraping("OddsScraper/scrape_neds.R")
run_scraping("OddsScraper/scrape_unibet.R")
  
