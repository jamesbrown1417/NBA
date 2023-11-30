# Libraries and functions
library(tidyverse)
library(jsonlite)
library(httr)

# Function to get bookmaker edge percentage
get_edge <- function(x) round(100 * (x - 1), 3)

# Wrap everything in try catch:
tryCatch(
  {
    # Get URL to make requests from
    game_id_url = "https://www.unibet.com.au/sportsbook-feeds/views/filter/basketball/nba/all/matches?includeParticipants=true&useCombined=true&ncid=1699768968"
    
    # Make request
    game_id_response = GET(game_id_url)
    
    # Convert response to JSON
    game_id_json = fromJSON(content(game_id_response, "text"))
    
    # Get event information and IDs
    event_info <-
      game_id_json$layout$sections$widgets[[2]]$matches$events[[1]]$event |> 
      tibble()
    
    # Get event IDs and names
    event_info <-
      event_info |>
      select(event_id = id, match = name) |>
      mutate(match = str_replace(match, " - ", " v "))
    
    # Add all markets url
    event_info <-
      event_info |>
      mutate(all_markets_url =
               paste0("https://oc-offering-api.kambicdn.com/offering/v2018/ubau/betoffer/event/" ,
                      event_id,
                      ".json?lang=en_AU&market=AU&ncid=1691104490"))
    
    # Create a function that takes the url and returns a json of the data
    get_unibet <- function(match, url) {
      response = GET(url)
      json = fromJSON(content(response, "text"))
      output <- tibble(json)
      output <- output$json[[1]] |> tibble()
      output <- unnest(output, cols = c("criterion") , names_repair = "universal")
      output <- unnest(output, cols = c("outcomes") , names_repair = "universal")
      output <- unnest(output, cols = c("tags") , names_repair = "universal")
      output$match <- match
      
      return(output)
    }
    
    #===================================================================================================
    # Get all market data
    #===================================================================================================
    
    all_markets <-
      event_info |>
      select(match, url = all_markets_url) |>
      pmap(get_unibet) |>
      bind_rows()
    
    #===================================================================================================
    # Head to Head
    #===================================================================================================
    
    # All head to head
    all_head_to_head <-
    all_markets |> 
      filter(label...4 == "Head To Head - Including Overtime") |> 
      select(match, date = closed, participant, oddsAmerican) |>
      mutate(oddsAmerican = as.numeric(oddsAmerican)) |>
      mutate(price = ifelse(oddsAmerican < 0, (100 / abs(oddsAmerican)) + 1, (oddsAmerican / 100) + 1)) |>
      distinct(match, date, participant, price) |> 
      separate(match, c("home_team", "away_team"), sep = " v ", remove = FALSE) |> 
      mutate(date = as_date(ymd_hms(date)))
    
    # All home teams
    home_head_to_head <-
      all_head_to_head |>
      filter(home_team == participant) |>
      select(match, date, home_team = participant, home_win = price)
    
    # All away teams
    away_head_to_head <-
      all_head_to_head |>
      filter(away_team == participant) |>
      select(match, date, away_team = participant, away_win = price)
    
    # Combine
    head_to_head_combined <-
      home_head_to_head |> 
      full_join(away_head_to_head)
    
    #===================================================================================================
    # Points
    #===================================================================================================
    
    
    #===================================================================================================
    # Write out data
    #===================================================================================================
    
    # Head to Heads
    head_to_head_all |> write_rds("~/python_projects/odds_scraper/AFL/AFL/Processing/processed_data/Unibet/unibet_h2h.rds")
    
    # Disposals
    disposals |> write_rds("~/python_projects/odds_scraper/AFL/AFL/Processing/processed_data/Unibet/unibet_disposals.rds")},
  error = function(e) {
    message("Error in scraping: ", e)
  })
