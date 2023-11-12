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
      output <- unnest(output, cols = c(criterion, betOfferType, outcomes, tags), names_repair = "universal")
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
    head_to_head_data <-
      game_id_json$events |>
      tibble() |>
      unnest(cols = c(event, betOffers), names_repair = "universal") |>
      select(name, outcomes) |>
      unnest(cols = c(outcomes), names_repair = "universal") |>
      select(match = name, label, participant, oddsAmerican) |>
      mutate(match = str_replace(match, " - ", " v ")) |>
      mutate(match = str_replace(match, "GWS Giants", "Greater Western Sydney")) |>
      mutate(participant = str_replace(participant, "GWS Giants", "Greater Western Sydney")) |>
      mutate(odds = as.numeric(oddsAmerican)) |>
      mutate(odds = ifelse(odds > 0, (odds + 100) / 100, (100 / abs(odds)) + 1)) |>
      filter(label %in% c("1", "2")) |>
      select(match, participant, odds) |>
      separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE)
    
    # Home teams
    home_h2h <-
      head_to_head_data |>
      filter(participant == home_team) |>
      select(match, home_team, home_win = odds)
    
    # Away teams
    away_h2h <-
      head_to_head_data |>
      filter(participant == away_team) |>
      select(match, away_team, away_win = odds)
    
    # Combine
    head_to_head_all <-
      full_join(home_h2h, away_h2h, by = "match") |>
      mutate(margin = 1 / home_win + 1 / away_win) |>
      mutate(margin = get_edge(margin)) |>
      mutate(agency = "Unibet") |>
      select(match, home_team, home_win, away_team, away_win, margin, agency)
    
    #===================================================================================================
    # Disposals
    #===================================================================================================
    
    disposals <-
      all_markets |>
      filter(str_detect(`label...4`, "Disposals")) |>
      filter(str_detect(`label...4`, "to get")) |>
      select(match,
             number_of_disposals = `label...4`,
             player_name = participant,
             odds = oddsAmerican) |>
      filter(!is.na(player_name)) |>
      mutate(odds = as.numeric(odds)) |>
      mutate(odds = ifelse(odds > 0, (odds + 100) / 100, (100 / abs(odds)) + 1)) |>
      mutate(odds = round(odds, 2)) |>
      mutate(number_of_disposals = str_extract(number_of_disposals, "\\d+")) |>
      mutate(number_of_disposals = paste0(number_of_disposals, "+"))
    
    # Fix player names
    disposals$player_name <- sapply(str_split(disposals$player_name, ", "), function(x) {
      paste(x[2], x[1], sep = " ")
    })
    
    # Final data
    disposals <-
      disposals |>
      select(match, player_name, number_of_disposals, price = odds) |>
      mutate(
        implied_probability = 1 / price,
        agency = "Unibet"
      )
    
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
