# Get Git Data

library(tidyverse)

files <- list.files("Data/all_arbs_history", pattern = "\\.rds$", full.names = TRUE)

combined <- map_dfr(files, function(f) {
  datetime_str <- str_extract(basename(f), "\\d{4}-\\d{2}-\\d{2}_\\d{2}-\\d{2}-\\d{2}")
  
  df <- read_rds(f)
  df$git_datetime <- ymd_hms(str_replace_all(datetime_str, c("_" = " ", "-(?=\\d{2}-\\d{2}$)" = ":", "-(?=\\d{2}$)" = ":")))
  df
})

combined_SB <-
  combined |>
  filter(over_agency == "Sportsbet" | under_agency == "Sportsbet") |>
  filter(hour(git_datetime) >= 8 & hour(git_datetime) < 14) |> 
  mutate(game_date = as_date(git_datetime)) |> 
  filter(margin > 0)

# Get results data
all_results <-
  read_csv("Data/all_player_stats_2025-2026.csv") |> 
  select(firstName, familyName, GAME_DATE, HOME_TEAM, AWAY_TEAM, points, assists, blocks, steals, threePointersMade, reboundsTotal)

# Prepare all_results for joining
all_results <- all_results |>
  mutate(
    player_name = paste(firstName, familyName),
    GAME_DATE = as_date(GAME_DATE) + 1,
    PRA = points + reboundsTotal + assists
  )

# Join and resolve outcomes
combined_SB_results <- combined_SB |>
  left_join(
    all_results,
    by = c("player_name", "game_date" = "GAME_DATE")
  ) |>
  mutate(
    actual_stat = case_match(
      market_name,
      "Player Points" ~ points,
      "Player Assists" ~ assists,
      "Player Rebounds" ~ reboundsTotal,
      "Player Blocks" ~ blocks,
      "Player Steals" ~ steals,
      "Player Threes" ~ threePointersMade,
      "Player PRAs" ~ PRA
    ),
    outcome = case_when(
      actual_stat > line ~ "over",
      actual_stat < line ~ "under",
      actual_stat == line ~ "push"
    )
  ) |> 
  filter(!is.na(outcome)) |> 
  distinct(match, game_date, market_name, player_name, line, over_agency, under_agency, actual_stat, outcome, .keep_all = TRUE) |> 
  filter(margin < 10) |> 
    mutate(
    over_stake = 100 * under_price / (over_price + under_price),
    under_stake = 100 * over_price / (over_price + under_price)
  ) |> mutate(
    over_profit = case_when(
      outcome == "over" ~ over_stake * (over_price - 1),
      outcome == "under" ~ -over_stake,
      outcome == "push" ~ 0
    ),
    under_profit = case_when(
      outcome == "under" ~ under_stake * (under_price - 1),
      outcome == "over" ~ -under_stake,
      outcome == "push" ~ 0
    )
  ) |> 
  mutate(total_profit = over_profit + under_profit) |> 
  filter(over_agency != under_agency)

# Calculate totals
results <-
combined_SB_results |> 
  filter(over_agency != "Dabble Pickem") |> 
  filter(under_agency != "Dabble Pickem") |>
  filter(game_date >= as_date("2026-01-01")) |>
  # filter(game_date <= as_date("2025-11-30")) |>
  group_by(over_agency, under_agency) |> 
  summarise(
    bets = n(),
    total_unders_profit = sum(under_profit),
    total_unders_stake = sum(under_stake),
    total_overs_profit = sum(over_profit),
    total_overs_stake = sum(over_stake),
    total_arb_profit = sum(total_profit),
    total_stake = sum(over_stake + under_stake),
    unders_roi_pct = 100 * total_unders_profit / total_unders_stake,
    overs_roi_pct = 100 * total_overs_profit / total_overs_stake,
    arb_roi_pct = 100 * total_arb_profit / total_stake
  ) |> 
  ungroup()

# overs
results_sb_overs <-
  results |> 
  filter(over_agency != "Sportsbet") |> 
  select(over_agency, bets, total_overs_stake, total_overs_profit, overs_roi_pct) |> 
  arrange(desc(overs_roi_pct))

# unders
results_sb_unders <-
  results |> 
  filter(over_agency == "Sportsbet") |> 
  select(under_agency, bets, total_unders_stake, total_unders_profit, unders_roi_pct) |> 
  arrange(desc(unders_roi_pct))

#===============================================================================
# Results by date
#===============================================================================

results_by_date <-
  combined_SB_results |> 
  filter(over_agency != "Dabble Pickem") |> 
  filter(under_agency != "Dabble Pickem") |> 
  group_by(game_date, over_agency, under_agency) |> 
  summarise(
    bets = n(),
    total_unders_profit = sum(under_profit),
    total_unders_stake = sum(under_stake),
    total_overs_profit = sum(over_profit),
    total_overs_stake = sum(over_stake),
    total_arb_profit = sum(total_profit),
    total_stake = sum(over_stake + under_stake),
    unders_roi_pct = 100 * total_unders_profit / total_unders_stake,
    overs_roi_pct = 100 * total_overs_profit / total_overs_stake,
    arb_roi_pct = 100 * total_arb_profit / total_stake
  ) |> 
  ungroup()

# Unders with Sportsbet
results_by_date_sb_unders <-
  results_by_date |>
  filter(over_agency == "Sportsbet") |> 
  select(game_date, under_agency, bets, total_unders_stake, total_unders_profit, unders_roi_pct) |> 
  arrange(game_date, desc(unders_roi_pct))

total_performance_by_date <-
results_by_date_sb_unders |>
  group_by(game_date) |>
  summarise(total_bets = sum(bets),
            total_stake = sum(total_unders_stake),
            total_profit = sum(total_unders_profit)) |>
  mutate(roi_pct = 100 * total_profit / total_stake) |> 
  arrange(desc(game_date))

#===============================================================================
# Results by market
#===============================================================================

results_by_market <-
  combined_SB_results |> 
  filter(over_agency != "Dabble Pickem") |> 
  filter(under_agency != "Dabble Pickem") |> 
  group_by(market_name, over_agency, under_agency) |> 
  summarise(
    bets = n(),
    total_unders_profit = sum(under_profit),
    total_unders_stake = sum(under_stake),
    total_overs_profit = sum(over_profit),
    total_overs_stake = sum(over_stake),
    total_arb_profit = sum(total_profit),
    total_stake = sum(over_stake + under_stake),
    unders_roi_pct = 100 * total_unders_profit / total_unders_stake,
    overs_roi_pct = 100 * total_overs_profit / total_overs_stake,
    arb_roi_pct = 100 * total_arb_profit / total_stake
  ) |> 
  ungroup()