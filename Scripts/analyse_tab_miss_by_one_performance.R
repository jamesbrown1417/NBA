# Get Git Data

library(tidyverse)

files <- list.files("Data/tab_points_miss_by_one_history", pattern = "\\.rds$", full.names = TRUE)

combined <- map_dfr(files, function(f) {
  datetime_str <- str_extract(basename(f), "\\d{4}-\\d{2}-\\d{2}_\\d{2}-\\d{2}-\\d{2}")

  df <- read_rds(f)
  df$git_datetime <- ymd_hms(str_replace_all(datetime_str, c("_" = " ", "-(?=\\d{2}-\\d{2}$)" = ":", "-(?=\\d{2}$)" = ":")))
  df
})

combined_tab <-
  combined |>
  filter(over_agency == "TAB") |>
  filter(hour(git_datetime) >= 8 & hour(git_datetime) < 14) |>
  mutate(game_date = as_date(git_datetime)) |>
  filter(margin > 0) |> 
  filter(line > 6) |> 
  filter(margin > 3 & margin < 10) |> 
  distinct(match, game_date, market_name, player_name, line, over_price, over_agency, under_price, under_agency, margin, .keep_all = TRUE)

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

# # Optional: Filter to only thursdays or fridays
# combined_tab <- combined_tab |>
#   filter(wday(game_date) %in% c(5, 6))  # 5 = Thursday, 6 = Friday

# Join and resolve outcomes
combined_tab_results <- combined_tab |>
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
  distinct(match, game_date, market_name, player_name, line, over_price, over_agency, under_price, under_agency, margin, actual_stat, outcome) |>
  filter(margin < 10) |>
  mutate(
    over_stake = 100 * under_price / (over_price + under_price),
    under_stake = 100 * over_price / (over_price + under_price)
  ) |>
  mutate(
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
  combined_tab_results |>
  filter(over_agency != "Dabble Pickem") |>
  filter(under_agency != "Dabble Pickem") |>
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
