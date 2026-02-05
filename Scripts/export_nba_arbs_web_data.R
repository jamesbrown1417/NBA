#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(purrr)
  library(jsonlite)
})

get_script_path <- function() {
  file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(file_arg) == 0) {
    stop("Run with: Rscript Scripts/export_nba_arbs_web_data.R", call. = FALSE)
  }
  normalizePath(sub("^--file=", "", file_arg[[1]]), mustWork = TRUE)
}

script_path <- get_script_path()
repo_root <- normalizePath(file.path(dirname(script_path), ".."), mustWork = TRUE)

data_dir <- file.path(repo_root, "Data")
processed_odds_dir <- file.path(data_dir, "processed_odds")
out_file <- file.path(repo_root, "Apps", "NBA_ARBS_WEB", "data", "nba-arbs-data.json")

datasets <- list(
  all_arbs = read_rds(file.path(data_dir, "all_arbs.rds")),
  all_middles = read_rds(file.path(data_dir, "all_middles.rds")),
  tab_miss_by_one = read_rds(file.path(data_dir, "tab_points_miss_by_one.rds")),
  betright_miss_by_one = read_rds(file.path(data_dir, "betright_points_miss_by_one.rds")),
  processed_odds = list.files(processed_odds_dir, full.names = TRUE) |> map_dfr(read_rds) |> select(match, player_name, market_name, line, over_price, agency, empirical_prob_last_20)
)

payload <- c(
  list(generated_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
  datasets
)

dir.create(dirname(out_file), recursive = TRUE, showWarnings = FALSE)
write_json(
  payload,
  out_file,
  auto_unbox = TRUE,
  pretty = TRUE,
  na = "null",
  dataframe = "rows"
)

cat(sprintf("Wrote %s\n", out_file))
cat(sprintf("all_arbs: %d rows\n", nrow(datasets$all_arbs)))
cat(sprintf("all_middles: %d rows\n", nrow(datasets$all_middles)))
cat(sprintf("tab_miss_by_one: %d rows\n", nrow(datasets$tab_miss_by_one)))
cat(sprintf("betright_miss_by_one: %d rows\n", nrow(datasets$betright_miss_by_one)))
cat(sprintf("processed_odds: %d rows\n", nrow(datasets$processed_odds)))
