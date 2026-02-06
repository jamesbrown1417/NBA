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
out_file <- file.path(repo_root, "Apps", "NBA_ARBS_WEB", "public", "data", "nba-arbs-data.json")

to_rel_path <- function(path) {
  sub(
    paste0("^", normalizePath(repo_root, winslash = "/", mustWork = TRUE), "/"),
    "",
    normalizePath(path, winslash = "/", mustWork = TRUE)
  )
}

format_mtime <- function(path) {
  info <- file.info(path)
  if (nrow(info) == 0 || is.na(info$mtime[1])) {
    return(NA_character_)
  }
  format(info$mtime[1], "%Y-%m-%d %H:%M:%S %Z")
}

read_dataset_with_meta <- function(path) {
  data <- read_rds(path)
  meta <- list(
    source = to_rel_path(path),
    file_mtime = format_mtime(path),
    rows = nrow(data),
    cols = ncol(data)
  )
  list(data = data, meta = meta)
}

all_arbs_bundle <- read_dataset_with_meta(file.path(data_dir, "all_arbs.rds"))
all_middles_bundle <- read_dataset_with_meta(file.path(data_dir, "all_middles.rds"))
tab_miss_by_one_bundle <- read_dataset_with_meta(file.path(data_dir, "tab_points_miss_by_one.rds"))
betright_miss_by_one_bundle <- read_dataset_with_meta(file.path(data_dir, "betright_points_miss_by_one.rds"))

processed_odds_files <- list.files(processed_odds_dir, full.names = TRUE)
processed_odds_data <- processed_odds_files |>
  map_dfr(read_rds) |>
  select(match, player_name, market_name, line, over_price, agency, empirical_prob_last_20)

processed_file_info <- file.info(processed_odds_files)
processed_latest <- if (nrow(processed_file_info) > 0) max(processed_file_info$mtime, na.rm = TRUE) else NA
processed_earliest <- if (nrow(processed_file_info) > 0) min(processed_file_info$mtime, na.rm = TRUE) else NA

datasets <- list(
  all_arbs = all_arbs_bundle$data,
  all_middles = all_middles_bundle$data,
  tab_miss_by_one = tab_miss_by_one_bundle$data,
  betright_miss_by_one = betright_miss_by_one_bundle$data,
  processed_odds = processed_odds_data
)

dataset_meta <- list(
  all_arbs = all_arbs_bundle$meta,
  all_middles = all_middles_bundle$meta,
  tab_miss_by_one = tab_miss_by_one_bundle$meta,
  betright_miss_by_one = betright_miss_by_one_bundle$meta,
  processed_odds = list(
    source_dir = to_rel_path(processed_odds_dir),
    files = length(processed_odds_files),
    latest_file_mtime = if (is.na(processed_latest)) NA_character_ else format(processed_latest, "%Y-%m-%d %H:%M:%S %Z"),
    earliest_file_mtime = if (is.na(processed_earliest)) NA_character_ else format(processed_earliest, "%Y-%m-%d %H:%M:%S %Z"),
    rows = nrow(processed_odds_data),
    cols = ncol(processed_odds_data)
  )
)

payload <- c(
  list(generated_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
  list(dataset_meta = dataset_meta),
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
