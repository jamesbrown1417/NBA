library(shiny)
library(DT)
library(dplyr)
library(purrr)
library(readr)
library(lubridate)
library(stringr)
library(scales)
library(parallel)

`%||%` <- function(x, y) if (is.null(x)) y else x

safe_pct <- function(profit, stake) {
  ifelse(is.finite(stake) & stake > 0, 100 * profit / stake, NA_real_)
}

ancestor_path <- function(path, levels_up) {
  out <- normalizePath(path, winslash = "/", mustWork = FALSE)
  if (levels_up == 0) {
    return(out)
  }
  for (i in seq_len(levels_up)) {
    out <- dirname(out)
  }
  out
}

find_repo_root <- function(start = getwd()) {
  for (i in 0:7) {
    candidate <- ancestor_path(start, i)
    if (dir.exists(file.path(candidate, "Data")) && dir.exists(file.path(candidate, "Scripts"))) {
      return(normalizePath(candidate, winslash = "/", mustWork = TRUE))
    }
  }
  stop("Could not locate repository root containing Data/ and Scripts/.")
}

parse_git_datetime <- function(path) {
  stamp <- str_extract(basename(path), "\\d{4}-\\d{2}-\\d{2}_\\d{2}-\\d{2}-\\d{2}")
  as.POSIXct(stamp, format = "%Y-%m-%d_%H-%M-%S", tz = "Australia/Adelaide")
}

read_history_snapshot <- function(file_path) {
  df <- read_rds(file_path)
  df$git_datetime <- parse_git_datetime(file_path)
  df
}

load_history_data <- function(repo_root) {
  history_dir <- file.path(repo_root, "Data", "all_arbs_history")
  files <- list.files(history_dir, pattern = "\\.rds$", full.names = TRUE) |>
    sort()
  if (length(files) == 0) {
    stop("No .rds files found in Data/all_arbs_history.")
  }

  history_limit <- suppressWarnings(as.integer(Sys.getenv("TOP_DOWN_APP_HISTORY_LIMIT", "")))
  if (!is.na(history_limit) && history_limit > 0 && length(files) > history_limit) {
    files <- tail(files, history_limit)
  }

  detected_cores <- suppressWarnings(as.integer(parallel::detectCores(logical = TRUE)))
  if (is.na(detected_cores) || detected_cores < 1L) {
    detected_cores <- 1L
  }
  workers <- max(1L, detected_cores - 1L)

  rows <- if (workers > 1L) {
    tryCatch(
      {
        cl <- parallel::makeCluster(workers)
        on.exit(parallel::stopCluster(cl), add = TRUE)
        parallel::clusterEvalQ(cl, {
          library(readr)
          library(stringr)
          NULL
        })
        parallel::clusterExport(
          cl,
          varlist = c("parse_git_datetime", "read_history_snapshot"),
          envir = environment()
        )
        parallel::parLapply(cl, files, read_history_snapshot)
      },
      error = function(e) {
        lapply(files, read_history_snapshot)
      }
    )
  } else {
    lapply(files, read_history_snapshot)
  }

  list(
    data = bind_rows(rows),
    file_count = length(files),
    workers_used = workers
  )
}

prepare_analysis_data <- function(repo_root) {
  history_bundle <- load_history_data(repo_root)

  combined <- history_bundle$data |>
    mutate(game_date = as_date(git_datetime)) |>
    filter(hour(git_datetime) >= 8 & hour(git_datetime) < 14) |>
    filter(margin > 0)

  results_path <- file.path(repo_root, "Data", "all_player_stats_2025-2026.csv")
  if (!file.exists(results_path)) {
    stop("Missing Data/all_player_stats_2025-2026.csv")
  }

  all_results <- read_csv(results_path, show_col_types = FALSE) |>
    select(firstName, familyName, GAME_DATE, HOME_TEAM, AWAY_TEAM, points, assists, blocks, steals, threePointersMade, reboundsTotal) |>
    mutate(
      player_name = paste(firstName, familyName),
      GAME_DATE = as_date(GAME_DATE) + 1,
      PRA = points + reboundsTotal + assists
    )

  analysis_data <- combined |>
    left_join(all_results, by = c("player_name", "game_date" = "GAME_DATE")) |>
    mutate(
      actual_stat = case_match(
        market_name,
        "Player Points" ~ points,
        "Player Assists" ~ assists,
        "Player Rebounds" ~ reboundsTotal,
        "Player Blocks" ~ blocks,
        "Player Steals" ~ steals,
        "Player Threes" ~ threePointersMade,
        "Player PRAs" ~ PRA,
        .default = NA_real_
      ),
      outcome = case_when(
        actual_stat > line ~ "over",
        actual_stat < line ~ "under",
        actual_stat == line ~ "push"
      )
    ) |>
    filter(!is.na(outcome)) |>
    distinct(
      match,
      game_date,
      market_name,
      player_name,
      line,
      over_agency,
      under_agency,
      actual_stat,
      outcome,
      .keep_all = TRUE
    ) |>
    filter(!is.na(over_price), !is.na(under_price), over_price > 0, under_price > 0) |>
    mutate(
      stake_denominator = over_price + under_price,
      over_stake = 100 * under_price / stake_denominator,
      under_stake = 100 * over_price / stake_denominator,
      over_profit = case_when(
        outcome == "over" ~ over_stake * (over_price - 1),
        outcome == "under" ~ -over_stake,
        outcome == "push" ~ 0
      ),
      under_profit = case_when(
        outcome == "under" ~ under_stake * (under_price - 1),
        outcome == "over" ~ -under_stake,
        outcome == "push" ~ 0
      ),
      total_profit = over_profit + under_profit,
      total_stake = over_stake + under_stake,
      agency_combo = paste(over_agency, "vs", under_agency)
    ) |>
    filter(over_agency != under_agency) |>
    filter(over_agency != "Dabble Pickem", under_agency != "Dabble Pickem") |>
    arrange(desc(game_date), desc(margin))

  if (nrow(analysis_data) == 0) {
    stop("No rows available for analysis after join and filtering.")
  }

  list(
    data = analysis_data,
    meta = history_bundle
  )
}

summarise_totals <- function(df) {
  totals <- df |>
    summarise(
      bets = n(),
      over_stake = sum(over_stake, na.rm = TRUE),
      over_profit = sum(over_profit, na.rm = TRUE),
      under_stake = sum(under_stake, na.rm = TRUE),
      under_profit = sum(under_profit, na.rm = TRUE),
      total_stake = sum(total_stake, na.rm = TRUE),
      total_profit = sum(total_profit, na.rm = TRUE)
    ) |>
    mutate(
      over_roi_pct = safe_pct(over_profit, over_stake),
      under_roi_pct = safe_pct(under_profit, under_stake),
      total_roi_pct = safe_pct(total_profit, total_stake)
    )

  totals
}

summarise_grouped <- function(df, group_cols) {
  df |>
    group_by(across(all_of(group_cols))) |>
    summarise(
      bets = n(),
      over_stake = sum(over_stake, na.rm = TRUE),
      over_profit = sum(over_profit, na.rm = TRUE),
      under_stake = sum(under_stake, na.rm = TRUE),
      under_profit = sum(under_profit, na.rm = TRUE),
      total_stake = sum(total_stake, na.rm = TRUE),
      total_profit = sum(total_profit, na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(
      over_roi_pct = safe_pct(over_profit, over_stake),
      under_roi_pct = safe_pct(under_profit, under_stake),
      total_roi_pct = safe_pct(total_profit, total_stake)
    )
}

repo_root <- find_repo_root()
analysis_bundle <- prepare_analysis_data(repo_root)
analysis_data <- analysis_bundle$data

all_agencies <- sort(unique(c(analysis_data$over_agency, analysis_data$under_agency)))
all_markets <- sort(unique(analysis_data$market_name))
min_date <- min(analysis_data$game_date, na.rm = TRUE)
max_date <- max(analysis_data$game_date, na.rm = TRUE)
default_start_date <- max(as.Date("2025-10-01"), min_date)

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .summary-note { margin-top: 8px; color: #4c617d; }
      .small-muted { color: #6b7280; font-size: 12px; }
    "))
  ),
  titlePanel("Top-Down Performance Dashboard (Shiny)"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("Filters"),
      dateRangeInput(
        inputId = "date_range",
        label = "Game Date Range",
        start = default_start_date,
        end = max_date,
        min = min_date,
        max = max_date
      ),
      selectizeInput(
        inputId = "over_agencies",
        label = "Over Agencies",
        choices = all_agencies,
        selected = all_agencies,
        multiple = TRUE
      ),
      selectizeInput(
        inputId = "under_agencies",
        label = "Under Agencies",
        choices = all_agencies,
        selected = all_agencies,
        multiple = TRUE
      ),
      selectizeInput(
        inputId = "market_names",
        label = "Markets",
        choices = all_markets,
        selected = all_markets,
        multiple = TRUE
      ),
      numericInput("margin_min", "Margin Lower", value = 0, step = 0.1),
      numericInput("margin_max", "Margin Upper", value = 10, step = 0.1),
      checkboxInput(
        inputId = "sportsbet_only",
        label = "Sportsbet-only rows (match original script logic)",
        value = TRUE
      ),
      actionButton("reset_filters", "Reset Filters"),
      div(
        class = "summary-note",
        span(
          class = "small-muted",
          sprintf(
            "Loaded %s snapshot files using %s worker(s).",
            comma(analysis_bundle$meta$file_count),
            analysis_bundle$meta$workers_used
          )
        )
      )
    ),
    mainPanel(
      width = 9,
      tabsetPanel(
        tabPanel(
          "Full Summary",
          br(),
          h4("Summary (Current Date Range + Filters)"),
          DTOutput("summary_table"),
          br(),
          h4("Summary by Agency Combination"),
          DTOutput("combo_summary_table"),
          br(),
          h4("Summary by Market"),
          DTOutput("market_summary_table")
        ),
        tabPanel(
          "Daily Details",
          br(),
          h4("Daily Details (Stake, Over ROI, Under ROI, Total ROI)"),
          DTOutput("daily_details_table")
        ),
        tabPanel(
          "Bet-Level Details",
          br(),
          DTOutput("bet_details_table")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  observeEvent(input$reset_filters, {
    updateDateRangeInput(
      session,
      inputId = "date_range",
      start = default_start_date,
      end = max_date
    )
    updateSelectizeInput(session, "over_agencies", selected = all_agencies)
    updateSelectizeInput(session, "under_agencies", selected = all_agencies)
    updateSelectizeInput(session, "market_names", selected = all_markets)
    updateNumericInput(session, "margin_min", value = 0)
    updateNumericInput(session, "margin_max", value = 10)
    updateCheckboxInput(session, "sportsbet_only", value = TRUE)
  })

  filtered_data <- reactive({
    req(input$date_range, input$over_agencies, input$under_agencies, input$market_names)

    if (length(input$over_agencies) == 0 || length(input$under_agencies) == 0 || length(input$market_names) == 0) {
      return(analysis_data[0, ])
    }

    lower_margin <- input$margin_min %||% 0
    upper_margin <- input$margin_max %||% 10
    if (is.na(lower_margin)) lower_margin <- 0
    if (is.na(upper_margin)) upper_margin <- 10
    if (lower_margin > upper_margin) {
      temp <- lower_margin
      lower_margin <- upper_margin
      upper_margin <- temp
    }

    df <- analysis_data |>
      filter(game_date >= input$date_range[1], game_date <= input$date_range[2]) |>
      filter(over_agency %in% input$over_agencies) |>
      filter(under_agency %in% input$under_agencies) |>
      filter(market_name %in% input$market_names) |>
      filter(margin >= lower_margin, margin <= upper_margin)

    if (isTRUE(input$sportsbet_only)) {
      df <- df |>
        filter(over_agency == "Sportsbet" | under_agency == "Sportsbet")
    }

    df
  })

  summary_table_data <- reactive({
    summarise_totals(filtered_data())
  })

  combo_summary_data <- reactive({
    summarise_grouped(filtered_data(), c("over_agency", "under_agency")) |>
      mutate(agency_combo = paste(over_agency, "vs", under_agency)) |>
      select(
        agency_combo,
        bets,
        over_stake,
        over_profit,
        over_roi_pct,
        under_stake,
        under_profit,
        under_roi_pct,
        total_stake,
        total_profit,
        total_roi_pct
      ) |>
      arrange(desc(total_roi_pct))
  })

  market_summary_data <- reactive({
    summarise_grouped(filtered_data(), "market_name") |>
      arrange(desc(total_roi_pct))
  })

  daily_details_data <- reactive({
    summarise_grouped(filtered_data(), "game_date") |>
      arrange(desc(game_date))
  })

  output$summary_table <- renderDT({
    datatable(
      summary_table_data(),
      rownames = FALSE,
      options = list(dom = "t", paging = FALSE, ordering = FALSE, scrollX = TRUE)
    ) |>
      formatRound(c("over_stake", "over_profit", "under_stake", "under_profit", "total_stake", "total_profit"), 2) |>
      formatRound(c("over_roi_pct", "under_roi_pct", "total_roi_pct"), 2)
  })

  output$combo_summary_table <- renderDT({
    datatable(
      combo_summary_data(),
      rownames = FALSE,
      options = list(pageLength = 15, scrollX = TRUE)
    ) |>
      formatRound(c("over_stake", "over_profit", "under_stake", "under_profit", "total_stake", "total_profit"), 2) |>
      formatRound(c("over_roi_pct", "under_roi_pct", "total_roi_pct"), 2)
  })

  output$market_summary_table <- renderDT({
    datatable(
      market_summary_data(),
      rownames = FALSE,
      options = list(pageLength = 15, scrollX = TRUE)
    ) |>
      formatRound(c("over_stake", "over_profit", "under_stake", "under_profit", "total_stake", "total_profit"), 2) |>
      formatRound(c("over_roi_pct", "under_roi_pct", "total_roi_pct"), 2)
  })

  output$daily_details_table <- renderDT({
    datatable(
      daily_details_data(),
      rownames = FALSE,
      options = list(pageLength = 20, scrollX = TRUE)
    ) |>
      formatRound(c("over_stake", "over_profit", "under_stake", "under_profit", "total_stake", "total_profit"), 2) |>
      formatRound(c("over_roi_pct", "under_roi_pct", "total_roi_pct"), 2)
  })

  output$bet_details_table <- renderDT({
    bet_df <- filtered_data() |>
      select(
        game_date,
        match,
        player_name,
        market_name,
        line,
        over_agency,
        over_price,
        under_agency,
        under_price,
        margin,
        outcome,
        over_stake,
        over_profit,
        under_stake,
        under_profit,
        total_stake,
        total_profit
      ) |>
      arrange(desc(game_date), desc(margin))

    datatable(
      bet_df,
      rownames = FALSE,
      options = list(pageLength = 25, scrollX = TRUE)
    ) |>
      formatRound(c("line", "over_price", "under_price", "margin", "over_stake", "over_profit", "under_stake", "under_profit", "total_stake", "total_profit"), 2)
  })
}

shinyApp(ui, server)
