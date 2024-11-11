library(shiny)
library(bslib)
library(gridlayout)
library(DT)
library(tidyverse)

# Function to convert time to decimal-------------------------------------------
convert_time_to_decimal_hms <- function(time_obj) {
  # Convert to hms object
  time_obj <- hms(time_obj)
  
  # Extract hours and minutes
  hours <- hour(time_obj)
  minutes <- minute(time_obj)
  
  # Convert to decimal
  decimal_time <- hours + (minutes / 60)
  return(decimal_time)
}

# Function to get correlation between players-----------------------------------
get_player_correlation <- function(seasons = NULL, name_a, name_b, metric_a, metric_b) {
  # Column names for later use
  col_name_a <- paste0(name_a, " ", metric_a)
  col_name_b <- paste0(name_b, " ", metric_b)
  
  # Get dataframe for player A
  df_player_a <- 
    all_player_stats %>%
    filter(PLAYER_NAME == name_a & SEASON_YEAR %in% seasons) |> 
    select(gameId, PLAYER_NAME, all_of(metric_a)) |> 
    rename(!!col_name_a := all_of(metric_a))
  
  # Get dataframe for player B
  df_player_b <- 
    all_player_stats %>%
    filter(PLAYER_NAME == name_b & SEASON_YEAR %in% seasons) |> 
    select(gameId, PLAYER_NAME, all_of(metric_b)) |> 
    rename(!!col_name_b := all_of(metric_b))
  
  # Merge the two dataframes
  df_merged <- inner_join(df_player_a, df_player_b, by = "gameId")
  
  # Compute correlation
  correlation <- cor(df_merged[[col_name_a]], df_merged[[col_name_b]], method = "pearson")
  cat(sprintf("The correlation between %s and %s is: %f\n", col_name_a, col_name_b, correlation))
  
  # Create plot
  ggplot(df_merged, aes(x = .data[[col_name_a]], y = .data[[col_name_b]])) +
    geom_point(color = "#3498db", alpha = 0.6, size = 3) +
    geom_smooth(method = "lm", se = FALSE, color = "#e74c3c", linetype = "dashed") +
    labs(
      x = col_name_a, 
      y = col_name_b,
      title = "Player Performance Correlation",
      subtitle = sprintf("Correlation between %s and %s", col_name_a, col_name_b),
      caption = sprintf("Pearson's r: %.2f", correlation)
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5, color = "grey50"),
      plot.caption = element_text(hjust = 1, color = "grey50"),
      text = element_text(size = 12),
      axis.title = element_text(face = "bold"),
      legend.position = "none"
    ) +
    annotate(
      "text", x = max(df_merged[[col_name_a]]), y = min(df_merged[[col_name_b]]), 
      label = sprintf("r = %.2f", correlation), 
      hjust = 1, vjust = 0, size = 5, color = "red1", fontface = "italic"
    )
}

# Function to compare player performance w or w/o teammate----------------------
compare_performance <- function(seasons = NULL, name, teammate_name, metric) {
  # Filter the data for games with the main player
  df_player <-
    all_player_stats %>%
    filter(PLAYER_NAME == name) %>%
    filter(SEASON_YEAR %in% seasons)
  
  # Find the game IDs where the teammate also played
  games_with_teammate <-
    all_player_stats %>%
    filter(SEASON_YEAR %in% seasons) %>%
    filter(PLAYER_NAME == teammate_name) %>% pull(gameId)
  
  # Label each game as 'With Teammate' or 'Without Teammate'
  df_player <- df_player %>% 
    mutate(Teammate = if_else(gameId %in% games_with_teammate, 'With Teammate', 'Without Teammate'))
  
  # Calculate mean and count for both conditions
  summary_stats <- df_player %>% group_by(Teammate) %>% summarise(mean_val = mean(!!sym(metric), na.rm = TRUE), n_games = n())
  
  # Create the violin plot
  plot <- ggplot(df_player, aes(x = Teammate, y = !!sym(metric), fill = Teammate)) +
    geom_violin(trim = FALSE, position = position_dodge(width = 0.9)) +
    geom_boxplot(width = 0.1, position = position_dodge(width = 0.9)) +
    labs(title = paste("Performance of", name, "with and without", teammate_name),
         x = "Condition",
         y = metric) +
    scale_fill_manual(values = c("Without Teammate" = "orange1", "With Teammate" = "royalblue1")) +
    annotate("text", x = Inf, y = Inf, 
             label = paste("With Teammate: ", summary_stats$n_games[summary_stats$Teammate == "With Teammate"], 
                           " games, Mean ", round(summary_stats$mean_val[summary_stats$Teammate == "With Teammate"], 2), "\n",
                           "Without Teammate: ", summary_stats$n_games[summary_stats$Teammate == "Without Teammate"], 
                           " games, Mean ", round(summary_stats$mean_val[summary_stats$Teammate == "Without Teammate"], 2)), 
             hjust = 1, vjust = 1) +
    theme_minimal()
  
  return(plot)
}

#===============================================================================
# Read in Data
#===============================================================================

# Player Info
all_rosters <- read_csv("../../Data/all_rosters.csv")
all_teams <- read_csv("../../Data/all_teams.csv")
all_player_stats_2021_2022 <- read_csv("../../Data/all_player_stats_2021-2022.csv") |> mutate(SEASON_YEAR = "2021-22")
all_player_stats_2022_2023 <- read_csv("../../Data/all_player_stats_2022-2023.csv") |> mutate(SEASON_YEAR = "2022-23")
all_player_stats_2023_2024 <- read_csv("../../Data/all_player_stats_2023-2024.csv") |> mutate(SEASON_YEAR = "2023-24")
all_player_stats_2024_2025 <- read_csv("../../Data/all_player_stats_2024-2025.csv") |> mutate(SEASON_YEAR = "2024-25")

# Team Info
all_team_stats_2021_2022 <- read_csv("../../Data/advanced_box_scores_2021-2022.csv") |> mutate(SEASON_YEAR = "2021-22")
all_team_stats_2022_2023 <- read_csv("../../Data/advanced_box_scores_2022-2023.csv") |> mutate(SEASON_YEAR = "2022-23")
all_team_stats_2023_2024 <- read_csv("../../Data/advanced_box_scores_2023-2024.csv") |> mutate(SEASON_YEAR = "2023-24")
all_team_stats_2024_2025 <- read_csv("../../Data/advanced_box_scores_2024-2025.csv") |> mutate(SEASON_YEAR = "2024-25")

# Player Tracker Data
all_player_tracking_2023_2024 <- read_csv("../../Data/player_track_box_scores_2023-2024.csv") |> mutate(SEASON_YEAR = "2023-24")
all_player_tracking_2024_2025 <- read_csv("../../Data/player_track_box_scores_2024-2025.csv") |> mutate(SEASON_YEAR = "2024-25")

# Combine player stats
all_player_stats <-
  all_player_stats_2024_2025 |>
  bind_rows(all_player_stats_2023_2024) |>
  bind_rows(all_player_stats_2022_2023) |>
  bind_rows(all_player_stats_2021_2022) |>
  left_join(all_rosters[c("PLAYER", "PLAYER_ID")], by = c("personId" = "PLAYER_ID")) |> 
  mutate(PRA = points + reboundsTotal + assists) |>
  rename(PLAYER_NAME = PLAYER,
         PTS = points,
         REB = reboundsTotal,
         AST = assists,
         STL = steals,
         BLK = blocks) |> 
  mutate(MIN = convert_time_to_decimal_hms(minutes)) |> 
  mutate(MIN = round(MIN, 2)) |> 
  relocate(MIN, .after = minutes)

# Get Game Dates DF
game_dates <-
  all_player_stats |> 
  distinct(gameId, GAME_DATE)

# Get Home Teams DF
home_teams <-
  all_player_stats |> 
  distinct(gameId, HOME_TEAM)

# Get Away Teams DF
away_teams <-
  all_player_stats |> 
  distinct(gameId, AWAY_TEAM)

# Combine team stats
all_team_stats <-
  all_team_stats_2024_2025 |>
  bind_rows(all_team_stats_2023_2024) |> 
  bind_rows(all_team_stats_2022_2023) |>
  bind_rows(all_team_stats_2021_2022) |>
  left_join(game_dates) |>
  left_join(home_teams) |> 
  left_join(away_teams) |>
  filter(!is.na(GAME_DATE)) |>
  transmute(
    gameId,
    teamId,
    teamName = paste(teamCity, teamName),
    homeTeam = HOME_TEAM,
    awayTeam = AWAY_TEAM,
    oppositionTeam = if_else(teamName == HOME_TEAM, AWAY_TEAM, HOME_TEAM),
    date = GAME_DATE,
    minutes,
    possessions,
    pacePer40,
    offensiveRating,
    defensiveRating,
    netRating,
    assistPercentage,
    defensiveReboundPercentage,
    offensiveReboundPercentage,
    reboundPercentage,
    trueShootingPercentage,
    effectiveFieldGoalPercentage,
    season = SEASON_YEAR)

# Create Home / Away variable
all_player_stats <-
  all_player_stats |>
  mutate(team_full = paste(teamCity, teamName)) |>
  mutate(home_away = if_else(team_full == HOME_TEAM, "Home", "Away"))

# Determine the operating system
os_type <- Sys.info()["sysname"]

# # Google sheets authentication -------------------------------------------------
# options(gargle_oauth_cache = ".secrets")
# drive_auth(cache = ".secrets", email = "cuzzy.punting@gmail.com")
# gs4_auth(token = drive_token())

# Conditional logic for loading data based on OS
if (TRUE) {
  # Read RDS Data for Windows
  player_points_data <- read_rds("../../Data/processed_odds/all_player_points.rds")
  player_assists_data <- read_rds("../../Data/processed_odds/all_player_assists.rds")
  player_rebounds_data <- read_rds("../../Data/processed_odds/all_player_rebounds.rds")
  player_pras_data <- read_rds("../../Data/processed_odds/all_player_pras.rds")
  player_steals_data <- read_rds("../../Data/processed_odds/all_player_steals.rds")
  player_threes_data <- read_rds("../../Data/processed_odds/all_player_threes.rds")
  player_blocks_data <- read_rds("../../Data/processed_odds/all_player_blocks.rds")
} else {
  # Google Sheets Data for other OS
  ss_name <- gs4_find("NBA Data")
  player_points_data <- read_sheet(ss = ss_name, sheet = "Player Points")
  player_assists_data <- read_sheet(ss = ss_name, sheet = "Player Assists")
  player_rebounds_data <- read_sheet(ss = ss_name, sheet = "Player Rebounds")
  player_pras_data <- read_sheet(ss = ss_name, sheet = "Player PRAs")
  player_steals_data <- read_sheet(ss = ss_name, sheet = "Player Steals")
  player_threes_data <- read_sheet(ss = ss_name, sheet = "Player Threes")
  player_blocks_data <- read_sheet(ss = ss_name, sheet = "Player Blocks")
}

# Add opposition defensive rating-----------------------------------------------

# Get defensive rating in last 5 games
def_rating_last_10 <-
  all_team_stats |> 
  arrange(teamName, desc(date)) |>
  group_by(teamName) |>
  slice_head(n = 10) |>
  summarise(def_rating = mean(defensiveRating, na.rm = TRUE)) |> 
  mutate(def_rating = round((def_rating - min(def_rating)) / (max(def_rating) - min(def_rating)) * 100, digits = 1))

# Get Pace per 40 vs opposition in last 5 games
pace_per_40_last_10 <-
  all_team_stats |> 
  arrange(oppositionTeam, desc(date)) |>
  group_by(oppositionTeam) |>
  slice_head(n = 10) |>
  summarise(pace_per_40 = mean(pacePer40, na.rm = TRUE)) |> 
  mutate(pace_per_40 = round((pace_per_40 - min(pace_per_40)) / (max(pace_per_40) - min(pace_per_40)) * 100, digits = 1))

# Add to player points
player_points_data <-
  player_points_data |> 
  left_join(def_rating_last_10, by = c("opposition_team" = "teamName")) |>
  left_join(pace_per_40_last_10, by = c("opposition_team" = "oppositionTeam"))

# Add to player assists
player_assists_data <-
  player_assists_data |> 
  left_join(def_rating_last_10, by = c("opposition_team" = "teamName")) |>
  left_join(pace_per_40_last_10, by = c("opposition_team" = "oppositionTeam"))

# Add to player rebounds
player_rebounds_data <-
  player_rebounds_data |> 
  left_join(def_rating_last_10, by = c("opposition_team" = "teamName")) |>
  left_join(pace_per_40_last_10, by = c("opposition_team" = "oppositionTeam"))

# Add to player pras
player_pras_data <-
  player_pras_data |> 
  left_join(def_rating_last_10, by = c("opposition_team" = "teamName")) |>
  left_join(pace_per_40_last_10, by = c("opposition_team" = "oppositionTeam"))

# Add to player blocks
player_blocks_data <-
  player_blocks_data |> 
  left_join(def_rating_last_10, by = c("opposition_team" = "teamName")) |>
  left_join(pace_per_40_last_10, by = c("opposition_team" = "oppositionTeam"))

# Add to player steals
player_steals_data <-
  player_steals_data |> 
  left_join(def_rating_last_10, by = c("opposition_team" = "teamName")) |>
  left_join(pace_per_40_last_10, by = c("opposition_team" = "oppositionTeam"))

# Add to player threes
player_threes_data <-
  player_threes_data |> 
  left_join(def_rating_last_10, by = c("opposition_team" = "teamName")) |>
  left_join(pace_per_40_last_10, by = c("opposition_team" = "oppositionTeam"))

#===============================================================================
# UI
#===============================================================================

ui <- page_navbar(
  title = "NBA",
  selected = "Player Stats",
  collapsible = TRUE,
  theme = bslib::bs_theme(),
  tags$head(
    tags$style(HTML("
      .tab-content, .tab-pane {
        height: 1250px;
        overflow-y: auto;
      }
      .dataTables_wrapper {
        overflow-x: auto;
      }
    "))
  ),
  nav_panel(
    title = "Player Stats",
    grid_container(
      layout = c("nba_stats player_stat_plot"),
      row_sizes = c("1fr"),
      col_sizes = c("250px", "1fr"),
      gap_size = "10px",
      grid_card(
        area = "nba_stats",
        card_header("Settings"),
        card_body(
          textInput(
            inputId = "player_name_input_a",
            label = "Select Player:",
            value = "LeBron James"
          ),
          selectInput(
            inputId = "season_input_a",
            label = "Select Season:",
            choices = all_player_stats$SEASON_YEAR |> unique(),
            multiple = TRUE,
            selectize = TRUE,
            selected = c("2021-22",
                         "2022-23",
                         "2023-24",
                         "2024-25"
                         )
          ),
          selectInput(
            inputId = "stat_input_a",
            label = "Select Statistic:",
            choices = c("PTS",
                        "REB",
                        "AST",
                        "PRA",
                        "BLK",
                        "STL",
                        "MIN"),
            multiple = FALSE,
            selected = "PTS"
          ),
          checkboxGroupInput(
            inputId = "home_status",
            label = "Home / Away Games",
            choices = list("Home" = "Home", "Away" = "Away"),
            selected = c("Home", "Away")
          ),
          markdown(mds = c("__Select Only Last n Games:__")),
          numericInput(
            inputId = "last_games",
            label = "Number of Games",
            value = NA
          ),
          markdown(mds = c("__Select Reference Line:__")),
          numericInput(
            inputId = "reference_line",
            label = "Line Value",
            value = 19.5
          ),
          markdown(mds = c("__Select Minutes Range:__")),
          numericInput(
            inputId = "minutes_minimum",
            label = "Min Minutes",
            value = 0
          ),
          numericInput(
            inputId = "minutes_maximum",
            label = "Max Minutes",
            value = 60
          )
        )
      ),
      grid_card(area = "player_stat_plot",
                card_body(
                  tabsetPanel(
                    id = "stat_tabs",
                    tabPanel(
                      "Plot",
                      plotOutput(outputId = "plot", height = "800px")
                    ),
                    tabPanel(
                      "Table",
                      DTOutput(
                        outputId = "player_stat_table",
                        width = "100%",
                        height = "800px"
                      )
                    )
                  )
                ))
    )
  ),
  nav_panel(
    title = "Team Stats",
    grid_container(
      layout = c("nba_team_stats team_stat_table"),
      row_sizes = c("1fr"),
      col_sizes = c("250px",
                    "1fr"),
      gap_size = "10px",
      grid_card(area = "nba_team_stats",
                card_header("Settings"),
                card_body(
                  selectInput(
                    inputId = "season_input_b",
                    label = "Select Season:",
                    choices = all_team_stats$season |> unique(),
                    multiple = TRUE,
                    selectize = TRUE,
                    selected = all_team_stats$season |> unique()
                  ),
                  markdown(mds = c("__Select Only Last n Games:__")),
                  numericInput(
                    inputId = "last_games_team",
                    label = "Number of Games",
                    value = NA
                  ),
                )),
      grid_card(area = "team_stat_table",
                card_body(
                  DTOutput(outputId = "team_metric_table", width = "100%")
                ))
    )
  ),
  nav_panel(title = "Odds Screen",
            grid_container(
              layout = c("odds_screen odds_table"),
              row_sizes = c("1fr"),
              col_sizes = c("250px",
                            "1fr"),
              gap_size = "10px",
              grid_card(area = "odds_screen",
                        card_header("Settings"),
                        card_body(
                          selectInput(
                            inputId = "agency_input",
                            label = "Select Agencies:",
                            choices = player_points_data$agency |> unique(),
                            multiple = TRUE,
                            selectize = TRUE,
                            selected = player_points_data$agency |> unique(),
                          ),
                          selectInput(
                            inputId = "market_input",
                            label = "Select Market:",
                            choices = c("Points", "Rebounds", "Assists", "PRAs", "Blocks", "Steals", "Threes"),
                            multiple = FALSE
                          ),
                          selectInput(
                            inputId = "match_input",
                            label = "Select Matches:",
                            choices = player_points_data$match |> unique(),
                            multiple = TRUE,
                            selectize = FALSE,
                            selected = player_points_data$match |> unique()
                          ),
                          textInput(
                            inputId = "player_name_input_b",
                            label = "Select Player:",
                            value = NA
                          ),
                          checkboxInput(
                            inputId = "only_unders",
                            label = "Only Show Markets With Unders",
                            value = FALSE
                          ),
                          checkboxInput(
                            inputId = "only_best",
                            label = "Only Show Best Market Odds - Overs",
                            value = FALSE
                          ),
                          checkboxInput(
                            inputId = "only_best_unders",
                            label = "Only Show Best Market Odds - Unders",
                            value = FALSE
                          ),
                          markdown(mds = c("__Select Odds Range:__")),
                          numericInput(
                            inputId = "odds_minimum",
                            label = "Min Odds",
                            value = NA
                          ),
                          numericInput(
                            inputId = "odds_maximum",
                            label = "Max Odds",
                            value = NA
                          ),
                          markdown(mds = c("__Select Difference Range 2023:__")),
                          numericInput(
                            inputId = "diff_minimum_23",
                            label = "Min Diff",
                            value = NA
                          ),
                          numericInput(
                            inputId = "diff_maximum_23",
                            label = "Max Diff",
                            value = NA
                          ),
                          markdown(mds = c("__Select Difference Range 2024:__")),
                          numericInput(
                            inputId = "diff_minimum_24",
                            label = "Min Diff",
                            value = NA
                          ),
                          numericInput(
                            inputId = "diff_maximum_24",
                            label = "Max Diff",
                            value = NA
                          ),
                          markdown(mds = c("__Select Difference Range 2023 - Unders:__")),
                          numericInput(
                            inputId = "diff_minimum_23_unders",
                            label = "Min Diff",
                            value = NA
                          ),
                          numericInput(
                            inputId = "diff_maximum_23_unders",
                            label = "Max Diff",
                            value = NA
                          ),
                          markdown(mds = c("__Select Difference Range 2024 - Unders:__")),
                          numericInput(
                            inputId = "diff_minimum_24_unders",
                            label = "Min Diff",
                            value = NA
                          ),
                          numericInput(
                            inputId = "diff_maximum_24_unders",
                            label = "Max Diff",
                            value = NA
                          )
                        )),
              grid_card(area = "odds_table",
                        card_body(
                          DTOutput(outputId = "scraped_odds_table", height = "1000px")
                        ))
            )),
  nav_panel(
    title = "With / Without Teammate",
    grid_container(
      layout = c("with_without_settings with_without_plot"),
      row_sizes = c("1fr"),
      col_sizes = c("500px", "1fr"),
      gap_size = "10px",
      
      grid_card(
        area = "with_without_settings",
        card_header("Settings"),
        card_body(
          textInput(
            inputId = "player_name",
            label = "Select Player:",
            value = "LeBron James"
          ),
          textInput(
            inputId = "teammate_name",
            label = "Select Teammate:",
            value = "Anthony Davis"
          ),
          selectInput(
            inputId = "season_input",
            label = "Select Season:",
            choices = all_player_stats$SEASON_YEAR |> unique(),
            multiple = TRUE,
            selectize = TRUE
          ),
          selectInput(
            inputId = "metric_input",
            label = "Select Statistic:",
            choices = c("PTS", "REB", "AST", "BLK", "MIN"),
            multiple = FALSE,
            selected = "PTS"
          )
        )
      ),
      
      grid_card(area = "with_without_plot",
                card_body(
                  plotOutput(outputId = "with_without_plot_output", height = "800px", width = "50%")
                ))
    )
  ),
  nav_panel(
    title = "Player Correlations",
    grid_container(
      layout = c("corr_settings corr_plot"),
      row_sizes = c("1fr"),
      col_sizes = c("500px", "1fr"),
      gap_size = "10px",
      
      grid_card(
        area = "corr_settings",
        card_header("Settings"),
        card_body(
          textInput(
            inputId = "player_name_corr",
            label = "Select Player 1:",
            value = "LeBron James"
          ),
          selectInput(
            inputId = "metric_input_corr_a",
            label = "Select Statistic:",
            choices = c("PTS", "REB", "AST", "BLK", "STL", "MIN"),
            multiple = FALSE,
            selected = "PTS"
          ),
          textInput(
            inputId = "teammate_name_corr",
            label = "Select Player 2:",
            value = "Anthony Davis"
          ),
          selectInput(
            inputId = "metric_input_corr_b",
            label = "Select Statistic:",
            choices = c("PTS", "REB", "AST", "BLK", "STL", "MIN"),
            multiple = FALSE,
            selected = "PTS"
          ),
          selectInput(
            inputId = "season_input_corr",
            label = "Select Season:",
            choices = all_player_stats$SEASON_YEAR |> unique(),
            multiple = TRUE,
            selectize = TRUE
          )
        )
      ),
      
      grid_card(area = "corr_plot",
                card_body(
                  plotOutput(outputId = "corr_plot_output", height = "800px", width = "50%")
                ))
    )
  )
)


#===============================================================================
# Server
#===============================================================================

server <- function(input, output) {
  bs_themer()
  #=============================================================================
  # Filter player stats
  #=============================================================================
  
  filtered_player_stats <- reactive({
    # Filter player stats
    filtered_player_stats <-
      all_player_stats |>
      filter(
        PLAYER_NAME == input$player_name_input_a,
        SEASON_YEAR %in% input$season_input_a,
        MIN >= input$minutes_minimum,
        MIN <= input$minutes_maximum,
        home_away %in% input$home_status
      ) |>
      arrange(GAME_DATE) |>
      mutate(game_number = row_number()) |> 
      select(Date = GAME_DATE,
             Home = HOME_TEAM,
             Away = AWAY_TEAM,
             Player = PLAYER_NAME,
             Team = teamTricode,
             MIN,
             FGM = fieldGoalsMade,
             FGA = fieldGoalsAttempted,
             FG_PCT = fieldGoalsPercentage,
             FG3M = threePointersMade,
             FG3A = threePointersAttempted,
             FG3_PCT = threePointersPercentage,
             FTM = freeThrowsMade,
             FTA = freeThrowsAttempted,
             FT_PCT = freeThrowsPercentage,
             PTS,
             REB,
             AST,
             PRA,
             BLK,
             STL,
             game_number) |> 
      arrange(desc(Date))
             
    # Filter by last n games
    if (!is.na(input$last_games)) {
      filtered_player_stats <-
        filtered_player_stats |>
        slice_head(n = input$last_games)
    }
    
    # Return filtered player stats
    return(filtered_player_stats)
    
  })
  
  #=============================================================================
  # Get Proportion above reference line
  #=============================================================================
  
  proportion_above_reference_line <- reactive({
    # Get proportion above reference line
    proportion_above_reference_line <-
      filtered_player_stats() |>
      filter(!!sym(input$stat_input_a) >= input$reference_line) |>
      nrow() / nrow(filtered_player_stats())
    
    # Get implied Odds
    implied_odds <- 1 / proportion_above_reference_line
    implied_odds_under <- 1 / (1 - proportion_above_reference_line)
    
    # Get string to output
    output_string <- paste0(
      "Proportion Above Reference Line: ",
      round(proportion_above_reference_line, 2),
      "\n",
      "Implied Odds - Over: ",
      round(implied_odds, 2),
      "\n",
      "Implied Odds - Under: ",
      round(implied_odds_under, 2),
      "\n",
      "Sample Size: ",
      nrow(filtered_player_stats())
    )
    
    return(output_string)
    
  })
  
  #=============================================================================
  # Plot player stats
  #=============================================================================
  
  output$plot <- renderPlot({
    # Create a new variable that checks if the y-value is above the reference line
    df_with_color <- filtered_player_stats() %>%
      mutate(color_condition = ifelse(
        !!sym(input$stat_input_a) >= input$reference_line,
        "limegreen",
        "red1"
      ))
    
    # Plot player stats
    p <- df_with_color %>%
      ggplot(aes(
        x = game_number,
        y = !!sym(input$stat_input_a),
        color = color_condition
      )) +
      
      # Basic Elements
      geom_point(size = 3) +
      geom_smooth(
        method = "loess",
        se = TRUE,
        inherit.aes = FALSE,
        mapping = aes(x = game_number, y = !!sym(input$stat_input_a))
      ) +
      geom_hline(
        yintercept = input$reference_line,
        linetype = "dashed",
        color = "grey4",
        size = 1
      )+
      
      # Add text
      annotate(
        geom = "text",
        x = 1,
        y = max(filtered_player_stats() %>% pull(!!sym(
          input$stat_input_a
        ))),
        label = proportion_above_reference_line(),
        hjust = 0,
        vjust = 1,
        color = "black",
        size = 6
      ) +
      
      # Aesthetics
      theme_bw() +
      theme(
        plot.background = element_rect(fill = "white", colour = "white"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12)
      ) +
      
      # Labels & Titles
      labs(title = "",
           x = "Game Number") +
      
      # Set manual color scale
      scale_color_identity() +
      
      # Additional
      theme(legend.position = "none")
    
    print(p)
  })
  
  #=============================================================================
  # Table player stats
  #=============================================================================
  
  output$player_stat_table <- renderDT({
    datatable(
      filtered_player_stats(),
      options = list(pageLength = 15, autoWidth = TRUE, scrollX = TRUE, scrollY = TRUE),
      width = "100%",
      height = "800px"
    )
  })
  
  #=============================================================================
  # Filter team stats
  #=============================================================================
  
  # Reactive function to filter team stats
  filtered_team_stats <- reactive({
    
    # Filter team stats
    filtered_team_stats <-
      all_team_stats |>
      filter(season %in% input$season_input_b)
  
      # Filter by last n games
      if (!is.na(input$last_games_team)) {
        filtered_team_stats <-
          filtered_team_stats |>
          group_by(teamId) |> 
          slice_head(n = input$last_games_team) |> 
          ungroup()
      }
      
      # Summarise stats
      filtered_team_stats <-
        filtered_team_stats |>
        select(
          teamName,
          possessions,
          pacePer40,
          offensiveRating,
          defensiveRating,
          netRating,
          assistPercentage,
          defensiveReboundPercentage,
          offensiveReboundPercentage,
          reboundPercentage,
          trueShootingPercentage,
          effectiveFieldGoalPercentage
        ) |> 
      group_by(teamName) |>
        summarise(across(.cols = where(is.numeric),
                         .fns = list(mean = mean))) |> 
        mutate(across(.cols = where(is.numeric), .fns = round, 2))
      
      # Return filtered team stats
      return(filtered_team_stats)
    
  })
  
  #=============================================================================
  # Table team stats
  #=============================================================================
  
  output$team_metric_table <- renderDT({
    datatable(
      filtered_team_stats(),
      options = list(pageLength = 15, autoWidth = TRUE),
      width = "100%",
      height = "800px"
    )
  })
  
  #=============================================================================
  # Table Odds
  #=============================================================================
  
  # Reactive function to scrape odds
  scraped_odds <- reactive({
    # Get odds---------------------------------------------------------------
    
    # Points
    if (input$market_input == "Points") {
      odds <-
        player_points_data |> 
        mutate(variation = round(variation, 2)) |>
        filter(agency %in% input$agency_input) |> 
        filter(match %in% input$match_input) |>
        select(-match)
    }
    
    # Rebounds
    if (input$market_input == "Rebounds") {
      odds <-
        player_rebounds_data |> 
        mutate(variation = round(variation, 2)) |>
        filter(agency %in% input$agency_input) |> 
        filter(match %in% input$match_input) |>

        select(-match) 
    }
    
    # Assists
    if (input$market_input == "Assists") {
      odds <-
        player_assists_data |> 
        mutate(variation = round(variation, 2)) |>
        filter(agency %in% input$agency_input) |> 
        filter(match %in% input$match_input) |>
        select(-match)
    }
    
    # Blocks
    if (input$market_input == "Blocks") {
      odds <-
        player_blocks_data |> 
        mutate(variation = round(variation, 2)) |>
        filter(agency %in% input$agency_input) |> 
        filter(match %in% input$match_input) |>
        select(-match)
    }
    
    # Steals
    if (input$market_input == "Steals") {
      odds <-
        player_steals_data |> 
        mutate(variation = round(variation, 2)) |>
        filter(agency %in% input$agency_input) |> 
        filter(match %in% input$match_input) |>
        select(-match)
    }
    
    # Threes
    if (input$market_input == "Threes") {
      odds <-
        player_threes_data |> 
        mutate(variation = round(variation, 2)) |>
        filter(agency %in% input$agency_input) |> 
        filter(match %in% input$match_input) |>
        select(-match)
    }
    
    # PRAs
    if (input$market_input == "PRAs") {
      odds <-
        player_pras_data |> 
        mutate(variation = round(variation, 2)) |>
        filter(agency %in% input$agency_input) |> 
        filter(match %in% input$match_input) |>
        select(-match)
    }
    
    if (input$only_best == TRUE) {
      odds <-
        odds |> 
        arrange(player_name, line, desc(over_price)) |>
        group_by(player_name, line) |> 
        slice_head(n = 1) |>
        ungroup()
    }
    
    if (input$only_best_unders == TRUE) {
      odds <-
        odds |> 
        arrange(player_name, line, desc(under_price)) |>
        group_by(player_name, line) |> 
        slice_head(n = 1) |>
        ungroup()
    }
    
    # Min and max differences
    if (!is.na(input$diff_minimum_24)) {
      odds <-
        odds |>
        filter(diff_over_2022_23 >= input$diff_minimum_24)
    }
    
    if (!is.na(input$diff_maximum_24)) {
      odds <-
        odds |>
        filter(diff_over_2022_23 <= input$diff_maximum_24)
    }
    
    if (!is.na(input$diff_minimum_23)) {
      odds <-
        odds |>
        filter(diff_over_2023_24 >= input$diff_minimum_23)
    }
    
    if (!is.na(input$diff_maximum_23)) {
      odds <-
        odds |>
        filter(diff_over_2023_24 <= input$diff_maximum_23)
    }
    
    if (!is.na(input$diff_minimum_23_unders)) {
      odds <-
        odds |>
        filter(diff_under_2023_24 >= input$diff_minimum_23_unders)
    }
    
    if (!is.na(input$diff_maximum_23_unders)) {
      odds <-
        odds |>
        filter(diff_under_2023_24 <= input$diff_maximum_23_unders)
    }
    
    if (!is.na(input$diff_minimum_24_unders)) {
      odds <-
        odds |>
        filter(diff_under_2022_23 >= input$diff_minimum_24_unders)
    }
    
    if (!is.na(input$diff_maximum_24_unders)) {
      odds <-
        odds |>
        filter(diff_under_2022_23 <= input$diff_maximum_24_unders)
    }
    
    # Odds Range
        if (!is.na(input$odds_minimum)) {
      odds <-
        odds |>
        filter(over_price >= input$odds_minimum)
    }
    
    if (!is.na(input$odds_maximum)) {
      odds <-
        odds |>
        filter(over_price <= input$odds_maximum)
    }
    
    if (input$only_unders == TRUE) {
      odds <-
        odds |>
        filter(!is.na(under_price))
    }
    
    if (input$player_name_input_b != "") {
      odds <-
        odds |>
        filter(str_detect(player_name, input$player_name_input_b))
    }
      
    # Remove columns we don't want
    odds <-
      odds |> 
      select(-group_by_header, -outcome_name, -outcome_name_under, -EventKey, -MarketKey, -OutcomeKey, -OutcomeKey_unders) |> 
      relocate(agency, .after = player_name)
    
    # Return odds
    return(odds)
  })
  
  # Table output
  output$scraped_odds_table <- renderDT({
    datatable(scraped_odds(),
              fillContainer = TRUE,
              filter = "top",
              options = list(
                pageLength = 17,
                autoWidth = FALSE,
                scrollX = TRUE, scrollY = TRUE,
                lengthMenu = c(5, 10, 15, 20, 25, 30)
              ))
  })
  
  #=============================================================================
  # With / Without Teammate
  #=============================================================================
  
  output$with_without_plot_output <- renderPlot({
    req(input$player_name, input$teammate_name, input$season_input, input$metric_input)
    
    plot <- compare_performance(season = input$season_input,
                                name = input$player_name,
                                teammate_name = input$teammate_name,
                                metric = input$metric_input)
    
    return(plot)
  })
  
  #=============================================================================
  # Player Correlations
  #=============================================================================
  
  output$corr_plot_output <- renderPlot({
    req(input$player_name_corr, input$teammate_name_corr, input$season_input_corr, input$metric_input_corr_b, input$metric_input_corr_a)
    
    plot <-
      get_player_correlation(
        seasons = input$season_input_corr,
        name_a = input$player_name_corr,
        name_b = input$teammate_name_corr,
        metric_a = input$metric_input_corr_a,
        metric_b = input$metric_input_corr_b
      )
    
    return(plot)
  })
  
}

#===============================================================================
# Run App
#===============================================================================

shinyApp(ui, server)
