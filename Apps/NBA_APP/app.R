library(shiny)
library(bslib)
library(gridlayout)
library(DT)
library(tidyverse)
library(googlesheets4)
library(googledrive)

#===============================================================================
# Read in Data
#===============================================================================

all_rosters <- read_csv("../../Data/all_rosters.csv")
all_teams <- read_csv("../../Data/all_teams.csv")
all_player_stats_2021_2022 <- read_csv("../../Data/all_player_stats_2021-2022.csv")
all_player_stats_2022_2023 <- read_csv("../../Data/all_player_stats_2022-2023.csv")
all_player_stats_2023_2024 <- read_csv("../../Data/all_player_stats_2023-2024.csv")

# Combine player stats
all_player_stats <-
  all_player_stats_2023_2024 |>
  bind_rows(all_player_stats_2022_2023) |>
  bind_rows(all_player_stats_2021_2022) |>
  mutate(MIN = round(MIN, 2))

# Google sheets authentication -------------------------------------------------
options(gargle_oauth_cache = ".secrets")
drive_auth(cache = ".secrets", email = "cuzzy.punting@gmail.com")
gs4_auth(token = drive_token())

# Google Sheets Data------------------------------------------------------------
ss_name <- gs4_find("NBA Data")
h2h_data <- read_sheet(ss = ss_name, sheet = "H2H")
player_points_data <- read_sheet(ss = ss_name, sheet = "Player Points")
player_assists_data <- read_sheet(ss = ss_name, sheet = "Player Assists")
player_rebounds_data <- read_sheet(ss = ss_name, sheet = "Player Rebounds")

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
                         "2023-24")
          ),
          selectInput(
            inputId = "stat_input_a",
            label = "Select Statistic:",
            choices = c("PTS",
                        "REB",
                        "AST",
                        "MIN"),
            multiple = FALSE,
            selected = "PTS"
          ),
          checkboxGroupInput(
            inputId = "starter_status",
            label = "Starting Status:",
            choices = list("choice a" = "a", "choice b" = "b")
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
                    choices = all_player_stats$SEASON_YEAR |> unique(),
                    multiple = TRUE,
                    selectize = TRUE,
                    selected = c("2021-22",
                                 "2022-23")
                  )
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
                            choices = c("Points", "Rebounds", "Assists"),
                            multiple = FALSE
                          ),
                          selectInput(
                            inputId = "match_input",
                            label = "Select Matches:",
                            choices = h2h_data$match |> unique(),
                            multiple = TRUE,
                            selectize = FALSE,
                            selected = h2h_data$match |> unique()
                          ),
                          checkboxInput(
                            inputId = "only_unders",
                            label = "Only Show Markets With Unders",
                            value = FALSE
                          ),
                          markdown(mds = c("__Select Difference Range:__")),
                          numericInput(
                            inputId = "diff_minimum",
                            label = "Min Diff",
                            value = NA
                          ),
                          numericInput(
                            inputId = "diff_maximum",
                            label = "Max Diff",
                            value = NA
                          )
                        )),
              grid_card(area = "odds_table",
                        card_body(
                          DTOutput(outputId = "scraped_odds_table", width = "100%")
                        ))
            ))
)

#===============================================================================
# Server
#===============================================================================

server <- function(input, output) {
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
        MIN <= input$minutes_maximum
      ) |>
      arrange(GAME_DATE) |>
      mutate(game_number = row_number())
    
    # Filter by last n games
    if (!is.na(input$last_games)) {
      filtered_player_stats <-
        filtered_player_stats |>
        slice_tail(n = input$last_games)
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
    
    # Get string to output
    output_string <- paste0(
      "Proportion above reference line: ",
      round(proportion_above_reference_line, 2),
      "\n",
      "Implied Odds: ",
      round(implied_odds, 2)
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
      geom_hline(
        yintercept = input$reference_line,
        linetype = "dashed",
        color = "grey4",
        size = 1
      ) +
      
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
      options = list(pageLength = 15, autoWidth = TRUE),
      width = "100%",
      height = "800px"
    )
  })
  
  #=============================================================================
  # Table team stats
  #=============================================================================
  
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
    
    # Min and max differences
    if (!is.na(input$diff_minimum)) {
      odds <-
        odds |>
        filter(diff_over >= input$diff_minimum)
    }
    
    if (!is.na(input$diff_maximum)) {
      odds <-
        odds |>
        filter(diff_over <= input$diff_maximum)
    }
    
    if (input$only_unders == TRUE) {
      odds <-
        odds |>
        filter(!is.na(under_price))
    }
      
    # Return odds
    return(odds)
  })
  
  # Table output
  output$scraped_odds_table <- renderDT({
    datatable(
      scraped_odds(),
      options = list(pageLength = 15, autoWidth = FALSE)
    )
  })
  
}

#===============================================================================
# Run App
#===============================================================================

shinyApp(ui, server)
