library(shiny)
library(bslib)
library(gridlayout)
library(DT)
library(tidyverse)

#===============================================================================
# Read in Data
#===============================================================================

all_rosters <- read_csv("C:/Users/a1645197/NBA/Data/all_rosters.csv")
all_teams <- read_csv("C:/Users/a1645197/NBA/Data/all_teams.csv")
all_player_stats_2021_2022 <- read_csv("C:/Users/a1645197/NBA/Data/all_player_stats_2021-2022.csv")
all_player_stats_2022_2023 <- read_csv("C:/Users/a1645197/NBA/Data/all_player_stats_2022-2023.csv")

# Combine player stats
all_player_stats <- 
  all_player_stats_2022_2023 |>
  bind_rows(all_player_stats_2021_2022) |> 
  mutate(MIN = round(MIN, 2))

#===============================================================================
# UI
#===============================================================================

ui <- page_navbar(
  title = "NBA",
  selected = "Player Stats",
  collapsible = TRUE,
  theme = bslib::bs_theme(),
  nav_panel(
    title = "Player Stats",
    grid_container(
      layout = c(
        "nba_stats player_stat_plot"
      ),
      row_sizes = c(
        "1fr"
      ),
      col_sizes = c(
        "250px",
        "1fr"
      ),
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
            selected = c(
              "2021-22",
              "2022-23"
            )
          ),
          selectInput(
            inputId = "stat_input_a",
            label = "Select Statistic:",
            choices = c(
              "PTS",
              "REB",
              "AST",
              "MIN"
            ),
            multiple = FALSE,
            selected = "PTS"
          ),
          checkboxGroupInput(
            inputId = "starter_status",
            label = "Starting Status:",
            choices = list("choice a" = "a", "choice b" = "b")
          ),
          markdown(
            mds = c(
              "__Select Reference Line:__"
            )
          ),
          numericInput(
            inputId = "reference_line",
            label = "Line Value",
            value = 9.5
          ),
          markdown(
            mds = c(
              "__Select Minutes Range:__"
            )
          ),
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
      grid_card(
        area = "player_stat_plot",
        card_body(plotOutput(outputId = "plot"))
      )
    )
  ),
  nav_panel(
    title = "Team Stats",
    grid_container(
      layout = c(
        "nba_team_stats team_stat_table"
      ),
      row_sizes = c(
        "1fr"
      ),
      col_sizes = c(
        "250px",
        "1fr"
      ),
      gap_size = "10px",
      grid_card(
        area = "nba_team_stats",
        card_header("Settings"),
        card_body(
          selectInput(
            inputId = "season_input_b",
            label = "Select Season:",
            choices = all_player_stats$SEASON_YEAR |> unique(),
            multiple = TRUE,
            selectize = TRUE,
            selected = c(
              "2021-22",
              "2022-23"
            )
          )
        )
      ),
      grid_card(
        area = "team_stat_table",
        card_body(
          DTOutput(outputId = "team_metric_table", width = "100%")
        )
      )
    )
  ),
  nav_panel(title = "Odds Screen")
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
    
    # Plot player stats
    p <- filtered_player_stats() %>%
      ggplot(aes(x = game_number, y = !!sym(input$stat_input_a))) +
      
      # Basic Elements
      geom_point(size = 3, color = "dodgerblue") +
      geom_hline(yintercept = input$reference_line, linetype = "dashed", color = "orangered1", size = 1) +
      
      # Add text
      annotate(
        geom = "text",
        x = 1,
        y = max(filtered_player_stats() %>% pull(!!sym(input$stat_input_a))),
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
      labs(
        title = "",
        x = "Game Number"
      ) +
      
      # Color
      scale_color_manual(values = c("dodgerblue", "grey")) +
      
      # Additional
      theme(legend.position = "none")
    
    print(p)
  })
  
  #=============================================================================
  # Table team stats
  #=============================================================================
  
  

}

#===============================================================================
# Run App
#===============================================================================

shinyApp(ui, server)
