#===============================================================================
# Libraries and functions
#===============================================================================

library(tidyverse)
library(shiny)
library(shinythemes)
library(DT)
library(readxl)
`%notin%` <- Negate(`%in%`)

#===============================================================================
# Create compare sgm function
#===============================================================================

# Source scripts
# source("betright_sgm.R")
source("tab_sgm.R")
source("sportsbet_sgm.R")
# source("pointsbet_sgm.R")

compare_sgm(player_names = c("Devin Booker", "Kevin Durant"), prop_line = c("24.5", "24.5"), prop_type = c("Player Points", "Player Points"), over_under = c("Overs", "Overs"))

#===============================================================================
# Create compare sgm function
#===============================================================================

compare_sgm <- function(player_names, prop_line, prop_type, over_under) {
  # Function to handle errors in the call_sgm functions
  handle_call_sgm <- function(func, sgm, player_names, prop_line, prop_type, over_under) {
    tryCatch({
      func(sgm, player_names, prop_line, prop_type, over_under)
    }, error = function(e) {
      # Return a dataframe with NA values if an error occurs
      data.frame(Selections=NA, Unadjusted_Price=NA, Adjusted_Price=NA, Adjustment_Factor=NA, Agency=NA)
    })
  }
  
  # Get individual dataframes
  # pointsbet_data <- handle_call_sgm(call_sgm_pointsbet, pointsbet_sgm,player_names, prop_line, prop_type, over_under)
  sportsbet_data <- handle_call_sgm(call_sgm_sportsbet, sportsbet_sgm, player_names, prop_line, prop_type, over_under)
  tab_data <- handle_call_sgm(call_sgm_tab, tab_sgm, player_names, prop_line, prop_type, over_under)
  # betright_data <- handle_call_sgm(call_sgm_betright, betright_sgm, player_names, prop_line, prop_type, over_under)
  
  # Bind together and return
  bind_rows(sportsbet_data,
            # pointsbet_data,
            tab_data,
            # betright_data
            ) |>
    mutate(Adjusted_Price = round(Adjusted_Price, 2),
           Unadjusted_Price = round(Unadjusted_Price, 2),
           Adjustment_Factor = round(Adjustment_Factor, 2)
           ) |>
    arrange(desc(Adjusted_Price))
}

#===============================================================================
# Function to get all combinations and get SGM prices
#===============================================================================

# sgm_combinations <- function(player_names, disposal_counts) {
#   
#   # Creat tibble out of output
#   input_dat <- tibble(player_names, disposal_counts)
#   
#   # Get all row combinations
#   row_combinations_2 <- combn(nrow(input_dat), 2, simplify = FALSE)
# 
#   # Get list of all 2-way combinations
#   combinations_list_2 <-
#     lapply(row_combinations_2, function(indices) {
#       input_dat[indices, ]
#   })
#   
#   # # Get list of all 3-way combinations
#   # combinations_list_3 <-
#   #   lapply(row_combinations_3, function(indices) {
#   #     input_dat[indices, ]
#   #   })
#   # 
#   # # Get list of all 4-way combinations
#   # combinations_list_4 <-
#   #   lapply(row_combinations_4, function(indices) {
#   #     input_dat[indices, ]
#   #   })
#   # 
#   # # Get list of all 5-way combinations
#   # combinations_list_5 <-
#   #   lapply(row_combinations_5, function(indices) {
#   #     input_dat[indices, ]
#   #   })
#   
#   # Create a function that takes a tibble and applies compare_sgm to each col
#   compare_sgm_tibble <- function(input_tibble) {
#     
#     # Get input cols
#     player_names_tib <- input_tibble$player_names
#     disposal_counts_tib <- input_tibble$disposal_counts
#     
#     # Apply compare SGM
#     compare_sgm(player_names_tib, disposal_counts_tib)
#   }
#   
#   # Get list of every combination
#   all_combinations_list <- c(combinations_list_2, combinations_list_3, combinations_list_4, combinations_list_5)
#   
#   # Map over lists
#   output_list <- map(all_combinations_list, compare_sgm_tibble)
#   
#   # Get a tibble to summarise data
#   return_data <- bind_rows(output_list, .id = "id")
#   
#   return_data <-
#     return_data |>
#     mutate(unadjusted_implied_prob = 1/Unadjusted_Price, adjusted_implied_prob = 1/Adjusted_Price) |>
#     mutate(implied_prob_difference = unadjusted_implied_prob - adjusted_implied_prob) |>
#     mutate(unadjusted_implied_prob = round(unadjusted_implied_prob, 2),
#            adjusted_implied_prob = round(adjusted_implied_prob, 2),
#            implied_prob_difference = round(implied_prob_difference, 2))
#   
#   # Get biggest difference between first and second
#   return_data <-
#     return_data |>
#     group_by(id) |>
#     mutate(best_price = first(adjusted_implied_prob),
#            second_best_price = nth(adjusted_implied_prob, 2)) |>
#     mutate(price_drop = second_best_price - best_price) |>
#     select(-best_price, -second_best_price) |>
#     ungroup() |>
#     mutate(price_drop = round(price_drop, 2))
#   
#   return(return_data)
# }

# Read in datasets--------------------------------------------------------------
uri <- Sys.getenv("mongodb_connection_string")

disposals_con <- mongo(collection = "Disposals", db = "Odds", url = uri)
disposals <- disposals_con$find('{}') |> tibble()

# Unique matches
matches <-
  disposals |>
  arrange(start_time) |> 
  filter(start_time > now()) |> 
  distinct(match) |>
  pull()
  
# Unique agencies
agencies <-
  disposals |>
  distinct(agency) |>
  pull()

# Create disposals dataframe to display
disposals_display <-
  disposals |>
  group_by(player_name, match, number_of_disposals) |>
  mutate(market_best = max_player_diff == diff_2023) |>
  ungroup() |>
  arrange(desc(max_player_diff)) |> 
  transmute(match,
         player_name,
         number_of_disposals,
         price,
         agency,
         prob_2023 = round(empirical_probability_2023, 2),
         prob_last_10 = round(empirical_probability_last_10, 2),
         diff_2023 = round(diff_2023, 2),
         diff_last_10 = round(diff_last_10, 2),
         market_best)

##%######################################################%##
#                                                          #
####                         UI                         ####
#                                                          #
##%######################################################%##

ui <- fluidPage(
  theme = shinytheme("flatly"),
  titlePanel("Multitool"),
  
  tabsetPanel(
    
    # SGM Tab
    tabPanel("SGM",
             sidebarLayout(
               sidebarPanel(
                 selectInput("match", "Select Match", choices = matches, selected = NULL),
                 selectInput("agency", "Select Agency", choices = agencies, selected = NULL),
                 checkboxInput("best_odds", "Only Show Best Market Odds?", value = FALSE),
                 h3("Selections"),  
                 DT::dataTableOutput("selected"), 
                 h3("Pairwise Correlations"),
                 DT::dataTableOutput("correlations"),
                 h3("SGM Information"),
                 uiOutput("summary"),
                 h3("Odds Comparison"),
                 actionButton("get_comparison", label = "Compare Odds"),
                 DT::dataTableOutput("odds_compare"),
                 h3("SGM Combinations"),
                 actionButton("get_combos", label = "Get All Combinations"),
               ),
               
               mainPanel(
                 tabsetPanel(
                   tabPanel("Player List", 
                            DT::dataTableOutput("table")
                   ),
                   tabPanel("Selected Combinations", 
                            DT::dataTableOutput("all_selected_combinations")
                   )
                 )
               )
             )
    ),
    
    # Cross Game Multi Tab
    tabPanel("Cross Game Multi",
             sidebarLayout(
               sidebarPanel(
                 selectInput("agency_cross", "Select Agency", choices = agencies, selected = NULL),
                 checkboxInput("best_odds_cross", "Only Show Best Market Odds?", value = FALSE),
                 h3("Selections"),  
                 DT::dataTableOutput("selected_cross"), 
                 h3("Multi Information"),
                 uiOutput("summary_cross")
               ),
               
               mainPanel(
                 DT::dataTableOutput("table_cross")
               )
             )
    )
  )
)

##%######################################################%##
#                                                          #
####                       Server                       ####
#                                                          #
##%######################################################%##

server <- function(input, output, session) {
  
  # For the "SGM" panel
  output$table <- renderDT({
    filtered_data <- disposals_display[disposals_display$match == input$match & disposals_display$agency == input$agency,]
    
    if (input$best_odds) {filtered_data <- filtered_data |> filter(market_best) |> select(-market_best)}
    
    datatable(filtered_data, selection = "multiple")
  }, server = FALSE) # We are setting this as FALSE for client-side processing of the DataTable
  
  observeEvent(input$table_rows_selected,{
    output$selected <- renderDT({
      if(!is.null(input$table_rows_selected)){
        filtered_data <- disposals_display[disposals_display$match == input$match & disposals_display$agency == input$agency,]
        if (input$best_odds) {filtered_data <- filtered_data |> filter(market_best) |> select(-market_best)}
        selected_data <- filtered_data[input$table_rows_selected, c("player_name", "number_of_disposals", "price")]
        datatable(selected_data)
      }
    })
  })
  
  output$correlations <- renderDT({
    filtered_data <- disposals_display[disposals_display$match == input$match & disposals_display$agency == input$agency,]
    if (input$best_odds) {filtered_data <- filtered_data |> filter(market_best) |> select(-market_best)}
    selected_data <- filtered_data[input$table_rows_selected, c("player_name", "number_of_disposals", "price")]
    
    correlations_table <- correlations_2023 |> filter(player_a %in% selected_data$player_name & player_b %in% selected_data$player_name)
    datatable(correlations_table)
  })
  
  observeEvent(input$get_comparison, {
    # Get selected data
    filtered_data <- disposals_display[disposals_display$match == input$match & disposals_display$agency == input$agency,]
    if (input$best_odds) {filtered_data <- filtered_data |> filter(market_best) |> select(-market_best)}
    selected_data <- filtered_data[input$table_rows_selected, c("player_name", "number_of_disposals", "price")]
    
    player_names = selected_data$player_name
    number_of_disposals = selected_data$number_of_disposals
    
    # Call function
    comparison_df <- compare_sgm(player_names, number_of_disposals)
    
    # populate DTOutput
    output$odds_compare <- renderDT({
      datatable(comparison_df)
    })
  })
  
  observeEvent(input$get_combos, {
    # Get selected data
    filtered_data <- disposals_display[disposals_display$match == input$match & disposals_display$agency == input$agency,]
    if (input$best_odds) {filtered_data <- filtered_data |> filter(market_best) |> select(-market_best)}
    selected_data <- filtered_data[input$table_rows_selected, c("player_name", "number_of_disposals", "price")]
    
    player_names = selected_data$player_name
    number_of_disposals = selected_data$number_of_disposals
    
    # Call function
    combo_df <- sgm_combinations(player_names, number_of_disposals)
    
    # populate DTOutput
    output$all_selected_combinations <- renderDT({
      datatable(combo_df, extensions = "Buttons", options = list(buttons = c('copy', 'csv', 'excel')))
    })
  })
  
  output$summary <- renderUI({
    if(!is.null(input$table_rows_selected)){
      filtered_data <- disposals_display[disposals_display$match == input$match & disposals_display$agency == input$agency,]
      if (input$best_odds) {filtered_data <- filtered_data |> filter(market_best) |> select(-market_best)}
      selected_data <- filtered_data[input$table_rows_selected, ]
      uncorrelated_price <- prod(selected_data$price)
      empirical_price <- 1 / prod(selected_data$prob_2023)
      HTML(paste0("<strong>Uncorrelated Price:</strong>", " $", round(uncorrelated_price, 2), "<br/>",
                  " <strong>Theoretical Uncorrelated Price:</strong>", " $", round(empirical_price, 2)))
    }
  })
  
  # For the "Cross Game Multi" panel
  output$table_cross <- renderDT({
    filtered_data_cross <- disposals_display[disposals_display$agency == input$agency_cross,]
    
    if (input$best_odds_cross) {filtered_data_cross <- filtered_data_cross |> filter(market_best) |> select(-market_best)}
    
    datatable(filtered_data_cross, selection = "multiple")
  }, server = FALSE) 
  
  observeEvent(input$table_cross_rows_selected,{
    output$selected_cross <- renderDT({
      if(!is.null(input$table_cross_rows_selected)){
        filtered_data_cross <- disposals_display[disposals_display$agency == input$agency_cross,]
        selected_data_cross <- filtered_data_cross[input$table_cross_rows_selected, c("player_name", "number_of_disposals", "price")]
        datatable(selected_data_cross)
      }
    })
  })
  
  output$summary_cross <- renderUI({
    if(!is.null(input$table_cross_rows_selected)){
      filtered_data_cross <- disposals_display[disposals_display$agency == input$agency_cross,]
      selected_data_cross <- filtered_data_cross[input$table_cross_rows_selected, ]
      uncorrelated_price_cross <- prod(selected_data_cross$price)
      empirical_price_cross <- 1 / prod(selected_data_cross$prob_2023)
      diff = 1/empirical_price_cross - 1/uncorrelated_price_cross
      HTML(paste0("<strong>Multi Price:</strong>", " $", round(uncorrelated_price_cross, 2), "<br/>",
                  " <strong>Theoretical Multi Price:</strong>", " $", round(empirical_price_cross, 2), "<br/>",
                  " <strong>Edge:</strong>", " ", round(100*diff, 3), "%"))
    }
  })
}


##%######################################################%##
#                                                          #
####                      Run App                       ####
#                                                          #
##%######################################################%##

shinyApp(ui = ui, server = server)
