#' UI for the Climbing Crag Filter Module
#'
#' This UI function creates the interactive filtering options for users to select climbing 
#' grades and hardest moves from available options. These selections are used to filter the 
#' climbing routes on the map.
#'
#' @param id A string identifier for the module, used to namespace UI elements.
#'
#' @return A UI element for the filter module, including checkbox inputs for grades and hardest moves.
#'
#' @export
#'
#' @examples 
#' # Use in the Shiny app UI
#' filterModuleUI("filter_df")
filterModuleUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    fluidRow(
      column(6,shiny::checkboxGroupInput(ns("grades"), "Grades:", choices = NULL, selected = NULL)),
      column(6,
             shiny::checkboxGroupInput(ns("hardest_moves"), "Hardest Moves:", choices = NULL, selected = NULL),
             shiny::checkboxInput(ns("remove_tidal"), "Exclude tide-dependent climbs", value = FALSE)
             )
    )
  )
}


#' Server Logic for the Climbing Crag Filter Module
#'
#' This server function handles the filtering logic for the climbing routes dataset based on 
#' user input (grades and hardest moves). It updates the choices in the filter options and 
#' generates a reactive filtered dataset.
#'
#' @param id A string identifier for the module.
#' @param df A reactive data frame (tibble) containing the climbing route data.
#'
#' @return A reactive filtered data frame that updates based on user-selected filters.
#'
#' @export
#'
filterModuleServer <- function(id, df) {
  shiny::moduleServer(id, function(input, output, session) {
    # Initialize checkbox group inputs based on unique values in the dataset
    shiny::observe({
      shiny::req(df())
      
      trad_order <- c("M", "D", "VD", "HVD", "S", "HS", "VS", "HVS",
                      "E1", "E2", "E3", "E4", "E5", "E6", "E7", "E8", "E9", "E10", "E11")
      
      sport_order <- c("3", "4a", "4b", "4c", "5a", "5b", "5c", 
                       "6a",  "6b", "6c", "7a", "7b",  "7c",  
                       "8a",  "8b",  "8c", "9a",  "9b",  "9c")
      
      grades <- unique(c(df()$overall_grade_1, df()$overall_grade_2)|>purrr::discard(is.na))
      sort_grades <- intersect(trad_order,grades)
      
      moves <- unique(c(df()$hardest_move_1, df()$hardest_move_2)|>purrr::discard(is.na))
      sort_moves <- intersect(sport_order,moves)
      
      shiny::updateCheckboxGroupInput(
        session, "grades",
        choices = sort_grades,
        selected = sort_grades
      )
      shiny::updateCheckboxGroupInput(
        session, "hardest_moves",
        choices = sort_moves,
        selected = sort_moves
      )
    })
    
    # Reactive expression to filter the dataset based on selected grades and hardest moves
    filtered_df <- shiny::reactive({
      shiny::req(df())

      df_filtered <- filter_df(
        df(),
        overall_grade_1 = input$grades,
        overall_grade_2 = input$grades,
        hardest_move_1 = input$hardest_moves,
        hardest_move_2 = input$hardest_moves
      )
      if (input$remove_tidal) {
        df_filtered <- subset(df_filtered, tide_height == "All" & tide_season == "All")
      }
      
      df_filtered
      
    })
    
    # Return the reactive filtered dataframe
    return(filtered_df)
  })
}


