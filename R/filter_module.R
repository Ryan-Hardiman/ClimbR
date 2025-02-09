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
    shiny::checkboxGroupInput(ns("grades"), "Grades:", choices = NULL, selected = NULL),
    shiny::checkboxGroupInput(ns("hardest_moves"), "Hardest Moves:", choices = NULL, selected = NULL)
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
#' @examples 
#' # Use in the Shiny app server function
#' filterModuleServer("filter_df", df)
filterModuleServer <- function(id, df) {
  shiny::moduleServer(id, function(input, output, session) {
    # Initialize checkbox group inputs based on unique values in the dataset
    shiny::observe({
      shiny::req(df())
      shiny::updateCheckboxGroupInput(session, "grades",
                                      choices = unique(c(df()$overall_grade_1, df()$overall_grade_2)|>purrr::discard(is.na)),
                                      selected = unique(c(df()$overall_grade_1, df()$overall_grade_2)|>purrr::discard(is.na))
      )
      shiny::updateCheckboxGroupInput(session, "hardest_moves",
                                      choices = unique(c(df()$hardest_move_1, df()$hardest_move_2)|>purrr::discard(is.na)),
                                      selected = unique(c(df()$hardest_move_1, df()$hardest_move_2)|>purrr::discard(is.na))
      )
    })
    
    # Reactive expression to filter the dataset based on selected grades and hardest moves
    filtered_df <- shiny::reactive({
      shiny::req(df())
      df() |>
        filter_df(
          overall_grade_1 = input$grades,
          overall_grade_2 = input$grades,
          hardest_move_1 = input$hardest_moves,
          hardest_move_2 = input$hardest_moves
        )
    })
    
    # Return the reactive filtered dataframe
    return(filtered_df)
  })
}


