#' UI for the Climbing Crag Visualization App
#'
#' @return A Shiny UI definition
#' @import shiny
#' @import shinyTime
#' @import htmltools
#' @export
app_ui <- function() {
  shiny::fluidPage(
    shiny::titlePanel("Needs a good name"),
    shiny::tabsetPanel(

      # Make a module asap..      
      # Intro Page
      shiny::tabPanel("Introduction",
                      htmltools::h2("Welcome to the Climbing Logbook App"),
                      htmltools::p("This app allows you to track your climbing progress, and narrow down on crags based on real tide data and your climbing needs."),
                      htmltools::h4("Legal Notice"),
                      htmltools::p("Please use this app responsibly. The data provided is for personal tracking purposes only. The developers are not responsible for any inaccuracies. Climbing is an inherently dangerous sport, climb at your own risk.")
      ),
      
      # Make a module asap..
      # Filter Page
      shiny::tabPanel("Filter Climbs",
                      shiny::helpText("Filtering climbs in this tab affects the crags shown in the `map` tab."),
                      
                      shiny::fluidPage(
                        fluidRow(
                          column(3, 
                                 shiny::checkboxGroupInput("grades",
                                                              "Climb Grades",
                                                              choices = c("D", "D+", "VD", "VD+", "S", "S+", "VS", "HVS", "E1", "E2", "E3", "E4", "E5", "E6", "E7"),
                                                              selected = "D"),
                                 shiny::radioButtons("tide_data", "Tide Data Source:", choices = c("Real-world" = "real", "Manual Input" = "manual"))),
                          column(3, 
                                 shiny::checkboxGroupInput("hardest_move",
                                                              "Hardest Move",
                                                              choices = c("3c", "4a", "4b", "4c", "5a", "5b", "5c", "6a", "6b", "6c"),
                                                              selected = "3c"),
                                 shiny::checkboxInput("tide_filter", "Exclude tide-dependent climbs", value = TRUE),
                                 shinyTime::timeInput("start_time", "Climb Start Time:"),
                                 shiny::numericInput("duration", "Climb Duration (hrs):", value = 2, min = 0.5, step = 0.5)),
                          column(6, )
                        )
                      )
      ),
      
      # Make a module asap..
      # Map Page
      shiny::tabPanel("Map View",
                      shiny::helpText("Click on a crag to see climb details below."),
                      shiny::mainPanel(
                        leaflet::leafletOutput("crag_map", height = "600px"),
                        shiny::tableOutput("selected_crag_info")
                      )
      )
    )
  )
}


#' Server logic for the Climbing Crag Visualization App
#'
#' @param input, output, session Shiny server arguments
#' @param df A tibble containing crag data
#' @return A Shiny server function
#' @importFrom shiny renderTable reactiveVal observeEvent
#' @export
app_server <- function(input, output, session, df) {
  
####Split into modules asap..
  #Map Page code
  selected_crag <- shiny::reactiveVal(NULL)
  
  output$crag_map <- leaflet::renderLeaflet({
    plot_crags(df) # Calls the separate plot function
  })
  
  shiny::observeEvent(input$crag_map_marker_click, {
    clicked <- input$crag_map_marker_click
    selected_crag(df |> dplyr::filter(latitude == clicked$lat & longitude == clicked$lng))
  })
  
  output$selected_crag_info <- shiny::renderTable({
    selected_crag()|>dplyr::select(climb_name,overall_grade_1,overall_grade_2,hardest_move_1,hardest_move_2)
  })
}

#' Run the Shiny App
#'
#' @param df A tibble containing crag data
#' @return Launches the Shiny App
#' @importFrom shiny shinyApp
#' @export
run_app <- function(df) {
  shiny::shinyApp(
    ui = app_ui(),
    server = function(input, output, session) {
      app_server(input, output, session, df)
    }
  )
}
