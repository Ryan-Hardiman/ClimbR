#' UI for the Climbing Crag Visualization App
#'
#' @return A Shiny UI definition
#' @importFrom shiny fluidPage titlePanel sidebarLayout sidebarPanel mainPanel
#' @export
app_ui <- function() {
  shiny::fluidPage(
    shiny::titlePanel("Climbing Crag Map"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::helpText("Click on a crag to see climb details below.")
      ),
      shiny::mainPanel(
        leaflet::leafletOutput("crag_map", height = "600px"),
        shiny::tableOutput("selected_crag_info")
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
  selected_crag <- shiny::reactiveVal(NULL)
  
  output$crag_map <- leaflet::renderLeaflet({
    plot_crags(df) # Calls the separate plot function
  })
  
  shiny::observeEvent(input$crag_map_marker_click, {
    clicked <- input$crag_map_marker_click
    selected_crag(df |> dplyr::filter(latitude == clicked$lat & longitude == clicked$lng))
  })
  
  output$selected_crag_info <- shiny::renderTable({
    selected_crag()
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
