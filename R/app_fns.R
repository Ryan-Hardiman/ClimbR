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
                        shiny::fluidRow(
                          filterModuleUI("filter_df"),
                          "####################",
                          "NOT IMPLEMENTED YET",
                          shiny::checkboxInput("tide_filter", "Exclude tide-dependent climbs", value = TRUE),
                          shinyTime::timeInput("start_time", "Climb Start Time:"),
                          shiny::numericInput("duration", "Climb Duration (hrs):", value = 2, min = 0.5, step = 0.5),
                          shiny::column(6, )
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
      ),
      
      
      # Decide whether to keep (+ modulise)
      # Tide Page
      shiny::tabPanel("Tide page",
                      shiny::helpText("This is an approximation of todays tide data."),
                      shiny::plotOutput("tide_plot")
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
  
  filtered_dataframe <- filterModuleServer("filter_df", reactive(df))
  
  
  ####Split into modules asap..
  #Map Page code
  selected_crag <- shiny::reactiveVal(NULL)
  
  output$crag_map <- leaflet::renderLeaflet({
    req(filtered_dataframe())
    plot_crags(filtered_dataframe()) # Calls the separate plot function
  })
  
  shiny::observeEvent(input$crag_map_marker_click, {
    clicked <- input$crag_map_marker_click
    selected_crag(df |> dplyr::filter(latitude == clicked$lat & longitude == clicked$lng))
  })
  
  output$selected_crag_info <- shiny::renderTable({
    selected_crag()|>dplyr::select(climb_name,overall_grade_1,overall_grade_2,hardest_move_1,hardest_move_2)
  })
  
  
  # Tide Page (requires internet connection)
  output$tide_plot <- renderPlot({
    get_tides()|>
      interpolate_tides()|>
      dplyr::mutate(
        hours = floor(time), 
        minutes = round((time - hours) * 60),
        datetime = lubridate::ymd_hms("2025-02-09 00:00:00") + lubridate::hours(hours) + lubridate::minutes(minutes)  # Example date
      )|>
      ggplot2::ggplot(ggplot2::aes(x = datetime, y = height))+
      ggplot2::geom_line()+
      ggplot2::scale_x_datetime(date_labels = "%I:%M %p", date_breaks = "4 hour") +  # 12-hour format
      ggplot2::labs(x = "Time", y = "Height (m)", title = "Tide Height Over Time") +
      ggplot2::theme_minimal()
  }, res = 96)
  
}

#' Run the Shiny App
#'
#' @param df A tibble containing crag data
#' @return Launches the Shiny App
#' @importFrom shiny shinyApp
#' @export
#' @example df <- data.frame(name_of_area = sample(paste0("Crag_",1:5),26,replace = T),climb_name = letters,overall_grade_1 = sample(paste0("E",1:11),26,replace=T),hardest_move_1 = sample(paste0(3:6,c("a","b","c")),26,replace = T))|>dplyr::mutate(overall_grade_2 = overall_grade_1, hardest_move_2 = hardest_move_1)
#' run_app(df)
run_app <- function(df) {
  shiny::shinyApp(
    ui = app_ui(),
    server = function(input, output, session) {
      app_server(input, output, session, df)
    }
  )
}
