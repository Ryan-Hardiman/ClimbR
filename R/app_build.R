heatmap_param_choices=c("Minimum Dissolved Oxygen","Temperature, water","pH","DO-temperature habitat profile width")
names(heatmap_param_choices)=c("Dissolved oxygen", "Temperature", "pH", "DO/temperature lens")

ui <- basicPage(
  checkboxGroupButtons(
    inputId = "ClimbGrades",
    choices = c(
      "M",
      "D",
      "HD",
      "VD",
      "HVD",
      "S",
      "HS",
      "VS",
      "HVS",
      "E1",
      "E2",
      "E3",
      "E4",
      "E5",
      "E6"
    ),
    individual = TRUE,
    direction = "horizontal",
    justified = TRUE,
    label = "Eclude Grades"
  )
)

ui <-fluidPage(
  # Input widgets
  fluidRow(
    column(5,
           conditionalPanel(condition="input.plot_tabs!='User guide'",
                            tabsetPanel(id="ui_tab",
                                        tabPanel("Map",
                                                 column(12,h4("Click a site"),withSpinner(leaflet::leafletOutput("map", height="600px"),size=2, color="#7c2599"))
                                        ),
                                        tabPanel("Table",
                                                 column(12, h4("Click a site"), div(dataTableOutput("table_input"), style = "font-size:70%"))
                                        )
                            )
           ),
           conditionalPanel(condition="input.plot_tabs=='User guide'",
                            column(12)
           )
    ),
    column(7,tabsetPanel(id="plot_tabs",
                         
                         tabPanel("Time series",
                                  fluidRow(column(8,
                                                  uiOutput("date_slider"),
                                                  radioButtons("ts_plot_type","Plot type:", choices=c("Heatmap", "Habitable width", "Water column exceedances"), inline=T),
                                                  conditionalPanel(condition="input.ts_plot_type=='Heatmap'",
                                                                   selectInput("heatmap_param",label="Heatmap parameter:", choices= heatmap_param_choices)
                                                  ),
                                                  checkboxInput("show_dates", label="Show all profile dates", value=TRUE),
                                                  conditionalPanel(condition="input.ts_plot_type=='Heatmap'",
                                                                   plotOutput("heatmap")
                                                  ),
                                                  conditionalPanel(condition="input.ts_plot_type=='Habitable width'",
                                                                   plotOutput("hab_width")
                                                  ),
                                                  conditionalPanel(condition="input.ts_plot_type=='Water column exceedances'",
                                                                   plotOutput("pct_exc")
                                                  )
                                  ))
                         ),
                         tabPanel("Individual profiles",
                                  fluidRow(
                                    column(4, uiOutput("date_select"))
                                  ),
                                  fluidRow(
                                    column(4,h4("Profile plot"),plotOutput("ind_prof_plot", height="500px")),
                                    column(8,h4("Profile data"),div(dataTableOutput("profile_table"), style = "font-size:80%"))
                                  )
                         ),
                         tabPanel("User guide",
                                  fluidRow(
                                    column(8,
                                           includeMarkdown(paste0(here(),"/User guide.Rmd"))
                                    )
                                  )
                         )
    ))
  )
)