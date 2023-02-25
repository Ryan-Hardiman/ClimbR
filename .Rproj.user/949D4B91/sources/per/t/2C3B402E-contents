#=====
library(readr)
library(here)
Island_Climbing<-read_rds(paste0(here(),"/data/Island_Climbing.rds"))
source(here("R","functions.R"))
#source(here("R","old_data_editing_code.R"))

#Desired climb time
ClimbTime <- hm("36:00")
ClimbDuration <- hm("4:00")
source(here("R","web_scraping.R"))
source(here("R","tide_function.R"))
#source(here("R","app_build.R"))#<--- Used for ShinyApp later on when this all works


#Below is an example run of the map function which filters for climbs in the E2 difficulty range and ones that are not underwater!
Map_Gen(climb_grade = c("E2", "E3"), limit_by_tide = 1) #as long as limit_by_tide != NULL it filters underwater climbs out
















####
# API STUFF
####

base<- "https://tics.seeker.gg/api/event"
raw_results <-GET(base)


converted_content <- content(raw_results,as="text")
 
converted_json <- fromJSON(converted_content)





server <- function(input, output, session) {
  # Loading modal to keep user out of trouble while map draws...
    #showModal(modalDialog(title="MAP LOADING - PLEASE WAIT...","Please wait for map to draw before proceeding.",size="l",footer=NULL))

  # Remove modal when app is ready
     #observe({
     #  req(map)
     #  removeModal()
     #})
  
  # Load data
  read_rds(paste0(here(),"/data/Island_Climbing.rds"))
  
  map = leaflet::createLeafletMap(session, 'map')
  
  session$onFlushed(once = T, function(){
    output$map <- leaflet::renderLeaflet(
      Map_Gen()
    )
    
  })
  
  
  }
shinyApp(ui, server)

