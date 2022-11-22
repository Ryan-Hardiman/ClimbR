library(readxl) #used
library(zoo) #used
library(leaflet) #used
library(tidyverse) #used
library(lubridate)
library(rvest)#used
library(here)#used
library(viridis)#used to create the colour palette
library(rsconnect)
#Below are used for webdesign
library(shiny) #used
library(shinyWidgets) #used
library(rsconnect)
library(shinycssloaders)
library(DT)
library(rmarkdown)

library(httr)
library(jsonlite)


#Filters the Island Climbing list to below optional constriction

Climbing_Filter <- function(Name_Of_Area = NULL,
                            Climb_Name = NULL,
                            Climb_Grade = NULL,
                            Hardest_Move = NULL,
                            Tide_Season = NULL,
                            Tide_Height = NULL,
                            Hours_Innacessible = NULL,
                            Lead = NULL,
                            Seconded = NULL){
  
  Filtered_Data <- Island_Climbing
  if(!is.null(Name_Of_Area)){
    Filtered_Data <- Filtered_Data %>% filter(Filtered_Data$`Name Of Area` %in% Name_Of_Area)
  }
  if(!is.null(Climb_Name)){
    Filtered_Data <- Filtered_Data %>% filter(Filtered_Data$`Climb Name` %in% Climb_Name)
  }
  if(!is.null(Climb_Grade)){
    Filtered_Data <- Filtered_Data %>% filter(Filtered_Data$`Overall_Grade_1` %in% Climb_Grade | Filtered_Data$`Overall_Grade_2` %in% Climb_Grade)
  }
  if(!is.null(Hardest_Move)){
    Filtered_Data <- Filtered_Data %>% filter(Filtered_Data$`Hardest_Move_1` %in% Hardest_Move | Filtered_Data$`Hardest_Move_2` %in% Hardest_Move)
  }
  if(!is.null(Tide_Season)){
    Filtered_Data <- Filtered_Data %>% filter(Filtered_Data$`Tide Season` %in% c(Tide_Season,"All"))
  }
  if(!is.null(Tide_Height)){
    Filtered_Data <- Filtered_Data %>% filter(Filtered_Data$`Tide Height` %in% c(Tide_Height,"All"))
  }
  if(!is.null(Hours_Innacessible)){
    Filtered_Data <- Filtered_Data %>% filter(Filtered_Data$`Hours Innaccessible` >= Hours_Innacessible)
  }
  if(!is.null(Lead)){
    Filtered_Data <- Filtered_Data %>% filter(!is.na(Lead))
  }
  if(!is.null(Seconded)){
    Filtered_Data <- Filtered_Data %>% filter(!is.na(Seconded))
  }
  Filtered_Data %>% select(`Name Of Area` , `Climb Name`)
}

