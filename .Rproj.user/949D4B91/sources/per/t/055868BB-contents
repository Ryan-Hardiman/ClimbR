library(readxl) #used
library(zoo) #used
library(leaflet) #used
library(tidyverse) #used
library(lubridate) #used
library(rvest)#used
library(here)#used
library(viridis)#used to create the colour palette
#Below are used for webdesign
library(shiny) #used
library(shinyWidgets) #used
library(rsconnect)
library(shinycssloaders)
library(DT)
library(rmarkdown) #used
library(magrittr)
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
                            Seconded = NULL,
                            limit_by_tide = NULL){
  
  Filtered_Data <-Island_Climbing |> mutate(id = row_number())
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
  if(!is.null(limit_by_tide)){
    Filtered_Data <- tide_filter(Filtered_Data, Tide_Table, ClimbTime, ClimbDuration)
  }
  Filtered_Data %>% select(`Name Of Area` , `Climb Name`, id)
}


#TideWave <- function(x){
#  (Tide_Table$Tide[Tide_Index] - Tide_Table$Tide[max(1,Tide_Index + 1 %% 4)]) / 2 * cos(
#    if_else(Tide_Table$Tide_State[Tide_Index] == "h", 0 , pi) +pi * period_to_seconds(x) / period_to_seconds(Tide_Table$Time[max(1,Tide_Index + 1 %% 4)] -Tide_Table$Time[Tide_Index])
#  ) + (Tide_Table$Tide[max(1,Tide_Index + 1 %% 4)] + Tide_Table$Tide[Tide_Index]) / 2}

get_unique_tide_times <- function(filtered_climb_list){
  filtered_climb_list |>
    select(`Tide Season`,
           `Tide Height`,
           `Hours to be inaccesible`) |>
    unique()
}





Clean_Tide_Tables <- function(table){
  times <- (table |> pluck(2))[-1] |> hm()
  tides <- gsub('.{1}$',
                '',
                c((table|>pluck(3))[-1])
                ) |> as.numeric()
  state <- if_else((table |> pluck(1))[-1]=="\u25BE", "l","h")
  output<- data.frame(times =times,
                      tides = tides,
                      state = state)
  output |> mutate(season = if_else(max(output$tides)-min(output$tides)>6,
                           "Spring",
                           "Neap")
           )
}


tide_filter <- function(filtered_climb_list, tide_table, climb_time, climb_duration){
 tide_limit<- get_limit_table(filtered_climb_list, 
                  tide_table,
                  climb_time) |>
    mutate(access = map(height_limit,
                        ~climbable_check(
                          ClimbTime,
                          ClimbDuration,
                          Tide_Table,
                          .x))
           )
  left_join(filtered_climb_list,
            tide_limit,
            by = c('Tide Season' = 'Tide Season', 
                   'Tide Height' = 'Tide Height',
                   'Hours to be inaccesible' = 'Hours to be inaccesible')
            )|>
    filter(access == TRUE)|>
    select(-access)
  
}

#add_offset <- function(table){
#  table
#}
#


get_limit_table <- function(filtered_climb_list, tide_table, climb_time) {
  
  season_id <- get_closest_tide_change(tide_table, climb_time) |>
    select(season) |>
    pluck(1) |>
    unique() |> 
    pluck(1)
  
  base <- get_unique_tide_times(filtered_climb_list) |>
    group_by(`Tide Height`,`Hours to be inaccesible`) |>
    mutate(height_limit = if_else(
      `Tide Height` %in% c("High", "Low"),
      get_base_height_limit(
        Tide_Table,
        ClimbTime,
        if_else(`Tide Height` == "High",
                "h",
                "l"
        ),
        seconds(`Hours to be inaccesible`*3600)) +
        if_else(`Tide Season` == "Spring", 
                2.5*(`Tide Season`!= season_id),
                if_else( `Tide Season` == "Neap",
                         -2.5*(`Tide Season`!= season_id),
                         if_else(
                           `Tide Height` == "High", 
                           -2.5*("Spring"!=season_id),
                           2.5*("Spring"==season_id)
                         )
                )),
      ifelse(`Tide Season` == "Neap",
             get_base_height_limit(
               Tide_Table,
               ClimbTime,
               "h",
               seconds(`Hours to be inaccesible`*3600))  -2.5*(`Tide Season`!= season_id) ,
             -1)
    )
    )%>% ungroup()
  base 
}








climb_times <- function(climb_start, climb_duration){
  tibble(t = seq(period_to_seconds(climb_start),
                 period_to_seconds(climb_start)+period_to_seconds(climb_duration),
                 by = 60)
  )
}

tide_data <- function(tide_table){data_to_points(tide_table)} 

real_times_and_heights <- function(climb_start, climb_duration, tide_table){
  tide_height_df <- tide_data(tide_table)
  tide_height_df |> 
    filter(t %in% 
             c(climb_times(climb_start, climb_duration) |> pluck(1)))
}


climbable_check <- function(climb_start, climb_duration, tide_height_df, height_limit){
  check <- (real_times_and_heights(climb_start, climb_duration, tide_height_df) |>
     pluck("h") > height_limit) |>
    summary() |>
    pluck("TRUE")
  
  if(is.null(check)){FALSE} else if(check != nrow(real_times_and_heights(climb_start, climb_duration, tide_height_df))){FALSE} else{TRUE}
}


get_closest_tide_change <- function(tide_table, climb_time){ #grabbing the next high and low tide info
  x <- tide_table |> 
    rowwise() |>
    mutate(difference =
             abs(
               period_to_seconds(times)-
                 period_to_seconds(climb_time)
             )
    ) |> ungroup()
  high <- x |> filter(state == "h") |> filter(difference == min(difference))
  low  <- x |> filter(state == "l") |> filter(difference == min(difference))

output <- rbind(low, high)
output
}

get_base_height_limit <- function(tide_table, climb_time, high_low, hours_inaccessible){
time <- get_closest_tide_change(tide_table, climb_time) |>
    filter(state == high_low)


height <- data_to_points(tide_table) |>
  filter(t == period_to_seconds(time$times) + period_to_seconds(hours_inaccessible)) |>
  select(h) |> 
  pluck(1,1) |> 
  unique()

height
}




get_height_limit <- function(tide_table, climb_time,  hours_inaccessible, unique_season_height){
  
  high_base <- get_base_height_limit(tide_table, climb_time, "h", hours_inaccessible)
  
  low_base <- get_base_height_limit(tide_table, climb_time, "l", hours_inaccessible)
  
  spring <- tribble(~Low,~High, ~season,
                    if(tide_table$season |> pluck(1) =="Spring"){low_base}else(low_base-2.5),
                    if(tide_table$season |> pluck(1) == "Spring"){high_base}else(high_base+2.5),
                    "Spring")
  neap <- tribble(~Low,~High, ~season,
                  if(tide_table$season  |> pluck(1) == "Neap"){low_base}else(low_base+2.5),
                  if(tide_table$season  |> pluck(1) == "Neap"){high_base}else(high_base-2.5),
                  "Neap")
  
  all <- tribble(~Low,~High, ~season,neap$Low, spring$High, "All")
  
  output <- rbind(spring, neap, all) |> mutate(All = -1)
  output
}

#gets the acceptable height based on the 
season_and_state_height <- function(tide_table, climb_time,  duration, season, height){
  get_height_limit(tide_table, climb_time,  duration) |>
    filter(season == !!season) |>
    select(all_of(height)) |>
    pluck(1)
}

#Pivoting wider in aim of speeding up the mutate? 
wide_get_height <- function(tide_table, climb_time,  duration){
  get_height_limit(Tide_Table, ClimbTime, ClimbDuration)%>% pivot_wider(names_from = season, values_from = c(All, High, Low))
}



#Below is the major code that is used to generate the shiny plot of the filtered climbs

#====
Map_Gen <- function(area_name = NULL,
                    climb_name = NULL,
                    climb_grade = NULL,
                    hardest_move = NULL,
                    tide_season = NULL,
                    tide_height = NULL,
                    hours_innacessible = NULL,
                    is_lead = NULL,
                    is_seconded = NULL,
                    limit_by_tide = NULL) {
  FilterList <- Climbing_Filter(Name_Of_Area = area_name,
                                Climb_Name = climb_name,
                                Climb_Grade = climb_grade,
                                Hardest_Move = hardest_move,
                                Tide_Season = tide_season,
                                Tide_Height = tide_height,
                                Hours_Innacessible = hours_innacessible,
                                Lead = is_lead,
                                Seconded= is_seconded,
                                limit_by_tide = limit_by_tide)
  
  FilteredList_Group_Counted <- Island_Climbing |> 
    filter(row_number() %in% FilterList$id) |>
    group_by(`Name Of Area`) |>
    mutate(number = n()) |>
    ungroup()
  
  #Creating a colour wheel with the number of distinct values for no. of climbs
  MyColour <- viridis_pal(option = "C")(
    nrow(unique(FilteredList_Group_Counted|> select(number)))
  )
  
  FilteredList_Group_Counted <- merge(FilteredList_Group_Counted|> 
                                        select(number) |> 
                                        unique() |> 
                                        arrange(number) |> 
                                        mutate(id=row_number()),
                                      FilteredList_Group_Counted,
                                      by="number")
  pal <- colorFactor(palette = MyColour, domain=FilteredList_Group_Counted$number)
  
  leaflet()|> 
    addTiles()|> 
    addCircles(data=
                 FilteredList_Group_Counted,
               lat = ~Latitide,
               lng = ~Longitude,
               radius =
                 5*1.02^FilteredList_Group_Counted$number,
               fillOpacity = 1,
               popup = FilteredList_Group_Counted$`Name Of Area`,
               color = "black",
               #fillColor = "white",
               fillColor = MyColour[FilteredList_Group_Counted$id],
               weight = 0.0001+1.02^FilteredList_Group_Counted$number,
               opacity = 0.9,
               stroke=TRUE,
    )|> 
    addLegend("bottomright",
              pal = pal,
              values = unique(FilteredList_Group_Counted$number),
              title = "# Of Climbs",
              labFormat = labelFormat(prefix = ""),
              opacity = 1
             )
}


