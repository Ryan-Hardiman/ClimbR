








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

test <- get_limit_table(Island_Climbing, 
                            Tide_Table,
                            ClimbTime)  %>%  mutate(access = map(height_limit,
                                                                 ~climbable_check(
                                                                   ClimbTime,
                                                                   ClimbDuration,
                                                                   Tide_Table,
                                                                 .x)))



test <- left_join(
  Island_Climbing,
  get_limit_table(Island_Climbing, 
                  Tide_Table,
                  ClimbTime
                  ),
  by = c(
    'Tide Season' ='Tide Season',
    'Tide Height'='Tide Height',
    'Hours to be inaccesible' = 'Hours to be inaccesible' ))

