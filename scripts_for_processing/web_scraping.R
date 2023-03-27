#Below we scrape tide data from seeker.gg
Website <- read_html("https://seeker.gg/Tides") |> html_table()#Grabs data from Seeker.gg

#Today_Table <- Website |>
#  pluck(1) |>
#  Clean_Tide_Tables()    #Today data
#  
#Tomorrow_Table <- Website |>
#  pluck(2)|> 
#  Clean_Tide_Tables() |>
#  mutate(times = times + hours(24))   #Tomorrows data

#Option here to add on many more days (possibly stopping at a week) but so far can only do it manually as above, could write a function to pluck(x+n) and then add hours (24*(n-1))

#Tide_Table <- rbind(Today_Table , Tomorrow_Table)

# Below is code to get a weeks worth of tides
             day_n_tide <- function(n){
               
               output <- Website |> 
                 pluck(n) |> 
                 Clean_Tide_Tables() |>
                 mutate(times = times + hours(24*(n-1)))
               
             }
             
             
             Tide_Table <- do.call("rbind", map(c(1,2,3,4,5,6,7), ~day_n_tide(.x)) )


