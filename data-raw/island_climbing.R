## code to prepare `island_climbing` dataset goes here

library(readxl)
library(dplyr)
library(stringr)
library(janitor)
library(zoo)
library(tidyr)

island_climbing_raw <- read_excel("data-raw/Island-Climbing.xlsx",
                              sheet = "Ride the Tide") |>
  clean_names()

island_climbing <- island_climbing_raw |>
  rename(latitude = latitide) |>
  mutate(climb_grade = str_replace(climb_grade, "^Vs", "VS") |>
           str_replace("^Hs", "HS"),
         tide_height = str_replace(tide_height, "HIgh", "High"),
         latitude = na.locf(latitude),
         longitude = na.locf(longitude),
         longitude = if_else(name_of_area == "The Sphinx", -2.673698, as.numeric(longitude))) |>
  drop_na(climb_name) |>
  mutate(overall_grade_1 = str_extract(climb_grade, "E[1-9]{1,3}|[MVDHS]{1,3}"), #finds the first overall climb grade
         overall_grade_2 = str_extract(climb_grade, "(?<=\\/)E[1-9]|(?<=\\/)[MVDHS]{1,3}"), #finds the second overall climb grade
         hardest_move_1 =  str_extract(climb_grade, "[1-9][a-c]{1}"), #finds the first hardest move
         hardest_move_2 =  str_extract(climb_grade, "(?<=\\/)[1-9][a-c]")) |> #finds the second hardest move
  select(-c(general_location, climb_grade, lead:date_13)) |>
  rename(hours_to_be_inaccessible = hours_to_be_inaccesible)

usethis::use_data(island_climbing, overwrite = TRUE)
