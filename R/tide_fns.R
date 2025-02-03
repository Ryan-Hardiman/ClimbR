#' Get tides
#'
#' @return Web-scraped data from "Seeker.gg" 
#' @export
#'
#' @examples
get_tides <- function(){
  rvest::read_html("https://seeker.gg/Tides") |>
    rvest::html_table() |>
    purrr::pluck(1)|>
    purrr::set_names(c("dir","time","height"))|>
    dplyr::filter(!dir =="")|>
    dplyr::mutate(
      time = lubridate::hm(time)|>lubridate::period_to_seconds(),
      height = stringr::str_extract(height,"[\\d\\.]+")|>as.numeric(),
      dir = ifelse(dir == "▴","up","down")
      )
}


#' @return A tibble with interpolated tide heights for each time step.
#' @importFrom dplyr mutate
#' @importFrom tibble tibble
#' @importFrom lubridate hours
#' @importFrom minpack.lm nlsLM
#' @export
#'
#' @examples
#' library(tibble)
#' tide_data <- tibble(time = c(3.05, 9.05, 15.5, 21.5), height = c(1.2, 9.5, 1.1, 9.1))
#' interpolate_tides(tide_data)
interpolate_tides <- function(tide_data, time_step = 60) {
  # Load necessary libraries
  requireNamespace("dplyr", quietly = TRUE)
  requireNamespace("lubridate", quietly = TRUE)
  requireNamespace("tibble", quietly = TRUE)
  requireNamespace("minpack.lm", quietly = TRUE)
 
  # Estimate tidal period (assumes semidiurnal cycle)
  tidal_period <- mean(diff(tide_data$time)) * 2
  
  # Fit nonlinear least squares model (cosine function)
  fit <- minpack.lm::nlsLM(
    height ~ A * cos(2 * pi / tidal_period * (time - t0)) + D,
    data = tide_data,
    start = list(A = (max(tide_data$height) - min(tide_data$height)) / 2,
                 D = mean(tide_data$height),
                 t0 = tide_data$time[which.max(tide_data$height)])
  )
  
  # Extract fitted parameters
  A_fit  <- coef(fit)["A"]
  D_fit  <- coef(fit)["D"]
  t0_fit <- coef(fit)["t0"]
  
  # Generate a smooth time sequence for interpolation
  time_seq <- seq(min(tide_data$time), max(tide_data$time) + 12 * 3600, by = time_step)
  
  # Compute interpolated tide heights using fitted parameters
  tide_heights <- A_fit * cos(2 * pi / tidal_period * (time_seq - t0_fit)) + D_fit
  
  # Return a tibble with interpolated tide heights
  tibble::tibble(
    time = time_seq / 3600,  # Convert back to hours
    height = tide_heights
  )
}


#' Filter Data Frame by Tide Conditions
#'
#' Filters a data frame of climbing routes based on tide conditions at a specified start time and duration.
#' The function checks tide data, interpolates the tide heights over time, and determines whether the tide
#' is within the acceptable range for climbing, filtering out any routes that are inaccessible based on tide
#' height and season.
#'
#' @param df A tibble or data frame containing the climbing routes data. It must have the following columns:
#'   - `tide_season`: Season of tide conditions (e.g., "All", "Spring").
#'   - `tide_heights`: Tide state (e.g., "Low", "High").
#' @param start_time Numeric value representing the start time for the climbing session in seconds.
#' @param duration Numeric value representing the duration of the climbing session in seconds.
#'
#' @return A filtered tibble or data frame containing only the climbing routes that are accessible during the
#'   specified time frame, based on tide conditions.
#'
#' @examples
#' # Assuming `df` is a tibble containing climbing route information
#' filtered_df <- filter_df_by_tide(df, start_time = 16250, duration = 3600)
#'
#' @import dplyr
#' @export
filter_df_by_tide <- function(df, start_time, duration){
  tides <- get_tides()
  
  future_tides <- tides |>
    interpolate_points() |>
    dplyr::mutate(
      season = ifelse(height >= 3 & height <=6.5, "All", "Spring"),
      state = dplyr::case_when(
        season == "Spring" & height <= 3 ~ "Low",
        season == "Spring" & height >= 6.5 ~ "High",
        season == "All" & height <= 4.5 ~ "Low",
        season == "All" & height >= 4.5 ~ "High",
        TRUE ~ "All"
      )
    ) |>
    dplyr::filter(
      time * 3600 >= start_time,
      time * 3600 <= start_time + duration
    )
  
  season_present <- future_tides |> dplyr::pull(season) |> unique()
  state_present <- future_tides |> dplyr::pull(state) |> unique()
  
  df |> dplyr::filter(
    tide_season %in% c("All", season_present),
    tide_heights %in% c("All", "High", state_present)
  )
}


