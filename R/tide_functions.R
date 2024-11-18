


gen_wave_points <- function(h1, h2, t1, t2, tstep = 60) {
  
  # this whole function is very hard coded for the above data structure
  offset <- 0.75^(h2 < h1) - 1
  
  output <- tibble::tibble(
    t = seq(lubridate::period_to_seconds(t1), lubridate::period_to_seconds(t2), tstep),
    h =  0.5 * (h2 - h1) * cos((lubridate::t-period_to_seconds(t1))/(lubridate::period_to_seconds(t2)-lubridate::period_to_seconds(t1)) * pi - pi) + 0.5*(h1 + h2)
  )
  output
}

gen_waves_from_tide_t <- function(tide_times) {
  (tide_times[-4]) |> 
    tibble::as_tibble() |>
    dpylr::mutate(next_time = dpylr::lead(times),
           next_state = dpylr::lead(state),
           next_height = dpylr::lead(tides)) |> 
    dpylr::filter(!is.na(next_time)) |> 
    dplyr::mutate(points = purrr::pmap(list(tides, next_height, times, next_time),
                         gen_wave_points)) |> 
    dpylr::select(points) |>
    tidyr::unnest(points)
}



#head(points)


#points |>
#  ggplot(aes(x = t, y = h)) + 
#  geom_line() +
#  scale_y_continuous(breaks = 1000:20000, minor_breaks = FALSE) + 
#  theme_bw()
#


#points |>
#  mutate(t = t/3600) |>
#  ggplot(aes(x = t, y = h)) + 
#  geom_hline(yintercept = 5) +
#  geom_line() +
#  geom_point(aes(colour = h > 5)) +
#  scale_y_continuous(breaks = 0:10, minor_breaks = TRUE) + 
#  theme_bw() +
#  labs(colour = "Can I go Climbing?")
#



