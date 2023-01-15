times <- tribble(
  ~time,  ~state,  ~height,
  0,   "low",        2,
  0.25,  "high",        8,
  0.5,   "low",        3,
  0.75,  "high",        9,
  1,   "low",        1,
  1.25,  "high",        8
)

gen_points <- function(h1, h2, t1, t2, tstep = 0.01) {
  
  # this whole function is very hard coded for the above data structure
  offset <- 0.75^(h2 < h1) - 1
  
  output <- tibble(
    t = seq(t1, t2, tstep),
    h =  0.5 * (h2 - h1) * cos((t + offset) / 0.5 * 2 * pi - pi) + 0.5*(h1 + h2)
  )
  output
}


data_to_points <- function(tide_times) {
  tide_times %>%
    mutate(next_time = lead(time),
           next_state = lead(state),
           next_height = lead(height)) %>% 
    filter(!is.na(next_time)) %>% 
    mutate(points = pmap(list(height, next_height, time, next_time),
                         gen_points)) %>% 
    select(points) %>% unnest(points)
}



points <- data_to_points(times)

head(points)


points %>%
  ggplot(aes(x = t, y = h)) + 
  geom_line() +
  scale_y_continuous(breaks = -100:100, minor_breaks = FALSE) + 
  theme_bw()


points %>%
  ggplot(aes(x = t, y = h)) + 
  geom_hline(yintercept = 6) +
  geom_line() +
  geom_point(aes(colour = h < 6)) +
  scale_y_continuous(breaks = -100:100, minor_breaks = FALSE) + 
  theme_bw() +
  labs(colour = "Can I go Climbing?")



