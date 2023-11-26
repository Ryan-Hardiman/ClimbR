`%==%` <- function (e1, e2) {
  if (is.null(e2)) {
    e2 <- e1
  }
    return(e1 %in% e2 )
  
}


restrict_tides <- function(
    noa = NULL,
    cn = NULL,
    ts = NULL,
    th = NULL,
    h2bi = NULL,
    og1 = NULL,
    og2 = NULL,
    hm1 = NULL,
    hm2 = NULL,
    duration = NULL,
    climb_time = NULL,
    tide_table = NULL){
  
  if(length(duration, climb_time, tide_table)<3){
    not_present <- map(
      list(
        duration,
        climb_time,
        tide_table
      ), 
      ~if(length(.x) == 0){
        deparse(substitute(.x))
      } 
    )
    stop("Error: the following are expected but were null - " & 
           not_present[grepl("..1"), not_present] %>%
           names() %>%
           paste(collapse = ", ")
           )
    
  }
  
  
  
  df <- island_climbing %>% 
    filter(
      name_of_area %==% noa &
        climb_name %==% cn &
        overall_grade_1 %==% og1 &
        overall_grade_2 %==% og2 &
        hardest_move_1 %==% hm1 &
        hardest_move_2 %==% hm2
      )
}


