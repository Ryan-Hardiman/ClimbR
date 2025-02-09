#' Create an interactive Leaflet map of climbing crags
#'
#' @param df A tibble containing crag data with columns: name_of_area, latitude, longitude, and climb_name.
#' @return A Leaflet map visualizing crags with markers sized and colored by the number of climbs.
#' @importFrom dplyr group_by summarise n ungroup mutate
#' @importFrom leaflet leaflet addTiles addCircleMarkers addLegend
#' @importFrom viridis viridis_pal
#' @export
#'
#' @examples
#' plot_crags(my_crag_data)
plot_crags <- function(df) {
  
  # Vectorized function to format grades
  format_grades <- function(grade1, grade2) {
    ifelse(!is.na(grade2), paste0(grade1, " / ", grade2), grade1)
  }
  
  # Vectorized function to format hardest moves
  format_moves <- function(move1, move2) {
    ifelse(!is.na(move2), paste0("(", move1, " / ", move2, ")"),ifelse(!is.na(move1), paste0("(", move1, ")"),""))
  }
  
  
  # Summarize data by crag
  crag_summary <- df |> 
    dplyr::group_by(name_of_area, latitude, longitude) |> 
      dplyr::summarise(
        climb_count = dplyr::n(),
        climbs_list = paste0(
          "<li>", 
          paste(
            unique(paste0(
              climb_name, " - ", 
              format_grades(overall_grade_1, overall_grade_2), " ",
              format_moves(hardest_move_1, hardest_move_2)
            )), 
            collapse = "</li><li>"
          ), 
          "</li>"
        ),
        .groups = "drop"
      ) |> 
        dplyr::ungroup()
  
  # Define color scale
  max_climbs <- max(crag_summary$climb_count, na.rm = TRUE)
  colour_scale <- viridis::viridis_pal(option = "C")(max_climbs)
  my_pal <- leaflet::colorFactor(palette = colour_scale, domain = crag_summary$climb_count)
  
  # Create Leaflet map
  leaflet::leaflet(crag_summary) |> 
    leaflet::addTiles() |> 
    leaflet::addCircleMarkers(
      ~longitude, ~latitude,
      radius = ~5 * 1.009^climb_count,
      fillOpacity = 1,
      color = "black",
      fillColor = ~my_pal(climb_count),
      weight = 1.2,
      opacity = 0.9,
      stroke = TRUE,
      popup = ~as.character(paste0(
        "<b>", name_of_area, "</b><br>",
        "<div style='max-height: 100px; overflow-y: auto; padding:5px; border:1px solid #ccc; background-color:#f9f9f9;'>",
        "<ul style='margin: 0; padding-left: 20px;'>", climbs_list, "</ul>",
        "</div>"
      ))
    ) |> 
    leaflet::addLegend(
      "bottomright",
      pal = my_pal,
      values = ~climb_count,
      title = "# Of Climbs",
      opacity = 1
    ) |> 
    htmlwidgets::onRender("
    function(el, x) {
      // Find the legend container inside the Leaflet map
      let legends = el.getElementsByClassName('leaflet-control');

      for (let i = 0; i < legends.length; i++) {
        let legend = legends[i];

        if (legend.innerHTML.includes('# Of Climbs')) {
          legend.style.maxHeight = '250px';
          legend.style.overflowY = 'auto';
          legend.style.backgroundColor = 'rgba(255, 255, 255, 0.8)'; // Ensure visibility
          legend.style.padding = '5px';
          legend.style.border = '1px solid #ccc';
        }
      }
    }
  ")
}
