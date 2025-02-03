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
  # Summarize data by crag
  crag_summary <- df |> 
    dplyr::group_by(name_of_area, latitude, longitude) |> 
    dplyr::summarise(
      climb_count = dplyr::n(),
      climbs_list = paste0("<li>", paste(unique(climb_name), collapse = "</li><li>"), "</li>")
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
      popup = ~paste0("<b>", name_of_area, "</b><br><ul>", climbs_list, "</ul>")
    ) |> 
    leaflet::addLegend(
      "bottomright",
      pal = my_pal,
      values = ~climb_count,
      title = "# Of Climbs",
      opacity = 1
    )
}
