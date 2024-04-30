#' Generate spatial lines dataset from sensor configuration
#'
#' @param config data.table, sensor configuration for multiple sensors
#'
#' @return an`sf` object with poly lines for each corridor and the line length
#'   in miles
#' @export
#' @details
#'  ## Important considerations
#'
#'    Lines are generated based on the detector's corridor route, direction, and category,
#'    defined specifically for I-35 and T.H.5 north and south of the cities,
#'    and east and west of the cities. When run with the
#'    full configuration dataset, it can result in some lines over 200 miles
#'    long and passing over areas where the road does not exist. Rather than
#'    running the entire configuration, identify specific corridors and sensors
#'    and narrow the configuration dataset to just those sensors.
#'
#'    If your goal is to map the entire road network, consider using one of the
#'    datasets available from MnDOT or the Metropolitan Council on
#'    [Minnesota Geospatial Commons](https://gisdata.mn.gov/).
#'
#'
#' @examples
#' \dontrun{
#'
#' library(tc.sensors)
#' library(ggplot2)
#'
#' configuration <- pull_configuration()
#'
#' spatial_lines <- generate_spatial_lines(config = configuration)
#'
#' # generate map
#' ggplot() +
#'   geom_sf(data = spatial_lines)
#'
#' # generate line length histogram
#' ggplot() +
#'   geom_histogram(data = spatial_lines, mapping = aes(x = length_miles))
#' }
#'
#' @import data.table
#' @importFrom sf st_as_sf st_cast st_set_crs st_length st_make_valid
#' @importFrom units set_units
#' @importFrom dplyr mutate group_by summarize
generate_spatial_lines <- function(config) {
  # browser()

  # config_df <- as.data.table(config)[r_node_n_type == "Station",][
  #   , corridor_category := ifelse(corridor_route == "I-35" & r_node_lat > 45, "I35 north of cities",
  #                                 ifelse(corridor_route == "I-35" & r_node_lat <= 45, "I35 south of cities",
  #                                        ifelse(corridor_route == "T.H.5" & r_node_lon < -93.3, "5 west of cities",
  #                                               ifelse(corridor_route == "T.H.5" & r_node_lon > -93.3, "5 east of cities",
  #                                                      "Other")))
  #   )
  # ][
  #   , corridor_id := paste(corridor_route, corridor_dir, corridor_category, r_node_n_type, sep = "_")
  #   ][, .(detectors = paste(detector_name, collapse = ",")),
  #     keyby = .(corridor_id)]

  config_coords <- as.data.table(config)[r_node_n_type == "Station", ][
    , corridor_category :=
      ifelse(corridor_route == "I-35" & r_node_lat > 45, "I35 north of cities",
             ifelse(corridor_route == "I-35" & r_node_lat <= 45, "I35 south of cities",
                    ifelse(corridor_route == "T.H.5" & r_node_lon < -93.3, "5 west of cities",
                           ifelse(corridor_route == "T.H.5" & r_node_lon > -93.3, "5 east of cities", "Other")
                    )
             )
      )
  ][
    , corridor_id := paste(corridor_route, corridor_dir, corridor_category, sep = "_")
  ][
    , .(corridor_id, r_node_lat, r_node_lon)
  ]

  lines_sf <- sf::st_as_sf(config_coords, coords = c("r_node_lon", "r_node_lat")) %>%
    dplyr::group_by(corridor_id) %>%
    dplyr::summarise(do_union = FALSE, .groups = "keep") %>%
    sf::st_make_valid() %>%
    suppressMessages(sf::st_cast("LINESTRING", warn = FALSE)) %>%
    sf::st_set_crs(4326) %>%
    dplyr::mutate(length_miles = sf::st_length(geometry) %>%
                    units::set_units("mile") %>%
                    as.numeric())

  return(lines_sf)
}
