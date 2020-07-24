#' Find the distance between all sensors based on corridor and direction. Essential for estimating travel time.
#'
#' @param config data.table, sensor configuration for multiple sensors
#' @param interpolate_missing logical, default is `TRUE`. Whether to interpolate
#'   missing distance values.
#'
#' @details
#'   It is best to call this function on the entire sensor configuration data table;
#'   you can access the table with `pull_configuration()`. Non-station node types
#'   will be
#'
#'   ## Interpolation
#'     Where upstream detector does not exist, or where distance is
#'     beyond 3 miles, interpolate so as not to assume large VMT
#'     merely because vehicles crossed an isolated detector.
#'
#'     If there is no upstream detector, or distance attributed is greater
#'     than 1.5 miles, interpolate with the corridor median.
#'
#'     If, after interpolating with corridor median, distance is still
#'     missing (i.e. NA for entire corridor), or distance attributed is
#'      greater than 3 miles (want a conservative assumption for how
#'      many miles of travel volume at a particular sensor depicts),
#'      interpolate with metro-wide network median.
#'
#' @return The original data.table with additional columns
#'   - `distance` the distance between the given sensor and the
#'     nearest upstream sensor in miles. Valid for `Station` node
#'     types only; all others will appear as `NA`.
#'
#' @export
#' @import data.table
#' @importFrom geosphere distm distHaversine
#' @importFrom purrr map
#' @importFrom stats median
#'
#' @examples
#' \dontrun{
#'
#' library(tc.sensors)
#'
#' config <- pull_configuration()
#'
#' add_distance(config, interpolate_missing = TRUE)
#' }
add_distance <- function(config,
                         interpolate_missing = TRUE) {
  # browser()
  .config <- as.data.table(config)

  # input checks ---------------------------------------------------------------
  if (nrow(.config) < 1) {
    stop("There must be more than one sensor in the configuration")
  }

  if (min(.config[, .(n = .N), keyby = .(corridor_route)][, n]) < 2) {
    stop("There must be at least two sensors for each corridor in the configuration")
  }

  # Select stations (want distance between stations, not between ramps etc.)
  config_stations <- as.data.table(.config)[r_node_n_type == "Station", ]

  if (nrow(config_stations) == 0) {
    stop("There must be station node types in the configuration")
  }


  # Conflate lanes so they have the same upstream detector; create index
  corridor_indexed <- unique(config_stations[
    , .(corridor_route, corridor_dir, r_node_lat, r_node_lon)
  ])[
    , `:=`(corridor_index = seq_len(.N)),
    keyby = .(corridor_route, corridor_dir)
  ]

  # Create lagged index in order to connect lat/longs of upstream detector to detector of interest
  corridor_indexed_lagged <- data.table::setnames(
    copy(corridor_indexed)[
      , `:=`(corridor_index = corridor_index - 1)
    ],
    c("r_node_lat", "r_node_lon"),
    c("r_node_lat_up", "r_node_lon_up")
  )

  # Join upstream detectors to full detector dataset
  config_full <- merge(
    corridor_indexed[config_stations,
      on =
        .(
          r_node_lon, r_node_lat,
          corridor_route, corridor_dir
        ),
      allow.cartesian = TRUE
    ],
    corridor_indexed_lagged,
    all.x = TRUE, all.y = FALSE, by.x = c(
      "corridor_route", "corridor_dir", "corridor_index"
    ),
    by.y = c(
      "corridor_route", "corridor_dir", "corridor_index"
    ), allow.cartesian = TRUE
  )

  dist_results <- purrr::map(c(1:nrow(config_full)), function(i) {
    geosphere::distm(
      x = c(
        as.numeric(config_full[i, ][, r_node_lon]),
        as.numeric(config_full[i, ][, r_node_lat])
      ),
      y = c(
        as.numeric(config_full[i, ][, r_node_lon_up]),
        as.numeric(config_full[i, ][, r_node_lat_up])
      ),
      fun = geosphere::distHaversine
    ) / 1609.344 # convert meters to miles
  })

  distance_table <- data.table::data.table(distance = unlist(dist_results))
  config_stations$distance <- distance_table$distance
  config_final <- config_stations

  if (interpolate_missing == TRUE) {
    config_final <- config_final[
      , `:=`(distance = ifelse(is.na(distance) | distance > 1.5,
        stats::median(distance, na.rm = TRUE), distance
      )),
      keyby = .(corridor_route, corridor_dir)
    ][
      , `:=`(distance = ifelse(is.na(distance) | distance > 3,
        stats::median(distance, na.rm = TRUE), distance
      ))
    ]
  }
  config_final_joined <- config_final[as.data.table(config),
    on = .(
      detector_name, detector_label, detector_category,
      detector_lane, detector_field, detector_abandoned, r_node_name,
      r_node_n_type, r_node_transition, r_node_label, r_node_lon,
      r_node_lat, r_node_lanes, r_node_shift, r_node_s_limit, r_node_station_id,
      r_node_attach_side, corridor_route, corridor_dir, date
    ),
    allow.cartesian = TRUE
  ]

  # nrow(config) == nrow(config_final_joined)

  return(config_final_joined)
}
