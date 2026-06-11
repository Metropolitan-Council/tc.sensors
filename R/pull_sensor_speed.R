#' @title Pull sensor speed data
#'
#' @description Fetch measured speed data for a single date and sensor from the
#'   Mayfly API. Use [pull_sensor_ids()] to obtain metro sensor IDs.
#'
#' @inheritParams pull_sensor
#' @inheritParams mayfly_request
#' @inheritParams pull_configuration
#'
#' @return data.table containing variables speed, sensor, date, hour, min.
#'
#' @details
#'   ## Filtering
#'     The filtering parameters allow filtering of vehicle observations based on
#'     measured speed, vehicle length, and headway. This can be useful for traffic
#'     analysis, identifying speeding, or filtering unrealistic measurements.
#'
#' @examples
#' \dontrun{
#' # Simple example
#' speed_data <- pull_sensor_speed(5474, "2018-10-14")
#'
#' # Passenger cars traveling at highway speeds
#' highway_cars <- pull_sensor_speed(5474, "2018-10-14",
#'   speed_mph_min = 55,
#'   speed_mph_max = 75,
#'   length_ft_min = 10,
#'   length_ft_max = 20
#' )
#' }
#'
#' @import data.table
#' @importFrom httr2 req_url_query
#' @importFrom cli cli_alert
#'
#' @family loop sensor functions
#'
#' @export
pull_sensor_speed <- function(sensor, pull_date,
                              fill_gaps = TRUE,
                              length_ft_min = NULL,
                              length_ft_max = NULL,
                              headway_sec_min = NULL,
                              headway_sec_max = NULL,
                              speed_mph_min = NULL,
                              speed_mph_max = NULL,
                              district = "metro",
                              .quiet = TRUE) {
  # Parse date flexibly
  date_parsed <- parse_date_flexible(pull_date)

  # Use extension_pull for data retrieval
  loop_data <- extension_pull(
    endpoint = "speed",
    column_name = "speed",
    sensor = sensor,
    pull_date = date_parsed$date_standard,
    district = district,
    length_ft_min = length_ft_min,
    length_ft_max = length_ft_max,
    headway_sec_min = headway_sec_min,
    headway_sec_max = headway_sec_max,
    speed_mph_min = speed_mph_min,
    speed_mph_max = speed_mph_max,
    quiet = .quiet
  )

  # Add date and sensor info
  loop_data[, `:=`(date = date_parsed$date_standard, sensor = sensor)]

  # Add time components
  if (nrow(loop_data) == 1 && is.na(loop_data$speed[1])) {
    if (fill_gaps == TRUE) {
      if (.quiet == FALSE) {
        cli::cli_alert("Filling gaps...")
      }

      loop_data <- data.table::as.data.table(
        expand.grid(
          speed = NA,
          date = date_parsed$date_standard,
          sensor = sensor,
          hour = 0:23,
          min = seq(0, 59.5, 0.5)
        )
      )
    } else {
      loop_data[, `:=`(hour = NA, min = NA)]
    }
  } else {
    loop_data[, `:=`(
      hour = rep(0:23, each = 120),
      min = rep(seq(0, 59.5, by = 0.5), 24)
    )]
  }

  return(loop_data)
}
