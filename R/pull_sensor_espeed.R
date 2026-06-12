#' @title Pull sensor estimated speed data
#'
#' @description Fetch estimated speed data for a single date and sensor from the
#'   Mayfly API. Use [pull_sensor_ids()] to obtain metro sensor IDs.
#'   Estimated speed is calculated following methods described [here](https://github.com/mnit-rtmc/iris/blob/master/mayfly/ESPEED.md).
#'
#' @inheritParams pull_sensor
#' @inheritParams mayfly_request
#' @inheritParams pull_configuration
#'
#' @return data.table containing variables espeed, sensor, date, hour, min.
#'
#' @details
#'   ## Estimated vs Measured Speed
#'     Estimated speed is calculated using free-flow speed and traffic conditions.
#'     Use [pull_sensor_speed()] for measured speed data.
#'
#'   ## Filtering
#'     The filtering parameters allow filtering of vehicle observations based on
#'     estimated speed, vehicle length, and headway. This can be useful for traffic
#'     analysis or identifying specific traffic patterns.
#'
#' @examples
#' \dontrun{
#' library(tc.sensors)
#' # Simple example
#' espeed_data <- pull_sensor_espeed(5474, "2025-10-14")
#'
#' # With filtering - passenger cars at typical speeds
#' filtered_data <- pull_sensor_espeed(5474, "2025-10-14",
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
pull_sensor_espeed <- function(sensor, pull_date,
                               fill_gaps = TRUE,
                               length_ft_min = NULL,
                               length_ft_max = NULL,
                               headway_sec_min = NULL,
                               headway_sec_max = NULL,
                               district = "metro",
                               .quiet = TRUE) {
  # Parse date flexibly
  date_parsed <- parse_date_flexible(pull_date)

  # Use extension_pull for data retrieval
  loop_data <- extension_pull(
    endpoint = "espeed",
    column_name = "espeed",
    sensor = sensor,
    pull_date = date_parsed$date_standard,
    district = district,
    length_ft_min = length_ft_min,
    length_ft_max = length_ft_max,
    headway_sec_min = headway_sec_min,
    headway_sec_max = headway_sec_max,
    speed_mph_min = NULL,
    speed_mph_max = NULL,
    quiet = .quiet
  )

  # Add date and sensor info
  loop_data[, `:=`(date = date_parsed$date_standard, sensor = sensor)]

  # Add time components
  if (nrow(loop_data) == 1 && is.na(loop_data$espeed[1])) {
    if (fill_gaps == TRUE) {
      if (.quiet == FALSE) {
        cli::cli_alert("Filling gaps...")
      }

      loop_data <- data.table::as.data.table(
        expand.grid(
          espeed = NA,
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
