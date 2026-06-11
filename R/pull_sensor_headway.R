#' @title Pull sensor headway data
#'
#' @description Fetch headway (time between vehicles in seconds) data for a single
#'   date and sensor from the Mayfly API. Use [pull_sensor_ids()] to obtain metro sensor IDs.
#'   At this time very few sensors in the metro area report headway data. Sensor 6206 is one example.
#'
#' @inheritParams pull_sensor
#' @inheritParams mayfly_request
#' @inheritParams pull_configuration
#'
#' @return data.table containing variables headway, sensor, date, hour, min.
#'
#' @details
#'   ## Filtering
#'     The filtering parameters allow filtering of vehicle observations based on
#'     headway (time between vehicles), vehicle length, and speed. This can be useful
#'     for identifying specific traffic patterns or filtering outliers.
#'
#' @examples
#' \dontrun{
#' # Simple example
#' headway_data <- pull_sensor_headway(6206, "2018-10-14")
#'
#' # With filtering - passenger cars with 2-10 second headway
#' filtered_data <- pull_sensor_headway(6206, "2018-10-14",
#'   headway_sec_min = 2,
#'   headway_sec_max = 10,
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
pull_sensor_headway <- function(
  sensor, pull_date,
  fill_gaps = TRUE,
  length_ft_min = NULL,
  length_ft_max = NULL,
  headway_sec_min = NULL,
  headway_sec_max = NULL,
  speed_mph_min = NULL,
  speed_mph_max = NULL,
  district = "metro",
  .quiet = TRUE
) {
  # Parse date flexibly
  date_parsed <- parse_date_flexible(pull_date)

  # Use extension_pull for data retrieval
  loop_data <- extension_pull(
    endpoint = "headway",
    column_name = "headway",
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
  if (nrow(loop_data) == 1 && is.na(loop_data$headway[1])) {
    if (fill_gaps == TRUE) {
      if (.quiet == FALSE) {
        cli::cli_alert("Filling gaps...")
      }

      loop_data <- data.table::as.data.table(
        expand.grid(
          headway = NA,
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
