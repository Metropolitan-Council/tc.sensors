#' @title Pull vehicle length data for a single sensor and date
#'
#' @description Fetch vehicle length data for a single date and sensor from the
#'   Mayfly API. Use [pull_sensor_ids()] to obtain metro sensor IDs. Vehicle length
#'   can be used for vehicle classification (e.g., cars vs. trucks).
#'
#' @inheritParams pull_sensor
#' @inheritParams mayfly_request
#' @inheritParams pull_configuration
#'
#' @return data.table containing variables length_ft, sensor, date, hour, min.
#'
#' @details
#'   ## Vehicle Classification
#'     Vehicle length filtering enables classification of vehicle types:
#'     - Motorcycles: < 7 feet
#'     - Passenger cars: 7-19 feet
#'     - Small trucks/SUVs: 19-24 feet
#'     - Large trucks: > 24 feet
#'
#'     These are approximate ranges and may vary based on local vehicle mix.
#'
#'   ## Filtering
#'     Additional filtering by speed and headway allows for more refined traffic
#'     analysis and vehicle classification.
#'
#' @examples
#' \dontrun{
#' # Simple example
#' length_data <- pull_sensor_length(5474, "2018-10-14")
#'
#' # Vehicle classification - passenger cars only
#' cars_only <- pull_sensor_length(5474, "2018-10-14",
#'   length_ft_min = 7,
#'   length_ft_max = 19
#' )
#'
#' # Large trucks traveling at highway speeds
#' trucks_highway <- pull_sensor_length(5474, "2018-10-14",
#'   length_ft_min = 24,
#'   speed_mph_min = 55,
#'   speed_mph_max = 75
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
pull_sensor_length <- function(sensor, pull_date,
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
    endpoint = "length",
    column_name = "length_ft",
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
  if (nrow(loop_data) == 1 && is.na(loop_data$length_ft[1])) {
    if (fill_gaps == TRUE) {
      if (.quiet == FALSE) {
        cli::cli_alert("Filling gaps...")
      }

      loop_data <- data.table::as.data.table(
        expand.grid(
          length_ft = NA,
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
