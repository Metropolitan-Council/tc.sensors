#' @title Pull sensor volume and occupancy
#'
#' @description Create a tidy data frame, containing volume and occupancy,
#'     for a single date and sensor.
#'     Use [pull_sensor_ids()] to obtain metro sensor IDs.
#'
#' @param pull_date character, the date of data to pull.
#'   Accepts either `"YYYY-MM-DD"` or `"YYYYMMDD"` format.
#' @param sensor character, the sensor ID.
#'   See [pull_sensor_ids()] to obtain metro sensor IDs.
#' @param fill_gaps logical, whether to fill gaps in the time series with `NA`
#'   values. Default is `TRUE`
#' @param length_ft_min numeric, minimum vehicle length in feet for filtering. Optional. Default is `NULL.`
#' @param length_ft_max numeric, maximum vehicle length in feet for filtering (non-inclusive). Optional. Default is `NULL.`
#' @param headway_sec_min numeric, minimum headway in seconds for filtering. Optional. Default is `NULL.`
#' @param headway_sec_max numeric, maximum headway in seconds for filtering (non-inclusive). Optional. Default is `NULL.`
#' @param speed_mph_min numeric, minimum speed in mph for filtering. Optional. Default is `NULL.`
#' @param speed_mph_max numeric, maximum speed in mph for filtering (non-inclusive). Optional. Default is `NULL.`
#' @inheritParams mayfly_request
#' @inheritParams pull_configuration
#'
#' @return data frame containing variables volume, occupancy, sensor, date, time.
#'
#' @details
#'   ## Output
#'
#'     A complete year's worth of data for volume or occupancy for one sensor
#'      usually results in a file that is around ~30-31KB.
#'
#'     Also note that if you assign `pull_sensor()`'s output, the result is returned in-memory,
#'     and there must be sufficient space in-memory to do so.
#'
#'   ## Missing data
#'
#'     Occupancy *can* be missing while volume data exists and vice versa.
#'     It is unknown how a loop could be monitoring volume and not occupancy.
#'
#'   ## Filtering
#'
#'     The length, headway, and speed filtering parameters allow filtering of
#'     vehicle observations. This can be useful for vehicle classification
#'     (e.g., filtering by length to separate cars from trucks) or identifying
#'     specific traffic patterns. Filters apply to both volume and occupancy data.
#'
#' @examples
#' \dontrun{
#' # Simple example
#' library(tc.sensors)
#' library(purrr)
#' library(data.table)
#' loop_data <- pull_sensor(5474, "2025-10-14")
#'
#' # With filtering - passenger cars only (7-19 feet)
#' library(tc.sensors)
#' cars_only <- pull_sensor(5474, "2025-10-14",
#'   length_ft_min = 7,
#'   length_ft_max = 19
#' )
#'
#' # Mapping example
#' date_range <- seq(as.Date("2024/01/01"), as.Date("2024/01/02"), by = "days")
#' loop_data <- map(date_range, ~ pull_sensor(8564, .x))
#' loops_full <- rbindlist(loop_data)
#'
#' # Parallel mapping example with furrr
#' library(furrr)
#' plan(multisession, workers = parallel::detectCores() - 1)
#'
#' date_range <- seq(as.Date("2024/01/01"), as.Date("2024/01/02"), by = "days")
#' loop_data <- future_map(date_range, ~ pull_sensor(8564, .x))
#' loops_full <- rbindlist(loop_data)
#' }
#' @import data.table
#' @importFrom cli cli_alert
#' @importFrom httr2 req_url_query
#'
#' @family loop sensor functions
#'
#' @export
pull_sensor <- function(sensor, pull_date,
                        fill_gaps = TRUE,
                        length_ft_min = NULL,
                        length_ft_max = NULL,
                        headway_sec_min = NULL,
                        headway_sec_max = NULL,
                        speed_mph_min = NULL,
                        speed_mph_max = NULL,
                        district = "metro",
                        .quiet = TRUE) {
  # browser()

  # Parse date flexibly
  date_parsed <- parse_date_flexible(pull_date)
  pull_date_std <- date_parsed$date_standard

  volume <- extension_pull(
    endpoint = "counts",
    column_name = "volume",
    pull_date = pull_date_std,
    sensor = sensor,
    district = district,
    length_ft_min = length_ft_min,
    length_ft_max = length_ft_max,
    headway_sec_min = headway_sec_min,
    headway_sec_max = headway_sec_max,
    speed_mph_min = speed_mph_min,
    speed_mph_max = speed_mph_max,
    quiet = .quiet
  )
  occupancy <- extension_pull(
    endpoint = "occupancy",
    column_name = "occupancy",
    pull_date = pull_date_std,
    sensor = sensor,
    district = district,
    length_ft_min = length_ft_min,
    length_ft_max = length_ft_max,
    headway_sec_min = headway_sec_min,
    headway_sec_max = headway_sec_max,
    speed_mph_min = speed_mph_min,
    speed_mph_max = speed_mph_max,
    quiet = .quiet
  )

  # Combine volume and occupancy data.tables
  loop_uneven <- cbind(volume, occupancy)
  data.table::setDT(loop_uneven)

  loop_date_sensor <- loop_uneven[, `:=`(date = pull_date_std, sensor = sensor)]

  # Add time
  if (nrow(loop_date_sensor) == 1) {
    if (fill_gaps == TRUE) {
      if (.quiet == FALSE) {
        cli::cli_alert("Filling gaps...")
      }

      loop_date_sensor <- data.table::as.data.table(
        expand.grid(
          volume = NA,
          occupancy = NA,
          date = pull_date_std,
          sensor = sensor,
          hour = 0:23,
          min = seq(0, 59.5, 0.5)
        )
      )
    } else if (fill_gaps == FALSE) {
      # Return empty data.table if both volume and occupancy are missing for entire day
      loop_date_sensor[, `:=`(hour = NA, min = NA)]
    }
  } else {
    # Add hour and minutes if either volume or occupancy (or both) are available
    loop_date_sensor[, `:=`(
      hour = rep(0:23, each = 120),
      min = rep(seq(0, 59.5, by = 0.5), 24)
    )]
  }

  return(loop_date_sensor)
}


#' Pull extension - generic sensor data retrieval
#'
#' @param endpoint string, Mayfly API endpoint: "counts", "occupancy", "speed",
#'   "espeed", "headway", or "length"
#' @param column_name string, name for the returned data column
#' @param quiet logical, whether to hide messages. Default is `TRUE`
#' @inheritParams pull_sensor
#' @keywords internal
#'
#' @return a data.table
#'
#' @export
extension_pull <- function(endpoint, column_name, sensor, pull_date,
                           district = "metro",
                           length_ft_min = NULL,
                           length_ft_max = NULL,
                           headway_sec_min = NULL,
                           headway_sec_max = NULL,
                           speed_mph_min = NULL,
                           speed_mph_max = NULL,
                           quiet = TRUE) {
  # Validate date is available before making request
  # Parse date flexibly
  date_parsed <- parse_date_flexible(pull_date)

  validate_date_available(date_parsed$date_standard, district = district, .quiet = quiet)

  # Convert date components
  pull_year <- date_parsed$year
  date_yyyymmdd <- date_parsed$date_yyyymmdd

  # Build request
  req <- mayfly_request(
    endpoint = endpoint,
    district = district,
    year = pull_year,
    date = date_yyyymmdd,
    detector = sensor
  )

  # Add query parameters if filtering
  filter_params <- list()
  if (!is.null(length_ft_min)) filter_params$length_ft_min <- length_ft_min
  if (!is.null(length_ft_max)) filter_params$length_ft_max <- length_ft_max
  if (!is.null(headway_sec_min)) filter_params$headway_sec_min <- headway_sec_min
  if (!is.null(headway_sec_max)) filter_params$headway_sec_max <- headway_sec_max
  if (!is.null(speed_mph_min)) filter_params$speed_mph_min <- speed_mph_min
  if (!is.null(speed_mph_max)) filter_params$speed_mph_max <- speed_mph_max

  if (length(filter_params) > 0) {
    req <- httr2::req_url_query(req, !!!filter_params)
  }

  # Perform request
  data <- mayfly_perform(req, .quiet = quiet)

  # Convert to data.table with single column
  if (is.null(data) || length(data) == 0) {
    df_default <- data.table::data.table(value = NA)
  } else {
    df_default <- data.table::data.table(value = data)
  }

  names(df_default) <- column_name

  return(df_default)
}
