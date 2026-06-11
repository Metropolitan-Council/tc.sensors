#' @title Pull sensor volume and occupancy
#'
#' @description Create a tidy data frame, containing volume and occupancy,
#'     for a single date and sensor.
#'     Use [pull_sensor_ids()] to obtain metro sensor IDs.
#'
#' @param pull_date character, the date of data to pull.
#'   Needs to be in `"YYYY-MM-DD"` format.
#' @param sensor character, the sensor ID.
#'   See [pull_sensor_ids()] to obtain metro sensor IDs.
#' @param fill_gaps logical, whether to fill gaps in the time series with `NA`
#'   values. Default is `TRUE`
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
#'     Approximate time to pull one sensor's and one extension's
#'      ("v" or "c" for volume or occupancy, respectively) data across
#'       a year on a Mac is 1.33 minutes.
#'
#'     Also note that if you assign `pull_sensor()`'s output, the result is returned in-memory,
#'     and there must be sufficient space in-memory to do so.
#'
#'   ## Missing data
#'
#'     Occupancy *can* be missing while volume data exists and vice versa.
#'     It is unknown how a loop could be monitoring volume and not occupancy.

#'
#' @examples
#' \dontrun{
#' # Simple example
#' loop_data <- pull_sensor(5474, "2018-10-14")
#'
#' # Mapping example
#' date_range <- seq(as.Date("2019/01/01"), as.Date("2019/01/02"), by = "days")
#' loop_data <- pmap(list(8564, date_range), pull_sensor)
#' loops_full <- rbindlist(loop_data)
#'
#' # Parallel mapping example
#' ## takes longer if only pulling 1-2 days because libraries are copied to each core
#' library(parallel)
#' cl <- makeCluster(detectCores() - 1) # Leaving one core unused
#' params <- list(8564, date_range)
#'
#' clusterSetRNGStream(cl, 1)
#' loop_data <- params %>%
#'   lift(clusterMap, cl = cl)(fun = pull_sensor)
#' stopCluster(cl)
#'
#' loops_full <- rbindlist(loop_data)
#' }
#' @import data.table
#' @importFrom tibble enframe as_tibble
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr bind_cols rename
#' @importFrom cli cli_alert
#'
#' @family loop sensor functions
#'
#' @export
pull_sensor <- function(sensor, pull_date,
                        fill_gaps = TRUE,
                        .quiet = TRUE) {
  # browser()

  volume <- extension_pull("v", "volume", pull_date = pull_date, sensor = sensor, quiet = .quiet)
  occupancy <- extension_pull("c", "occupancy", pull_date = pull_date, sensor = sensor, quiet = .quiet)

  loop_uneven <- data.table::as.data.table(dplyr::bind_cols(volume, occupancy))

  loop_date_sensor <- loop_uneven[, `:=`(date = pull_date, sensor = sensor)]

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
          date = pull_date,
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
