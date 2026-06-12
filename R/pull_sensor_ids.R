#' @title Function to pull all sensor IDs for a district
#'
#' @description Create a tidy dataframe containing sensor IDs for a MnDOT district,
#'   mainly to be used with [pull_sensor()]. Data is fetched from the Mayfly API.
#'
#' @param pull_date character, the date to query for available sensors.
#'   Accepts either `"YYYY-MM-DD"` or `"YYYYMMDD"` format.
#'   If `NULL` (default), uses the most recent available date.
#' @inheritParams mayfly_request
#' @inheritParams pull_configuration
#' @return data.table containing variable "detector"
#'
#' @family loop sensor functions
#' @examples
#' \dontrun{
#' library(tc.sensors)
#' sensors <- pull_sensor_ids()
#' # Get sensors for a different district
#' sensors_d1 <- pull_sensor_ids(district = "d6")
#' # Get sensors for a specific date
#' sensors_date <- pull_sensor_ids(pull_date = "2020-01-15")
#' }
#' @import data.table
#'
#' @export
pull_sensor_ids <- function(pull_date = NULL, district = "metro", .quiet = TRUE) {
  # Mayfly /detectors requires year and date params.

  if (!is.null(pull_date)) {
    # Use provided date
    date_parsed <- parse_date_flexible(pull_date)
    query_year <- date_parsed$year
    query_date <- date_parsed$date_yyyymmdd
  } else {
    # Use the most recent available year and date
    years <- mayfly_get_years(district = district)
    if (is.null(years) || length(years) == 0) {
      cli::cli_abort("Unable to fetch available years from Mayfly API")
    }
    query_year <- max(years)

    dates <- mayfly_get_dates(district = district, year = query_year)

    if (is.null(dates) || length(dates) == 0) {
      cli::cli_abort("Unable to fetch available dates from Mayfly API for {query_year}")
    }
    query_date <- max(dates)
  }

  # Query Mayfly API for detector list
  req <- mayfly_request(
    endpoint = "detectors",
    district = district,
    date = query_date
  )
  detectors <- mayfly_perform(req, .quiet = .quiet)

  if (is.null(detectors) || length(detectors) == 0) {
    cli::cli_abort("Unable to fetch detector list from Mayfly API")
  }

  # Return as data.table matching old format
  data.table::data.table(detector = detectors)
}
