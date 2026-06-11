#' @title Function to pull all sensor IDs for a district
#'
#' @description Create a tidy dataframe containing sensor IDs for a MnDOT district,
#'   mainly to be used with [pull_sensor()]. Data is fetched from the Mayfly API.
#'
#' @inheritParams mayfly_request
#' @inheritParams pull_configuration
#' @return data.table containing variable "detector"
#'
#' @family loop sensor functions
#' @examples
#' \dontrun{
#' sensors <- pull_sensor_ids()
#' # Get sensors for a different district
#' sensors_d1 <- pull_sensor_ids(district = "d1")
#' }
#' @import data.table
#'
#' @export
pull_sensor_ids <- function(district = "metro", .quiet = TRUE) {
  # Mayfly /detectors requires year and date params.
  # Use the most recent available year and date.
  years <- mayfly_get_years(district = district)
  if (is.null(years) || length(years) == 0) {
    cli::cli_abort("Unable to fetch available years from Mayfly API")
  }
  latest_year <- max(years)

  dates <- mayfly_get_dates(district = district, year = latest_year)
  if (is.null(dates) || length(dates) == 0) {
    cli::cli_abort("Unable to fetch available dates from Mayfly API for {latest_year}")
  }
  latest_date <- max(dates)

  # Query Mayfly API for detector list
  req <- mayfly_request(
    endpoint = "detectors",
    district = district,
    year = latest_year,
    date = latest_date
  )
  detectors <- mayfly_perform(req, .quiet = .quiet)

  if (is.null(detectors) || length(detectors) == 0) {
    cli::cli_abort("Unable to fetch detector list from Mayfly API")
  }

  # Return as data.table matching old format
  data.table::data.table(detector = detectors)
}
