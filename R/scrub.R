#' @title Scrub sensor level data
#'
#' @inheritParams aggregate_sensor
#'
#' @return All unique observations from the data.table provided
#' @export
#'
scrub_sensor <- function(sensor_data, interval_length = NA) {
  sensor_data[!duplicated(sensor_data, by = c("date", "hour", "min", "sensor"), fromLast = TRUE)]
}


#' Replace impossible volume and occupancy values with `NA` at given interval
#'
#' @inheritParams aggregate_sensor
#'
#' @return the original data.table with impossible volume and occupancy values
#'   replaced with `NA`.
#' @export
#'
#' @import data.table
#' @importFrom cli cli_abort
#'
#' @details
#'   ## Criteria
#'     - Hourly
#'       - total hourly occupancy exceeds 216,000 scans
#'       - total hourly volume exceeds 2,300 cars
#'     - 30-sec
#'       - total 30-second volume exceeds 20 cars
#'       - total 30-second occupancy exceed 1,800 scans
#'     - Percent nulls > 10.
#'
#' @author@R c(person("Ashley", "Asmus"),
#'   person("Liz", "Roten"))
#'
replace_impossible <- function(sensor_data,
                               interval_length = NA) {
  if (length(unique(sensor_data$sensor)) > 1) {
    cli::cli_abort("More than one sensor is in this dataset.")
  }

  if (is.na(interval_length)) {
    if (nrow(sensor_data) != 2880 * length(unique(sensor_data$date))) {
      cli::cli_abort("For multiple dates, you must have at least 2,880 rows for each date you want covered.")
    }

    sensor_data[, volume := ifelse(volume >= 20, NA, volume)][, occupancy := ifelse(occupancy >= 1800, NA, occupancy)]
  } else {
    if (interval_length > 24) {
      cli::cli_abort("Interval cannot exceed 24 hours.")
    }

    sensor_data[, volume.sum := ifelse(volume.sum >= (interval_length * 2300), NA, volume.sum)][, occupancy.sum := ifelse(occupancy.sum >= (interval_length * 216000), NA, occupancy.sum)][, volume.sum := ifelse(volume.pct.null >= 10, NA, volume.sum)][, occupancy.sum := ifelse(occupancy.pct.null >= 10, NA, occupancy.sum)][, speed := ifelse(is.na(volume.sum), NA, speed)][, speed := ifelse(is.na(occupancy.sum), NA, speed)]
  }

  return(sensor_data)
  # 60 scans per second * 60 secs per min * 60 mins per hour = 216,000 scans per hour
}
