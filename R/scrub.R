#' @title Scrub sensor level data
#'
#' @inheritParams aggregate_sensor_data
#'
#' @return
#' @export
#'
scrub_sensor <- function(sensor_data, interval_length) {
  if (interval_length == 1) {
    ## delete duplicates at hourly level
    sensor_data[!duplicated(sensor_data, by = c("date", "hour", "sensor"), fromLast = TRUE)]
  }
}


#' Replace impossible volume and occupancy values with `NA` at given interval
#'
#' @inheritParams aggregate_sensor_data
#'
#' @return the original data.table with impossible volume and occupancy values
#'   replaced with `NA`.
#' @export
#'
#' @import data.table
#'
#' @details
#'   # Flag criteria
#'     - Hourly
#'       - total hourly occupancy exceeds 216,000 scans
#'       - total hourly volume exceeds 2,300
#'     - 30-sec
#'       - total 30-second volume exceeds 20 cars
#'       - total 30-second occupancy exceed 1,800
#'
#'    To find impossible values at any given interval length, we can multiply the
#'
#'
#' @author@R c(person("Ashley", "Asmus"),
#'   person("Liz", "Roten"))
#'
replace_impossible <- function(sensor_data,
                              interval_length = NA) {
  if (length(unique(sensor_data$sensor)) > 1) {
    stop("More than one sensor is in this dataset.")
  }


  if (is.na(interval_length)) {
    if (nrow(sensor_data) != 2880 * length(unique(sensor_data$date))) {
      stop("For multiple dates, you must have at least 2,880 rows for each date you want covered.")
    }

    sensor_data[, volume := ifelse(volume >= 20, NA, volume)][, occupancy := ifelse(occupancy >= 1800, NA, occupancy)]
  } else {
    if (interval_length > 24) {
      stop("Interval cannot exceed 24 hours.")
    }

    sensor_data[, volume.sum := ifelse(volume.sum >= (interval_length * 2300), NA, volume.sum)][, occupancy.sum := ifelse(occupancy.sum >= (interval_length * 216000), NA, occupancy.sum)]
  }

  return(sensor_data)
  # 60 scans per second * 60 secs per min * 60 mins per hour = 216,000 scans per hour
}

#' Append a column with a given date's day type (weekday or weekend),
#'   day of week, and day category (holiday, weekend, or weekday)
#'
#' @inheritParams aggregate_sensor_data
#'
#' @return The original data.table with additional columns
#'   - `day_type` either "Weekday" or "Weekend"
#'   - `day_of_week` one of "Monday", "Tuesday", etc.
#'   - `day_category` one of "Weekday", "Weekend", or "Holiday"
#'
#' @export
#' @import data.table
#' @importFrom tis isHoliday
#' @importFrom dplyr case_when
#'
add_day_type <- function(sensor_data) {
  sensor_data[, `:=`(c("day_of_week", "day_type", "holiday", "day_category"), {
    day_of_week <- weekdays(date)
    day_type <- dplyr::case_when(day_of_week %in% c(
      "Saturday",
      "Sunday"
    ) ~ "Weekend", TRUE ~ "Weekday")
    holiday <- tis::isHoliday(date)
    day_category <- dplyr::case_when(
      holiday == TRUE ~ "Holiday",
      TRUE ~ day_type
    )
    .(day_of_week, day_type, holiday, day_category)
  })][, holiday := NULL]
}
