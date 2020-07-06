#' @title Scrub sensor level data
#'
#' @param sensor_data data frame returned from `pull_sensor()`
#' @param level string, options are "raw", "fifteen", "hour"
#'
#' @return
#' @export
#'
scrub_sensor <- function(sensor_data) {

}


#' Flag impossible values at given aggregation level
#'
#' @inheritParams scrub_sensor
#'
#' @return
#' @export
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
flag_impossible <- function(sensor_data) {


  # 60 scans per second * 60 secs per min * 60 mins per hour = 216,000 scans per hour
}

#' Append a column with a given date's day type (weekday or weekend),
#'   day of week, and day category (holiday, weekend, or weekday)
#'
#' @inheritParams scrub_sensor
#'
#' @return The original data frame with additional columns
#'   - `day_type` either "Weekday" or "Weekend"
#'   - `day_of_week` one of "Monday", "Tuesday", etc.
#'   - `day_category` one of "Weekday", "Weekend", or "Holiday"
#'
#' @export
#' @import data.table
#' @importFrom tis isHoliday
#' @importFrom dplyr case_when
#'
get_day_type <- function(sensor_data) {


  sensor_data[, `:=`(c("day_of_week", "day_type", "holiday", "day_category"),
          {
            day_of_week <- weekdays(date)
            day_type <- dplyr::case_when(day_of_week %in% c("Saturday",
                                                            "Sunday") ~ "Weekend", TRUE ~ "Weekday")
            holiday <- tis::isHoliday(date)
            day_category <- dplyr::case_when(holiday == TRUE ~ "Holiday",
                                             TRUE ~ day_type)
            .(day_of_week, day_type, holiday, day_category)
          })][, holiday := NULL]

}
