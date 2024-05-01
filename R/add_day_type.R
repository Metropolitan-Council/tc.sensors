#' Append a column with a given date's day type (weekday or weekend),
#'   day of week, and day category (holiday, weekend, or weekday)
#'
#' @inheritParams aggregate_sensor
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
  sensor_data[
    , `:=`(c("day_of_week", "day_type", "holiday", "day_category"), {
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
    })
  ][
    , holiday := NULL
  ]
}
