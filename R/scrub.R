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


