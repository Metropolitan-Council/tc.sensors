#' Aggregate raw sensor data to a chosen level
#'
#' @param sensor_data data frame for single sensor returned from `pull_sensor()`
#' @param intverval_length numeric, the interval length in hours. Default is `1`.
#'   `0.25` indicates 15 minutes.
#'
#' @return
#' @export
#'
#' @import data.table
#'
aggregate_sensor_data <- function(sensor_data, config, interval_length) {

  if(interval_length > 24){
    stop("Interval cannot exceed 24 hours.")

    if(length(unique(sensor_data$date)) <= 1){
      stop("For intervals greater than 24 hours, you must have data for more than one date")
    }
  }

  if(nrow(sensor_data) != 2880 * length(unique(sensor_data$date))){
    stop("For multiple dates, you must have at least 2,880 rows for each date you want covered.")
  }


  if(length(unique(sensor_data$sensor)) > 1){
    stop("More than one sensor is in this dataset.")
  }

  sensor_data <- data.table::as.data.table(sensor_data)
  # browser()

  if (interval_length < 1) {
    # if the interval is 15, then there are 30 observations in 15 minutes
    interval_length_min <- 60 * interval_length
    bins <- seq(0, 60, interval_length_min)

    sensor_data[, interval_min_bin := findInterval(sensor_data$min, bins)][, start_min := min(min), by = .(date, hour, interval_min_bin)]


    sensor_data_agg <- sensor_data[, as.list(unlist(lapply(.SD, function(x) {
      list(
        pct_nulls = round(100 * sum(is.na(x)) / interval_length * 2),
        sum = round(interval_length * 2 * mean(x, na.rm = T))
      )
    }))),
    by = .(date, hour, start_min, sensor),
    .SDcols = c("volume", "occupancy")
    ][
      ,
      start_datetime := as.POSIXct(paste(date, hour, start_min),
                                   format = "%Y-%m-%d %H %M"
      )
    ][, occupancy.pct := (occupancy.sum / interval_scans)][, speed := ifelse(volume.sum != 0,
                                                                             ((volume.sum * as.numeric(config[, "detector_field"][[1]])) / (5280 * occupancy.pct)) / interval_length, 0
    )][, start_datetime := as.character(start_datetime)][, hour := NULL][, date := NULL][, start_min := NULL]
  } else {
    bins <- seq(0, 24, interval_length)
    sensor_data[, date := data.table::as.IDate(date)][, year := data.table::year(date)][, interval_bin := findInterval(sensor_data$hour, bins)]
    data.table::setorder(sensor_data, date)

    sensor_data_agg <- sensor_data[, as.list(unlist(lapply(.SD, function(x) {
      list(
        sum = sum(x, na.rm = T),
        mean = mean(x, na.rm = T),
        pct.null = round(100 * sum(is.na(x)) / length(x))
      )
    }))),
    by = .(date, interval_bin, sensor),
    .SDcols = c("volume", "occupancy")
    ][, occupancy.pct := (occupancy.sum / interval_scans)][, speed := ifelse(volume.sum != 0,
                                                                             ((volume.sum * as.numeric(config[, "detector_field"][[1]])) / (5280 * occupancy.pct)) / interval_length, 0
    )]
  }
  return(sensor_data_agg)
}
