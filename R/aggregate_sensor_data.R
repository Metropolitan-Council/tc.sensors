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
aggregate_sensor_data <- function(sensor_data, config, interval_length, calculations = c("volume.sum", "occupancy.sum")) {
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
    ][, start_datetime := as.character(start_datetime)][, hour := NULL][, date := NULL][, start_min := NULL]
  } else {
    sensor_data[, date := data.table::as.IDate(date)][, year := data.table::year(date)]
    data.table::setorder(sensor_data, date)

    sensor_data_agg <- sensor_data[, as.list(unlist(lapply(.SD, function(x) {
      list(
        nulls = sum(is.na(x)),
        sum = sum(x, na.rm = T),
        mean = mean(x, na.rm = T)
      )
    }))),
    by = .(date, hour, sensor),
    .SDcols = c("volume", "occupancy")
    ]

    if (interval_length == 24) {
      # browser()


      sensor_data_agg <- sensor_data_agg[, as.list(unlist(lapply(
        .SD,
        function(x) {
          list(
            sum = sum(x),
            mean = mean(x)
          )
        }
      ))),
      by = .(date, sensor),
      .SDcols = calculations
      ]

      sensor_data_agg[, c("volume.sum.mean", "occupancy.sum.mean") := NULL]
      setnames(sensor_data_agg, old = "volume.sum.sum", new = "volume.sum")
      setnames(sensor_data_agg, old = "occupancy.sum.sum", new = "occupancy.sum")
      # setnames(sensor_data_agg, old = 'speed.mean', new = 'speed')
    }
  }
  return(sensor_data_agg)
}
