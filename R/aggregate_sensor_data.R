#' Aggregate raw sensor data to a chosen level
#'
#' @param sensor_data data frame for single sensor returned from `pull_sensor()`
#' @param intverval_length numeric, the interval length in hours. Default is `1`.
#'   `0.25` indicates 15 minutes.
#'
#' @return a data.table with values for volume, occupancy, and speed
#'   - `date` IDate, the given date
#'   - `interval_bin` numeric, the observations interval bin
#'   - `{measure}.pct_null` numeric, the percentage of observations with null
#'      values for the given measure
#'   - `{measure}.sum` numeric, the measure's total over the given interval
#'   - `{measure}.mean` numeric, the measure's mean over the given interval
#'   - `speed` numeric, the mean traffic speed over the given interval
#'
#' @details
#'   ## Calculating speed
#'
#'     There are 60 scans per second, which means there are 60*60 = 1,800 scans per
#'     30-second interval. The occupancy value in the 30-second interval data
#'     represents the number of scans that were occupied of the 1,800 scans in that
#'     interval.
#'
#'     With 60 scans per second, 60 seconds per minute there are 3,600 scans per minute.
#'     With 3,600 scans per minute, 60 minutes per hour there are 216,000 scans per hour.
#'     To find the number of scans in 15 minutes, we can multiply 0.25 * 216000 = 54,000 scans.
#'
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


  # number of scans in the given interval length
  interval_scans <- interval_length * 216000

  # there are 60 scans/second
  # 60*30 = 1,800 scans/ 30 sec (the interval we are given)
  # 60*60 = 3,600 scans/minute
  # 3600*60 = 216,000 scans per hour
  # 216,000 = number of scans in one hour
  # 5,280 = number of feet per mile, used to convert length of car to miles

  if (interval_length < 1) {
    interval_length_min <- 60 * interval_length
    bins <- seq(0, 60, interval_length_min)

    sensor_data[, interval_min_bin := findInterval(sensor_data$min, bins)][, start_min := min(min), by = .(date, hour, interval_min_bin)]

    sensor_data_agg <- sensor_data[, as.list(unlist(lapply(.SD, function(x) {
      list(
        sum = sum(x, na.rm = T),
        mean = mean(x, na.rm = T),
        pct.null = round(100 * sum(is.na(x)) / length(x))
      )
    }))),
    by = .(date, hour, start_min, sensor),
    .SDcols = c("volume", "occupancy")
    ][
      ,
      start_datetime := as.POSIXct(paste(date, hour, start_min),
                                   format = "%Y-%m-%d %H %M"
      )
    ][, occupancy.pct := (occupancy.sum / interval_scans)
    ][, speed := ifelse(volume.sum != 0,
                        ((volume.sum * as.numeric(config[, "detector_field"][[1]]))
                         / (5280 * occupancy.pct)) / interval_length, 0
    )][, start_datetime := as.character(start_datetime)
    ][, hour := NULL][, date := NULL][, start_min := NULL]
  } else {
    bins <- seq(0, 24, interval_length)
    sensor_data[, date := data.table::as.IDate(date)
    ][, year := data.table::year(date)
    ][, interval_bin := findInterval(sensor_data$hour, bins)]
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
    ][, occupancy.pct := (occupancy.sum / interval_scans)
    ][, speed := ifelse(volume.sum != 0,
                        ((volume.sum * as.numeric(config[, "detector_field"][[1]])) /
                           (5280 * occupancy.pct)) / interval_length, 0
    )]
  }
  return(sensor_data_agg)
}
