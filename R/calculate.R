#' Calculate speed in miles per hour at different temporal aggregation levels
#'
#' @param sensor_data a data table of sensor data at 30 second intervals
#' @param intverval_length numeric, the interval length in hours. Default is `1`.
#'   `0.25` indicates 15 minutes.
#'
#' @details
#'   There are 60 scans per second, which means there are 60*60 = 1,800 scans per
#'   30-second interval. The occupancy value in the 30-second interval data
#'   represents the number of scans that were occupied of the 1,800 scans in that
#'   interval.
#'
#'   With 60 scans per second, 60 seconds per minute there are 3,600 scans per minute.
#'   With 3,600 scans per minute, 60 minutes per hour there are 216,000 scans per hour.
#'   To find the number of scans in 15 minutes, we can multiply 0.25 * 216000 = 54,000 scans.
#'
#'
#'
#' @return
#' @export
#' @family calculations
#'
#' @import data.table
calculate_speed <- function(sensor_data, configuration, interval_length = 1) {
  sensor_data <- data.table::as.data.table(sensor_data)
  sensor_data_agg <- aggregate_sensor_data(sensor_data, config = configuration, interval_length = interval_length)

  sensor_data_config <- merge(sensor_data_agg, configuration,
    all.x = T, all.y = F,
    by.x = "sensor", by.y = "detector_name"
  )
  # number of scans in the given interval length
  interval_scans <- interval_length * 216000

  # average length of vehicles that pass over the sensor
  detector_field_val <- as.numeric(sensor_data_config$detector_field[[1]])

  # there are 60 scans/second
  # 60*30 = 1,800 scans/ 30 sec (the interval we are given)
  # 60*60 = 3,600 scans/minute
  # 3600*60 = 216,000 scans per hour
  # 216,000 = number of scans in one hour
  # 5,280 = number of feet per mile, used to convert length of car to miles
  #
  sensor_data_speed <- sensor_data_config[, occupancy.pct := (occupancy.sum / interval_scans)]
  sensor_data_speed[, speed := ifelse(volume.sum != 0,
    (volume.sum * as.numeric(detector_field)) / (5280 * occupancy.pct), 0
  )]


  # Vehicles per hour = Volume in 10 min * 6;
  # Occupancy (per hour is assumed to be same as per 10-min interval, since it's a %) = Occupancy in 10 min interval/(Total scans in 30 seconds (1800) * 20 30-sec periods in one 10-min interval) - note that these coefficients do NOT need to be changed if interval changes, since an interval change results in an increase of the multiplier for volume (increase of intervals in an hour), but a DECREASE of the denominator (occupancy's) multiplier

  return(sensor_data_speed)
}
