#' Aggregate raw sensor data to a chosen level
#'
#' @param sensor_data data frame for single sensor returned from `pull_sensor()`
#' @param interval_length numeric, the interval length in hours.
#'   `NA` indicates no aggregation (30 second data)
#'   `0.25` indicates 15 minutes.
#'   Default is `1`.
#' @param config data.table, a configuration file for the given sensor
#' @param replace_impossible logical, whether to replace impossible values with `NA`.
#'   Default is `TRUE` and *highly* recommended.
#' @param interpolate_missing logical, whether to interpolate missing volume and occupancy
#'   values at the raw data level. Only applies if `replace_impossible` is `TRUE`. Note
#'   that this option increases the function runtime.
#' @param occupancy_pct_threshold numeric, the lowest possible occupancy percentage
#'   to use when calculating speed. Default is `0.0020` or 0.02%. Increasing the threshold
#'   results in more stable speed values, while lowering it may increase speed variability.
#'   A higher occupancy threshold is recommended for shorter interval lengths
#'
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
#'
#'   ## Calculating speed
#'
#'     There are 60 scans per second, which means there are 60 * 60 = 1,800 scans per
#'     30-second interval. The occupancy value in the 30-second interval data
#'     represents the number of scans that were occupied of the 1,800 scans in that
#'     interval.
#'
#'     With 60 scans per second, 60 seconds per minute there are 3,600 scans per minute.
#'     With 3,600 scans per minute, 60 minutes per hour there are 216,000 scans per hour.
#'     To find the number of scans in 15 minutes, we can multiply 0.25 * 216000 = 54,000 scans.
#'
#'     Speed, in miles per hour, is calculate by multiplying the number of
#'     vehicles per hour by field length in miles, divided by the
#'     occupancy for the given interval.
#'
#'   ## Impossible values
#'
#'     Any observation with a volume that exceeds 20 vehicles or an occupancy that exceeds 1,800 scans
#'     will be replaced with `NA`. It is impossible for more than twenty vehicles to pass over a sensor
#'     in only 30 seconds, and the maximum number of scans in 30 seconds is 1,800
#'     (60 scans/second * 30 seconds).
#'
#'   ### Interpolating missing values
#'
#'     `interpolate_missing` indicates whether to interpolate missing volume and occupancy values
#'     at the raw data level. The interpolated value for a given observation is the mean of
#'     the two observations on either side of the observation. This method preserves the variable's
#'     overall distribution.
#'
#' @export
#'
#' @import data.table
#' @importFrom cli cli_abort
#' @importFrom units set_units as_units
#'
#' @examples
#' \dontrun{
#'
#' library(tc.sensors)
#' library(dplyr)
#' config <- pull_configuration()
#'
#' config_sample <- dplyr::filter(config, config$detector_abandoned == "f") %>%
#'   dplyr::sample_n(1)
#' yesterday <- as.Date(Sys.Date() - 365)
#'
#' sensor_results <- pull_sensor(
#'   sensor = config_sample$detector_name[[1]],
#'   pull_date = yesterday
#' )
#'
#' aggregate_sensor(sensor_results,
#'   interval_length = 1,
#'   config = config_sample
#' )
#' }
aggregate_sensor <- function(sensor_data,
                             config,
                             interval_length,
                             replace_impossible = TRUE,
                             interpolate_missing = FALSE,
                             occupancy_pct_threshold = 0.0020) {
  # input checks ---------------------------------------------------------------
  if (is.na(interval_length)) {
    cli::cli_abort("No aggregation to do!")
  }

  if (interval_length > 24) {
    cli::cli_abort("Interval cannot exceed 24 hours.")

    if (length(unique(sensor_data$date)) <= 1) {
      cli::cli_abort("For intervals greater than 24 hours, you must have data for more than one date")
    }
  }

  if (nrow(sensor_data) != (2880 * length(unique(sensor_data$date)))) {
    cli::cli_abort("For multiple dates, you must have at least 2,880 rows for each date you want covered.")
  }

  if (length(unique(sensor_data$sensor)) > 1) {
    cli::cli_abort("More than one sensor is in this dataset.")
  }

  # format data ----------------------------------------------------------------
  sensor_data <- data.table::as.data.table(sensor_data)
  config <- data.table::as.data.table(config)[detector_name == sensor_data$sensor[[1]]]

  # number of scans in the given interval length
  # 3600 * 60 = 216,000 scans per hour
  interval_scans <- interval_length * 216000
  field_length <- as.numeric(config[, "detector_field"][[1]])
  # convert field length from feet to miles
  field_length_miles <- field_length %>%
    units::as_units("feet") %>%
    units::set_units("miles") %>%
    as.numeric()

  if (replace_impossible == TRUE) {
    sensor_data <- tc.sensors::replace_impossible(
      sensor_data = sensor_data,
      interval_length = NA
    )

    if (interpolate_missing == TRUE) {
      sensor_data <- sensor_data[
        , `:=`(volume.rollmean = data.table::shift(
          # calculate rolling average volume
          data.table::frollapply(volume, 3, mean,
            align = "center",
            na.rm = TRUE,
            hasNA = TRUE
          )
        )),
        by = .(sensor)
      ][
        , volume := ifelse(is.na(volume), volume.rollmean, volume)
      ][
        # calculate rolling average occupancy
        , `:=`(occupancy.rollmean = data.table::shift(
          data.table::frollapply(occupancy, 3, mean,
            align = "center",
            na.rm = TRUE,
            hasNA = TRUE
          )
        )),
        by = .(sensor)
      ][
        , occupancy := ifelse(is.na(occupancy), occupancy.rollmean, occupancy)
      ][
        , .(volume, occupancy, date, sensor, hour, min)
      ]
    }
  }

  # find interval length in minutes
  interval_length_min <- interval_length * 60
  n_rows_expected <- interval_length_min * 2 # two scans/observations per minute

  # there are 60 scans/second in the raw data
  # 60 * 30 = 1,800 scans/ 30 sec (the interval we are given)
  # 60 * 60 = 3,600 scans/minute
  # 3600 * 60 = 216,000 scans per hour
  # 216,000 = number of scans in one hour

  if (interval_length < 1) {
    # if the interval length is less than an hour

    # create minute bins
    bins <- seq(0, 60, interval_length * 60)

    # add minute bins to sensor_data
    sensor_data[
      , interval_min_bin := findInterval(sensor_data$min, bins)
    ][
      , start_min := min(min),
      by = .(date, hour, interval_min_bin)
    ]

    sensor_data_agg <- sensor_data[
      , as.list(unlist(lapply(.SD, function(x) {
        list(
          # create sum, mean, and pct.null columns for volume and occupancy
          # resulting in "volume.sum", "volume.mean", etc.
          # grouped by date, hour, start_min, interval, and sensor
          sum = round(sum(x, na.rm = TRUE)),
          mean = mean(x, na.rm = TRUE),
          pct.null = round(100 * sum(is.na(x)) / length(x), 1)
        )
      }))),
      by = .(date, hour, start_min, interval_min_bin, sensor),
      .SDcols = c("volume", "occupancy")
    ][
      , start_datetime := as.character(as.POSIXct(
        paste(date, hour, start_min),
        format = "%Y-%m-%d %H %M"
      ))
    ][
      , occupancy.pct := (occupancy.sum / interval_scans)
    ][
      , speed := ifelse(
        # if volume is not 0 and occupancy.pct is gte occupancy threshold
        volume.sum != 0 & occupancy.pct >= occupancy_pct_threshold,
        # speed is volume * number of interval periods in a single hour * field length in miles
        # divided by occupancy
        (volume.sum * (60 / interval_length_min) * field_length_miles) / occupancy.pct,
        # otherwise, NA
        NA
      )
    ]
  } else {
    # if the interval length is greater than or equal to 1 hour
    # create hour bins
    bins <- seq(0, 24, interval_length)

    # add hour bins to sensor_data
    sensor_data[
      , date := data.table::as.IDate(date)
    ][
      , year := data.table::year(date)
    ][
      , interval_bin := findInterval(sensor_data$hour, bins)
    ]

    # order by date
    data.table::setorder(sensor_data, date)

    sensor_data_agg <- sensor_data[
      , as.list(unlist(lapply(.SD, function(x) {
        list(
          # create sum, mean, and pct.null columns for volume and occupancy
          # resulting in "volume.sum", "volume.mean", etc.
          # grouped by date, interval, and sensor
          sum = round(sum(x, na.rm = TRUE)),
          mean = mean(x, na.rm = TRUE),
          pct.null = round(100 * sum(is.na(x)) / length(x), 1)
        )
      }))),
      by = .(date, interval_bin, sensor),
      .SDcols = c("volume", "occupancy")
    ][
      # if total occupancy is gte the number of interval scans, make NA
      , occupancy.sum := ifelse(occupancy.sum >= interval_scans, NA, occupancy.sum)
    ][
      # calculate occupancy percent of all interval scans
      , occupancy.pct := (occupancy.sum / interval_scans)
    ][
      , speed := ifelse(
        # if volume is not 0 and occupancy percent meets threshold value,
        volume.sum != 0 & occupancy.pct >= occupancy_pct_threshold,
        # calculate speed
        # volume * field length in miles
        # divided by occupancy
        # all over the interval length in hours
        ((volume.sum * field_length_miles) /
          (occupancy.pct)) / interval_length,
        # otherwise, speed is NA
        NA
      )
    ]
  }

  return(sensor_data_agg)
}
