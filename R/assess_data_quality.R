#' Assess data quality in speed and travel time records
#'
#' @inheritParams aggregate_sensor
#'
#' @return
#'
#' @details
#'   ## Data quality checks
#'
#'   There are many ways to assess sensor data quality.
#'
#'   A report by Cambridge Systematics, Inc. for the Florida Department of
#'   Transportation (FDOT) used the following checks on five-minute speed/travel time
#'   records.
#'     1. Tukey Method: Rank all travel times for a section, and treat any value greater
#'       than the 75th percentile plus 1.5 times the interquartile distance, or less than the
#'       25th percentile minus 1.5 times the inter-quartile distance as an outlier. This
#'       technique is robust because it uses the quartile values instead of variance to
#'       describe the spread of the data.
#'     2. Two consecutive travel times cannot change more than 40%.
#'     3. If a travel time is more than one standard deviation above or below the
#'       moving average of the 10 previous entries, the travel time will be removed.
#'
#'   \insertCite{cambridge_systematics_inc_use_2015}{tc.sensors}
#'
#' @references
#'   \insertAllCited{}
#'
#' @export
#'
assess_data_quality <- function(sensor_data,
                                config,
                                interval_length){



}
