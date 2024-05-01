#' @title Pull sensor volume and occupancy
#'
#' @description Create a tidy data frame, containing volume and occupancy,
#'     for a single date and sensor.
#'     Use [pull_sensor_ids()] to obtain metro sensor IDs.
#'
#' @param pull_date character, the date of data to pull.
#'   Needs to be in `"YYYY-MM-DD"` format.
#' @param sensor character, the sensor ID.
#'   See [pull_sensor_ids()] to obtain metro sensor IDs.
#' @param fill_gaps logical, whether to fill gaps in the time series with `NA`
#'   values. Default is `TRUE`
#' @inheritParams pull_configuration
#'
#' @return data frame containing variables volume, occupancy, sensor, date, time.
#'
#' @details
#'   ## Output
#'
#'     A complete year's worth of data for volume or occupancy for one sensor
#'      usually results in a file that is around ~30-31KB.
#'
#'     Approximate time to pull one sensor's and one extension's
#'      ("v" or "c" for volume or occupancy, respectively) data across
#'       a year on a Mac is 1.33 minutes.
#'
#'     Also note that if you assign `pull_sensor()`'s output, the result is returned in-memory,
#'     and there must be sufficient space in-memory to do so.
#'
#'   ## Missing data
#'
#'     Occupancy *can* be missing while volume data exists and vice versa.
#'     It is unknown how a loop could be monitoring volume and not occupancy.

#'
#' @examples
#' \dontrun{
#' # Simple example
#' loop_data <- pull_sensor(5474, "2018-10-14")
#'
#' # Mapping example
#' date_range <- seq(as.Date("2019/01/01"), as.Date("2019/01/02"), by = "days")
#' loop_data <- pmap(list(8564, date_range), pull_sensor)
#' loops_full <- rbindlist(loop_data)
#'
#' # Parallel mapping example
#' ## takes longer if only pulling 1-2 days because libraries are copied to each core
#' library(parallel)
#' cl <- makeCluster(detectCores() - 1) # Leaving one core unused
#' params <- list(8564, date_range)
#'
#' clusterSetRNGStream(cl, 1)
#' loop_data <- params %>%
#'   lift(clusterMap, cl = cl)(fun = pull_sensor)
#' stopCluster(cl)
#'
#' loops_full <- rbindlist(loop_data)
#' }
#' @import data.table
#' @importFrom tibble enframe as_tibble
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr bind_cols rename
#' @importFrom cli cli_alert
#'
#' @family loop sensor functions
#'
#' @export
pull_sensor <- function(sensor, pull_date,
                        fill_gaps = TRUE,
                        .quiet = TRUE) {
  # browser()

  volume <- extension_pull("v", "volume", pull_date = pull_date, sensor = sensor, quiet = .quiet)
  occupancy <- extension_pull("c", "occupancy", pull_date = pull_date, sensor = sensor, quiet = .quiet)

  loop_uneven <- data.table::as.data.table(dplyr::bind_cols(volume, occupancy))

  loop_date_sensor <- loop_uneven[, `:=`(date = pull_date, sensor = sensor)]

  # Add time
  if (nrow(loop_date_sensor) == 1) {
    if (fill_gaps == TRUE) {
      if (.quiet == FALSE) {
        cli::cli_alert("Filling gaps...")
      }

      loop_date_sensor <- data.table::as.data.table(
        expand.grid(
          volume = NA,
          occupancy = NA,
          date = pull_date,
          sensor = sensor,
          hour = 0:23,
          min = seq(0, 59.5, 0.5)
        )
      )
    } else if (fill_gaps == FALSE) {
      # Return empty data.table if both volume and occupancy are missing for entire day
      loop_date_sensor[, `:=`(hour = NA, min = NA)]
    }
  } else {
    # Add hour and minutes if either volume or occupancy (or both) are available
    loop_date_sensor[, `:=`(
      hour = rep(0:23, each = 120),
      min = rep(seq(0, 59.5, by = 0.5), 24)
    )]
  }

  return(loop_date_sensor)
}



#' Pull extension
#'
#' @param ext string, either `"v"` for volume or `"c"` for occupancy
#' @param ext_name string, either `"volume"` or `"occupancy"`
#' @param quiet logical, whether to hide messages. Default is `TRUE`
#' @inheritParams pull_sensor
#' @keywords internal
#'
#' @return a tibble
#'
#' @export
extension_pull <- function(ext, ext_name, sensor, pull_date, quiet = TRUE) {
  # browser()

  pull_year <- format.Date(as.Date(pull_date, format = "%Y-%m-%d"), "%Y")
  pull_month <- format.Date(as.Date(pull_date, format = "%Y-%m-%d"), "%m")
  pull_day <- format.Date(as.Date(pull_date, format = "%Y-%m-%d"), "%d")

  df_default <- tibble::as_tibble(NA)

  try(
    df_default <- tibble::enframe(
      jsonlite::fromJSON(
        txt = paste0(
          "http://data.dot.state.mn.us/trafdat/metro/",
          pull_year,
          "/",
          pull_year,
          pull_month,
          pull_day,
          "/",
          sensor,
          ".",
          ext,
          "30.json"
        )
      )
    ) %>%
      dplyr::select(value),
    silent = quiet
  )
  names(df_default) <- ext_name

  return(df_default)
}
