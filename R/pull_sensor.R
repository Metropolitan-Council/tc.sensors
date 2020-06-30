#' @title Pull sensor volume and occupancy
#'
#'
#' @description Create a tidy dataframe, containing volume and occupancy, for a single date and sensor.
#'   Use \code{\link{pull_sensor_ids}} to obtain metro sensor IDs.
#'
#' @param pull_date an object of class string which indicates the date of data to pull.
#'   Needs to by in "YYYY-MM-DD" format.
#' @param sensor an object of class integer or string which indicates the sensor ID.
#'   See documentation for \code{\link{pull_sensor_ids}} to obtain metro sensor IDs.
#' @inheritParams pull_configuration
#'
#' @return dataframe containing variables volume, occupancy, sensor, date, time.
#'   Note that occupancy *can* be missing while volume data exists and vice versa.
#'   It is unknown how a loop could be monitoring volume and not occupancy.
#'   Also note that if you assign the output of pull_loops, the result is returned in-memory,
#'   and there must be sufficient space in-memory to do so.
#'
#' @examples
#' \dontrun{
#' # Simple example
#' loop_data <- pull_sensor(5474, "2018-10-14")
#' # :   # Mapping example
#' date_range <- seq(as.Date("2019/01/01"), as.Date("2019/01/02"), by = "days")
#' loop_data <- pmap(list(8564, date_range), pull_sensor)
#' loops_full <- rbindlist(loop_data)
#'
#' # Parallel mapping example;
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
#' @importFrom tibble enframe as_tibble
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr bind_cols rename
#' @importFrom rlang .data
#'
#' @family loop sensor functions
#'
#' @export
pull_sensor <- function(sensor, pull_date, .quiet = TRUE) {
  # browser()
  # exts <- c("v", "c")
  # loops_ls <- map(exts, extension_pull)

  volume <- extension_pull("v", pull_date = pull_date, sensor = sensor, quiet = .quiet)
  occupancy <- extension_pull("c", pull_date = pull_date, sensor = sensor, quiet = .quiet)

  loop_uneven <- dplyr::bind_cols(volume, occupancy)
  names(loop_uneven) <- c("volume", "occupancy")

  loop_date_sensor <- loop_uneven %>%
    dplyr::mutate(
      date = pull_date,
      sensor = sensor
    )

  # Add time
  if (nrow(loop_date_sensor) == 1) {
    # Return essentially empty dataframe if both volume and occupancy are missing for entire day
    loop_date_sensor %>%
      dplyr::mutate(
        hour = NA,
        min = NA
      )
  } else {
    # Add hour and minutes if either volume or occupancy (or both) are available
    dplyr::bind_cols(
      loop_date_sensor,

      tibble::as_tibble(rep(0:23, each = 120)) %>%
        dplyr::rename(hour = .data$value),

      tibble::as_tibble(rep(seq(from = 0, to = 59.5, by = 0.5), 24)) %>%
        dplyr::rename(min = .data$value)
    )
  }
}



#' Pull extension
#'
#' @param ext either \code{"v"} for volume or \code{"c"} for occupancy
#' @param quiet boolean, whether to hide messages. Default is TRUE
#' @inheritParams pull_sensor
#' @keywords internal
#'
#' @return a tibble
#'
#' @export
extension_pull <- function(ext, sensor, pull_date, quiet = TRUE) {
  # browser()
  pull_year <- format.Date(as.Date(pull_date, format = "%Y-%m-%d"), "%Y")
  pull_month <- format.Date(as.Date(pull_date, format = "%Y-%m-%d"), "%m")
  pull_day <- format.Date(as.Date(pull_date, format = "%Y-%m-%d"), "%d")

  df_default <- tibble::as_tibble(NA)

  try(df_default <- tibble::enframe(
    jsonlite::fromJSON(
      txt = paste0(
        "http://data.dot.state.mn.us:8080/trafdat/metro/",
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
    dplyr::select(.data$name),
  silent = quiet
  )

  return(df_default)
}
