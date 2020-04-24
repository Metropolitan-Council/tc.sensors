#' Pull sensor volume and occupancy
#'
#'
#' @description Create a tidy dataframe, containing volume and occupancy, for a single date and sensor.
#'   Use \code{\link{pull_sensor_ids}} to obtain metro sensor IDs.
#'
#' @param pull_date an object of class string which indicates the date of data to pull.  Needs to by in "\%Y-\%m-\%d" format.
#' @param sensor an object of class integer or string which indicates the sensor ID.
#'   See documentation for \code{\link{pull_sensor_ids}} to obtain metro sensor IDs.
#'
#' @return dataframe containing variables volume, occupancy, sensor, date, time.
#'   Note that occupancy *can* be missing while volume data exists and vice versa.
#'   It is unknown how a loop could be monitoring volume and not occupancy.
#'   Also note that if you assign the output of pull_loops, the result is returned in-memory, and there must be sufficient space in-memory to do so.
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
#' # Parallel mapping example; takes longer if only pulling one or two days because libraries have to be copied to each core
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
#' @importFrom magrittr %>%
#' @importFrom tibble enframe as_tibble
#' @importFrom jsonlite fromJSON
#' @importFrom rowr cbind.fill
#'
#' @family loop sensor functions
#'
#' @export
pull_sensor <- function(sensor, pull_date) {
  extension_pull <- function(ext, ...) {
    pull_year <- format.Date(as.Date(pull_date, format = "%Y-%m-%d"), "%Y")
    pull_month <- format.Date(as.Date(pull_date, format = "%Y-%m-%d"), "%m")
    pull_day <- format.Date(as.Date(pull_date, format = "%Y-%m-%d"), "%d")

    df_default <- as_tibble(NA, validate = F)

    try(df_default <- enframe(fromJSON(paste0("http://data.dot.state.mn.us:8080/trafdat/metro/", pull_year, "/", pull_year, pull_month, pull_day, "/", sensor, ".", ext, "30.json"))) %>%
      select(-name))

    return(df_default)
  }

  # exts <- c("v", "c")
  # loops_ls <- map(exts, extension_pull)

  volume <- extension_pull("v")
  occupancy <- extension_pull("c")

  loop_uneven <- rowr::cbind.fill(volume, occupancy, fill = NA)
  names(loop_uneven) <- c("volume", "occupancy")

  loop_date_sensor <- loop_uneven %>%
    mutate(
      date = pull_date,
      sensor = sensor
    )

  # Add time
  if (nrow(loop_date_sensor) == 1) {
    # Return essentially empty dataframe if both volume and occupancy are missing for entire day
    loop_date_sensor %>%
      mutate(
        hour = NA,
        min = NA
      )
  } else {
    # Add hour and minutes if either volume or occupancy (or both) are available
    bind_cols(
      loop_date_sensor,
      as_tibble(rep(0:23, each = 120)) %>% rename(hour = value),
      as_tibble(rep(seq(from = 0, to = 59.5, by = 0.5), 24)) %>% rename(min = value)
    )
  }
}