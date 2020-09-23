#' @title Pull data from multiple sensors for a given date range
#'
#' @param start_date character, the start date in "YYYY-MM-DD" format
#' @param end_date character, the end date in "YYYY-MM-DD" format
#' @param config data.table, sensor configuration for multiple sensors
#' @param use_furrr logical, whether to use `{furrr}` for parallel processing
#' @param ... Additional arguments to pass to `pull_sensor()`. Default is `TRUE`
#'
#' @return list, one data.table for each unique sensor.
#' @export
#' @examples
#' \dontrun{
#' library(tc.sensors)
#'
#' sensor_config <- pull_configuration()
#'
#' pull_sensor_range(start_date = "2018-01-01",
#' end_date = "2018-01-01",
#' config = sensor_config)
#'
#' }
#'
#'
#' @importFrom future plan multisession
#' @importFrom tictoc tic toc
#' @importFrom furrr future_map future_map_dfr
#' @importFrom purrr map map_dfr
pull_sensor_range <- function(start_date,
                              end_date,
                              config,
                              use_furrr = TRUE,
                              ...){

  tictoc::tic()
  date_range <- seq(from = as.Date(start_date),
                    to = as.Date(end_date), by = "days")

  if(length(date_range) > 40) {
    warning("Date range is greater than 60 days. Consider breaking up your date range
            into shorter time periods")
  }

    if(use_furrr == TRUE){
      future::plan(future::multisession)


      sensor_data <- furrr::future_map(
        config$detector_name,
        .f = function(det_name) {
          furrr::future_map_dfr(seq(1:length(date_range)),
                                .f = function(x) {
                                  pull_sensor(det_name, date_range[[x]],
                                              ...)
                                })
        })

      tictoc::toc()
      return(sensor_data)

    } else if(use_furrr == FALSE){

      warning("Pulling data without furrr increases the function run time significantly.")

      sensor_data <- purrr::map(
        config$detector_name,
        .f = function(det_name) {
          purrr::map_dfr(seq(1:length(date_range)),
                         .f = function(x) {
                           pull_sensor(det_name, date_range[[x]],
                                       ...)
                         })
        }
      )

      tictoc::toc()
      return(sensor_data)
    }

}
