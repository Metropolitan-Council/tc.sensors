#' @title Append weather data from Automated Surface Observing System (ASOS) via
#'   the Iowa State University API.
#'
#' @description Access temperature, precipitation, wind speed, and visibility
#'   measures for a given ASOS station in Minnesota.
#'
#' @param sensor_data data.table, a data.table of aggregated sensor data.
#' @param save_raw boolean, whether to save the raw data. Default is `FALSE`
#' @param save_location character, relative path to save location
#' @param interval_length nunmeric, interval length in hours. Default is `1`.
#' @param station character, ASOS station code. Default is `"MSP"` for the Minneapolis/St. Paul airport
#' @param time_zone character, time zone code. Default is `"America%2FChicago"`.
#'
#' @details
#'   For additional Minnesota station ID codes, see
#'   the [Mesonet station directory](http://mesonet.agron.iastate.edu/sites/networks.php?network=MN_ASOS).
#'
#' @return the original data.table with columns added:
#'   - `tmpf.mean` numeric, the mean temperature in Fahrenheit
#'   - `p01i.sum` numeric, the total precipitation in inches
#'   - `p01i.mean` numeric, the mean precipitation in inches
#'   - `sknt.mean` numeric, the mean wind speed in knots
#'   - `vsby.mean` numeric, the mean visibility distance in miles
#'
#'   See [Mesonet](http://mesonet.agron.iastate.edu/request/download.phtml?network=MN_ASOS) for additional variables and descriptions.
#' @export
#'
#' @import data.table
#' @importFrom curl has_internet
#' @importFrom tis day
#'
#' @examples
#'
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
#'   pull_date = yesterday )
#'
#' aggregate_sensor_data(sensor_results,
#'   interval_length = 1,
#'   config = config_sample) %>%
#' add_weather()
#' }
add_weather <- function(sensor_data,
                        save_raw = FALSE,
                        save_location = ".",
                        interval_length = 1,
                        station = "MSP",
                        time_zone = "America%2FChicago") {
  if (curl::has_internet() == FALSE) {
    stop("You must be connected to the internet to access weather data")
  }

  if (interval_length < 1) {
    stop("Choose an interval length of at least one hour")
  }

  min_date <- min(sensor_data$date)
  max_date <- max(sensor_data$date) + 1

  request <- paste0(
    "http://mesonet.agron.iastate.edu/cgi-bin/request/asos.py?",
    "data=all",
    "&tz=America%2FChicago",
    "&format=onlycomma",
    "&latlon=yes",
    "&missing=null",
    "&trace=0.0001",
    "&station=", # station ID
    station,
    # min year, month, day
    "&year1=",
    data.table::year(min_date),
    "&month1=",
    data.table::month(min_date),
    "&day1=",
    tis::day(min_date),
    # max year, month, day
    "&year2=",
    data.table::year(max_date),
    "&month2=",
    data.table::month(max_date),
    "&day2=",
    tis::day(max_date)
  )

  # meta <- str_c("https://mesonet.agron.iastate.edu/geojson/network/", network, ".geojson", sep="")
  # jdict <- fromJSON(url(meta))

  message(paste("Downloading:", station, "for", min_date, "to", max_date - 1, sep = " "))
  data <- data.table::data.table(read.csv(request, na.strings = "null"))

  if (save_raw == TRUE) {
    curl::curl_download(request, destfile = paste0(
      save_location, "/MN_ASOS_",
      station,
      "_",
      time_zone,
      "_",
      format(min_date, "%Y%m%d"),
      "_to_",
      format(max_date, "%Y%m%d"),
      ".csv"
    ))
  }

  bins <- seq(0, 24, interval_length)

  clean_weather <- data[, date := data.table::as.IDate(valid)][, hour := data.table::hour(valid)][!is.na(tmpf)][, interval_bin := findInterval(hour, bins)]
  data.table::setorder(clean_weather, date)


  weather_sensor_agg <- clean_weather[, as.list(unlist(lapply(.SD, function(x) {
    list(
      sum = sum(x, na.rm = T),
      mean = round(mean(x, na.rm = T), 1)
    )
  }))),
  by = .(date, interval_bin),
  .SDcols = c("tmpf", "p01i", "sknt", "vsby")
  ][, .(
    date, interval_bin, tmpf.mean, p01i.sum, p01i.mean,
    sknt.mean, vsby.mean
  )][sensor_data, on = .(date, interval_bin), allow.cartesian = FALSE]


  return(weather_sensor_agg)
}
