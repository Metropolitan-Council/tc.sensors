#' Function to pull all sensor IDs in the Twin Cities metro
#'
#' Create a tidy dataframe containing sensor IDs for MnDOT metro district, mainly to be used with pull_sensor
#'
#' @return dataframe containing variable "detector"
#'
#' @family loop sensor functions
#' @examples
#' \dontrun{
#' sensors <- sensor_pull()
#' }
#' @importFrom xml2 read_xml xml_find_all xml_attr
#' @importFrom dplyr transmute
#' @importFrom tibble enframe
#' @importFrom utils download.file
#'
#' @export
pull_sensor_ids <- function(.quiet = TRUE) {
  url <- "http://data.dot.state.mn.us/iris_xml/metro_config.xml.gz"
  tmp <- tempfile()
  utils::download.file(url, tmp, quiet = .quiet)
  metro_config <- xml2::read_xml(gzfile(tmp))

  tibble::enframe(trimws(xml2::xml_attr(xml2::xml_find_all(metro_config, "//detector"), "name"))) %>%
    dplyr::transmute(detector = value)
}
