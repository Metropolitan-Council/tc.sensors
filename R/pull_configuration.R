#' @title Pull metro sensor configuration
#'
#' @description Read MnDOT JSON feed and wrangle into a tidy dataframe containing 20 variables related to sensor configuration.
#'   Useful for mapping (contains lat/lons) and calculating performance measures (contains detector_field).
#'
#' @param return_opt an object of class string which indicates how to return the data.
#'    \code{"within_dir"} will return the data within the directory as a csv entitled
#'    "Configuration of Metro Detectors YYYY-MM-DD".
#'     \code{"in-memory"} will return the data in R, but requires assignment.
#'
#' @return dataframe containing 20 variables, including detector_field and lat/lons,
#'   for each sensor in MnDOT's metro district
#' @family loop sensor functions
#'
#' @examples
#' \dontrun{
#' config <- pull_configuration("in-memory") # Assign to an object
#' pull_configuration("within_dir") # No assignment necessary
#' }
#'
#' @importFrom xml2 read_xml xml_find_all xml_attr xml_path
#' @importFrom dplyr mutate transmute select rename bind_rows bind_cols left_join
#' @importFrom tidyr separate unite
#' @importFrom tibble enframe
#' @importFrom purrr map2
#' @importFrom data.table fwrite
#' @importFrom utils download.file
#'
#' @export
pull_configuration <- function(return_opt = "in_memory", .quiet = TRUE) {
  url <- "http://data.dot.state.mn.us/iris_xml/metro_config.xml.gz"
  tmp <- tempfile()
  utils::download.file(url, tmp, quiet = .quiet)
  metro_config <- xml2::read_xml(gzfile(tmp))

  # ------------------
  # PATHS
  # ------------------

  # Detector paths - connect rnodes and corridors to this
  detector_paths <- tibble::enframe(
    xml2::xml_path(
      xml2::xml_find_all(metro_config, "//detector")
    )
  ) %>%
    dplyr::mutate(detector_path = value) %>%
    tidyr::separate(detector_path, into = c(
      "front", "tms_config",
      "device", "rnode",
      "detector"
    ), sep = "/") %>%
    tidyr::unite(rnode_path, front, tms_config, device, rnode, sep = "/") %>%
    dplyr::mutate(rnode_path = trimws(rnode_path)) %>%
    dplyr::mutate(corridor_path = rnode_path) %>%
    tidyr::separate(corridor_path, into = c(
      "front", "tms_config",
      "device", "rnode"
    ), sep = "/") %>%
    tidyr::unite(corridor_path, front, tms_config, device, sep = "/") %>%
    dplyr::mutate(corridor_path = trimws(corridor_path)) %>%
    dplyr::select(-name) %>%
    dplyr::rename(detector_path = value)

  # Rnode paths
  rnode_paths <- tibble::enframe(
    xml2::xml_path(xml2::xml_find_all(metro_config, "//r_node"))
  ) %>%
    dplyr::transmute(rnode_path = value)

  # Corridor paths
  corridor_paths <- tibble::enframe(
    xml2::xml_path(xml2::xml_find_all(metro_config, "//corridor"))
  ) %>%
    dplyr::transmute(corridor_path = value)

  # ------------------
  # ATTRIBUTES (rnodes & detectors)
  # ------------------


  d_attr_ls <- list(
    "name",
    "label",
    "category",
    "lane",
    "field",
    "abandoned"
  )

  rn_attr_ls <- list(
    "name",
    "n_type",
    "transition",
    "label",
    "lon",
    "lat",
    "lanes",
    "shift",
    "s_limit",
    "station_id",
    "attach_side"
  )

  c_attr_ls <- list("route", "dir")


  attr_all_ls <- list(d_attr_ls, rn_attr_ls, c_attr_ls)
  categories <- list("detector", "r_node", "corridor")

  attributes_full <- purrr::map2(categories, attr_all_ls, attr_to_df, metro_config)

  # Bind paths to attributes
  d_paths_attr <- dplyr::bind_cols(detector_paths, attributes_full[[1]])
  rnode_paths_attr <- dplyr::bind_cols(rnode_paths, attributes_full[[2]])
  corr_paths_attrs <- dplyr::bind_cols(corridor_paths, attributes_full[[3]])

  detector_rnodes_full <- dplyr::left_join(d_paths_attr,
    rnode_paths_attr,
    by = c("rnode_path")
  )
  configuration <- dplyr::left_join(detector_rnodes_full, corr_paths_attrs, by = c("corridor_path"))

  config_tidy <- configuration %>%
    dplyr::select(-rnode, -rnode_path, -detector, -detector_path, -corridor_path) %>%
    dplyr::mutate(date = Sys.Date())

  if (return_opt == "in_memory") {
    return(config_tidy)
  } else if (return_opt == "within_dir") {
    data.table::fwrite(config_tidy, paste0("Configuration of Metro Detectors ", Sys.Date(), ".csv"))
  }

  config_tidy
}

#' Clean node and detector attributes
#'
#' @param category string, one of "detector", "r_node", or "corridor"
#' @param attribute string. Options vary for each category.
#'   Detector attributes
#'     - "name"
#'     - "label"
#'     - "category"
#'     - "lane"
#'     - "field"
#'     - "abandoned"
#'   Node attributes
#'     - "name"
#'     - "n_type"
#'     - "transition"
#'     - "label"
#'     - "lon"
#'     - "lat"
#'     - "lanes"
#'     - "shift"
#'     - "s_limit"
#'     - "station_id"
#'     - "attach_side"
#'   Corridor attributes
#'     - "route"
#'     - "dir"
#' @param metro_config sensor configuration
#'
#'
#' @return formatted configuration
#' @export
#' @keywords internal
#'
#' @importFrom xml2 xml_attr xml_find_all
#'
attr_clean <- function(category, attribute, metro_config) {
  trimws(
    xml2::xml_attr(
      xml2::xml_find_all(
        metro_config, paste0("//", category)
      ), attribute
    )
  )
}


#' Make data frame for category and attributes
#'
#' @inheritParams attr_clean
#' @param attr_ls list of attributes corresponding to the given category
#'
#' @return a data frame of clean attributes for the given category
#' @export
#'
#' @keywords internal
#'
#' @importFrom purrr map2
#' @importFrom dplyr bind_rows
#'
attr_to_df <- function(category, attr_ls, metro_config) {
  attributes_ls <- purrr::map2(category, attr_ls, attr_clean, metro_config)
  names(attributes_ls) <- paste(category, attr_ls, sep = "_")
  dplyr::bind_rows(attributes_ls)
}
