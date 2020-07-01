#' @title Pull metro sensor configuration
#'
#' @description Read MnDOT JSON feed and wrangle into a tidy dataframe containing 20 variables related to sensor configuration.
#'   Useful for mapping (contains lat/lons) and calculating performance measures (contains detector_field).
#'
#' @param return_opt character, indicate how to return the data.
#'    \code{"within_dir"} will return the data within the directory as a csv entitled
#'    "Configuration of Metro Detectors YYYY-MM-DD".
#'     \code{"in-memory"} will return the data in R, but requires assignment.
#' @param .quiet boolean, whether to hide messages. Default is TRUE
#'
#' @return dataframe containing 20 variables, including detector_field and lat/lons,
#'   for each sensor in MnDOT's metro district
#'   - `detector_name` character, the detector's unique identifier in numbers
#'   - `detector_label` character, the detector's label including abbreviations of the roads associated with the roadway node.
#'   - `detector_category` character, the detector lane type in code. [Source](https://github.com/mnit-rtmc/iris/blob/5b3dcbbcd6d177b2a1d37576bdd06b7d33a6facd/src/us/mn/state/dot/tms/LaneType.java)
#'     - "" Mainline
#'     - "A" Auxiliary, mainline auxiliary (ends within a mile)
#'     - "B" Bypass, ramp meter bypass
#'     - "CD" Collector/Distributor
#'     - "D" Shoulder, mainline shoulder
#'     - "G" Green, ramp meter displayed green count
#'     - "H" High-Occupancy Vehicle
#'     - "HT" High-Occupancy Vehicle or Toll
#'     - "M" Merge, Freeway on-ramp (counts all merging traffic)
#'     - "O" Omnibus, bus only
#'     - "P" Passage, ramp meter passage
#'     - "PK" Parking, parking space presence detector
#'     - "Q" Queue, ramp metering queue
#'     - "R" Reversible mainline
#'     - "V" Velocity, mainline speed loop
#'     - "X" Exit, freeway exit ramp
#'   - `detector_lane` character, the detector's lane. Lanes are numbered from right-to-left, starting with the right lane as 1.
#'   - `detector_field` character, the detector's field length in feet
#'   - `detector_abandoned` character, whether the detector is no longer in use.
#'   - `r_node_name` character, abbreviated roadway node name.
#'   - `r_node_n_type` character, one of "Station", "Exit", "Entrance", or "Intersection"
#'   - `r_node_transition` character, how the entrance or exit nodes connect with linked nodes
#'   - `r_node_label` character, unique road name, including affixes such as “St” or “Rd” or stall number
#'   - `r_node_lon` character, the roadway node longitude
#'   - `r_node_lat` character, the roadway node latitude
#'   - `r_node_lanes` character, for an entrance or exit ramp, this is the number of lanes entering or exiting the corridor. Otherwise, it is the number of lanes on the corridor.
#'   - `r_node_shift` character,  the difference (number of lanes) between the corridor reference lane and the attach side of the roadway node.
#'   - `r_node_s_limit` character, the posted speed limit in miles per hour
#'   - `r_node_station_id` character, A unique identifier for the detectors associated with a station roadway node
#'   - `r_node_attach_side` character, whether the r_node is attached to the left side of the road. This can be used to create left entrance or exit ramps.
#'   - `corridor_route` character, the corridor route name. All roadway nodes with the same road and direction-of-travel are grouped into corridors.
#'   - `corridor_dir` character, the corridor route direction. One of "EB", "NB", "SB", or "WB"
#'   - `date`date, the date the configuration was accessed
#'
#' @details Additional documentation on the IRIS system can be found on MNIT [documentation page](https://mnit-rtmc.github.io/iris/index.html).
#'
#'
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
#' @importFrom rlang .data
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
    dplyr::mutate(detector_path = .data$value) %>%
    tidyr::separate(.data$detector_path, into = c(
      "front", "tms_config",
      "device", "rnode",
      "detector"
    ), sep = "/") %>%
    tidyr::unite(rnode_path, .data$front,
      .data$tms_config, .data$device,
      .data$rnode,
      sep = "/"
    ) %>%
    dplyr::mutate(rnode_path = trimws(.data$rnode_path)) %>%
    dplyr::mutate(corridor_path = .data$rnode_path) %>%
    tidyr::separate(.data$corridor_path, into = c(
      "front", "tms_config",
      "device", "rnode"
    ), sep = "/") %>%
    tidyr::unite(corridor_path, .data$front,
      .data$tms_config, .data$device,
      sep = "/"
    ) %>%
    dplyr::mutate(corridor_path = trimws(.data$corridor_path)) %>%
    dplyr::select(-.data$name) %>%
    dplyr::rename(detector_path = .data$value)

  # Rnode paths
  rnode_paths <- tibble::enframe(
    xml2::xml_path(xml2::xml_find_all(metro_config, "//r_node"))
  ) %>%
    dplyr::transmute(rnode_path = .data$value)

  # Corridor paths
  corridor_paths <- tibble::enframe(
    xml2::xml_path(xml2::xml_find_all(metro_config, "//corridor"))
  ) %>%
    dplyr::transmute(corridor_path = .data$value)

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

  attributes_full <- purrr::map2(
    categories, attr_all_ls,
    attr_to_df, metro_config
  )

  # Bind paths to attributes
  d_paths_attr <- dplyr::bind_cols(detector_paths, attributes_full[[1]])
  rnode_paths_attr <- dplyr::bind_cols(rnode_paths, attributes_full[[2]])
  corr_paths_attrs <- dplyr::bind_cols(corridor_paths, attributes_full[[3]])

  detector_rnodes_full <- dplyr::left_join(
    d_paths_attr,
    rnode_paths_attr,
    by = c("rnode_path")
  )
  configuration <- dplyr::left_join(detector_rnodes_full,
    corr_paths_attrs,
    by = c("corridor_path")
  )

  config_tidy <- configuration %>%
    dplyr::select(
      -.data$rnode, -.data$rnode_path,
      -.data$detector, -.data$detector_path,
      -.data$corridor_path
    ) %>%
    dplyr::mutate(date = Sys.Date())

  if (return_opt == "in_memory") {
    return(config_tidy)
  } else if (return_opt == "within_dir") {
    data.table::fwrite(
      config_tidy,
      paste0(
        "Configuration of Metro Detectors ",
        Sys.Date(), ".csv"
      )
    )
  }

  config_tidy
}

#' Clean node and detector attributes
#'
#' @param category character, one of "detector", "r_node", or "corridor"
#' @param attribute character. Options vary for each category.
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
