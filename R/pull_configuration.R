#' Pull metro sensor configuration
#'
#' Read MnDOT JSON feed and wrangle into a tidy dataframe containing 20 variables related to sensor configuration.
#'   Useful for mapping (contains lat/lons) and calculating performance measures (contains detector_field).
#'
#' @param return_opt an object of class string which indicates how to return the data.
#'    "within_dir" will return the data within the directory as a csv entitled
#'    "Configuration of Metro Detectors <<date in format yyyy-mm-dd>>".
#'     "in-memory" will return the data in R, but requires assignment.
#' @return dataframe containing 20 variables, including detector_field and lat/lons,
#'   for each sensor in MnDOT's metro district
#' @family loop sensor functions
#' @examples
#' \dontrun{
#' config <- pull_configuration("in-memory") # Assign to an object
#' pull_configuration("within_dir") # No assignment necessary
#' }
#'
#' @importFrom xml2 read_xml xml_find_all xml_attr xml_path
#' @importFrom dplyr mutate select rename bind_rows bind_cols left_join
#' @importFrom tidyr separate unite
#' @importFrom purrr map2
#' @importFrom magrittr %>%
#' @importFrom data.table fwrite
#' @importFrom utils download.file
#'
#' @export
pull_configuration <- function(return_opt = c("within_dir", "in_memory")) {
  url <- "http://data.dot.state.mn.us/iris_xml/metro_config.xml.gz"
  tmp <- tempfile()
  utils::download.file(url, tmp)
  metro_config <- read_xml(gzfile(tmp))

  # ------------------
  # PATHS
  # ------------------

  # Detector paths - connect rnodes and corridors to this
  detector_paths <- enframe(xml_path(xml_find_all(metro_config, "//detector"))) %>%
    mutate(detector_path = value) %>%
    separate(detector_path, into = c("front", "tms_config", "device", "rnode", "detector"), sep = "/") %>%
    unite(rnode_path, front, tms_config, device, rnode, sep = "/") %>%
    mutate(rnode_path = trimws(rnode_path)) %>%
    mutate(corridor_path = rnode_path) %>%
    separate(corridor_path, into = c("front", "tms_config", "device", "rnode"), sep = "/") %>%
    unite(corridor_path, front, tms_config, device, sep = "/") %>%
    mutate(corridor_path = trimws(corridor_path)) %>%
    select(-name) %>%
    rename(detector_path = value)

  # Rnode paths
  rnode_paths <- enframe(xml_path(xml_find_all(metro_config, "//r_node"))) %>%
    transmute(rnode_path = value)

  # Corridor paths
  corridor_paths <- enframe(xml_path(xml_find_all(metro_config, "//corridor"))) %>%
    transmute(corridor_path = value)

  # ------------------
  # ATTRIBUTES (rnodes & detectors)
  # ------------------

  attr_clean <- function(category, attribute) {
    trimws(xml_attr(xml_find_all(metro_config, paste0("//", category)), attribute))
  }

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

  attr_to_df <- function(category, attr_ls) {
    attributes_ls <- map2(category, attr_ls, attr_clean)
    names(attributes_ls) <- paste(category, attr_ls, sep = "_")
    bind_rows(attributes_ls)
  }

  attr_all_ls <- list(d_attr_ls, rn_attr_ls, c_attr_ls)
  categories <- list("detector", "r_node", "corridor")

  attributes_full <- map2(categories, attr_all_ls, attr_to_df)

  # Bind paths to attributes
  d_paths_attr <- bind_cols(detector_paths, attributes_full[[1]])
  rnode_paths_attr <- bind_cols(rnode_paths, attributes_full[[2]])
  corr_paths_attrs <- bind_cols(corridor_paths, attributes_full[[3]])

  detector_rnodes_full <- left_join(d_paths_attr, rnode_paths_attr, by = c("rnode_path"))
  configuration <- left_join(detector_rnodes_full, corr_paths_attrs, by = c("corridor_path"))

  config_tidy <- configuration %>%
    select(-rnode, -rnode_path, -detector, -detector_path, -corridor_path) %>%
    mutate(date = Sys.Date())

  if (return_opt == "in_memory") {
    return(config_tidy)
  } else if (return_opt == "within_dir") {
    fwrite(config_tidy, paste0("Configuration of Metro Detectors ", Sys.Date(), ".csv"))
  }

  config_tidy
}
