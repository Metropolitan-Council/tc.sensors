# setup

yesterday <- as.Date(Sys.Date() - 3)

config_raw <- pull_configuration()

config <- pull_configuration() %>%
  dplyr::filter(detector_abandoned == "f",
                detector_category == "",
                r_node_n_type == "Station") %>%
  data.table::as.data.table()

