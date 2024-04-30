testthat::test_that("spatial lines are generated", {
  lines_sf <- generate_spatial_lines(config = config)

  testthat::expect_true("sf" %in% class(lines_sf))

  unique_corridors <- nrow(unique(config[
    , corridor_category := ifelse(corridor_route == "I-35" & r_node_lat > 45, "I35 north of cities",
      ifelse(corridor_route == "I-35" & r_node_lat <= 45, "I35 south of cities",
        ifelse(corridor_route == "T.H.5" & r_node_lon < -93.3, "5 west of cities",
          ifelse(corridor_route == "T.H.5" & r_node_lon > -93.3, "5 east of cities", "Other")
        )
      )
    )
  ][
    , corridor_id := paste(corridor_route, corridor_dir, corridor_category, sep = "_")
  ][
    , .(detectors = paste(detector_name, collapse = ",")),
    keyby = .(corridor_id)
  ][, "corridor_id"]))

  testthat::expect_true(unique_corridors - length(unique(lines_sf$corridor_id)) < 5)
})
