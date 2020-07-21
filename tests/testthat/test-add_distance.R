testthat::skip_if_offline()


test_that("distance is calculated correctly", {

  config <- pull_configuration()

  testthat::expect_error(add_distance(
    config = config[r_node_n_type == "Entrance",],
    interpolate_missing = TRUE
  ))
})
