testthat::test_that("Test that data can be pulled from a random sensor id for yesterday's date", {

  config_sample <- dplyr::filter(config, config$detector_abandoned == "f") %>%
    dplyr::sample_n(1)


  sensor_results <- pull_sensor(
    sensor = config_sample$detector_name[[1]],
    pull_date = yesterday,
    fill_gaps = TRUE
  )

  testthat::expect_equal(class(sensor_results)[[1]], "data.table")
  testthat::expect_equal(dim(sensor_results)[[1]], 2880)
  testthat::expect_equal(dim(sensor_results)[[2]], 6)
})
