testthat::skip_if_offline()
testthat::test_that("Test that data can be pulled from a random sensor id for yesterday's date", {
  config <- pull_configuration()


  config_sample <- dplyr::filter(config, config$detector_abandoned == "f") %>%
    dplyr::sample_n(1)

  yesterday <- as.Date(Sys.Date() - 1)

  sensor_results <- pull_sensor(
    sensor = config_sample$detector_name[[1]],
    pull_date = yesterday
  )

  if (nrow(sensor_results) < 2880) {
    config_sample <- dplyr::filter(config, config$detector_abandoned == "f") %>%
      dplyr::sample_n(1)

    sensor_results <- pull_sensor(
      sensor = config_sample$detector_name[[1]],
      pull_date = yesterday
    )
  }

  testthat::expect_equal(class(sensor_results)[[1]], "tbl_df")
  testthat::expect_equal(dim(sensor_results)[[2]], 6)
})
