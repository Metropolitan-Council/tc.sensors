testthat::skip_if_offline()
testthat::test_that("Test that data can be pulled from a random sensor id from the last two days", {
  config <- pull_configuration()

  config_sample <- dplyr::filter(config, config$detector_abandoned == "f") %>%
    dplyr::sample_n(1)

  yesterday <- as.Date(Sys.Date() - 1)
  day_before <- as.Date(Sys.Date() - 2)

  sensor_results <- pull_sensor_range(start_date = day_before,
                                      end_date = yesterday,
                                      config = config_sample)

  testthat::expect_equal(class(sensor_results)[[1]], "list")
  testthat::expect_equal(dim(sensor_results[[1]])[[1]], 5760)
  testthat::expect_equal(dim(sensor_results[[1]])[[2]], 7)
})
