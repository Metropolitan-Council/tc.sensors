test_that("fifteen minute aggregation works", {
  testthat::skip_if_offline()

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

  agg <- aggregate_sensor_data(sensor_results, interval_length = 0.25, config = config_sample)
  testthat::expect_equal(dim(agg)[[1]], 96)

  agg_hour <- aggregate_sensor_data(sensor_results, interval_length = 1, config = config_sample)
  testthat::expect_equal(dim(agg_hour)[[1]], 24)

  agg_day <- aggregate_sensor_data(sensor_results, interval_length = 24, config = config_sample)
  testthat::expect_equal(dim(agg_day)[[1]], 1)
})
