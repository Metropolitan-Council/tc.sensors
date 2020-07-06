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
  testthat::expect_equal(sum(sensor_results$volume), sum(agg$volume.sum))
  testthat::expect_equal(sum(sensor_results$occupancy), sum(agg$occupancy.sum))

  agg_hour <- aggregate_sensor_data(sensor_results, interval_length = 1, config = config_sample)
  testthat::expect_equal(dim(agg_hour)[[1]], 24)
  testthat::expect_equal(sum(sensor_results$volume, na.rm = T), sum(agg_hour$volume.sum))
  testthat::expect_equal(sum(sensor_results$occupancy, na.rm = T), sum(agg_hour$occupancy.sum))
  testthat::expect_lt(mean(agg$speed) - mean(agg_hour$speed), 1)

  agg_day <- aggregate_sensor_data(sensor_results, interval_length = 24, config = config_sample)
  testthat::expect_equal(dim(agg_day)[[1]], 1)
  testthat::expect_equal(sum(sensor_results$volume, na.rm = T), sum(agg_day$volume.sum))
  testthat::expect_equal(sum(sensor_results$occupancy, na.rm = T), sum(agg_day$occupancy.sum))
  testthat::expect_lt(mean(agg$speed) - agg_day$speed, 1)
})
