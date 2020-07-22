testthat::skip_if_offline()

test_that("Aggregation functions as expected", {
  config <- pull_configuration()

  yesterday <- as.Date(Sys.Date() - 3)

  config_sample <- dplyr::filter(config, config$detector_abandoned == "f") %>%
    dplyr::sample_n(1)

  sensor_results <- pull_sensor(
    sensor = config_sample$detector_name[[1]],
    pull_date = yesterday,
    fill_gaps = TRUE
  )


  # test aggregation at 15 minutes----------------------------------------------
  agg <- aggregate_sensor(sensor_results,
    interval_length = 0.25,
    config = config_sample
  )
  testthat::expect_equal(dim(agg)[[1]], 96)

  testthat::expect_equivalent(sum(sensor_results$volume, na.rm = T), sum(agg$volume.sum, na.rm = T))
  testthat::expect_equal(sum(sensor_results$occupancy, na.rm = T), sum(agg$occupancy.sum, na.rm = T))

  # test aggregation at 1 hour--------------------------------------------------
  agg_hour <- aggregate_sensor(sensor_results,
    interval_length = 1,
    config = config_sample
  )
  testthat::expect_equal(dim(agg_hour)[[1]], 24)
  testthat::expect_equal(sum(sensor_results$volume, na.rm = T), sum(agg_hour$volume.sum))
  testthat::expect_equal(sum(sensor_results$occupancy, na.rm = T), sum(agg_hour$occupancy.sum))
  ifelse(!is.na(agg$speed),
    testthat::expect_equivalent(mean(agg$speed) - mean(agg_hour$speed, na.rm = T)), NA
  )
  # test aggregation at 24 hours------------------------------------------------
  agg_day <- aggregate_sensor(sensor_results,
    interval_length = 24,
    config = config_sample
  )
  testthat::expect_equal(dim(agg_day)[[1]], 1)
  testthat::expect_equal(sum(sensor_results$volume, na.rm = T), sum(agg_day$volume.sum))
  testthat::expect_equal(sum(sensor_results$occupancy, na.rm = T), sum(agg_day$occupancy.sum))
  ifelse(!is.na(agg$speed),
    testthat::expect_equivalent(mean(agg$speed), mean(agg_day$speed, na.rm = T)), no = NA
  )

  # test argument checks--------------------------------------------------------
  testthat::expect_error(aggregate_sensor(sensor_results,
    config = config_sample,
    interval_length = 48
  ))

  testthat::expect_error(aggregate_sensor(sensor_results,
    config = config_sample,
    interval_length = NA
  ))


  testthat::expect_error(aggregate_sensor(rbind(
    sensor_results,
    data.table::data.table(
      volume = 10,
      occupancy = 12,
      date = Sys.Date(),
      sensor = config_sample$detector_name,
      hour = 0,
      min = 30
    )
  ),
  config = config_sample, interval_length = 24
  ))

  testthat::expect_error(aggregate_sensor(rbind(
    sensor_results,
    data.table::data.table(
      volume = 10,
      occupancy = 12,
      date = Sys.Date(),
      sensor = 24601,
      hour = 0,
      min = 30
    )
  ),
  config = config_sample, interval_length = 24
  ))
})
