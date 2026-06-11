testthat::test_that("Aggregation functions as expected", {
  testthat::try_again(
    # some sensors might not pull correctly.
    # re-try this code with different sensors if it fails
    times = 5,
    code = {
      config_sample <- config %>%
        dplyr::sample_n(1)

      sensor_results <- pull_sensor(
        sensor = config_sample %>% dplyr::pull(detector_name),
        pull_date = yesterday,
        fill_gaps = TRUE
      )

      # test aggregation at 15 minutes----------------------------------------------
      agg <- aggregate_sensor(sensor_results,
        interval_length = 0.25,
        config = config_sample
      )
      testthat::expect_equal(dim(agg)[[1]], 96)

      testthat::expect_equal(
        round(mean(sensor_results$volume, na.rm = TRUE)),
        round(mean(agg$volume.mean, na.rm = TRUE)),
        tolerance = 12
      )

      testthat::expect_equal(
        sum(sensor_results$occupancy, na.rm = TRUE),
        sum(agg$occupancy.sum, na.rm = TRUE),
        tolerance = 12
      )

      # test aggregation at 1 hour--------------------------------------------------
      agg_hour <- aggregate_sensor(sensor_results,
        interval_length = 1,
        config = config_sample
      )
      testthat::expect_equal(dim(agg_hour)[[1]], 24)
      testthat::expect_equal(
        round(mean(sensor_results$volume, na.rm = TRUE)),
        round(mean(agg_hour$volume.mean)),
        tolerance = 0.1
      )

      testthat::expect_equal(
        round(sum(sensor_results$occupancy, na.rm = TRUE)),
        round(sum(agg_hour$occupancy.sum, na.rm = TRUE)),
        tolerance = 1
      )

      ifelse(!all(is.na(agg$speed)),
        testthat::expect_true(round(mean(agg$speed, na.rm = TRUE)) -
          round(mean(agg_hour$speed, na.rm = TRUE)) < 3), NA
      )
      # test aggregation at 24 hours------------------------------------------------
      agg_day <- aggregate_sensor(sensor_results,
        interval_length = 24,
        config = config_sample
      )
      testthat::expect_equal(dim(agg_day)[[1]], 1)

      testthat::expect_equal(
        round(mean(sensor_results$volume, na.rm = TRUE)),
        round(mean(agg_day$volume.mean))
      )

      testthat::expect_equal(
        round(sum(sensor_results$occupancy, na.rm = TRUE)),
        round(sum(agg_day$occupancy.sum, na.rm = TRUE)),
        tolerance = 1
      )

      ifelse(!all(is.na(agg_day$speed)),
        testthat::expect_true(
          round(mean(agg$speed, na.rm = TRUE)) -
            round(mean(agg_day$speed, na.rm = TRUE)) < 3
        ), NA
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


      testthat::expect_error(
        aggregate_sensor(
          rbind(
            sensor_results,
            data.table::data.table(
              volume = 10,
              occupancy = 12,
              date = Sys.Date(),
              sensor = config_sample,
              hour = 0,
              min = 30
            )
          ),
          config = config_sample, interval_length = 24
        )
      )

      testthat::expect_error(
        aggregate_sensor(
          rbind(
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
        )
      )
    }
  )
})
