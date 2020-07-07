testthat::try_again(2, {
  test_that("Weather data functions as expected", {
    testthat::skip_if_offline()
    config <- pull_configuration()


    config_sample <- dplyr::filter(config, config$detector_abandoned == "f") %>%
      dplyr::sample_n(1)

    yesterday <- as.Date(Sys.Date() - 365)

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

    # test aggregation at 15 minutes----------------------------------------------

    # test aggregation at 1 hour--------------------------------------------------

    agg_hour <- aggregate_sensor_data(sensor_results,
      interval_length = 1,
      config = config_sample
    )


    # test argument checks--------------------------------------------------------
    testthat::expect_error(add_weather(agg_hour,
      interval_length = 0.25
    ))


    agg_hour_weather <- add_weather(agg_hour,
      interval_length = 1
    )

    testthat::expect_equal(dim(agg_hour_weather)[[1]], 24)

    # test aggregation at 24 hours------------------------------------------------
    agg_day_weather <- aggregate_sensor_data(sensor_results,
      interval_length = 24,
      config = config_sample
    ) %>%
      add_weather(interval_length = 24)

    testthat::expect_equal(dim(agg_day_weather)[[1]], 1)
  })
})