testthat::test_that("Impossible values are replaced", {
  testthat::try_again(
    times = 5,
    code = {
      config_sample <- config %>%
        dplyr::sample_n(1)

      sensor_results <- pull_sensor(
        sensor = config_sample$detector_name[[1]],
        pull_date = yesterday,
        fill_gaps = TRUE
      )

      ifelse(is.na(sensor_results$volume),
             {
               config_sample <- config %>%
                 dplyr::sample_n(1)

               sensor_results <- pull_sensor(
                 sensor = config_sample$detector_name[[1]],
                 pull_date = yesterday,
                 fill_gaps = TRUE
               )
             },
             NA
      )

      imp_rem <- replace_impossible(
        sensor_data = data.table::as.data.table(sensor_results),
        interval_length = NA
      )

      testthat::expect_true(max(imp_rem$volume, na.rm = TRUE) < 20 | is.na(max(imp_rem$volume)))
      testthat::expect_true(max(imp_rem$occupancy, na.rm = TRUE) < 1800 | is.na(max(imp_rem$occupancy)))

      # test aggregation at 15 minutes----------------------------------------------
      agg <- aggregate_sensor(sensor_results,
                              interval_length = 0.25,
                              config = config_sample
      ) %>%
        replace_impossible(interval_length = 0.25)

      testthat::expect_true(max(agg$volume.sum, na.rm = TRUE) < 0.25 * 2300)
      testthat::expect_true(max(agg$occupancy.sum, na.rm = TRUE) < 0.25 * 216000)

      testthat::expect_equal(dim(agg)[[1]], 96)

      # test aggregation at 1 hour--------------------------------------------------
      agg_hour <- aggregate_sensor(sensor_results,
                                   interval_length = 1,
                                   config = config_sample
      ) %>%
        replace_impossible(interval_length = 1)

      testthat::expect_true(max(agg_hour$volume.sum, na.rm = TRUE) < 2300)
      testthat::expect_true(max(agg_hour$occupancy.sum, na.rm = TRUE) < 216000)

      # test aggregation at 24 hours------------------------------------------------
      agg_day <- aggregate_sensor(sensor_results,
                                  interval_length = 24,
                                  config = config_sample
      ) %>%
        replace_impossible(interval_length = 24)

      testthat::expect_true(max(agg_day$volume.sum, na.rm = TRUE) < 24 * 2300)
      testthat::expect_true(max(agg_day$occupancy.sum, na.rm = TRUE) < 24 * 216000)


      # test argument checks--------------------------------------------------------
      testthat::expect_error(replace_impossible(sensor_results,
                                                interval_length = 48
      ))

      testthat::expect_error(
        replace_impossible(
          rbind(
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
          interval_length = 24
        )
      )
    })
})
