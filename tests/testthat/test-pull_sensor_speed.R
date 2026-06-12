testthat::test_that("pull_sensor_speed retrieves data successfully", {
  testthat::try_again(
    times = 5,
    code = {
      config_sample <- config %>%
        dplyr::sample_n(1)

      speed_results <- pull_sensor_speed(
        sensor = config_sample$detector_name[[1]],
        pull_date = yesterday,
        fill_gaps = TRUE
      )

      testthat::expect_s3_class(speed_results, "data.table")
      testthat::expect_true("speed" %in% names(speed_results))
      testthat::expect_true("sensor" %in% names(speed_results))
      testthat::expect_true("date" %in% names(speed_results))
      testthat::expect_true("hour" %in% names(speed_results))
      testthat::expect_true("min" %in% names(speed_results))
      testthat::expect_equal(dim(speed_results)[[1]], 2880)
    }
  )
})

testthat::test_that("pull_sensor_speed accepts both date formats", {
  testthat::try_again(
    times = 3,
    code = {
      config_sample <- config %>%
        dplyr::sample_n(1)
      sensor_id <- config_sample$detector_name[[1]]

      # YYYY-MM-DD format
      result_dash <- pull_sensor_speed(
        sensor_id,
        format(yesterday, "%Y-%m-%d"),
        fill_gaps = TRUE
      )

      # YYYYMMDD format
      result_compact <- pull_sensor_speed(
        sensor_id,
        format(yesterday, "%Y%m%d"),
        fill_gaps = TRUE
      )

      testthat::expect_equal(dim(result_dash), dim(result_compact))
      testthat::expect_equal(result_dash$date, result_compact$date)
    }
  )
})

testthat::test_that("pull_sensor_speed filtering works correctly", {
  testthat::try_again(
    times = 3,
    code = {
      config_sample <- config %>%
        dplyr::sample_n(1)
      sensor_id <- config_sample$detector_name[[1]]

      # Test with speed filters
      result <- pull_sensor_speed(
        sensor_id,
        yesterday,
        speed_mph_min = 0,
        speed_mph_max = 100,
        fill_gaps = TRUE
      )

      testthat::expect_s3_class(result, "data.table")
      testthat::expect_true("speed" %in% names(result))
      testthat::expect_equal(nrow(result), 2880)
    }
  )
})

testthat::test_that("pull_sensor_speed filtering returns values within specified range", {
  testthat::try_again(
    times = 3,
    code = {
      # Sample multiple sensors and find first with enough data
      max_attempts <- 10

      result <- purrr::map(1:max_attempts, ~ {
        config_sample <- config %>%
          dplyr::sample_n(1)
        sensor_id <- config_sample$detector_name[[1]]

        # Apply specific filter range
        pull_sensor_speed(
          sensor_id,
          yesterday,
          speed_mph_min = 20,
          speed_mph_max = 70,
          fill_gaps = FALSE # Don't fill gaps to get actual values
        )
      }) %>%
        purrr::keep(~ nrow(.x) >= 2880) %>%
        purrr::pluck(1, .default = NULL)

      # Skip test if no sensor with enough data found
      if (is.null(result) || nrow(result) < 2880) {
        testthat::skip("Could not find sensor with sufficient filtered data")
      }

      # Remove NA values and check range
      non_na_speeds <- result$speed[!is.na(result$speed)]

      if (length(non_na_speeds) > 0) {
        testthat::expect_true(
          all(non_na_speeds >= 20),
          info = paste("Min speed:", min(non_na_speeds))
        )
        testthat::expect_true(
          all(non_na_speeds <= 70),
          info = paste("Max speed:", max(non_na_speeds))
        )
      }
    }
  )
})

testthat::test_that("pull_sensor_speed rejects invalid dates", {
  config_sample <- config %>%
    dplyr::sample_n(1)
  sensor_id <- config_sample$detector_name[[1]]

  # Invalid format
  testthat::expect_error(
    pull_sensor_speed(sensor_id, "not-a-date"),
    "Invalid date format"
  )

  # Wrong separator
  testthat::expect_error(
    pull_sensor_speed(sensor_id, "2024/10/14"),
    "Invalid date format"
  )

  # Invalid date
  testthat::expect_error(
    pull_sensor_speed(sensor_id, "2024-13-45"),
    "Invalid date format"
  )
})

testthat::test_that("pull_sensor_speed handles NULL parameters", {
  config_sample <- config %>%
    dplyr::sample_n(1)
  sensor_id <- config_sample$detector_name[[1]]

  testthat::expect_error(
    pull_sensor_speed(sensor_id, NULL)
  )
})

testthat::test_that("pull_sensor_speed handles unavailable dates", {
  config_sample <- config %>%
    dplyr::sample_n(1)
  sensor_id <- config_sample$detector_name[[1]]

  future_date <- format(Sys.Date() + 365, "%Y-%m-%d")

  testthat::expect_error(
    pull_sensor_speed(sensor_id, future_date),
    "not available|Unable to fetch"
  )
})

testthat::test_that("pull_sensor_speed works with different districts", {
  testthat::try_again(
    times = 3,
    code = {
      config_sample <- config %>%
        dplyr::sample_n(1)
      sensor_id <- config_sample$detector_name[[1]]

      # Default metro district should work with 2026 data
      result_metro <- pull_sensor_speed(
        sensor_id,
        yesterday,
        district = "metro",
        fill_gaps = TRUE
      )

      testthat::expect_s3_class(result_metro, "data.table")
    }
  )

  # Test that district parameter is being used - d1 only has 2016 data
  # so 2026 date should error with district-specific message
  testthat::expect_error(
    pull_sensor_speed("999", yesterday, district = "d1"),
    "d1 district"
  )
})

testthat::test_that("pull_sensor_speed fill_gaps parameter works", {
  testthat::try_again(
    times = 3,
    code = {
      config_sample <- config %>%
        dplyr::sample_n(1)
      sensor_id <- config_sample$detector_name[[1]]

      # With fill_gaps = TRUE
      result_filled <- pull_sensor_speed(
        sensor_id,
        yesterday,
        fill_gaps = TRUE
      )

      # With fill_gaps = FALSE
      result_unfilled <- pull_sensor_speed(
        sensor_id,
        yesterday,
        fill_gaps = FALSE
      )

      testthat::expect_s3_class(result_filled, "data.table")
      testthat::expect_s3_class(result_unfilled, "data.table")
    }
  )
})
