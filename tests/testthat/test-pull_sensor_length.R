testthat::test_that("pull_sensor_length retrieves data successfully", {
  testthat::try_again(
    times = 5,
    code = {
      config_sample <- config %>%
        dplyr::sample_n(1)

      length_results <- pull_sensor_length(
        sensor = config_sample$detector_name[[1]],
        pull_date = yesterday,
        fill_gaps = TRUE
      )

      testthat::expect_s3_class(length_results, "data.table")
      testthat::expect_true("length_ft" %in% names(length_results))
      testthat::expect_true("sensor" %in% names(length_results))
      testthat::expect_true("date" %in% names(length_results))
      testthat::expect_true("hour" %in% names(length_results))
      testthat::expect_true("min" %in% names(length_results))
      testthat::expect_equal(dim(length_results)[[1]], 2880)
    }
  )
})

testthat::test_that("pull_sensor_length accepts both date formats", {
  testthat::try_again(
    times = 3,
    code = {
      config_sample <- config %>%
        dplyr::sample_n(1)
      sensor_id <- config_sample$detector_name[[1]]

      # YYYY-MM-DD format
      result_dash <- pull_sensor_length(
        sensor_id,
        format(yesterday, "%Y-%m-%d"),
        fill_gaps = TRUE
      )

      # YYYYMMDD format
      result_compact <- pull_sensor_length(
        sensor_id,
        format(yesterday, "%Y%m%d"),
        fill_gaps = TRUE
      )

      testthat::expect_equal(dim(result_dash), dim(result_compact))
      testthat::expect_equal(result_dash$date, result_compact$date)
    }
  )
})

testthat::test_that("pull_sensor_length filtering works correctly", {
  testthat::try_again(
    times = 3,
    code = {
      config_sample <- config %>%
        dplyr::sample_n(1)
      sensor_id <- config_sample$detector_name[[1]]

      # Test with length filters - passenger cars (7-19 feet)
      result <- pull_sensor_length(
        sensor_id,
        yesterday,
        length_ft_min = 7,
        length_ft_max = 19,
        fill_gaps = TRUE
      )

      testthat::expect_s3_class(result, "data.table")
      testthat::expect_true("length_ft" %in% names(result))
      testthat::expect_equal(nrow(result), 2880)
    }
  )
})

testthat::test_that("pull_sensor_length filtering returns values within specified range", {
  testthat::try_again(
    times = 3,
    code = {
      # Sample multiple sensors and find first with enough data
      max_attempts <- 10

      result <- purrr::map(1:max_attempts, ~ {
        config_sample <- config %>%
          dplyr::sample_n(1)
        sensor_id <- config_sample$detector_name[[1]]

        # Apply specific filter range - passenger cars
        pull_sensor_length(
          sensor_id,
          yesterday,
          length_ft_min = 10,
          length_ft_max = 20,
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
      non_na_lengths <- result$length_ft[!is.na(result$length_ft)]

      if (length(non_na_lengths) > 0) {
        testthat::expect_true(
          all(non_na_lengths >= 10),
          info = paste("Min length:", min(non_na_lengths))
        )
        testthat::expect_true(
          all(non_na_lengths <= 20),
          info = paste("Max length:", max(non_na_lengths))
        )
      }
    }
  )
})

testthat::test_that("pull_sensor_length vehicle classification filtering", {
  testthat::try_again(
    times = 3,
    code = {
      config_sample <- config %>%
        dplyr::sample_n(1)
      sensor_id <- config_sample$detector_name[[1]]

      # Motorcycles (< 7 feet)
      motorcycles <- pull_sensor_length(
        sensor_id,
        yesterday,
        length_ft_max = 7
      )

      # Large trucks (> 24 feet)
      trucks <- pull_sensor_length(
        sensor_id,
        yesterday,
        length_ft_min = 24
      )

      testthat::expect_s3_class(motorcycles, "data.table")
      testthat::expect_s3_class(trucks, "data.table")
    }
  )
})

testthat::test_that("pull_sensor_length rejects invalid dates", {
  config_sample <- config %>%
    dplyr::sample_n(1)
  sensor_id <- config_sample$detector_name[[1]]

  # Invalid format
  testthat::expect_error(
    pull_sensor_length(sensor_id, "2024-99-99"),
    "Invalid date format"
  )

  # Wrong separator
  testthat::expect_error(
    pull_sensor_length(sensor_id, "2024/10/14"),
    "Invalid date format"
  )

  # Non-date string
  testthat::expect_error(
    pull_sensor_length(sensor_id, "bad-date"),
    "Invalid date format"
  )
})

testthat::test_that("pull_sensor_length handles NULL parameters", {
  config_sample <- config %>%
    dplyr::sample_n(1)
  sensor_id <- config_sample$detector_name[[1]]

  testthat::expect_error(
    pull_sensor_length(sensor_id, NULL)
  )
})

testthat::test_that("pull_sensor_length handles unavailable dates", {
  config_sample <- config %>%
    dplyr::sample_n(1)
  sensor_id <- config_sample$detector_name[[1]]

  future_date <- format(Sys.Date() + 365, "%Y-%m-%d")

  testthat::expect_error(
    pull_sensor_length(sensor_id, future_date),
    "not available|Unable to fetch"
  )
})

testthat::test_that("pull_sensor_length works with different districts", {
  testthat::try_again(
    times = 3,
    code = {
      config_sample <- config %>%
        dplyr::sample_n(1)
      sensor_id <- config_sample$detector_name[[1]]

      # Default metro district should work with 2026 data
      result_metro <- pull_sensor_length(
        sensor_id,
        yesterday,
        district = "metro",
        fill_gaps = TRUE
      )

      testthat::expect_s3_class(result_metro, "data.table")
    }
  )

  # Test that district parameter is being used - d6 only has 2009-2016 data
  # so 2026 date should error with district-specific message
  testthat::expect_error(
    pull_sensor_length("999", yesterday, district = "d6"),
    "d6 district"
  )
})

testthat::test_that("pull_sensor_length fill_gaps parameter works", {
  testthat::try_again(
    times = 3,
    code = {
      config_sample <- config %>%
        dplyr::sample_n(1)
      sensor_id <- config_sample$detector_name[[1]]

      # With fill_gaps = TRUE
      result_filled <- pull_sensor_length(
        sensor_id,
        yesterday,
        fill_gaps = TRUE
      )

      # With fill_gaps = FALSE
      result_unfilled <- pull_sensor_length(
        sensor_id,
        yesterday,
        fill_gaps = FALSE
      )

      testthat::expect_s3_class(result_filled, "data.table")
      testthat::expect_s3_class(result_unfilled, "data.table")
    }
  )
})
