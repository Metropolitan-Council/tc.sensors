testthat::test_that("pull_sensor_espeed retrieves data successfully", {
  testthat::try_again(
    times = 5,
    code = {
      config_sample <- config %>%
        dplyr::sample_n(1)

      espeed_results <- pull_sensor_espeed(
        sensor = config_sample$detector_name[[1]],
        pull_date = yesterday,
        fill_gaps = TRUE
      )

      testthat::expect_s3_class(espeed_results, "data.table")
      testthat::expect_true("espeed" %in% names(espeed_results))
      testthat::expect_true("sensor" %in% names(espeed_results))
      testthat::expect_true("date" %in% names(espeed_results))
      testthat::expect_true("hour" %in% names(espeed_results))
      testthat::expect_true("min" %in% names(espeed_results))
      testthat::expect_equal(dim(espeed_results)[[1]], 2880)
    }
  )
})

testthat::test_that("pull_sensor_espeed accepts both date formats", {
  testthat::try_again(
    times = 3,
    code = {
      config_sample <- config %>%
        dplyr::sample_n(1)
      sensor_id <- config_sample$detector_name[[1]]

      # YYYY-MM-DD format
      result_dash <- pull_sensor_espeed(
        sensor_id,
        format(yesterday, "%Y-%m-%d"),
        fill_gaps = TRUE
      )

      # YYYYMMDD format
      result_compact <- pull_sensor_espeed(
        sensor_id,
        format(yesterday, "%Y%m%d"),
        fill_gaps = TRUE
      )

      testthat::expect_equal(dim(result_dash), dim(result_compact))
      testthat::expect_equal(result_dash$date, result_compact$date)
    }
  )
})

testthat::test_that("pull_sensor_espeed rejects invalid dates", {
  config_sample <- config %>%
    dplyr::sample_n(1)
  sensor_id <- config_sample$detector_name[[1]]

  # Invalid format
  testthat::expect_error(
    pull_sensor_espeed(sensor_id, "2024/13/45"),
    "Invalid date format"
  )

  # Wrong length
  testthat::expect_error(
    pull_sensor_espeed(sensor_id, "202410"),
    "Invalid date format"
  )

  # Non-date string
  testthat::expect_error(
    pull_sensor_espeed(sensor_id, "not-valid"),
    "Invalid date format"
  )
})

testthat::test_that("pull_sensor_espeed handles NULL parameters", {
  config_sample <- config %>%
    dplyr::sample_n(1)
  sensor_id <- config_sample$detector_name[[1]]

  testthat::expect_error(
    pull_sensor_espeed(sensor_id, NULL)
  )
})

testthat::test_that("pull_sensor_espeed handles unavailable dates", {
  config_sample <- config %>%
    dplyr::sample_n(1)
  sensor_id <- config_sample$detector_name[[1]]

  future_date <- format(Sys.Date() + 365, "%Y-%m-%d")

  testthat::expect_error(
    pull_sensor_espeed(sensor_id, future_date),
    "not available|Unable to fetch"
  )
})

testthat::test_that("pull_sensor_espeed works with different districts", {
  testthat::try_again(
    times = 3,
    code = {
      config_sample <- config %>%
        dplyr::sample_n(1)
      sensor_id <- config_sample$detector_name[[1]]

      # Default metro district should work with 2026 data
      result_metro <- pull_sensor_espeed(
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
    pull_sensor_espeed("999", yesterday, district = "d6"),
    "d6 district"
  )
})

testthat::test_that("pull_sensor_espeed fill_gaps parameter works", {
  testthat::try_again(
    times = 3,
    code = {
      config_sample <- config %>%
        dplyr::sample_n(1)

      sensor_id <- config_sample$detector_name[[1]]

      # With fill_gaps = TRUE
      result_filled <- pull_sensor_espeed(
        sensor_id,
        yesterday,
        fill_gaps = TRUE
      )

      # With fill_gaps = FALSE
      result_unfilled <- pull_sensor_espeed(
        sensor_id,
        yesterday,
        fill_gaps = FALSE
      )

      testthat::expect_s3_class(result_filled, "data.table")
      testthat::expect_s3_class(result_unfilled, "data.table")
    }
  )
})

testthat::test_that("pull_sensor_espeed length filtering works", {
  testthat::try_again(
    times = 3,
    code = {
      # Sample multiple sensors and find first with enough data
      max_attempts <- 10

      result <- purrr::map(1:max_attempts, ~ {
        config_sample <- config %>%
          dplyr::sample_n(1)
        sensor_id <- config_sample$detector_name[[1]]

        # Apply length filter - passenger cars only
        pull_sensor_espeed(
          sensor_id,
          yesterday,
          length_ft_min = 10,
          length_ft_max = 20,
          fill_gaps = FALSE
        )
      }) %>%
        purrr::keep(~ nrow(.x) >= 2880) %>%
        purrr::pluck(1, .default = NULL)

      # Skip test if no sensor with enough data found
      if (is.null(result) || nrow(result) < 2880) {
        testthat::skip("Could not find sensor with sufficient filtered data")
      }

      # Verify we got data back
      testthat::expect_s3_class(result, "data.table")
      testthat::expect_true("espeed" %in% names(result))
      testthat::expect_gte(nrow(result), 2880)
    }
  )
})

testthat::test_that("pull_sensor_espeed filtering reduces data appropriately", {
  testthat::try_again(
    times = 3,
    code = {
      config_sample <- config %>%
        dplyr::sample_n(1)
      sensor_id <- config_sample$detector_name[[1]]

      # Get unfiltered data (all vehicles)
      result_all <- pull_sensor_espeed(
        sensor_id,
        yesterday,
        fill_gaps = FALSE
      )

      # Get filtered data (only small vehicles)
      result_filtered <- pull_sensor_espeed(
        sensor_id,
        yesterday,
        length_ft_min = 5,
        length_ft_max = 15,
        fill_gaps = FALSE
      )

      # Count non-NA values
      count_all <- sum(!is.na(result_all$espeed))
      count_filtered <- sum(!is.na(result_filtered$espeed))

      # Filtered should have same or fewer observations
      # (filtering removes some vehicle observations)
      testthat::expect_true(
        count_filtered <= count_all,
        info = paste("Unfiltered:", count_all, "Filtered:", count_filtered)
      )
    }
  )
})
