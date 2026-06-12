testthat::test_that("Test that data can be pulled from a random sensor id for yesterday's date", {
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

      testthat::expect_false(all(is.na(sensor_results$volume)))
      testthat::expect_false(all(is.na(sensor_results$occupancy)))
      testthat::expect_equal(class(sensor_results)[[1]], "data.table")
      testthat::expect_equal(dim(sensor_results)[[1]], 2880)
      testthat::expect_equal(dim(sensor_results)[[2]], 6)
    }
  )
})

# ============================================================================
# Error and Failure Tests
# ============================================================================

testthat::test_that("pull_sensor rejects invalid date formats", {
  config_sample <- config %>%
    dplyr::sample_n(1)
  sensor_id <- config_sample$detector_name[[1]]

  # Invalid date format
  testthat::expect_error(
    pull_sensor(sensor_id, "2024/10/14"),
    "Invalid date format"
  )

  # Wrong length
  testthat::expect_error(
    pull_sensor(sensor_id, "202410"),
    "Invalid date format"
  )

  # Non-date string
  testthat::expect_error(
    pull_sensor(sensor_id, "invalid-date"),
    "Invalid date format"
  )
})

testthat::test_that("pull_sensor rejects NULL or missing parameters", {
  config_sample <- config %>%
    dplyr::sample_n(1)
  sensor_id <- config_sample$detector_name[[1]]

  # NULL date - should error in parse_date_flexible
  testthat::expect_error(
    pull_sensor(sensor_id, NULL)
  )
})

testthat::test_that("pull_sensor handles unavailable dates gracefully", {
  config_sample <- config %>%
    dplyr::sample_n(1)
  sensor_id <- config_sample$detector_name[[1]]

  # Far future date (unlikely to be available)
  future_date <- format(Sys.Date() + 365, "%Y-%m-%d")

  # Should error with date/API unavailable message
  testthat::expect_error(
    pull_sensor(sensor_id, future_date),
    "not available|Unable to fetch"
  )
})

testthat::test_that("pull_sensor works with both date formats", {
  testthat::try_again(
    times = 3,
    code = {
      config_sample <- config %>%
        dplyr::sample_n(1)
      sensor_id <- config_sample$detector_name[[1]]

      # YYYY-MM-DD format
      result_dash <- pull_sensor(
        sensor_id,
        format(yesterday, "%Y-%m-%d"),
        fill_gaps = TRUE
      )

      # YYYYMMDD format
      result_compact <- pull_sensor(
        sensor_id,
        format(yesterday, "%Y%m%d"),
        fill_gaps = TRUE
      )

      testthat::expect_equal(dim(result_dash), dim(result_compact))
    }
  )
})

testthat::test_that("pull_sensor filtering parameters work", {
  testthat::try_again(
    times = 3,
    code = {
      config_sample <- config %>%
        dplyr::sample_n(1)
      sensor_id <- config_sample$detector_name[[1]]

      # Test filtering with multiple parameters
      result <- pull_sensor(
        sensor_id,
        yesterday,
        length_ft_min = 7,
        length_ft_max = 19,
        speed_mph_min = 0,
        speed_mph_max = 100
      )

      testthat::expect_s3_class(result, "data.table")
      testthat::expect_true("volume" %in% names(result))
      testthat::expect_true("occupancy" %in% names(result))
    }
  )
})

testthat::test_that("pull_sensor filtering reduces volume counts appropriately", {
  testthat::try_again(
    times = 3,
    code = {
      config_sample <- config %>%
        dplyr::sample_n(1)
      sensor_id <- config_sample$detector_name[[1]]

      # Get unfiltered data
      result_all <- pull_sensor(
        sensor_id,
        yesterday,
        fill_gaps = FALSE
      )

      # Get filtered data (only passenger cars at typical speeds)
      result_filtered <- pull_sensor(
        sensor_id,
        yesterday,
        length_ft_min = 10,
        length_ft_max = 20,
        speed_mph_min = 30,
        speed_mph_max = 70,
        fill_gaps = FALSE
      )

      # Filtered data should have same or fewer total vehicles
      total_all <- sum(result_all$volume, na.rm = TRUE)
      total_filtered <- sum(result_filtered$volume, na.rm = TRUE)

      testthat::expect_true(
        total_filtered <= total_all,
        info = paste("Unfiltered:", total_all, "Filtered:", total_filtered)
      )
    }
  )
})

testthat::test_that("pull_sensor_ids handles invalid district gracefully", {
  # Non-existent district should return error or empty
  testthat::expect_error(
    pull_sensor_ids(district = "invalid_district_xyz"),
    "Unable to fetch|NULL"
  )
})
