testthat::test_that("parse_date_flexible handles YYYY-MM-DD format correctly", {
  result <- parse_date_flexible("2024-10-14")

  testthat::expect_equal(result$date_standard, "2024-10-14")
  testthat::expect_equal(result$date_yyyymmdd, "20241014")
  testthat::expect_equal(result$year, "2024")
  testthat::expect_s3_class(result$date_obj, "Date")
  testthat::expect_equal(as.character(result$date_obj), "2024-10-14")
})

testthat::test_that("parse_date_flexible handles YYYYMMDD format correctly", {
  result <- parse_date_flexible("20241014")

  testthat::expect_equal(result$date_standard, "2024-10-14")
  testthat::expect_equal(result$date_yyyymmdd, "20241014")
  testthat::expect_equal(result$year, "2024")
  testthat::expect_s3_class(result$date_obj, "Date")
  testthat::expect_equal(as.character(result$date_obj), "2024-10-14")
})

testthat::test_that("parse_date_flexible produces same output for both formats", {
  result_dash <- parse_date_flexible("2024-10-14")
  result_compact <- parse_date_flexible("20241014")

  testthat::expect_equal(result_dash$date_standard, result_compact$date_standard)
  testthat::expect_equal(result_dash$date_yyyymmdd, result_compact$date_yyyymmdd)
  testthat::expect_equal(result_dash$year, result_compact$year)
  testthat::expect_equal(result_dash$date_obj, result_compact$date_obj)
})

testthat::test_that("parse_date_flexible handles edge case dates", {
  # Leap year
  result_leap <- parse_date_flexible("2024-02-29")
  testthat::expect_equal(result_leap$date_standard, "2024-02-29")
  testthat::expect_equal(result_leap$date_yyyymmdd, "20240229")

  # Year start
  result_start <- parse_date_flexible("2024-01-01")
  testthat::expect_equal(result_start$date_standard, "2024-01-01")

  # Year end
  result_end <- parse_date_flexible("2024-12-31")
  testthat::expect_equal(result_end$date_standard, "2024-12-31")
})

testthat::test_that("parse_date_flexible rejects invalid date formats", {
  # Too short
  testthat::expect_error(
    parse_date_flexible("2024"),
    "Invalid date format"
  )

  # Wrong separator
  testthat::expect_error(
    parse_date_flexible("2024/10/14"),
    "Invalid date format"
  )

  # Invalid date
  testthat::expect_error(
    parse_date_flexible("2024-13-01"),
    "Invalid date format"
  )

  # Invalid leap year date
  testthat::expect_error(
    parse_date_flexible("2023-02-29"),
    "Invalid date format"
  )

  # Wrong length compact format
  testthat::expect_error(
    parse_date_flexible("202410"),
    "Invalid date format"
  )

  # Non-date string
  testthat::expect_error(
    parse_date_flexible("not-a-date"),
    "Invalid date format"
  )
})

testthat::test_that("pull_sensor accepts both date formats", {
  testthat::try_again(
    times = 3,
    code = {
      config_sample <- config %>%
        dplyr::sample_n(1)

      sensor_id <- config_sample$detector_name[[1]]

      # Test with YYYY-MM-DD format
      result_dash <- pull_sensor(
        sensor = sensor_id,
        pull_date = format(yesterday, "%Y-%m-%d"),
        fill_gaps = TRUE
      )

      # Test with YYYYMMDD format
      result_compact <- pull_sensor(
        sensor = sensor_id,
        pull_date = format(yesterday, "%Y%m%d"),
        fill_gaps = TRUE
      )

      # Results should be identical
      testthat::expect_equal(result_dash$date, result_compact$date)
      testthat::expect_equal(result_dash$sensor, result_compact$sensor)
      testthat::expect_equal(dim(result_dash), dim(result_compact))

      # Date should be in standard format
      testthat::expect_equal(as.character(result_dash$date[[1]]), format(yesterday, "%Y-%m-%d"))
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

      # Test with YYYY-MM-DD format
      result_dash <- pull_sensor_speed(
        sensor = sensor_id,
        pull_date = format(yesterday, "%Y-%m-%d"),
        fill_gaps = TRUE
      )

      # Test with YYYYMMDD format
      result_compact <- pull_sensor_speed(
        sensor = sensor_id,
        pull_date = format(yesterday, "%Y%m%d"),
        fill_gaps = TRUE
      )

      # Results should be identical
      testthat::expect_equal(result_dash$date, result_compact$date)
      testthat::expect_equal(result_dash$sensor, result_compact$sensor)

      # Date should be in standard format
      testthat::expect_equal(as.character(result_dash$date[[1]]), format(yesterday, "%Y-%m-%d"))
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

      # Test with YYYY-MM-DD format
      result_dash <- pull_sensor_length(
        sensor = sensor_id,
        pull_date = format(yesterday, "%Y-%m-%d"),
        fill_gaps = TRUE
      )

      # Test with YYYYMMDD format
      result_compact <- pull_sensor_length(
        sensor = sensor_id,
        pull_date = format(yesterday, "%Y%m%d"),
        fill_gaps = TRUE
      )

      # Date should be in standard format
      testthat::expect_equal(as.character(result_dash$date[[1]]), format(yesterday, "%Y-%m-%d"))
      testthat::expect_equal(as.character(result_compact$date[[1]]), format(yesterday, "%Y-%m-%d"))
    }
  )
})

testthat::test_that("pull_sensor_headway accepts both date formats", {
  testthat::try_again(
    times = 3,
    code = {
      config_sample <- config %>%
        dplyr::sample_n(1)

      sensor_id <- config_sample$detector_name[[1]]

      # Test with YYYY-MM-DD format
      result_dash <- pull_sensor_headway(
        sensor = sensor_id,
        pull_date = format(yesterday, "%Y-%m-%d"),
        fill_gaps = TRUE
      )

      # Test with YYYYMMDD format
      result_compact <- pull_sensor_headway(
        sensor = sensor_id,
        pull_date = format(yesterday, "%Y%m%d"),
        fill_gaps = TRUE
      )

      # Date should be in standard format
      testthat::expect_equal(as.character(result_dash$date[[1]]), format(yesterday, "%Y-%m-%d"))
      testthat::expect_equal(as.character(result_compact$date[[1]]), format(yesterday, "%Y-%m-%d"))
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

      # Test with YYYY-MM-DD format
      result_dash <- pull_sensor_espeed(
        sensor = sensor_id,
        pull_date = format(yesterday, "%Y-%m-%d"),
        fill_gaps = TRUE
      )

      # Test with YYYYMMDD format
      result_compact <- pull_sensor_espeed(
        sensor = sensor_id,
        pull_date = format(yesterday, "%Y%m%d"),
        fill_gaps = TRUE
      )

      # Date should be in standard format
      testthat::expect_equal(as.character(result_dash$date[[1]]), format(yesterday, "%Y-%m-%d"))
      testthat::expect_equal(as.character(result_compact$date[[1]]), format(yesterday, "%Y-%m-%d"))
    }
  )
})

# ============================================================================
# Error and Failure Tests for Date Parsing
# ============================================================================

testthat::test_that("parse_date_flexible handles NA and NULL", {
  # NULL should error
  testthat::expect_error(
    parse_date_flexible(NULL)
  )

  # Empty string should error
  testthat::expect_error(
    parse_date_flexible(""),
    "Invalid date format"
  )
})
