testthat::test_that("pull_sensor_headway retrieves data successfully", {
  testthat::try_again(
    times = 5,
    code = {
      config_sample <- config %>%
        dplyr::sample_n(1)

      headway_results <- pull_sensor_headway(
        sensor = config_sample$detector_name[[1]],
        pull_date = yesterday,
        fill_gaps = TRUE
      )


      testthat::expect_s3_class(headway_results, "data.table")
      testthat::expect_true("headway" %in% names(headway_results))
      testthat::expect_true("sensor" %in% names(headway_results))
      testthat::expect_true("date" %in% names(headway_results))
      testthat::expect_true("hour" %in% names(headway_results))
      testthat::expect_true("min" %in% names(headway_results))
      testthat::expect_equal(dim(headway_results)[[1]], 2880)
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

      # YYYY-MM-DD format
      result_dash <- pull_sensor_headway(
        sensor_id,
        format(yesterday, "%Y-%m-%d"),
        fill_gaps = TRUE
      )

      # YYYYMMDD format
      result_compact <- pull_sensor_headway(
        sensor_id,
        format(yesterday, "%Y%m%d"),
        fill_gaps = TRUE
      )

      testthat::expect_equal(dim(result_dash), dim(result_compact))
      testthat::expect_equal(result_dash$date, result_compact$date)
    }
  )
})

testthat::test_that("pull_sensor_headway filtering works correctly", {
  testthat::try_again(
    times = 3,
    code = {
      config_sample <- config %>%
        dplyr::sample_n(1)
      sensor_id <- config_sample$detector_name[[1]]

      # Test with headway filters
      result <- pull_sensor_headway(
        sensor_id,
        yesterday,
        headway_sec_min = 10,
        headway_sec_max = 60,
        fill_gaps = TRUE
      )

      testthat::expect_s3_class(result, "data.table")
      testthat::expect_true("headway" %in% names(result))
      testthat::expect_equal(nrow(result), 2880)
    }
  )
})

# testthat::test_that("pull_sensor_headway filtering returns values within specified range", {
#     testthat::try_again(
#         times = 4,
#         code = {
#             # Sample multiple sensors and find first with enough data
#             max_attempts <- 12

#             result <- purrr::map(1:max_attempts, ~ {
#                 config_sample <- config %>%
#                     dplyr::sample_n(1)
#                 sensor_id <- config_sample$detector_name[[1]]

#                 # Apply specific filter range
#                 pull_sensor_headway(
#                     sensor_id,
#                     yesterday,
#                     headway_sec_min = 1000,
#                     headway_sec_max = 60000,
#                     fill_gaps = FALSE # Don't fill gaps to get actual values
#                 )
#             }) %>%
#                 purrr::keep(~ nrow(.x) >= 2880) %>%
#                 purrr::pluck(1, .default = NULL)

#             # Skip test if no sensor with enough data found
#             if (is.null(result) || nrow(result) < 2880) {
#                 testthat::skip("Could not find sensor with sufficient filtered data")
#             }

#             # Remove NA values and check range
#             non_na_headways <- result$headway[!is.na(result$headway)]

#             if (length(non_na_headways) > 0) {
#                 testthat::expect_true(
#                     all(non_na_headways >= 1000),
#                     info = paste("Min headway:", min(non_na_headways))
#                 )
#                 testthat::expect_true(
#                     all(non_na_headways <= 60000),
#                     info = paste("Max headway:", max(non_na_headways))
#                 )
#             }
#         }
#     )
# })

testthat::test_that("pull_sensor_headway rejects invalid dates", {
  config_sample <- config %>%
    dplyr::sample_n(1)
  sensor_id <- config_sample$detector_name[[1]]

  # Invalid format
  testthat::expect_error(
    pull_sensor_headway(sensor_id, "20240001"),
    "Invalid date format"
  )

  # Wrong separator
  testthat::expect_error(
    pull_sensor_headway(sensor_id, "2024/10/14"),
    "Invalid date format"
  )

  # Non-date string
  testthat::expect_error(
    pull_sensor_headway(sensor_id, "invalid"),
    "Invalid date format"
  )
})

testthat::test_that("pull_sensor_headway handles NULL parameters", {
  config_sample <- config %>%
    dplyr::sample_n(1)
  sensor_id <- config_sample$detector_name[[1]]

  testthat::expect_error(
    pull_sensor_headway(sensor_id, NULL)
  )
})

testthat::test_that("pull_sensor_headway handles unavailable dates", {
  config_sample <- config %>%
    dplyr::sample_n(1)
  sensor_id <- config_sample$detector_name[[1]]

  future_date <- format(Sys.Date() + 365, "%Y-%m-%d")

  testthat::expect_error(
    pull_sensor_headway(sensor_id, future_date),
    "not available|Unable to fetch"
  )
})

testthat::test_that("pull_sensor_headway works with different districts", {
  testthat::try_again(
    times = 3,
    code = {
      config_sample <- config %>%
        dplyr::sample_n(1)
      sensor_id <- config_sample$detector_name[[1]]

      # Default metro district should work with 2026 data
      result_metro <- pull_sensor_headway(
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
    pull_sensor_headway("999", yesterday, district = "d1"),
    "d1 district"
  )
})

testthat::test_that("pull_sensor_headway fill_gaps parameter works", {
  testthat::try_again(
    times = 3,
    code = {
      config_sample <- config %>%
        dplyr::sample_n(1)
      sensor_id <- config_sample$detector_name[[1]]

      # With fill_gaps = TRUE
      result_filled <- pull_sensor_headway(
        sensor_id,
        yesterday,
        fill_gaps = TRUE
      )

      # With fill_gaps = FALSE
      result_unfilled <- pull_sensor_headway(
        sensor_id,
        yesterday,
        fill_gaps = FALSE
      )

      testthat::expect_s3_class(result_filled, "data.table")
      testthat::expect_s3_class(result_unfilled, "data.table")
    }
  )
})
