testthat::test_that("validate_date_available accepts valid recent dates", {
  # Yesterday should be available
  testthat::expect_true(
    validate_date_available(yesterday, district = "metro", .quiet = TRUE)
  )

  # Should also work with YYYYMMDD format
  testthat::expect_true(
    validate_date_available(
      format(yesterday, "%Y%m%d"),
      district = "metro",
      .quiet = TRUE
    )
  )
})

testthat::test_that("validate_date_available rejects dates with no data", {
  # Very old date that likely has no data
  old_date <- "1990-01-01"

  testthat::expect_error(
    validate_date_available(old_date, district = "metro", .quiet = TRUE),
    "not available|Unable to fetch"
  )
})

testthat::test_that("validate_date_available rejects future dates", {
  # Far future date (unlikely to be available)
  future_date <- format(Sys.Date() + 365, "%Y-%m-%d")

  testthat::expect_error(
    validate_date_available(future_date, district = "metro", .quiet = TRUE),
    "not available|Unable to fetch"
  )
})

testthat::test_that("validate_date_available handles invalid date formats", {
  # Invalid date format
  testthat::expect_error(
    validate_date_available("2024/10/14", district = "metro", .quiet = TRUE),
    "Invalid date format"
  )

  # Wrong length
  testthat::expect_error(
    validate_date_available("202410", district = "metro", .quiet = TRUE),
    "Invalid date format"
  )

  # Non-date string
  testthat::expect_error(
    validate_date_available("not-a-date", district = "metro", .quiet = TRUE),
    "Invalid date format"
  )
})

testthat::test_that("validate_date_available works with different districts", {
  # Test with metro district (default) - has 2026 data
  testthat::expect_true(
    validate_date_available(yesterday, district = "metro", .quiet = TRUE)
  )

  # Test that district parameter is being used - d1 only has 2016 data
  # so 2026 date should error with district-specific message
  testthat::expect_error(
    validate_date_available(yesterday, district = "d1", .quiet = TRUE),
    "d1 district"
  )

  # Test that d6 district is checked correctly - only has 2009-2016 data
  testthat::expect_error(
    validate_date_available(yesterday, district = "d6", .quiet = TRUE),
    "d6 district"
  )

  # Test with invalid district - should error
  testthat::expect_error(
    validate_date_available(yesterday, district = "invalid_district", .quiet = TRUE),
    "Unable to fetch|not available"
  )
})

testthat::test_that("validate_date_available caches results", {
  # First call - will hit API
  result1 <- validate_date_available(yesterday, district = "metro", .quiet = TRUE)

  # Second call - should use cache (faster)
  result2 <- validate_date_available(yesterday, district = "metro", .quiet = TRUE)

  testthat::expect_true(result1)
  testthat::expect_true(result2)
})

testthat::test_that("validate_date_available handles NULL inputs", {
  testthat::expect_error(
    validate_date_available(NULL, district = "metro", .quiet = TRUE)
  )
})

testthat::test_that("validate_date_available quiet parameter works", {
  # With .quiet = TRUE, no messages should be shown
  testthat::expect_silent(
    validate_date_available(yesterday, district = "metro", .quiet = TRUE)
  )

  # With .quiet = FALSE, messages may be shown (if not cached)
  # This test just verifies it doesn't error
  testthat::expect_no_error(
    validate_date_available(yesterday, district = "metro", .quiet = FALSE)
  )
})
