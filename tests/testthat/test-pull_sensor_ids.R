test_that("pull_sensor_ids works", {
  sensor_ids <- pull_sensor_ids()

  testthat::expect_equal(class(sensor_ids)[[1]], "tbl_df")
  testthat::expect_true(dim(sensor_ids)[[1]] > 9000)
  testthat::expect_equal(dim(sensor_ids)[[2]], 1)
})
