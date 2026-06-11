test_that("pull_sensor_ids works", {
  sensor_ids <- pull_sensor_ids()

  testthat::expect_equal(class(sensor_ids)[[1]], "data.table")
  testthat::expect_true(dim(sensor_ids)[[1]] > 5000)
  testthat::expect_equal(dim(sensor_ids)[[2]], 1)
})

test_that("pull_sensor_ids works for District 6", {
  sensor_ids <- pull_sensor_ids(district = "d6")

  testthat::expect_equal(class(sensor_ids)[[1]], "data.table")
  testthat::expect_true(dim(sensor_ids)[[1]] > 100)
  testthat::expect_equal(dim(sensor_ids)[[2]], 1)
})


test_that("pull_sensor_ids works for District 1", {
  sensor_ids <- pull_sensor_ids(district = "d1")

  testthat::expect_equal(class(sensor_ids)[[1]], "data.table")
  testthat::expect_true(dim(sensor_ids)[[1]] > 2)
  testthat::expect_equal(dim(sensor_ids)[[2]], 1)
})

test_that("pull_sensor_ids works for metro signals", {
  sensor_ids <- pull_sensor_ids(district = "metro_signals")

  testthat::expect_equal(class(sensor_ids)[[1]], "data.table")
  testthat::expect_true(dim(sensor_ids)[[1]] > 2)
  testthat::expect_equal(dim(sensor_ids)[[2]], 1)
})
