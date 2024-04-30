testthat::test_that("Pull configuration returns 9,114 x 20 tibble", {
  testthat::expect_equal(class(config_raw)[[1]], "tbl_df")
  testthat::expect_true(dim(config_raw)[[1]] > 9000)
  testthat::expect_equal(dim(config_raw)[[2]], 20)
})
