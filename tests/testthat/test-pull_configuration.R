testthat::test_that("Pull configuration returns 9,114 x 20 tibble", {

  testthat::expect_equal(class(config)[[1]], "tbl_df")
  testthat::expect_true(dim(config)[[1]] > 9000)
  testthat::expect_equal(dim(config)[[2]], 20)
})
