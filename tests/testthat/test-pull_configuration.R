testthat::skip_if_offline()
testthat::test_that("Pull configuration returns 9,114 x 20 tibble", {
  config <- pull_configuration()

  testthat::expect_equal(class(config)[[1]], "tbl_df")
  testthat::expect_equal(dim(config)[[1]], 9114)
  testthat::expect_equal(dim(config)[[2]], 20)
})
