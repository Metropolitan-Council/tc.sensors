testthat::skip_if_offline()


test_that("distance is calculated correctly", {
  config <- as.data.table(pull_configuration())

  th22 <-  config[corridor_route == "T.H.22",]
  th100 <- config[corridor_route == "T.H.100",]

  # test errors ----------------------------------------------------------------
  testthat::expect_error(add_distance(
    config = th22[r_node_n_type == "Exit", ],
    interpolate_missing = TRUE
  ))

  testthat::expect_error(add_distance(th22[1,]))
  testthat::expect_error(add_distance(rbind(th100, th22[1,])))

  # test output ----------------------------------------------------------------
  pulled <- th22 %>%
    add_distance(interpolate_missing = TRUE)

  # returns same number of rows as input
  testthat::expect_true(nrow(th22) == nrow(pulled))

  # max distance no greater than 3 miles
  testthat::expect_true(max(pulled$distance, na.rm = T) < 3)

  })
