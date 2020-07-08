library(testthat)
library(tc.sensors)

set.seed(as.numeric(format(Sys.Date(), "%d")) + 24601)
test_check("tc.sensors")
