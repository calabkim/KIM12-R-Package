# testfwd_stepwise.R
library(KIM12)
load("testfwd_stepwise.RData")
test_that("fwd_stepwise() returns the correct object", {
  expect_equal(fwd_stepwise(testy,testx,testmc), testfwd)
})


