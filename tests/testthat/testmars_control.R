# testmars_control.R
library(KIM12)
load("testmc.RData")
test_that("mars.control() returns the correct object", {
  expect_equal(mars.control(Mmax=10), testmc)
})

