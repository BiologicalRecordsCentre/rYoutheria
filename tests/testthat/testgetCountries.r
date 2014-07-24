context("Test getCountries")

library(rYoutheria)

test_that("Testing functionality", {
  expect_is(test <- getCountries(), 'data.frame')
  expect_equal(ncol(test),2)
})