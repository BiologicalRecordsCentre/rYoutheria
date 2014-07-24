context("Test getLocData")

library(rYoutheria)

test_that("Testing errors and warnings are given", {
  expect_warning(test <- getLocData(country = "foo"),
                 regexp = 'No Data returned for this country')
  expect_warning(test <- getLocData(StudyUnitId=12),
                 regexp = 'No Data returned for this StudyUnitId')
  expect_error(getLocData(StudyUnitId=12,country='tom'),
               regexp = 'Cannot use both StudyUnitId and country at the same time')
})

test_that("Testing data is returned", {
  expect_is(test <- getLocData(country='India'), 'data.frame')
  expect_equal(ncol(test), 16)
  expect_is(test <- getLocData(StudyUnitId=169), 'data.frame')
  expect_equal(ncol(test), 16)
})