context("Test getMeasurementData")

library(rYoutheria)

test_that("Testing errors and warnings are given", {
  expect_error(test <- getMeasurementData(999),
               regexp = 'All measurement types specified are invalid:')
  expect_warning(test <- getMeasurementData(c(1,999)),
                 regexp = 'Some measurement types unknown:')
  expect_error(test <- getMeasurementData('s'),
               regexp = 'All measurement types specified are invalid:')
  expect_warning(test <- getMeasurementData(c('Body Mass','s')),
                 regexp = 'Some measurement types unknown:')
  expect_warning(test <- getMeasurementData(measurementType = 1, MSW93Binomial='Pon'),
                 regexp = 'No data was returned')
  expect_warning(test <- getMeasurementData(measurementType = 1, MSW93Binomial=c('Tom', 'Dick', 'Harry', 'Peroryctes raffrayana')),
                 regexp = 'There were no results returned for the following species: Tom, Dick, Harry')
  expect_warning(test <- getMeasurementData(measurementType = 1, MSW05Binomial='Pon'),
                 regexp = 'No data was returned')
  expect_warning(test <- getMeasurementData(measurementType = 1, MSW05Binomial=c('Tom', 'Dick', 'Harry', 'Peroryctes raffrayana')),
                 regexp = 'There were no results returned for the following species: Tom, Dick, Harry')
  expect_error(test <- getMeasurementData(measurementType = 1, MSW05Binomial='Tom',MSW93Binomial='Dick'),
               regexp = 'Cannot filter by MSW05Binomial and MSW93Binomial')  
})

test_that("Testing search by measurement type", {
  expect_is(test <- getMeasurementData('Body Mass'), 'data.frame')
  expect_equal(ncol(test), 11)
  expect_is(test2 <- getMeasurementData(c('Body Mass','Population Density')), 'data.frame')
  expect_true(nrow(test) < nrow(test2))
})

test_that("Testing MSW93binomial searches", {
  expect_is(test <- getMeasurementData(measurementType = 1, MSW93Binomial='Pongo pygmaeus'), 'data.frame')
  expect_equal(ncol(test), 11)
  expect_is(test2 <- getMeasurementData(measurementType = 1, MSW93Binomial=c('Pongo pygmaeus','Hodomys alleni')), 'data.frame')
  expect_true(nrow(test) < nrow(test2))  
})

test_that("Testing MSW05binomial searches", {
  expect_is(test <- getMeasurementData(measurementType = 1, MSW05Binomial='Pongo pygmaeus'), 'data.frame')
  expect_equal(ncol(test), 11)
  expect_is(test2 <- getMeasurementData(measurementType = 1, MSW05Binomial=c('Pongo pygmaeus','Hodomys alleni')), 'data.frame')
  expect_true(nrow(test) < nrow(test2))  
})




