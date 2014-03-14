context("Test getGenus")

library(rYoutheria)

test_that("Testing errors and warnings are given", {
  expect_error(getGenus(genus=TRUE), regexp='argument must be numeric, integer, charater or NULL')
})

test_that("Testing functionality", {
  expect_is(test <- getGenus(), 'data.frame')
  expect_is(test2 <- getGenus(1), 'data.frame')
  expect_true(nrow(test2) == 1)
  expect_true(ncol(test2) == 2)
  expect_is(test3 <- getGenus(1:5), 'data.frame')
  expect_true(nrow(test3) == 5) 
  expect_is(test4 <- getGenus('Uropsilus'), 'data.frame')
  #expect_true(nrow(test4) == 1)
  expect_is(test5 <- getGenus(c('Uropsilus','Urotrichus')), 'data.frame')
  #expect_true(nrow(test5) == 2)  
})