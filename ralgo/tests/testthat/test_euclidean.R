context('euclidean tests')

test_that("euclidean Character", {
  expect_that(euclidean(2,"A"))
  expect_that(euclidean(2,"A"))
})

test_that("euclidean Infinity", {
  expect_that(euclidean(Inf,Inf))
  expect_that(euclidean(0,0))
  expect_that(euclidean(Inf,-Inf))
})

test_that("euclidean Logic", {
  expect_that(euclidean(TRUE,TRUE)) # Returns 1
  expect_that(euclidean(TRUE,FALSE)) # Error
  expect_that(euclidean(FALSE,FALSE)) # Returns 0
})

test_that("euclidean Logic,error", {
  expect_error(euclidean(TRUE,TRUE))
})

test_that("euclidean equal"){
  expect_that(euclidian(200,150), equals(50))
  expect_that(euclidian(5,20), equals(5))
}

