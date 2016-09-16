context('euclidean tests')

test_that("euclidean Character", {
  expect_error(euclidean(2, 'A'), 'args must be numeric!')
  expect_error(euclidean(2,0.5), 'args must be intergers!')
})

test_that("euclidean Infinity", {
  expect_error(euclidean(Inf,Inf))
  expect_equal(euclidean(0,0), 0)
  expect_error(euclidean(Inf,-Inf))
})

test_that("euclidean Logic", {
  expect_error(euclidean(TRUE,TRUE))
  expect_error(euclidean(TRUE,FALSE))
  expect_error(euclidean(FALSE,FALSE))
})

test_that("euclidean equal", {
  expect_equivalent(euclidean(200,150), 50)
  expect_equivalent(euclidean(5,20), 5)
})

