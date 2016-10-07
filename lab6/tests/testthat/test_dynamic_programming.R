context('Test dynamic programming')

test_that('knapsack dynamic',{
  expect_equal(knapsack_dynamic(knapsack_objects[1:8,],3500)$value, 16770)
  expect_equal(knapsack_dynamic(knapsack_objects[1:8,],3500)$elements, c(5,8))
  expect_equal(knapsack_dynamic(knapsack_objects[1:12,],3500)$value, 16770)
  expect_equal(knapsack_dynamic(knapsack_objects[1:12,],3500)$elements, c(5,8))
  expect_equal(knapsack_dynamic(knapsack_objects[1:8,],2000)$value, 15428)
  expect_equal(knapsack_dynamic(knapsack_objects[1:8,],2000)$elements, c(3,8))
  expect_equal(knapsack_dynamic(knapsack_objects[1:12,],2000)$value, 15428)
  expect_equal(knapsack_dynamic(knapsack_objects[1:12,],2000)$elements, c(3,8))
})

test_that('knapsack dynamic',{
  expect_error(knapsack_dynamic(knapsack_objects,TRUE))
  expect_error(knapsack_dynamic(knapsack_objects,FALSE))
  expect_error(knapsack_dynamic(knapsack_objects,Inf))
  expect_error(knapsack_dynamic(knapsack_objects,-Inf))
})

test_that('knapsack dynamic',{
  expect_error(knapsack_dynamic(c(1:00),10))
  expect_error(knapsack_dynamic(knapsack_objects,"x"))
  expect_error(knapsack_dynamic(c(1:00),10))
})

colnames(knapsack_objects) <- c("a","b")

test_that('knapsack dynamic',{
  expect_error(knapsack_dynamic(knapsack_objects,2000))
})

colnames(knapsack_objects) <- c("w","v")
knapsack_objects$w <- -100

test_that('knapsack dynamic',{
  expect_error(knapsack_dynamic(knapsack_objects,3500))
})