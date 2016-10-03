context('Test Brute force search')

set.seed(42)
n <- 2000
knapsack_objects <-
  data.frame(
    w=sample(1:4000, size = n, replace = TRUE),
    v=runif(n = n, 0, 10000)
  )

test_that('Brute force search',{
  expect_equal(brute_force_knapsack(knapsack_objects[1:8,],3500)$value, 16770)
  expect_equal(brute_force_knapsack(knapsack_objects[1:8,],3500)$elements, c(5,8))
  expect_equal(brute_force_knapsack(knapsack_objects[1:12,],3500)$value, 16770)
  expect_equal(brute_force_knapsack(knapsack_objects[1:12,],3500)$elements, c(5,8))
  expect_equal(brute_force_knapsack(knapsack_objects[1:8,],2000)$value, 15428)
  expect_equal(brute_force_knapsack(knapsack_objects[1:8,],2000)$elements, c(3,8))
  expect_equal(brute_force_knapsack(knapsack_objects[1:12,],2000)$value, 15428)
  expect_equal(brute_force_knapsack(knapsack_objects[1:12,],2000)$elements, c(3,8))
})


test_that('Brute force search',{
  expect_error(brute_force_knapsack(knapsack_objects,TRUE))
  expect_error(brute_force_knapsack(knapsack_objects,FALSE))
  expect_error(brute_force_knapsack(knapsack_objects,Inf))
  expect_error(brute_force_knapsack(knapsack_objects,-Inf))
})

test_that('Brute force search',{
  expect_error(brute_force_knapsack(c(1:00),10))
  expect_error(brute_force_knapsack(knapsack_objects,"x"))
  expect_error(brute_force_knapsack(c(1:00),10))
})

colnames(knapsack_objects) <- c("a","b")

test_that('Brute force search',{
  expect_error(brute_force_knapsack(knapsack_objects,2000))
})

colnames(knapsack_objects) <- c("w","v")
knapsack_objects$w <- -100

test_that('Brute force search',{
  expect_error(brute_force_knapsack(knapsack_objects,3500))
})