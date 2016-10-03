context('Test Greedy heuristic')

set.seed(42)
n <- 2000
knapsack_objects <-
  data.frame(
    w=sample(1:4000, size = n, replace = TRUE),
    v=runif(n = n, 0, 10000)
  )

test_that('greedy heuristic',{
  expect_equal(greedy_knapsack(knapsack_objects[1:800,],3500)$value, 192647)
  expect_equal(greedy_knapsack(knapsack_objects[1:1200,],2000)$value, 212337)
})

test_that('greedy heuristic',{
  expect_error(greedy_knapsack(knapsack_objects,TRUE))
  expect_error(greedy_knapsack(knapsack_objects,FALSE))
  expect_error(greedy_knapsack(knapsack_objects,Inf))
  expect_error(greedy_knapsack(knapsack_objects,-Inf))
})

test_that('greedy heuristic',{
  expect_error(greedy_knapsack(c(1:00),10))
  expect_error(greedy_knapsack(knapsack_objects,"x"))
  expect_error(greedy_knapsack(c(1:00),10))
})

colnames(knapsack_objects) <- c("a","b")

test_that('greedy heuristic',{
  expect_error(greedy_knapsack(knapsack_objects,2000))
})

colnames(knapsack_objects) <- c("w","v")
knapsack_objects$w <- -100

test_that('greedy heuristic',{
  expect_error(greedy_knapsack(knapsack_objects,3500))
})
