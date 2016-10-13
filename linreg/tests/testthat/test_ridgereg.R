library(testthat)
library(Formula)
library(MASS)

data(iris)

# extract result from lm.ridge()
fit1 <- lm.ridge(formula = Petal.Length ~ Species, data = iris, lambda = 3)



# extract result from lingre
test1 <- ridgereg(formula = Petal.Length ~ Species, data = iris, lambda = 3)

# unit test for each value
test_that("linreg methods", {
  expect_equivalent(floor(test1$beta_estimate[1]), floor(coef(fit1))[1])
  expect_equivalent(floor(test1$beta_estimate[2]), floor(coef(fit1))[2])
  expect_equivalent(floor(test1$beta_estimate[3]), floor(coef(fit1))[3])
 
})
