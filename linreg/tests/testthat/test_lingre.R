library(testthat)
context('linreg tests')

library(Formula)

data(iris)

fit <- lm(formula = Petal.Length ~ Petal.Width, data = iris)

summary(fit)
coefficients <- summary(fit)$coefficients
residuals <- summary(fit)$residuals
sigma<- summary(fit)$sigma
summary(fit)$fstatistic


test_that("linreg methods", {
  expect_equal(linreg$coefficients(formula = Petal.Length ~ Petal.Width, data = iris),
               coefficients)
  expect_equal(linreg$resid(formula = Petal.Length ~ Petal.Width, data = iris),
               residuals)
  expect_equal(linreg$df(formula = Petal.Length ~ Petal.Width, data = iris),
               df)
  expect_equal(linreg$sigma(formula = Petal.Length ~ Petal.Width, data = iris),
               sigma)

})


