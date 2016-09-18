library(testthat)
context('linreg tests')

library(Formula)

data(iris)

fit <- lm(formula = Petal.Length ~ Petal.Width, data = iris)

summary(fit)
coefficients <- summary(fit)$coefficients
residuals <- summary(fit)$residuals
sigma<- summary(fit)$sigma


test_that("linreg methods", {
  expect_equal(linreg$residuals(formula = Petal.Length ~ Petal.Width, data = iris),
               residuals)
  expect_equal(linreg_class$df(formula = Petal.Length ~ Petal.Width, data = iris),
               df)
  expect_equal(linreg_class$sigma2(formula = Petal.Length ~ Petal.Width, data = iris),
               sigma)

})


