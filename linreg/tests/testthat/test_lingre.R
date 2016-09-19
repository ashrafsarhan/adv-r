library(testthat)
context('linreg tests')

library(Formula)

data(iris)

# extract result from lm()
fit <- lm(formula = Sepal.Width ~ Sepal.Length, data = iris)

summary(fit)
coefficients <- summary(fit)$coefficients
beta_estimate <- coefficients[ ,1]
beta_variance <- (coefficients[ ,2])^2
t_ratio <- coefficients[ ,3]
residuals <- summary(fit)$residuals
sigma2 <- (summary(fit)$sigma)^2
df <- summary(fit)$df[2]
p_value <- summary(fit)$p_value



# extract result from lingre
test <- linreg(Sepal.Width ~ Sepal.Length, iris)

# unit test for each value
test_that("linreg methods", {
  expect_equivalent(as.numeric(test$df), df)
  expect_equivalent(round(as.numeric(test$sigma2), digits = 4), round(sigma2, digits = 4))
  expect_equivalent(as.numeric(test$t_ratio), round(as.numeric(t_ratio), digits = 2))
  expect_equivalent(as.numeric(test$beta_estimate), round(as.numeric(beta_estimate), digits = 4))
  expect_equivalent(round(as.numeric(test$beta_variance), digits = 4), round(as.numeric(beta_variance), digits = 4))
  expect_equivalent(round(sort(as.numeric(test$residuals)), digits = 2), 
               round(sort(as.numeric(residuals)), digits = 2))
})

