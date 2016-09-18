#' Store output from a OLS model in a linreg object
#'
#' This function calculates fitted values, residuals, degrees of freedom, t-ratio and p-values
#'
#'@param formula, writen as a formula-object, se exemple.
#'@param data, a data.frame object
#'@examples
#'linreg(Sepal.Length ~ Sepal.Width, iris)
#'@return a linreg class-object containing regression outputs
#'@author Yumeng Li, Mattias Karlsson, Ashraf Sarhan
#'@details This function calculates ordrinary least square models (OLS)
#'@export
linreg <- function(formula, data){


  # test
  stopifnot(formula == as.formula(formula), is.data.frame(data))

  #  model.frame picks the right variables from the formula-object
  X <- model.frame(formula = formula,
                   data = data)
  print(X)
  # stores dependent-variable
  Y <- X[,1]
  print(Y)
  # design matrix, independent-variables
  X <- model.matrix(object = formula,
                    data = X)
  print(X)
  XT <- t(X)
  XTX <- XT %*% X

  # test
  stopifnot(formula == as.formula(formula), is.data.frame(data))

  #  model.frame picks the right variables from the formula-object
  X <- model.frame(formula = formula,
                   data = data)

  # stores dependent-variable
  Y <- X[,1]

  # design matrix, independent-variables
  X <- model.matrix(object = formula,
                    data = X)
  XT <- t(X)
  XTX <- XT %*% X

  if(all(eigen(XTX)$values <= 0)){
    # test
    # if eigenvalues are zero or less you can't invert the matrix
    return("Cannot compute linear regression")
  } else {
    XTXInv <- solve(XTX) # calculates the inverse
    betaHat <- XTXInv %*% XT %*% Y # estimates
    betaHat <- round(x = betaHat, digits = 4) # matching with lm-function
    YHat <- as.vector(X %*% betaHat) # fitted-values
    eHat <- as.matrix(Y - YHat) # residuals

    df <- nrow(eHat) - ncol(X) # degrees of freedom

    s2 <- (t(eHat) %*% eHat) / df # variance
    sigmaHat <- sqrt(s2) # standard-deviation

    beta_variance <- c(s2) * XTXInv
    t_ratio <- c(betaHat) / sqrt(diag(beta_variance)) # T-ratio
    t_ratio <- round(x = t_ratio, digits = 2)
    p_value <- 1 - pt(q = abs(t_ratio), df = df) # p-values
    names(betaHat) <- names(t_ratio)

    # trying to store everything in the linreg_class
    result <- linreg_class$new(fitted_values = YHat,
                               residuals = c(eHat),
                               df = df,
                               sigma2 = c(s2),
                               beta_estimate = c(betaHat),
                               beta_variance = c(diag(beta_variance)),
                               t_ratio = t_ratio,
                               p_value = p_value)
  }
  return(result)
}
