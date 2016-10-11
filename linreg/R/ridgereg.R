#' Store output from a OLS model in a ridgereg object
#'
#' This function calculates fitted values, residuals, degrees of freedom, t-ratio and p-values
#'
#'@param formula, writen as a formula-object, see example.
#'@param data, a data.frame object with standardized variables
#'@param lambda, hyperparameter used to find the best model
#'@examples
#'@return a ridgereg class-object containing regression outputs
#'@author Yumeng Li, Mattias Karlsson, Ashraf Sarhan
#'@details This function calculates Ridge regression
#'@export
ridgereg <- function(formula, data, lambda){
  # Check if arguments is in a valid format
  stopifnot(formula == as.formula(formula), is.data.frame(data))
  
  # Model.frame picks the right variables from the formula-object
  X <- model.frame(formula = formula,
                   data = data)
  
  # Store the dependent-variable
  Y <- X[,1]
  
  # Design matrix, independent-variables
  X <- model.matrix(object = formula,
                    data = X)
  XT <- t(X)
  XTX <- XT %*% X
  
  if(all(eigen(XTX)$values <= 0)){
    # If eigenvalues are zero or less you can't invert the matrix
    return("Cannot compute linear regression")
  } else {
    
    # Creates the identity matrix. Multiplied with Lambda-parameter
    ridge_matrix <- diag(x = 1, nrow = dim(XTX)[1], ncol = dim(XTX)[2])
    lambda_matrix <- diag(x = lambda, nrow = dim(XTX)[1], ncol = dim(XTX)[2])
    ridge_m_lambda <- lambda_matrix %*% ridge_matrix
    
    XTXInv <- solve(XTX + ridge_m_lambda) # calculates the inverse
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
    
    # Return result as a class of rigereg
    result <- ridgereg_class$new(fitted_values = YHat,
                                 beta_estimate = c(betaHat)
)
  }
  invisible(result)
  # return(result)
}