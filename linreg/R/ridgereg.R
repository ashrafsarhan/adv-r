#' Store output from a OLS model in a ridgereg object
#'
#' This function calculates fitted values and coefficents
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
    result <- ridgereg_class$new(formula = as.character(formula),
                                 fitted_values = YHat,
                                 beta_estimate = c(betaHat)
    )
  }
  invisible(result)
  # return(result)
}

#' Ridge regression
#'
#'@param formula, a formula-object
#'@param data, a data.frame object with standardized variables
#'@param lambda, hyperparameter used to find the best model
#'@return a ridgereg class-object containing regression outputs
#'@author Leif Jonsson
#'@details This function adapts OLS with ridge regression\
#'@export
#'@importFrom stats model.matrix
rrfit<-function(formula, data, lambda = 0){
  X <- model.matrix(formula, data)
  yName <- all.vars(formula)[1]
  Y <- data[, yName]
  betas <- solve(t(X) %*% X + diag(lambda, nrow = ncol(X))) %*% t(X) %*% Y
  y.hat <- (X %*% betas)
  l<-list(coefficients=betas, y.hat = y.hat, formula=formula)
  class(l) <- "ridgeregression"
  return(l)
}

#' Ridge regression predict
#'
#'@param model, an ridgeregression model
#'@param newdata, new data to do prediction from
#'@return a vector with predictions
#'@author Leif Jonsson
#'@details This function adapts OLS with ridge regression
#'@export
rrpred<-function(model, newdata){
  cnames <- colnames(newdata)
  newdata <- cbind(1,newdata)
  colnames(newdata) <- c("(Intercept)",cnames)
  yHat <- as.matrix(newdata) %*% model$coefficients
  as.vector(yHat)
}