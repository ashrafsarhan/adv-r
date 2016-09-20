linreg_class <- setRefClass(
  "linreg_class",
  fields = list(
                  formula = "character",
                  fitted_values = "numeric",
                  residuals = "numeric",
                  df = "integer",
                  sigma2 = "numeric",
                  beta_estimate = "numeric",
                  beta_variance = "numeric",
                  t_ratio = "numeric",
                  p_value = "numeric"), 
  methods = list(
                  print = function() {
                    cat('Call:\n','linreg(formula = ',formula[2],'~',formula[3],')\n\n',sep = '')
                    beta_estimate 
                  },
                  plot = function() {
                    cat('should plot the following two plots using ggplot2. Remember to include ggplot2 in your package')
                  },
                  resid = function() {
                    return(residuals)
                  },
                  pred = function() {
                    return(fitted_values)
                  },
                  coef = function() {
                    return(beta_estimate) 
                  },
                  summary = function() {
                    cat('Call:\n','linreg(formula = ',formula[2],'~',formula[3],')\n\n',sep = '')
                    mat <- matrix(c(beta_estimate, beta_variance, t_ratio, p_value),nrow = length(beta_estimate), ncol=4,byrow=FALSE)
                    colnames(mat) <- c('Estimate', 'Std. Error', 't value', 'Pr(>|T|)')
                    rownames(mat) <- names(beta_estimate)
                    print.table(as.table(mat))
                    cat('\nResidual standard error:', sigma2, 'on', df, 'degress of freedom', sep = ' ')
                  })
  )

