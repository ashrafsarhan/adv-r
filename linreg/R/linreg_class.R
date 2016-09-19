linreg_class <- setRefClass(
  "linreg_class",
  fields = list(
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
                    print('should print out the coefficients and coefficient names, similar as done by the lm class.')
                  },
                  plot = function() {
                    print('should plot the following two plots using ggplot2. Remember to include ggplot2 in your package')
                  },
                  resid = function() {
                    print('should return the vector of residuals e.')
                  },
                  pred = function() {
                    print('should return the predicted values ŷ.')
                  },
                  coef = function() {
                    print('should return the coefficients as a named vector.')
                  },
                  summary = function() {
                    print('summary() should return a similar printout as printed for lm objects, but you only need to present \n 
                          the coefficients with their standard error, t-value and p-value as well as the estimate of ˆ and the degrees \n
                          of freedom in the model.')
                  })
  )

