ridgereg_class <- setRefClass(
  "ridgereg_class",
  fields = list(
    formula = "character",
    fitted_values = "numeric",
    beta_estimate = "numeric"),
  
  methods = list(
    print = function(){
      cat('Call:\n','linreg(formula = ',formula[2],'~',formula[3],')\n\n',sep = '')
    },
    predict = function(){
      return(fitted_values)
    },
    coef = function(){
      return(beta_estimate) 
    }
    )
)

