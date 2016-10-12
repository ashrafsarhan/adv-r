ridgereg_class <- setRefClass(
  "ridgereg_class",
  fields = list(
    formula = "character",
    fitted_values = "numeric",
    beta_estimate = "numeric"),
  
  methods = list(
    print = function(){
      cat("Call:\n","ridgereg(formula = ", formula[2],"~", formula[3],")")
    },
    predict = function(){
      return(fitted_values)
    },
    coef = function(){
      return(beta_estimate) 
    }
  )
)

#test1 <- ridgereg(formula = Petal.Length ~ Species, data = iris, lambda = 3)
#test1$print()
#test1$predict()
#test1$coef()

