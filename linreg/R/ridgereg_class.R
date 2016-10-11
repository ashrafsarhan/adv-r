ridgereg_class <- setRefClass(
  "ridgereg_class",
  fields = list(
    fitted_values = "numeric",
    beta_estimate = "numeric"),
 
  methods = list(
    pred = function() {
      return(fitted_values)
    },
    coef = function() {
      return(beta_estimate) 
    })
)

