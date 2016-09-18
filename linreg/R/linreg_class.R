linreg_class <- setRefClass(
  "linreg_class",
  fields = list(fitted_values = "numeric",
                residuals = "numeric",
                df = "integer",
                sigma2 = "numeric",
                beta_estimate = "numeric",
                beta_variance = "numeric",
                t_ratio = "numeric",
                p_value = "numeric"))

