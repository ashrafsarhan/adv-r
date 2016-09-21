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
                    #The first plot
                    df <- data.frame(x=fitted_values, y=residuals)
                    res_vs_fit_plot <-  ggplot(df, aes(x = x, y = y)) +
                      geom_hline(yintercept=0, linetype="dotted", color="blue")+
                      geom_smooth(color="yellow")+
                      geom_point(aes(x = x, y = y), color="red")
                    res_vs_fit_plot <- res_vs_fit_plot + ggtitle("Residuals vs Fitted") +
                      xlab("Fitted values") + ylab("Residuals")
                    
                    
                    #The second plot
                    std_residuals <- abs(residuals / sd(residuals))
                    df2 <- data.frame(x=fitted_values, y=sqrt(std_residuals))
                    scaleLocationPlot <- ggplot(df2, aes(x = x, y = y)) +
                      geom_smooth(color="gray")+
                      geom_point(aes(x = x, y = y), color="black")
                    scaleLocationPlot <- res_vs_fit_plot + ggtitle("Scale−Location") +
                      xlab("Fitted values") + ylab(expression(sqrt("Standardized residuals")))
                    
                    grid.arrange(res_vs_fit_plot, scaleLocationPlot)
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

