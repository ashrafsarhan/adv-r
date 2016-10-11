ridgereg_class <- setRefClass(
  "ridgereg_class",
  fields = list(
    formula = "character",
    fitted_values = "numeric",
    residuals = "numeric",
    df = "integer",
    sigma2 = "numeric",
    beta_estimate = "numeric",
    beta_variance = "numeric",
    t_ratio = "numeric",
    p_value = "numeric",
    lambda = "numeric"), 
  methods = list(
    print = function() {
      cat('Call:\n','linreg(formula = ',formula[2],'~',formula[3],')\n\n',sep = '')
      beta_estimate 
    },
    plot = function() {
      #Data
      df <- data.frame(x=fitted_values, y=residuals,id = 1:length(fitted_values))
      std_residuals <- as.numeric(sqrt(abs(scale(residuals))))
      df2 <- df
      df2$y <- std_residuals
      
      #The first plot
      res_vs_fit_plot <- ggplot(data = df,
                                aes(x = x, y = y)) +
        theme_bw() + 
        theme(plot.background = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              axis.text.y = element_text(angle = 90),
              axis.ticks.length = unit(0.3,"cm")) +
        ggtitle("Residuals vs Fitted") +
        xlab("Fitted values") +
        ylab("Residuals") +
        geom_point(size = 3, shape = 1) +
        geom_hline(yintercept = 0, linetype = "dotted", col = "blue") +
        geom_smooth(se = FALSE, aes(col = "red"), show.legend = FALSE, method = "lm") +
        
        
        #The second plot
        scaleLocationPlot <- ggplot(data = df2,
                                    aes(x = x, y = y)) +
        theme_bw() + 
        theme(plot.background = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              axis.text.y = element_text(angle = 90),
              axis.ticks.length = unit(0.3,"cm")) +
        ggtitle("Scale−Location") +
        xlab("Fitted values") +
        ylab(expression(sqrt("|Standardized residuals|"))) +
        geom_point(size = 3, shape = 1) +
        geom_smooth(se = FALSE, aes(col = "red"), show.legend = FALSE, method = "lm") +
        
        #Print plots
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

