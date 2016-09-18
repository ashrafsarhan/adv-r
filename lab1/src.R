#       ###    ########  ##     ##    ###    ##    ##  ######  ######## ########     ########     ##          ###    ########        ##   
#      ## ##   ##     ## ##     ##   ## ##   ###   ## ##    ## ##       ##     ##    ##     ##    ##         ## ##   ##     ##     ####   
#     ##   ##  ##     ## ##     ##  ##   ##  ####  ## ##       ##       ##     ##    ##     ##    ##        ##   ##  ##     ##       ##   
#    ##     ## ##     ## ##     ## ##     ## ## ## ## ##       ######   ##     ##    ########     ##       ##     ## ########        ##   
#    ######### ##     ##  ##   ##  ######### ##  #### ##       ##       ##     ##    ##   ##      ##       ######### ##     ##       ##   
#    ##     ## ##     ##   ## ##   ##     ## ##   ### ##    ## ##       ##     ##    ##    ##     ##       ##     ## ##     ##       ##   
#    ##     ## ########     ###    ##     ## ##    ##  ######  ######## ########     ##     ##    ######## ##     ## ########      ###### 

name <- 'Ashraf Sarhan'
liuid <- 'ashsa762'
library(markmyassignment)
lab_path <- "https://raw.githubusercontent.com/MansMeg/AdvRCourse/master/Labs/Tests/lab1.yml"
set_assignment(lab_path)

#1.1 Vectors 
#1.1.1
my_num_vector <- function(){
  a <- log10(11)
  b <- cos(pi/5)
  c <- exp(pi/3)
  d <- (1173 %% 7)/19
  result <- c(a, b, c, d)
  return(result)
}

#1.1.2
filter_my_vector <- function(x, leq) {
  x[x >= leq] = NA
  return(x)
}
      
#1.1.3
dot_prod <- function(a, b) {
  return(sum(a * b))
}

#1.1.4
approx_e <- function(N) {
  rounded_sum <- round(sum(1/factorial(0:N)), 5)
  return(rounded_sum)
}

#1.2 Matrices
#1.2.1
my_magic_matrix <- function() {
  mat <- matrix(c(4, 9, 2, 3, 5, 7, 8, 1, 6) , nrow = 3, byrow = TRUE)
  return(mat)
}

#1.2.2
calculate_elements <- function(A) {
  mat_size <- length(A)
  return(mat_size)
}

#1.2.3
row_to_zero <- function(A, i) {
  A[i,] = 0
  return(A)
}

#1.2.4
add_elements_to_matrix <- function(A, x, i, j) {
  A[i,j] <- A[i,j]+x
  return(A)
}

#1.3 Lists 
#1.3.1
my_magic_list <- function() {
  lst <- list(info = 'my own list', my_num_vector(), my_magic_matrix())
  return(lst)
}

#1.3.2
change_info <- function(x, text) {
  x$info = text
  return(x)
}

#1.3.3
add_note <- function(x, note) {
  x[['note']] = note
  return(x)
}

#1.3.4
sum_numeric_parts <- function(x) {
  accumlated_sum <- 0
  for (i in x) {
    if(is.numeric(i)) {
      accumlated_sum <- accumlated_sum + sum(i)
    }
  }
  return(accumlated_sum)
}

#1.4 data.frames
#1.4.1
my_data.frame <- function() {
  # Create data frame
  # A dataset is ~ table (list of vectors)
  id <- c(1,2,3)
  name <- c('John', 'Lisa', 'Azra')
  income <- c(7.30, 0.00, 15.21)
  rich <- c(FALSE, FALSE, TRUE)
  df <- data.frame(id, name, income, rich)
  return(df)
}

#1.4.2
sort_head <- function(df, var.name, n) {
  #Get descending ordered rows ids based on the var.name    
  ordr <- order(df[[var.name]] , decreasing = TRUE)
  #Get the actual rows 
  sorted <- df[ordr,]
  #Get the top n rows
  top_n <- head(sorted, n)
  return(top_n)
}

#1.4.3
add_median_variable <- function(df, j) {
  #Get the target col.
  target_col <- df[,j]
  #Calculate the median
  median <- median(target_col)
  #Compare target col. with median
  label_col <- c()
  for (v in target_col) {
    if (v > median) {
      label_col <- c(label_col, 'Greater') 
    }else if(v < median) {
      label_col <- c(label_col, 'Smaller') 
    }else {
      label_col <- c(label_col, 'Median')
    }
  }
  df[,'compared_to_median'] <- label_col
  return(df)
}

#1.4.4
#Although this function works fine and it gives the right answer, it's always gives a failure about "inherits from `table` not `numeric`" using mark_my_assignment
analyze_columns <- function(df , j) {
  result_lst <- list()
  for (idx in j) {
    dist <- df[,idx]
    #Mean
    m <- mean(dist)
    #Median
    med <- median(dist)
    #Standard deviation
    sd <- sd(dist)
    #Construct data matrix
    mat <- matrix(c("mean" = m, "median" = med, "sd" = sd), ncol = 3, byrow = TRUE,  
                  dimnames = list(c(""),c("mean", "median", "sd")))
    #Get the col. name
    col_name <- colnames(df)[idx] 
    #Beautify the mat using as.table
    result_lst[[col_name]] <- as.table(mat)
  }
  cor_mat <- cor(df[,j])
  result_lst[['correlation_matrix']] <- cor_mat 
  return(result_lst)
}
   

