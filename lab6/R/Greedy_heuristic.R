#'Greedy heuristic algorithm for the knapsack problem
#'
#'@param x is a data.frame containing variables 'w' and 'v', weights and values
#'@param W is the total size of the knapsack
#'@details This algorithm is approximative, it can be shown that it will return at least 50 % of the true maximum value. The value of this algorithm lies in the low computational time
#'@author Yumeng Li, Mattias Karlsson, Ashraf Sarhan
#'@examples
#'greedy_knapsack(knapsack_objects[1:800], 3500)
#'greedy_knapsack(knapsack_objects[1:1200], 2000)
#'@references
#'\url{https://en.wikipedia.org/wiki/Knapsack_problem#Greedy_approximation_algorithm}
#'@seealso
#'\code{\link{Greedy heuristic}}
#'@export

# #### Knapsack dataset ####
# # 2000 / 1000000
# set.seed(42)
# n <- 2000
# knapsack_objects <-
#   data.frame(
#     w=sample(1:4000, size = n, replace = TRUE),
#     v=runif(n = n, 0, 10000)
#     )

#### Assignment 1.1.4 Greedy heuristic ####

greedy_knapsack <- function(x, W){
  
  # W is the knapsack size
  # x is data.frame with weight and value
  # Only positive values
  
  stopifnot(is.data.frame(x),
            is.numeric(W),
            is.numeric(x$w),
            is.numeric(x$v),
            x$w >= 0,
            x$v >= 0)
  
  # Keep only observations lower then the knapsack size
  # Calculate the ratio: 'value/w' and reorder in descending order
  
  x$row <- row(x)
  x <- x[x$w < W,]
  x$ratio <- x$v/x$w
  x <- x[order(x$ratio, decreasing = TRUE),]

  # Return the maximum knapsack
  # Return as a list of two objects
  
  knapsack <- list(value = 0)
  knap_weight <- 0
  
  # Add observations from top until knapsack is maximized
  
  i <- 1
  
  while(knap_weight <= W){
    knap_weight <- knap_weight + x$w[i]
    
    knapsack$value <- knapsack$value + x$v[i]
    knapsack$elements[i] <- x$row[i]
    
    i <- i + 1
    if(i>nrow(x) | knap_weight+x$w[i]>W){
      break()
    }
  }
  
  knapsack$value <- round(knapsack$value,0)
  return(knapsack)
}
#system.time(greedy_knapsack(knapsack_objects, 2000))

# n = 1 000 000 ; size = 2000 ; Mattias computer
# user  system elapsed 
# 0.41    0.00    0.41 
