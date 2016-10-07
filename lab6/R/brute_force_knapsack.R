#' Brute-force search is guaranteed to give a correct answer in all situations for the knapsack problem.
#'
#' This function calculates all possible alternatives and return the maximum value.
#'
#'@param x, data.frame with with two variables weight(w) and value(v)
#'@param W, the size of knapsack
#'@examples
#'brute_force_knapsack(x = knapsack_objects[1:12,], W = 3500)
#'brute_force_knapsack(x = knapsack_objects[1:8,], W = 2000)
#'brute_force_knapsack(x = knapsack_objects[1:12,], W = 2000)
#'brute_force_knapsack(x = knapsack_objects[1:12,], W = 2000)
#'@return the maximum knapsack value and which elements
#'@author Yumeng Li, Mattias Karlsson, Ashraf Sarhan
#'@details This function calculates in the easiest way to enumerate all different combinations.
#'@seealso
#'\code{\link{brute_force_knapsack_forfiling}}
#'\code{\link{greedy_heuristic}}
#'\code{\link{knapsack_dynamic}}
#'@export


# generate data
# set.seed(42)
# n <- 2000
# knapsack_objects <-
#   data.frame(
#     w=sample(1:4000, size = n, replace = TRUE),
#     v=runif(n = n, 0, 10000)
#   )


brute_force_knapsack <- function(x, W){
  stopifnot(is.data.frame(x),
            is.numeric(W),
            W != Inf,
            W > 0,
            is.numeric(x$w),
            is.numeric(x$v),
            x$w >= 0,
            x$v >= 0)

  n = nrow(x)  # number of items
  maxv = 0     # max value
  ev = c()     # element vector

  for(i in 0:(2^n-1)){
    iv = intToBits(i)
    sv = 0     # sum value
    sw = 0     # sum weight
    tv = c()   # temporary value
    for(j in 1:n){
      if(iv[j] == 1){
        sv = sv + x[j, ]$v
        sw = sw + x[j, ]$w
        tv = c(tv, j)
      }
    }

    if((sv > maxv) && (sw <= W)){
      maxv = sv
      ev = tv
    }
  }

  list(value=round(maxv), elements=ev)
}

#brute_force_knapsack(x = knapsack_objects[1:12,], W = 3500)
#brute_force_knapsack(x = knapsack_objects[1:8,], W = 2000)
#brute_force_knapsack(x = knapsack_objects[1:12,], W = 2000)
#brute_force_knapsack(x = knapsack_objects[1:12,], W = 2000)

# system.time(brute_force_knapsack(knapsack_objects[1:16,], 2000))

# Yumeng's pc
# system.time(brute_force_knapsack(knapsack_objects[1:16,], 2000))
# user  system elapsed 
# 57.44  0.00 57.47 

