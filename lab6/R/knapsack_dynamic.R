#' Brute-force search is guaranteed to give a correct answer in all situations for the knapsack problem.
#'
#' This function calculates all possible alternatives and return the maximum value.
#'
#'@param x, data.frame with with two variables weight(w) and value(v)
#'@param W, the size of knapsack
#'@examples
#'knapsack_dynamic(x = knapsack_objects[1:12,], W = 3500)
#'knapsack_dynamic(x = knapsack_objects[1:8,], W = 2000)
#'knapsack_dynamic(x = knapsack_objects[1:12,], W = 2000)
#'@references
#'\url{https://en.wikipedia.org/wiki/Knapsack_problem#0.2F1_knapsack_problem}
#'@return the maximum knapsack value and which elements
#'@author Yumeng Li, Mattias Karlsson, Ashraf Sarhan
#'@details This function calculates in the easiest way to enumerate all different combinations.
#'@seealso
#'\code{\link{brute_force_knapsack_forfiling}}
#'\code{\link{greedy_heuristic}}
#'\code{\link{brute_force_knapsack}}
#'@export

knapsack_dynamic <- function(x, W){
  stopifnot(is.data.frame(x),
            is.numeric(W),
            W != Inf,
            W > 0,
            is.numeric(x$w),
            is.numeric(x$v),
            x$w >= 0,
            x$v >= 0)

  n = nrow(x)

  #initialize m, use 1st col and row as "0"
  m = data.frame(matrix(nrow=n+1, ncol=W+1))
  m[1, ] = 0

  for(i in 1:n){
    ii = i + 1
    for(j in 0:W){
      jj = j + 1
      if(x[i, ]$w > j)
        m[ii,jj] = m[ii-1, jj]
      else
        m[ii,jj] = max(m[ii-1,jj], m[ii-1, jj-x[i, ]$w] + x[i, ]$v)
    }
  }


  # get elements
  v = c()
  for(i in 1:n){
    v[i] = 0
  }

  j = W + 1
  i = n + 1
  while(i > 1){
    if(m[i, j] > m[i-1, j]){
      v[i-1] = 1
      j = j - x[i-1, ]$w
    }

    i = i - 1
  }

  ev = c()
  for(i in 1:n){
    if(v[i] == 1) ev = c(ev, i)
  }

  list(value=round(m[n+1, W+1]), elements=ev)

}

# system.time(knapsack_dynamic(knapsack_objects[1:16,], 2000))

# system.time(knapsack_dynamic(knapsack_objects[1:16,], 2000))
# user  system elapsed
# 6.30 0.00 6.32
