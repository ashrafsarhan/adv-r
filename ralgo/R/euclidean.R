#' Building a function to find the greatest common divisor of two numbers
#'
#' This function calculates by using the method of Euclidian algorithm
#'
#'@param a an integer of original numbers
#'@param b an integer of original numbers
#'@examples
#'euclidian(123612, 13892347912)
#'euclidian(123612, -13892347912)
#'euclidian(-100, 1000)
#'euclidian(100, 1000)
#'@return a positive integer the greatest common divisor of two integers
#'@author Yumeng Li, Mattias Karlsson, Ashraf Sarhan
#'@details This function runs a calculation by theory of the Euclidean algorithm
#'which calculates the greatest common divisor (GCD) of two natural numbers a and b.
#'The greatest common divisor g is the largest natural number that divides both a and b without leaving a remainder.
#'@references
#'\url{https://en.wikipedia.org/wiki/Euclidean_algorithm}
#'@seealso \code{\link{dijkstra}}
#'@export

euclidean <- function(a,b) {
  if (!(is.numeric(a) && is.numeric(b))) {
    stop('args must be numeric!')
  }else if(!(round(a) == a && round(b) == b)){
    stop('args must be intergers!')
  }else {
    tryCatch({
    x = abs(a)
    y = abs(b)
    while(abs(x-y) != 0) {
      z = abs(x-y)
      x = min(x,y)
      y = z
      if(abs(x) == abs(y)){
        break()
      }
    }
    return(abs(x))
    }, error = function(e) {
      message(e)
    })
  }
}
