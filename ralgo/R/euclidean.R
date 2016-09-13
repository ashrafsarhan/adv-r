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
#'@source \url{https://en.wikipedia.org/wiki/Euclidean_algorithm}
#'@seealso \code{\link{dijkstra}}
#'@export

euclidean <- function(a,b) {
  x = abs(a)
  y = abs(b)
  if((round(x) == x) && (round(y) == y)){
    while(abs(x-y) != 0) {
      z = abs(x-y)
      x = min(x,y)
      y = z
      if(abs(x) == abs(y)){
        break()
      }
    }
  }else{
    print("a, b must be intergers!")
  }
  return(abs(x))
}



