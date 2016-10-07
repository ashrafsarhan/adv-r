#' This dataset is used for demonstrating the knapsack problem,
#'
#' @format A data.frame with 2000 rows and two variables:
#' \describe{
#'  \item{w}{Contains weights}
#'  \item{v}{Contains values}
#' }
#' @export
"knapsack_objects"

# The dataset that should be included in the package
set.seed(42)
n <- 2000
knapsack_objects <-
  data.frame(
    w=sample(1:4000, size = n, replace = TRUE),
    v=runif(n = n, 0, 10000)
    )