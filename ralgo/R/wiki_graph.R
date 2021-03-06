#' This dataset is used for demonstrating the Dijkstra algorithm,
#' the three variables representing a graph.
#'
#' @format A data.frame with 18 rows and three variables:
#' \describe{
#'  \item{v1}{Contains edges for the graph}
#'  \item{v2}{Contains edges for the graph}
#'  \item{w}{Coresponds to the weight for each node}
#' }
#' @references \url{https://en.wikipedia.org/wiki/Dijkstra's_algorithm#Pseudocode}
"wiki_graph"

# The dataset that should be included in the package
wiki_graph <-
  data.frame(v1=c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6),
             v2=c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5),
             w=c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9))

# Check the graph data file and create .RData-file if it doesn't exist
save_graph_data <- function(file) {
  if(!file.exists(file)){
    save(wiki_graph,
         file = file)
  }
}
graph_data_file <- paste(getwd(), 'data', 'wiki_graph.RData', sep = '/')
save_graph_data(graph_data_file)

