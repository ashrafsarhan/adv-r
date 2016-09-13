#'Dijkstras algorithm
#'
#'@param graph is a data.frame contaning the edges in the graph, varibles v1, v2 and w 
#'@param init_node is a numeric value that exist in the graph data.frame
#'@details The algoritm takes a graph and an initial node and iteratively calculates the shortest distance between the initial node and every other node in the graph.
#'@author Yumeng Li, Mattias Karlsson, Ashraf Sarhan
#'@examples
#'dijkstra(wiki_graph, 1)
#'dijkstra(wiki_graph, 3)
#'@source \url{https://en.wikipedia.org/wiki/Dijkstra's_algorithm}
#'#'@seealso \code{\link{euclidean}}
#'@export
dijkstra <- function(graph, init_node){
  # initiate unvisited vertex set
  vertexQ = unique(graph$v1)
  
  # initiate result table
  rd = data.frame(matrix(nrow=length(vertexQ), ncol=2))
  colnames(rd) = c("dist", "prev")
  rd$dist = Inf
 
  # distance from source to source
  rd[init_node, ]$dist = 0
  
  while(length(vertexQ) > 0){
    for(u in vertexQ){
      if(rd[u, ]$dist != min(rd[vertexQ, ]$dist)){
        next
      }
      
      # remove u from unvisited set
      vertexQ = vertexQ[!vertexQ %in% u]
      
      # get neignbors of u that are still in Q
      ng = graph[graph$v1 == u, ]
      nv = ng$v2
      nv = nv[nv %in% vertexQ]
      
      # update path of neighbours
      for(v in nv){
        alt = rd[u, ]$dist + ng[ng$v2 == v, ]$w
        if(alt < rd[v, ]$dist){
          rd[v, ]$dist = alt
          rd[v, ]$prev = u
        }
      }
    }
    
  }
  return(rd$dist)

}
