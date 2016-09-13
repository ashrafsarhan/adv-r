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
