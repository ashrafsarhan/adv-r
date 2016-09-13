# Roxygen2 Document

# The varibles in the data.frame

# A reference to the wikipedia page
# https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm#Pseudocode


#### Assignment 1.2.3 ####

# The dataset that should be included in the package
wiki_graph <-
  data.frame(v1=c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6),
             v2=c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5),
             w=c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9))

setwd("C:/Github/adv-r/ralgo/data")
# If you want to re-create this .RData-file, change
# your workind-directory to your github/reporsitory

save(wiki_graph,
     file = "wiki_graph.RData")