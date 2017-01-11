
# citation network


library(dplyr)
library(igraph)


#'
#' @title Citation Network Construction
#' @name constructCitationNetwork
#' @description Construct igraph object from edges and nodes files
#' @param edgefile
#' @param nodefile
#' @param outputfile
#' 
constructCitationNetwork <- function(edgefile,nodefile,outputfile){
  
  edges <- read.csv(edgefile,header=FALSE,sep="\t",colClasses=c("character","character"))
  colnames(edges)=c("from","to");
  
  s=scan(file=nodefile,what="character",sep='\n')
  nodes = strsplit(s,'\t')
  nodes<- data.frame(name=sapply(nodes,function(c){c[1]}),title=sapply(nodes,function(c){c[2]}),year=sapply(nodes,function(c){as.numeric(c[3])}),cyb=sapply(nodes,function(c){as.numeric(c[4])}))
  
  e = as.tbl(edges) %>% filter(from %in% nodes$name & to %in% nodes$name)
  
  # construct the graph
  gcitation = graph.data.frame(as.data.frame(e),vertices=nodes)
  
  cybnodes=V(gcitation)[V(gcitation)$cyb==1]
  cybnames=cybnodes$name
  
  save(gcitation,cybnames,file=outputfile)
  
}

