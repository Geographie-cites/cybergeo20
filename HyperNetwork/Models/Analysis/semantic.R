
# semantic network construction from database

setwd(paste0(Sys.getenv('CS_HOME'),'/Cybergeo/cybergeo20/HyperNetwork/Models/Analysis'))

library(dplyr)
library(igraph)

#library(RSQLite)

#db = dbConnect(SQLite(),"../Semantic/bootstrap/run_kw1000_csize2000_b20/bootstrap.sqlite3")

#relevant = dbReadTable(db,'relevant')
#dico = dbReadTable(db,'dico')




###########
# PARAMETERS

kwFile = "2000"
kwthreshold = 2000
linkthreshold =15
connex=FALSE

g=exportNetwork(kwFile,kwthreshold,linkthreshold,connex,TRUE)

for(kwthreshold in c(100,200,500,1000,1500,2000)){
  for(linkthreshold in c(0,2,5,10,15,50,100)){
    for(connex in c(TRUE,FALSE)){
exportNetwork(kwFile,kwthreshold,linkthreshold,connex,TRUE)
}}}

#communities
com = cluster_fast_greedy(g)
com=cluster_louvain(g)
com=cluster_edge_betweenness(g)


## try some plotting ? hazardous with igraph.




############
## try some hierarchical clustering
scooccs = (cooccs+t(cooccs))/2
d = as.dist(m=1-scooccs/max(scooccs))
clust = hclust(d)
plot(clust,labels=FALSE)



#### Interdisciplinarity of papers

# ID -> kws





