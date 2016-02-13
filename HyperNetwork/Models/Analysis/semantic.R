
# semantic network construction from database

setwd(paste0(Sys.getenv('CS_HOME'),'/Cybergeo/cybergeo20/HyperNetwork/Models/Analysis'))

library(dplyr)
library(igraph)
library(RSQLite)

source('networkConstruction.R')

## sqlite data
db = dbConnect(SQLite(),"../Semantic/bootstrap/run_kw1000_csize5000_b20/bootstrap.sqlite3")
relevant = dbReadTable(db,'relevant')
dico = dbReadTable(db,'dico')




###########
# PARAMETERS

kwFile = "2000"
kwthreshold = 2000
linkthreshold =15
connex=FALSE


###########
# cybergeo
g=exportNetwork(importDicoCsv(kwFile),kwthreshold,linkthreshold,connex,TRUE,"graphs/cybergeo")

for(kwthreshold in c(100,200,500,1000,1500,2000)){
  for(linkthreshold in c(0,2,5,10,15,50,100)){
    for(connex in c(TRUE,FALSE)){
exportNetwork(importDicoCsv(kwFile),kwthreshold,linkthreshold,connex,TRUE,"graphs/cybergeo")
}}}

###########
# all

g=exportNetwork(list(relevant=relevant,dico=dico),
                kwthreshold=1500,linkthreshold=5,
                connex=FALSE,export=TRUE,exportPrefix="graphs/all/all",
                filterFile="graphs/all/filter.csv"
                )

# do some filtering
linkthreshold=200;exportPrefix="graphs/all/all"
kwthreshold=500
gg=induced_subgraph(g,1:kwthreshold)
gg=subgraph.edges(gg,which(E(gg)$weight>linkthreshold))
clust = clusters(gg);cmax = which(clust$csize==max(clust$csize))
gg = induced.subgraph(gg,which(clust$membership==cmax))
write.graph(gg,paste0(exportPrefix,"_kwth",kwthreshold,"_th",linkthreshold,"_connex",TRUE,".gml"),"gml")


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





