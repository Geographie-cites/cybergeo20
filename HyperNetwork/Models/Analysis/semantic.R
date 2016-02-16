
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

nw=exportNetwork(list(relevant=relevant,dico=dico),
                kwthreshold=1500,linkthreshold=5,
                connex=FALSE,export=TRUE,exportPrefix="graphs/all/all",
                filterFile="graphs/all/filter.csv"
                )
g=nw$g;keyword_dico=nw$keyword_dico

clust = clusters(g);cmax = which(clust$csize==max(clust$csize))
ggiant = induced.subgraph(g,which(clust$membership==cmax))


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


###############
## Interdisciplinarity of papers
# ID -> kws : dico
# 
# paper originality defined as 1 - sum p_i^2
#   -> pb : not all kws attributed to a community (filtered at two levels)
#     compensate with a mean field approach ? or compute originality only for papers with 
#     referenced kws ?

kmin = 5
kmax = 900
edge_th = 50

# filter on degree (work already on giant component ?)
d=degree(ggiant)
gg=induced_subgraph(ggiant,which(d>kmin&d<kmax))
gg=subgraph.edges(gg,which(E(gg)$weight>edge_th))

# filter on edge weight

# communities
com = cluster_louvain(gg)
# construct kw -> thematic dico
thematics = list()
for(i in 1:length(V(gg))){
  thematics[[V(g)$name[i]]]=com$membership[i]
}

# compute proba matrix
them_probas = matrix(0,length(names(keyword_dico)),length(unique(com$membership)))
for(i in 1:length(names(keyword_dico))){
  if(i%%100==0){show(i)}
  kwcount=0
  for(kw in keyword_dico[[names(keyword_dico)[i]]]){if(kw %in% names(thematics)){
    j=thematics[[kw]]
    them_probas[i,j]=them_probas[i,j]+1;kwcount=kwcount+1
  }}
  if(kwcount>0){them_probas[i,]=them_probas[i,]/kwcount}
}

# number of articles with originality
#length(which(rowSums(them_probas)>0))
originalities=apply(them_probas,MARGIN = 1,FUN = function(r){if(sum(r)==0){return(0)}else{return(1 - sum(r^2))}})
#hist(originalities,breaks=100)
#summary(originalities)

# cyb originalities ? -> needs cyb indexes (from citation network)
#   -> get cybnames from 'nw.R'
#  no inter for character -> numeric conversion
#as.numeric(cybnames)
#as.numeric(names(keyword_dico))
# dirty way -- DIIIIRTYYYY
cybindexes = c()
for(cyb in cybnames){indexes = which(names(keyword_dico)==cyb);if(length(indexes)>0){cybindexes=append(cybindexes,indexes[1])}}

#mean(originalities[cybindexes])
#hist(originalities[cybindexes],breaks=50)

# this was PAPER-level originality -> let try JOURNAL-level originality
cybprobas = them_probas[cybindexes,]
cybcumprobas = colSums(cybprobas)/length(which(rowSums(cybprobas)>0))
1 - sum(cybcumprobas^2)
# compare to a null model ?
Nb = 10000
nulljournalorigs=c()
for(i in 1:Nb){
  probas = them_probas[sample.int(nrow(them_probas), size = length(cybindexes), replace = FALSE),]
  cumprobas = colSums(probas)/length(which(rowSums(probas)>0))
  nulljournalorigs=append(nulljournalorigs,1 - sum(cumprobas^2))
}

hist(nulljournalorigs,breaks=1000)

# logical as random sampling

#  WOULD NEED JOURNAL FOR REFS TO BE ABLE TO COMPARE -> does mendeley provide some kind of journal id ?
#   -> check if 'source' result is that.


#######
## second order originality ?

cybsecorigin=c()
cybsecorigout=c()
cybsecorigall=c()
for(i in cybindexes){
  neighbors(gcitation,v=cybnodes[i],mode="in")
}








