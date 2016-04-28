
# sensitivity to threshold parameters

setwd(paste0(Sys.getenv('CS_HOME'),'/Cybergeo/cybergeo20/HyperNetwork/Models/Analysis'))
library(dplyr)
library(igraph)
source('networkConstruction.R')

db='relevant_full_50000_eth50_nonfiltdico'
load(paste0('processed/',db,'.RData'))
g=res$g;
#keyword_dico=res$keyword_dico

g = filterGraph(g,'data/filter.csv')

# Q : work on giant component ?
# 
clust = clusters(g);cmax = which(clust$csize==max(clust$csize))
ggiant = induced.subgraph(g,which(clust$membership==cmax))

kmin = 0

modularities = c();
comnumber=c();
dmax=c();
eth=c();
csizes=c();
gsizes=c();
gdensity=c();
for(kmax in seq(from=800,to=4000,by=100)){
  for(edge_th in seq(from=50,to=250,by=10)){
    show(paste0('kmax : ',kmax,' e_th : ',edge_th))
    dd = V(ggiant)$docfreq#strength(ggiant)
    d = degree(ggiant)
    gg=induced_subgraph(ggiant,which(d>kmin&d<kmax))
    gg=subgraph.edges(gg,which(E(gg)$weight>edge_th))
    clust = clusters(gg);cmax = which(clust$csize==max(clust$csize))
    gg = induced.subgraph(gg,which(clust$membership==cmax))
    com = cluster_louvain(gg)
    # measures
    gsizes=append(gsizes,length(V(gg)));
    gdensity=append(gdensity,2*length(E(gg))/(length(V(gg))*(length(V(gg))-1)))
    csizes=append(csizes,length(clusters(gg)$csize))
    modularities = append(modularities,modularity(com))
    comnumber=append(comnumber,length(communities(com)))
    dmax=append(dmax,kmax);eth=append(eth,edge_th)
  }
}

d = data.frame(degree_max=dmax,edge_th=eth,vertices=gsizes,components=csizes,modularity=modularities,communities=comnumber,density=gdensity)

save(d,paste0('sensitivity/',db,'.RData'))


