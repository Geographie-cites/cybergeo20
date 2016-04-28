
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
cbalance=c();
for(freqmax in c(10000,20000)){
  for(freqmin in c(50,100,200)){
    for(kmax in seq(from=600,to=6200,by=200)){
      for(edge_th in seq(from=60,to=300,by=20)){
        show(paste0('kmax : ',kmax,' e_th : ',edge_th))
        dd = V(ggiant)$docfreq
        d = degree(ggiant)
        gg=induced_subgraph(ggiant,which(d>kmin&d<kmax&dd>freqmin&dd<freqmax))
        gg=subgraph.edges(gg,which(E(gg)$weight>edge_th))
        clust = clusters(gg);cmax = which(clust$csize==max(clust$csize))
        gg = induced.subgraph(gg,which(clust$membership==cmax))
        com = cluster_louvain(gg)
        # measures
        gsizes=append(gsizes,length(V(gg)));
        gdensity=append(gdensity,2*length(E(gg))/(length(V(gg))*(length(V(gg))-1)))
        csizes=append(csizes,length(clust$csize))
        modularities = append(modularities,modularity(com))
        comnumber=append(comnumber,length(communities(com)))
        cbalance=append(cbalance,sum((sizes(com)/length(V(gg)))^2))
        dmax=append(dmax,kmax);eth=append(eth,edge_th)
      }
    }
  }
}

d = data.frame(degree_max=dmax,edge_th=eth,vertices=gsizes,components=csizes,modularity=modularities,communities=comnumber,density=gdensity,comunitiesbalance=cbalance)

save(d,file=paste0('sensitivity/',db,'_ext.RData'))


