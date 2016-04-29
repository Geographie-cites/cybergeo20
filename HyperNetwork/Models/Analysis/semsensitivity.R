
# sensitivity to threshold parameters

setwd(paste0(Sys.getenv('CS_HOME'),'/Cybergeo/cybergeo20/HyperNetwork/Models/Analysis'))
library(dplyr)
library(igraph)
source('networkConstruction.R')

db='relevant_full_50000_eth50_nonfiltdico'
load(paste0('processed/',db,'.RData'))
g=res$g;
keyword_dico=res$keyword_dico
rm(res);gc()

g = filterGraph(g,'data/filter.csv')
g = filterGraph(g,'data/french.csv')

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
freqsmin=c();freqsmax=c()
for(freqmax in c(5000,10000,20000)){
  for(freqmin in c(50,75,100,125,200)){
    for(kmax in seq(from=300,to=1500,by=50)){
      for(edge_th in seq(from=140,to=300,by=20)){
        show(paste0('kmax : ',kmax,' e_th : ',edge_th,' ; freqmin : ',freqmin,' ; freqmax : ',freqmax))
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
	freqsmin=append(freqsmin,freqmin);freqsmax=append(freqsmax,freqmax)
      }
    }
  }
}

d = data.frame(degree_max=dmax,edge_th=eth,vertices=gsizes,components=csizes,modularity=modularities,communities=comnumber,density=gdensity,comunitiesbalance=cbalance,freqmin=freqsmin,freqmax=freqsmax)

save(d,file=paste0('sensitivity/',db,'_ext_local.RData'))

#############################

load('sensitivity/relevant_full_50000_eth50_nonfiltdico_ext_local.RData')
names(d)[ncol(d)-2]="balance"
g = ggplot(d) + scale_fill_gradient(low="yellow",high="red")#+ geom_raster(hjust = 0, vjust = 0) 
plots=list()
for(indic in c("modularity","communities","components","vertices","density","balance")){
  plots[[indic]] = g+geom_raster(aes_string("degree_max","edge_th",fill=indic))+facet_grid(freqmax~freqmin)
}
multiplot(plotlist = plots,cols=3)





