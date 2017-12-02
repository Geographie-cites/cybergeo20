
# citation network

library(igraph)
library(dplyr)
library(Matrix)

setwd(paste0(Sys.getenv('CS_HOME'),'/Cybergeo/Models/cybergeo20/HyperNetwork/Models/Analysis'))

citnwfile=paste0(Sys.getenv('CS_HOME'),'/Cybergeo/Models/cybergeo20/HyperNetwork/Data/nw/citationNetwork.RData')
load(citnwfile)

load(paste0(Sys.getenv('CS_HOME'),'/Cybergeo/Models/cybergeo20/HyperNetwork/Data/nw/citationNetworkStats.RData'))

# intersections ! -> USE IDS : does not work
#  sort and setdiff ?

###
# export core

set.seed(0)

raw = induced_subgraph(gcitation,which(components(gcitation)$membership==1))
V(raw)$reduced_title = sapply(V(raw)$title,function(s){paste0(substr(s,1,30),"...")})
V(raw)$reduced_title = ifelse(degree(raw)>1000,V(raw)$reduced_title,rep("",vcount(raw)))

# subsampling
#core = induced_subgraph(raw,sample.int(n = vcount(raw),size=floor(vcount(raw)/2),replace = F))
core = raw
#core = delete_vertex_attr(core,'title')

#core_full = induced_subgraph(core,which(degree(core,mode="in")>1));V(core_full)$title=rep("",vcount(core_full));
#write_graph(core_full,file=paste0(Sys.getenv('CS_HOME'),'/Cybergeo/Models/cybergeo20/HyperNetwork/Data/nw/core_full.gml'),format = 'gml')


while(min(degree(core))<=1){
  show(min(degree(core)))
  core = induced_subgraph(core,which(degree(core)>1))
}
#write_graph(raw,file=paste0(Sys.getenv('CS_HOME'),'/Cybergeo/Models/cybergeo20/HyperNetwork/Data/nw/raw.gml'),format = 'gml')

#write_graph(core,file=paste0(Sys.getenv('CS_HOME'),'/Cybergeo/Models/cybergeo20/HyperNetwork/Data/nw/core_subsampleseed0_vprop0.5_notitles.gml'),format = 'gml')

#V(core)$title=rep("",vcount(core));V(core)$reduced_title=rep("",vcount(core))
#write_graph(core,file=paste0(Sys.getenv('CS_HOME'),'/Cybergeo/Models/cybergeo20/HyperNetwork/Data/nw/core_notitles.gml'),format = 'gml')

#V(core)$reduced_title[V(core)$reduced_title!=""]
#write_graph(core,file=paste0(Sys.getenv('CS_HOME'),'/Cybergeo/Models/cybergeo20/HyperNetwork/Data/nw/core_titlesdeg1000.gml'),format = 'gml')

citationcore = core

A = as_adjacency_matrix(core,sparse = T)
M = A+Matrix::t(A)
undirected_rawcore = graph_from_adjacency_matrix(M,mode="undirected")

source('corrs.R')


set.seed(0)
com = cluster_louvain(undirected_rawcore)

directedmodularity(com$membership,A)

nreps = 100
mods = c()
for(i in 1:nreps){
  show(i)
  mods=append(mods,directedmodularity(com$membership,A[sample.int(nrow(A),nrow(A),replace = F),sample.int(ncol(A),ncol(A),replace = F)]))
}
show(paste0(mean(mods)," +- ",sd(mods)))




## content

d=degree(citationcore,mode='in')
for(c in unique(com$membership)){#  c(17,3,22)
  show(paste0("Community ",c, " ; corpus prop ",100*length(which(com$membership==c))/vcount(citationcore)))
  #show(paste0("Size ",length(which(com$membership==c))))
  currentd=d[com$membership==c];dth=sort(currentd,decreasing = T)[5]
  show(data.frame(titles=V(citationcore)$title[com$membership==c&d>dth],degree=d[com$membership==c&d>dth]))
  #show(V(rawcore)$title[com$membership==c])
}

#citcomnames=list(
#  '24'='Traffic','10'="Complex Networks",'16'="Ecology",'17'='Critical Geography','18'='Sociology',
#  '6'='GIS','3'='Spatial Analysis','14'='ABMS','9'='Hydrology','21'='Socio-ecology','13'='Urban Networks',
#  '23'='Urban Simulation','2'='Urban Studies','22'='Economic Geography','28'='Environment',
#  '1'='Computational Social Science','5'='Accessibility/Land-Use','26'='Urban Ecology','4'='Tourism',
#  '11'='Time Geography'
#)# too detailed
citcomnames=list(
 '10'="Complex Networks",'16'="Ecology",'17'='Social Geography','18'='Sociology',
 '6'='GIS','3'='Spatial Analysis','14'='ABMS','21'='Socio-ecology','13'='Urban Networks',
'23'='Urban Simulation','2'='Urban Studies','22'='Economic Geography','5'='Accessibility/Land-Use',
'11'='Time Geography'
  )


V(citationcore)$citmemb = com$membership
citclass =  sapply(as.character(V(citationcore)$citmemb),function(n){ifelse(n%in%names(citcomnames),unlist(citcomnames[n]),'NA')})
V(citationcore)$citclass = citclass

#save(citationcore,citcomnames,com,undirected_rawcore,file=paste0(Sys.getenv('CS_HOME'),'/Cybergeo/Models/cybergeo20/HyperNetwork/Data/nw/citationNetworkComs.RData'))
# 
load(paste0(Sys.getenv('CS_HOME'),'/Cybergeo/Models/cybergeo20/HyperNetwork/Data/nw/citationNetworkComs.RData'))



##############
##############



# impact factor
#V(g)$cyb[is.na(V(g)$cyb)]=0

d = degree(g,v=cybnodes,mode="in")
nodes[which(nodes$name==cybnodes[242]$name),]
hist(d,breaks=100,main="Degree distribution, mean (impact factor) = 1.4");abline(v=mean(d),col='red')
cutoffs=c(0,5,8,9.5)
r=log(1:length(d));s=log(sort(d+1,decreasing=TRUE))

plot(r[r<max(cutoffs)],s[r<max(cutoffs)],pch='+',xlab='log(r)',ylab='log(s)',main="rank-citations,alpha = (-0.01,-1.56,-0.75)");
for(i in 2:length(cutoffs)){
  coefs=lm(s~r,data.frame(r=r[r>=cutoffs[i-1]&r<cutoffs[i]],s=s[r>=cutoffs[i-1]&r<cutoffs[i]]))$coefficients
  show(coefs)
  points(r[r>cutoffs[i-1]&r<cutoffs[i]],coefs[1]+coefs[2]*r[r>cutoffs[i-1]&r<cutoffs[i]],type='l',col="red")
}
title(main="rank-citations,alpha = (-0.01,-1.56,-0.75)")

##
# degree distrib for all graph
d = degree(gcitation)
# -> same as above

mean(V(gcitation)$year[d<10&V(gcitation)$year>0])
mean(V(gcitation)$year[d>1000&V(gcitation)$year>0])
mean(V(gcitation)$year[d<1000&d>10&V(gcitation)$year>0])

# no strong cluster -> necessary because of time constraint

clust = clusters(g);cmax = which(clust$csize==max(clust$csize))
g = induced.subgraph(g,which(clust$membership==cmax))


##########
# communities and cliques ?
#com <- spinglass.community(g)
#com <- edge.betweenness.community(g)
#com <- leading.eigenvector.community(g,steps=3)
#com <- fastgreedy.community(g)
#com <- walktrap.community(g)
#com <- cluster_louvain(g)


################
# Cliques
#
gc = induced.subgraph(gcitation,v=which(degree(gcitation,mode="all")>=3))
#clic = cliques(gc,min=4)
#clic_lengths = sapply(clic,length)
#hist(clic_lengths,breaks=20)

# write cliques to file to avoid recomputation
#clicmat = matrix("0",length(clic),8)
#for(r in 1:length(clic)){currentclic = V(gc)$name[clic[[r]]];clicmat[r,1:length(currentclic)]=currentclic}
#write.table(clicmat,file='clics_ids.csv',row.names = FALSE,col.names = FALSE,sep=";")
clicmat=read.table('clics_ids.csv',header=FALSE,sep=";",colClasses=rep("character",8))
clic=list();for(i in 1:nrow(clicmat)){clic[[i]]=unlist(clicmat[i,which(clicmat[i,]!="0")])}#reconstruct clics from clicmat
cyb = V(gc)$name[which(V(gc)$cyb==1)]
cybclics = which(sapply(clic,function(c){length(intersect(c,cyb))>1&length(c)>4}))

palette=c("#df691a","#1C6F91")

for(i in cybclics){
  sc = induced.subgraph(gc,V(gc)[clic[[i]]])
  lay=layout_as_tree(sc,circular=FALSE);
  lay[,2]=degree(sc,mode="in");lay[,1]=lay[,1]
  pdf(paste0(Sys.getenv('CS_HOME'),"/Cybergeo/cybergeo20/HyperNetwork/Results/Networks/Citations/cliques/cybclic_2cyb_cybpalette_novname_",i,".pdf"),width = 9.5,height=6)
  plot(sc,vertex.label=NA,edge.color="white",#V(sc)$title,
       vertex.color=palette[V(sc)$cyb+1],layout=lay)
  dev.off()
}





## Graph too big ?


##########
# diameter, centralities
clust = clusters(g,mode="weak")
hist(clust$csize[2:32],xlab="",main="Weak clusters size without giant component")



diameter(g)


#############
# shortest paths
path.length.hist(g)
barplot(path.length.hist(g)$res,main="path length distribution",ylab="count")

# example from vertex 1
paths = get.shortest.paths(g,from=V(g)[22],mode="all")
vs=c();pathnum=1000;for(i in 2:pathnum){vs=append(vs,paths$vpath[[i]])}
s=induced.subgraph(g,V(g)[vs])
write.graph(s,"test.gml","gml")

centrality = centralization.betweenness(s,directed=FALSE)$res
plot(s,layout=layout.reingold.tilford#layout.kamada.kawai
     ,vertex.size=10*centrality/max(centrality),
     vertex.label=NA,#1:length(V(s)),vertex.label.cex=1,
     edge.arrow.size=0.1)


clics = cliques(s,min=4)
sc = induced.subgraph(s,V(s)[clics[[1]]])
plot(sc,vertex.label=V(sc)$title)

v1 = V(g)[V(g)$title=="Dynamique de la mangrove de l???estuaire du Saloum (S??n??gal) entre 1972 et 2010"]
v2 = V(g)[V(g)$title=="Les conceptions initiales des ??l??ves turcs de CM2 relatives aux s??ismes"]
p = get.shortest.paths(g,from=v1,to=v2,mode="all")$vpath[[1]]
V(g)$title[p]

############
# centrality
bcentrality = centralization.betweenness(g)$res
cybcentr = bcentrality[V(g)$cyb==1]
hist(bcentrality[bcentrality>0])
bc = cybcentr[cybcentr>0]#bcentrality[bcentrality>0]
plot(log(1:length(bc)),log(sort(bc,decreasing=TRUE)),pch="+",xlab="log(rank)",ylab="log(bc)",main="rank-betweeness-centrality (cybergeo)")

V(g)[which(bcentrality==max(bcentrality))]
#"The constitution of society: Outline of the theory of structuration"


# plot graph -> difficult, too many edges ?

# -> export gml to visualize with gephi ?
write.graph(g,'citation.gml',format="gml")

#plot(g, layout=layout.fruchterman.reingold,   
#     edge.arrow.mode=1,
#     vertex.label=NA,
#     vertex.size=1#+5*bcentrality/max(bcentrality),
     #vertex.label.cex=0.2+(degree(g)/200),
     #vertex.label.cex = 1.5*bcentrality/max(bcentrality) + 0.1
#)

