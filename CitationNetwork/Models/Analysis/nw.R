
# citation network

library(igraph)

setwd(paste0(Sys.getenv('CS_HOME'),'/Cybergeo/cybergeo20/CitationNetwork/Data/nw'))

# issue in csv file with semicolon delimiter -> use tabulation instead.
edges <- read.csv('provnw2_edges.csv',header=FALSE,sep="\t",colClasses=c("character","character"))
nodes <- read.csv('provnw2_nodes.csv',header=FALSE,sep="\t",colClasses=c("character","character","numeric","numeric"),blank.lines.skip=FALSE)
colnames(nodes)=c("name","title","year","cyb")
colnames(edges)=c("from","to")
#vedges = unique(c(edges[,1],edges[,2]))
#vertices = merge(x=data.frame(v=vedges),y=nodes[!duplicated(nodes[,1]),],by.x=1,by.y=1,all.x=FALSE,all.y=TRUE)
#colnames(vertices)=c("name","title","year","cyb")

library(dplyr)
e = as.tbl(edges) %>% filter(from %in% nodes$name & to %in% nodes$name)

#e = merge(x=edges,y=data.frame(vertices$name),by.x=1,by.y=1,all.x=FALSE,all.y=TRUE)
#e = merge(x=data.frame(e$to,e$from),y=data.frame(vertices$name),by.x=1,by.y=1,all.x=FALSE,all.y=TRUE)

# construct the graph
g = graph.data.frame(as.data.frame(e),vertices=nodes)

# first analysis

# impact factor
#V(g)$cyb[is.na(V(g)$cyb)]=0
cybnodes=V(g)[V(g)$cyb==1]
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
d = degree(g)
# -> same as above


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

# cliques
#clic = cliques(g,min=3)
#clic_lengths = sapply(clic,length)
#hist(clic_lengths,breaks=20)

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
paths = get.shortest.paths(g,from=V(g)[1],mode="all")
vs=c();pathnum=1000;for(i in 2:pathnum){vs=append(vs,paths$vpath[[i]])}
s=induced.subgraph(g,V(g)[vs])
write.graph(s,"test.gml","gml")

centrality = centralization.betweenness(s,directed=FALSE)$res
plot(s,layout=layout.kamada.kawai,vertex.size=10*centrality/max(centrality),
     vertex.label=NA,#1:length(V(s)),vertex.label.cex=1,
     edge.arrow.size=0.1)


v1 = V(g)[V(g)$title=="Dynamique de la mangrove de l’estuaire du Saloum (Sénégal) entre 1972 et 2010"]
v2 = V(g)[V(g)$title=="Les conceptions initiales des élèves turcs de CM2 relatives aux séismes"]
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

