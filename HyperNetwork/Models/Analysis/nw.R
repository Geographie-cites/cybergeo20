
# citation network

library(igraph)

setwd(paste0(Sys.getenv('CS_HOME'),'/Cybergeo/cybergeo20/HyperNetwork/Data/nw'))


# issue in csv file with semicolon delimiter -> use tabulation instead.
#edges <- read.csv('provnw2_edges.csv',header=FALSE,sep="\t",colClasses=c("character","character"))
#nodes <- read.csv('provnw2_nodes.csv',header=FALSE,sep="\t",colClasses=c("character","character","numeric","numeric"),blank.lines.skip=FALSE)
#colnames(nodes)=c("name","title","year","cyb")
#colnames(edges)=c("from","to")
#vedges = unique(c(edges[,1],edges[,2]))
#vertices = merge(x=data.frame(v=vedges),y=nodes[!duplicated(nodes[,1]),],by.x=1,by.y=1,all.x=FALSE,all.y=TRUE)
#colnames(vertices)=c("name","title","year","cyb")

edges <- read.csv('full_edges.csv',header=FALSE,sep="\t",colClasses=c("character","character"))
#nodes <- read.table('full_nodes.csv',header=FALSE,sep="\t",colClasses=c("character","character","numeric","numeric"),fileEncoding="latin1",encoding="latin1")
colnames(edges)=c("from","to");#colnames(nodes)=c("name","cyb")

s=scan(file='full_nodes.csv',what="character",sep='\n')
nodes = strsplit(s,'\t')
nodes<- data.frame(name=sapply(nodes,function(c){c[1]}),title=sapply(nodes,function(c){c[2]}),year=sapply(nodes,function(c){as.numeric(c[3])}),cyb=sapply(nodes,function(c){as.numeric(c[4])}))

# need to filter ? -> one failed ref
#length(setdiff(nodes$name,vedges))
#setdiff(vedges,nodes$name)


library(dplyr)
e = as.tbl(edges) %>% filter(from %in% nodes$name & to %in% nodes$name)
# filters 36 edges

#e = merge(x=edges,y=data.frame(vertices$name),by.x=1,by.y=1,all.x=FALSE,all.y=TRUE)
#e = merge(x=data.frame(e$to,e$from),y=data.frame(vertices$name),by.x=1,by.y=1,all.x=FALSE,all.y=TRUE)

# construct the graph
gcitation = graph.data.frame(as.data.frame(e),vertices=nodes)

# first analysis

cybnodes=V(gcitation)[V(gcitation)$cyb==1]
cybnames=cybnodes$name
citingcyb=c();citedbycyb=c()
for(i in 1:length(cybnodes)){
  if(i %% 10==0){show(i)}
  citingcyb = append(citingcyb,neighbors(gcitation,v=cybnodes[i],mode="in")$name)
  citedbycyb = append(citedbycyb,neighbors(gcitation,v=cybnodes[i],mode="out")$name)
}


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
#com <- cluster_louvain(g)


################
# Cliques
#
gc = induced.subgraph(g,v=which(degree(g,mode="all")>=3))
clic = cliques(gc,min=4)
clic_lengths = sapply(clic,length)
#hist(clic_lengths,breaks=20)

# write cliques to file to avoid recomputation
#clicmat = matrix("0",length(clic),8)
#for(r in 1:length(clic)){currentclic = V(gc)$name[clic[[r]]];clicmat[r,1:length(currentclic)]=currentclic}
#write.table(clicmat,file='clics_ids.csv',row.names = FALSE,col.names = FALSE,sep=";")
clicmat=read.table('clics_ids.csv',header=FALSE,sep=";",colClasses=rep("character",8))
clic=list();for(i in 1:nrow(clicmat)){clic[[i]]=unlist(clicmat[i,which(clicmat[i,]!="0")])}#reconstruct clics from clicmat
cyb = V(gc)$name[which(V(gc)$cyb==1)]
cybclics = which(sapply(clic,function(c){length(intersect(c,cyb))>1&length(c)>4}))
for(i in cybclics){
  sc = induced.subgraph(gc,V(gc)[clic[[i]]])
  lay=layout_as_tree(sc,circular=FALSE);
  lay[,2]=degree(sc,mode="in")
  pdf(paste0(Sys.getenv('CS_HOME'),"/Cybergeo/cybergeo20/HyperNetwork/Results/Networks/Citations/cliques/cybclic_2cyb_",i,".pdf"),width = 9,height=6)
  plot(sc,vertex.label=V(sc)$title,vertex.color=V(sc)$cyb+4,layout=lay)
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

