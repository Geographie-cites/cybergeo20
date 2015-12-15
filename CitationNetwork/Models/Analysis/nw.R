
# citation network

library(igraph)

setwd(paste0(Sys.getenv('CS_HOME'),'/Cybergeo/cybergeo20/CitationNetwork/Data/nw'))


edges <- read.csv('provnw_links.csv',header=FALSE,sep=";",colClasses=c("character","character"))
nodes <- read.csv('provnw_nodes.csv',header=FALSE,sep=";",colClasses=c("character","character","character","character"))
colnames(nodes)=c("name","title","year","cyb")
colnames(edges)=c("from","to")
vedges = unique(c(edges[,1],edges[,2]))
vertices = merge(x=data.frame(v=vedges),y=nodes[!duplicated(nodes[,1]),],by.x=1,by.y=1,all.x=TRUE)

# construct the graph
g = graph.data.frame(edges,vertices=vertices)

# first analysis

# impact factor
V(g)$cyb[is.na(V(g)$cyb)]=0
d = degree(g,v=V(g)[V(g)$cyb==1],mode="in")
hist(d,breaks=100,main="Degree distribution")
r=log(1:length(d));s=log(sort(d+1,decreasing=TRUE))
coefs=lm(s~r,data.frame(r=r[r<4],s=s[r<4]))$coefficients
plot(r,s,pch='+',xlab='log(r)',ylab='log(s)',main=paste0("rank-citations, alpha=",coefs[2]));points(r[r<4],coefs[1]+coefs[2]*r[r<4],type='l',col="red")
