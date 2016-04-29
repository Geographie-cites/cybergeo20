
# citation network

#setwd(paste0(Sys.getenv('CS_HOME'),'/Cybergeo/cybergeo20/HyperNetwork/Data/nw'))


# issue in csv file with semicolon delimiter -> use tabulation instead.
#edges <- read.csv('provnw2_edges.csv',header=FALSE,sep="\t",colClasses=c("character","character"))
#nodes <- read.csv('provnw2_nodes.csv',header=FALSE,sep="\t",colClasses=c("character","character","numeric","numeric"),blank.lines.skip=FALSE)
#colnames(nodes)=c("name","title","year","cyb")
#colnames(edges)=c("from","to")
#vedges = unique(c(edges[,1],edges[,2]))
#vertices = merge(x=data.frame(v=vedges),y=nodes[!duplicated(nodes[,1]),],by.x=1,by.y=1,all.x=FALSE,all.y=TRUE)
#colnames(vertices)=c("name","title","year","cyb")

edges <- read.csv(paste0(Sys.getenv('CS_HOME'),'/Cybergeo/cybergeo20/HyperNetwork/Data/nw/full_edges.csv'),header=FALSE,sep="\t",colClasses=c("character","character"))
#nodes <- read.table('full_nodes.csv',header=FALSE,sep="\t",colClasses=c("character","character","numeric","numeric"),fileEncoding="latin1",encoding="latin1")
colnames(edges)=c("from","to");#colnames(nodes)=c("name","cyb")

s=scan(file=paste0(Sys.getenv('CS_HOME'),'/Cybergeo/cybergeo20/HyperNetwork/Data/nw/full_nodes.csv'),what="character",sep='\n')
nodes = strsplit(s,'\t')
nodes<- data.frame(name=sapply(nodes,function(c){c[1]}),title=sapply(nodes,function(c){c[2]}),year=sapply(nodes,function(c){as.numeric(c[3])}),cyb=sapply(nodes,function(c){as.numeric(c[4])}))

# need to filter ? -> one failed ref
#length(setdiff(nodes$name,vedges))
#setdiff(vedges,nodes$name)

e = as.tbl(edges) %>% filter(from %in% nodes$name & to %in% nodes$name)
# filters 36 edges
#vindexes=list()
#for(i in 1:nrow(nodes)){vindexes[[as.character(nodes$name[i])]]=i-1}
#citationedges = data.frame(source=sapply(e$from,function(s){vindexes[[s]]}),target=sapply(e$to,function(s){vindexes[[s]]}),value=rep(1,nrow(e)))


#e = merge(x=edges,y=data.frame(vertices$name),by.x=1,by.y=1,all.x=FALSE,all.y=TRUE)
#e = merge(x=data.frame(e$to,e$from),y=data.frame(vertices$name),by.x=1,by.y=1,all.x=FALSE,all.y=TRUE)

# construct the graph
gcitation = graph.data.frame(as.data.frame(e),vertices=nodes)

# first analysis

cybnodes=V(gcitation)[V(gcitation)$cyb==1]
cybnames=cybnodes$name

#
save(gcitation,cybnames,file=paste0(Sys.getenv('CS_HOME'),'/Cybergeo/cybergeo20/HyperNetwork/Data/nw/citationNetwork.RData'))



