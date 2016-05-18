
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
                connex=FALSE,export=FALSE,exportPrefix="graphs/all/all",
                filterFile="graphs/all/filter.csv"
                )
g=nw$g;keyword_dico=nw$keyword_dico


######################
######################
######################


db='relevant_full_5000'
load(paste0('processed/',db,'.RData'))
g=res$g;keyword_dico=res$keyword_dico

g = filterGraph(g,'graphs/all/filter.csv')

# complexity of cooccs
#sum(sapply(keyword_dico,function(x){length(x)^2}))

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
#scooccs = (cooccs+t(cooccs))/2
#d = as.dist(m=1-scooccs/max(scooccs))
#clust = hclust(d)
#plot(clust,labels=FALSE)


###############
## Interdisciplinarity of papers
# ID -> kws : dico
# 
# paper originality defined as 1 - sum p_i^2
#   -> pb : not all kws attributed to a community (filtered at two levels)
#     compensate with a mean field approach ? or compute originality only for papers with 
#     referenced kws ?

kmin = 0
kmax = 1000
edge_th =150
freqmin=100
freqmax=10000

sub = extractSubGraphCommunities(ggiant,kmin,kmax,freqmin,freqmax,edge_th);com=sub$com;gg=sub$gg
summarySubGraphCommunities(sub)



# -> for full_relevant_5000, (4200,90) is a good optimum

write.graph(gg,file = paste0('graphs/',db,'/optim1.gml'),format = "gml")
write.graph(gg,file = paste0('graphs/',db,'/filt_kmin',kmin,'_kmax',kmax,'_edge',edge_th,'_filtered','.gml'),format = "gml")
gg=read.graph(paste0('graphs/',db,'/filt_kmin',kmin,'_kmax',kmax,'_edge',edge_th,'_filtered','.gml'),format='gml')
save(gg,file=paste0('graphs/',db,'/filt_kmin',kmin,'_kmax',kmax,'_edge',edge_th,'_filtered','.RData'))
com = cluster_louvain(gg)

# issue in as_data_frame -> by hand
vdf = data.frame(name=V(gg)$name,community=com$membership)
erange=1:length(E(gg))#sample.int(n=length(E(gg)),size=300,replace=FALSE)
edf = data.frame(from=sapply(E(gg)[erange],function(e){head_of(gg,e)$name}),to=sapply(E(gg)[erange],function(e){tail_of(gg,e)$name}),weight=E(gg)$weight[erange])
vnames = unique(c(as.character(edf$from),as.character(edf$to)))
vdf = vdf[which(sapply(as.character(vdf$name),function(s){s%in%vnames})),]
vdf$size=rep(20,nrow(vdf))#sample.int(n=30,size=nrow(vdf),replace=TRUE)+5
vindexes = list()
for(i in 1:length(vnames)){vindexes[[vnames[i]]]=i-1}
edf = data.frame(source=sapply(as.character(edf$from),function(s){vindexes[[s]]}),target=sapply(as.character(edf$to),function(s){vindexes[[s]]}),value=10*edf$weight/max(edf$weight))
rownames(vdf)<-1:nrow(vdf)
save(vdf,edf,file='../../../Cybergeo20/data/semanticnw.RData')

# filter on edge weight

# communities
com = cluster_louvain(gg)
for(i in unique(com$membership)){show(i);show(V(gg)$name[which(com$membership==i)])}



# compute proba matrix
them_probas = computeThemProbas(gg,com,keyword_dico)
# or load from precomputed

# number of articles with originality
#length(which(rowSums(them_probas)>0))
originalities=apply(them_probas,MARGIN = 1,FUN = function(r){if(sum(r)==0){return(0)}else{return(1 - sum(r^2))}})
hist(originalities[originalities>0.6],breaks=100,main="",xlab="originalities")
#summary(originalities)

# cyb originalities ? -> needs cyb indexes (from citation network)
#   -> get cybnames from 'nw.R'
#  no inter for character -> numeric conversion
#as.numeric(cybnames)
#as.numeric(names(keyword_dico))
# dirty way -- DIIIIRTYYYY

load(paste0(Sys.getenv('CS_HOME'),'/Cybergeo/cybergeo20/HyperNetwork/Data/nw/citationNetwork.RData'))
cybergeo <- read.csv(paste0(Sys.getenv('CS_HOME'),'/Cybergeo/cybergeo20/Data/raw/cybergeo.csv'),colClasses = c('integer',rep('character',25)))
cyb = getCybindexes(them_probas,cybnames,cybergeo,keyword_dico)
cybindexes=cyb$cybindexes;iscyb=cyb$iscyb

#mean(originalities[cybindexes])
#hist(originalities[cybindexes],breaks=50)
#length(which(iscyb))
dat=data.frame(orig=originalities,cyb=iscyb)
sdat=as.tbl(dat)%>%group_by(cyb)%>%summarise(mean=mean(orig))
library(ggplot2)
g=ggplot(dat, aes(x=orig, fill=cyb))
g+ geom_density(alpha=.3)+geom_vline(data=sdat, aes(xintercept=mean,  colour=cyb),linetype="dashed", size=1)


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
#cybsecorigall=c()
for(i in 1:length(cybnodes)){
   show(i)
   neigh = neighbors(gcitation,v=cybnodes[i],mode="out")$name
   show(neigh)
   probas = rep(0,ncol(them_probas));count=0
   for(n in 1:length(neigh)){
     inds = which(names(keyword_dico)==neigh[n])
     if(length(inds)>0){probas=probas+them_probas[inds[1],];count=count+1}
   }
   if(count>0){probas=probas/count}
   if(sum(probas)>0){cybsecorigout=append(cybsecorigout,1-sum(probas^2))}
}


####
dat = data.frame(orig=c(cybsecorigin,cybsecorigout),type=c(rep("in",length(cybsecorigin)),rep("out",length(cybsecorigout)))
                 sdat=as.tbl(dat)%>%group_by(type)%>%summarise(mean=mean(orig))             
ggplot(dat, aes(x=orig, fill=type)) + geom_density(alpha=.3)+geom_vline(data=sdat, aes(xintercept=mean,  colour=type),linetype="dashed", size=1)




