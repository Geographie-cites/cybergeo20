
###
# compute sensitivity of modularities of the citation network to attacks

library(igraph)
#library(dplyr)
library(Matrix)

setwd(paste0(Sys.getenv('CS_HOME'),'/Cybergeo/Models/cybergeo20/HyperNetwork/Models/Analysis'))

citnwfile=paste0(Sys.getenv('CS_HOME'),'/Cybergeo/Models/cybergeo20/HyperNetwork/Data/nw/citationNetwork.RData')
load(citnwfile)

set.seed(0)

core = induced_subgraph(gcitation,which(components(gcitation)$membership==1))
while(min(degree(core))<=1){core = induced_subgraph(core,which(degree(core)>1))}

A = as_adjacency_matrix(core,sparse = T)
M = A+Matrix::t(A)
undirected_rawcore = graph_from_adjacency_matrix(M,mode="undirected")

source('corrs.R')

set.seed(0)
com = cluster_louvain(undirected_rawcore)


# sensitivity function
modSensitivity <- function(A,core,membs,affected,target,type){
  if(target == "node"){
    if(type == "removal"){
      nodesid = sample.int(nrow(A),size = floor((1 - affected)*nrow(A)),replace = F)
      return(directedmodularity(membs[nodesid],A[nodesid,nodesid]))
    }
    if(type == "rewiring"){
      nodesid = sample.int(ncol(A),size=floor((1 - affected)*ncol(A)),replace = F)
      cols = 1:ncol(A);cols[nodesid]=sample(cols[nodesid],size = length(nodesid),replace=F)
      directedmodularity(membs,A[,cols])
    }
  }
  if(target == "edge"){
    if(type == "removal"){
      gdel=subgraph.edges(core,sample.int(length(E(core)),size=floor((1-affected)*length(E(core))),replace=F),delete.vertices = F)
      Adel = as_adjacency_matrix(gdel,sparse = T)
      return(directedmodularity(membs,Adel))
    }
    if(type == "rewiring"){
      linksid = sample.int(length(E(core)),size=floor((1-affected)*length(E(core))),replace=F)
      gdel=subgraph.edges(core,linksid,delete.vertices = F)
      gdel=add.edges(gdel,sample.int(nrow(A),size=2*floor(affected*length(E(core))),replace = T))
      return(directedmodularity(membs,as_adjacency_matrix(gdel,sparse = T)))
    }
  }
}

# test
#modSensitivity(A,core,com$membership,0.3,"node","removal")


# define parameters
nreps = 100
affected = seq(0.05,0.5,0.05)
target=c("node","edge")
type = c("removal","rewiring")
params=data.frame()
for(k in 1:nreps){for(f in affected){for(trgt in target){for(tp in type){
  params=rbind(params,c(k,f,trgt,tp),stringsAsFactors=F)
}}}}

library(doParallel)
cl <- makeCluster(50,outfile='log')
registerDoParallel(cl)

startTime = proc.time()[3]

res <- foreach(i=1:nrow(params)) %dopar% {
  source('corrs.R')
  show(paste0('row : ',i,'/',nrow(params)))
  show(paste0("affected = ",params[i,2]," ; target = ",params[i,3]," ; type = ",params[i,4]))
  mod = modSensitivity(A,core,com$membership,params[i,2],params[i,3],params[i,4])
  return(mod)
}

stopCluster(cl)

save(res,file=paste0('res/modsens.RData'))
  
show(paste0("Ellapsed Time : ",proc.time()[3]-startTime))

  

