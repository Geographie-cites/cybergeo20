
# analyses

library(Matrix)

probas = export_probas[,2:ncol(export_probas)]


##
# publication-level originality

originalities=apply(probas,MARGIN = 1,FUN = function(r){if(sum(r)==0){return(0)}else{return(1 - sum(r^2))}})

dat=data.frame(orig=originalities,cyb=iscyb)
sdat=as.tbl(dat)%>%group_by(cyb)%>%summarise(mean=mean(orig))
gp=ggplot(dat, aes(x=orig, fill=cyb))
gp+ geom_density(alpha=.3)+geom_vline(data=sdat, aes(xintercept=mean,  colour=cyb),linetype="dashed", size=1)


## null model

kwlength=mean(sapply(V(g)$name,nchar))
abstractlengths=sapply(keyword_dico,length)
names(abstractlengths)<-1:length(abstractlengths)
memberships<-sub$com$membership
weighteddegree=strength(sub$gg)
nullweights=list();for(k in unique(memberships)){nullweights[[k]]=weighteddegree[which(sub$com$membership==k)]}
drawWeights<-function(m){w=c();for(k in m){w=append(w,sample(nullweights[[k]],1))};return(w)}

bsize=20000
borig=c()
for(b in 1:bsize){
  if(b%%1000==0){show(b)}
  nkws = floor(sample(abstractlengths,1)/kwlength)+1
  m=sample(memberships,nkws,replace=TRUE);w=drawWeights(m)
  probas=list();for(k in 1:length(m)){if(!(as.character(m[k]) %in% names(probas))){probas[[as.character(m[k])]]=w[k]}else{probas[[as.character(m[k])]]=probas[[as.character(m[k])]]+w[k]}}
  borig=append(borig,1-sum((sapply(probas,function(x){x/sum(w)}))^2))
}

dat=data.frame(originality=c(originalities,borig),type=c(rep("pubs",length(originalities)),rep("null",length(borig))))
sdat=as.tbl(dat)%>%group_by(type)%>%summarise(mean=mean(originality))  
ggplot(dat, aes(x=originality, fill=type)) + geom_density(alpha=.3)+geom_vline(data=sdat, aes(xintercept=mean,  colour=type),linetype="dashed", size=1)



##
#  2nd order interdisciplinarities

# OUT

m=Diagonal(nrow(citadjacency),1/rowSums(citadjacency))
citnorm=m%*%citadjacency
citorigs=data.frame(rownames(probas))
for(j in 1:ncol(probas)){
  citorigs = cbind(citorigs,as.numeric(citnorm%*%Matrix(as.matrix(probas[,j]),sparse=TRUE)[,1]))
}
citorigs=citorigs[,2:ncol(citorigs)]

originalities=apply(citorigs,MARGIN = 1,FUN = function(r){if(sum(r)==0){return(0)}else{return(1 - sum(r^2))}})

dat=data.frame(orig=originalities,cyb=iscyb)
sdat=as.tbl(dat)%>%group_by(cyb)%>%summarise(mean=mean(orig))
gp=ggplot(dat, aes(x=orig, fill=cyb))
gp+ geom_density(alpha=.3)+geom_vline(data=sdat, aes(xintercept=mean,  colour=cyb),linetype="dashed", size=1)

#IN

m=Diagonal(ncol(citadjacency),1/(sapply(colSums(citadjacency),function(x){max(1,x)})))
citnorm=citadjacency%*%m
citorigs=data.frame(rownames(probas))
for(j in 1:ncol(probas)){
  citorigs = cbind(citorigs,as.numeric(Matrix(probas[,j],ncol=nrow(probas),sparse=TRUE)%*%citnorm))
}
citorigs=citorigs[,2:ncol(citorigs)]

originalities=apply(citorigs[colSums(citadjacency)>0,],MARGIN = 1,FUN = function(r){if(sum(r)==0){return(0)}else{return(1 - sum(r^2))}})

dat=data.frame(orig=originalities,cyb=iscyb[colSums(citadjacency)>0])
sdat=as.tbl(dat)%>%group_by(cyb)%>%summarise(mean=mean(orig))
gp=ggplot(dat, aes(x=orig, fill=cyb))
gp+ geom_density(alpha=.3)+geom_vline(data=sdat, aes(xintercept=mean,  colour=cyb),linetype="dashed", size=1)




