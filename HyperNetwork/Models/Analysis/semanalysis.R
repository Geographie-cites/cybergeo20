
# analyses

library(Matrix)
library(ggplot2)

source(paste0(Sys.getenv('CN_HOME'),'/Models/Utils/R/plots.R'))

figdir=paste0(Sys.getenv('CS_HOME'),'/Cybergeo/Models/cybergeo20/HyperNetwork/Results/Interdisc/');dir.create(figdir)

citcomnameslabs = c("ABMS","Accessibility\nLand-Use","Complex\nNetworks","Ecology","Economic\nGeography","GIS","Social\nGeography",
                    "Socio-ecology","Sociology","Spatial\nAnalysis","Time\nGeography",
                    "Urban\nNetworks","Urban\nSimulation","Urban\nStudies")


probas = export_probas[,2:ncol(export_probas)]
rownames(probas)<-names(keyword_dico)

##
# publication-level originality

originalities=apply(probas,MARGIN = 1,FUN = function(r){if(sum(r)==0){return(0)}else{return(1 - sum(r^2))}})

dat=data.frame(originality=originalities,cyb=iscyb)
sdat=as.tbl(dat)%>%group_by(cyb)%>%summarise(mean=mean(originality))
gp=ggplot(dat)
gp+geom_density(aes(x=originality, fill=cyb),alpha=.3)+geom_vline(data=sdat, aes(xintercept=mean,  colour=cyb),linetype="dashed", size=1)


## null model

kwlength=mean(sapply(V(g)$name,nchar))
abstractlengths=sapply(keyword_dico,length)
names(abstractlengths)<-1:length(abstractlengths)
memberships<-sub$com$membership
weighteddegree=strength(sub$gg)
nullweights=list();for(k in unique(memberships)){nullweights[[k]]=weighteddegree[which(sub$com$membership==k)]}
drawWeights<-function(m){w=c();for(k in m){w=append(w,sample(nullweights[[k]],1))};return(w)}

bsize=100000
borig=c()
for(b in 1:bsize){
  if(b%%1000==0){show(b)}
  nkws = floor(sample(abstractlengths,1)/kwlength)+1
  m=sample(memberships,nkws,replace=TRUE);w=drawWeights(m)
  dprobas=list();for(k in 1:length(m)){if(!(as.character(m[k]) %in% names(dprobas))){dprobas[[as.character(m[k])]]=w[k]}else{dprobas[[as.character(m[k])]]=dprobas[[as.character(m[k])]]+w[k]}}
  borig=append(borig,1-sum((sapply(dprobas,function(x){x/sum(w)}))^2))
}

dat=data.frame(originality=c(originalities,borig),type=c(rep("pubs",length(originalities)),rep("null",length(borig))))
sdat=as.tbl(dat)%>%group_by(type)%>%summarise(mean=mean(originality))  
ggplot(dat, aes(x=originality, fill=type)) + geom_density(alpha=.3)+geom_vline(data=sdat, aes(xintercept=mean,  colour=type),linetype="dashed", size=1)



## by cit. class
#citclass=sapply(rownames(probas),function(n){ifelse(n%in%V(citationcore)$name,V(citationcore)$citclass[n],'NA')})
vertices = V(citationcore)[intersect(rownames(probas),V(citationcore)$name)]
originalities=apply(probas[vertices$name,],MARGIN = 1,FUN = function(r){if(sum(r)==0){return(0)}else{return(1 - sum(r^2))}})

dat=data.frame(originality=originalities[vertices$citclass!='NA'],citclass=vertices$citclass[vertices$citclass!='NA'])
sdat=as.tbl(dat)%>%group_by(citclass)%>%summarise(mean=mean(originality))
gp=ggplot(dat,aes(x=originality, color=citclass))
gp+geom_density()+geom_vline(data=sdat, aes(xintercept=mean,  colour=citclass),linetype="dashed", size=1)+xlab("Semantic originality")+scale_color_discrete(name='Citation class')+stdtheme
ggsave(paste0(figdir,'originalities_citclass.png'),width=30,height=20,units='cm')



###########
## compositions

selectedsem = colnames(probas)
subprobas=probas[vertices$name,]
  
compos=c();cit=c();sem=c()
for(citclass in citcomnames){
  show(citclass);
  currentprobas=subprobas[vertices$citclass==citclass,selectedsem];show(dim(currentprobas))
  compos=append(compos,colSums(currentprobas))
  cit=append(cit,rep(citclass,length(selectedsem)));sem=append(sem,selectedsem)
}

dd=data.frame(citclass=cit,semclass=sem,compo=compos)

g=ggplot(dd,aes(x=citclass,y=compo,fill=semclass))
g+geom_col()+xlab('Citation class')+ylab('Composition')+scale_colour_discrete(name='Semantic')#+stdtheme
ggsave(file=paste0(figdir,'compos.png'),width=20,height=10,units = 'cm')

# same with proportions
for(citclass in unique(dd$citclass)){
  dd$compo[dd$citclass==citclass]=dd$compo[dd$citclass==citclass]/sum(dd$compo[dd$citclass==citclass])
}
g=ggplot(dd,aes(x=as.character(citclass),y=compo,fill=semclass))
g+geom_col()+xlab('Citation class')+ylab('Proportion')+stdtheme+scale_fill_discrete(name='Semantic class')+scale_x_discrete(labels=citcomnameslabs)
ggsave(file=paste0(figdir,'compo_proportion.png'),width=55,height=25,units = 'cm')


for(semclass in unique(dd$semclass)){
  dd$compo[dd$semclass==semclass]=(dd$compo[dd$semclass==semclass]-min(dd$compo[dd$semclass==semclass]))/(max(dd$compo[dd$semclass==semclass])-min(dd$compo[dd$semclass==semclass]))
}
for(citclass in unique(dd$citclass)){
  dd$compo[dd$citclass==citclass]=dd$compo[dd$citclass==citclass]/sum(dd$compo[dd$citclass==citclass])
}
g=ggplot(dd,aes(x=citclass,y=compo,fill=semclass))
g+geom_col()+xlab('Citation class')+ylab('Proportion')+scale_colour_discrete(name='Semantic')#+stdtheme
ggsave(file=paste0(figdir,'compo_proportion.png'),width=20,height=10,units = 'cm')



#####
## correlations


source('corrs.R')

citprobas = Matrix(0,nrow=vcount(citationcore),ncol=length(unique(V(citationcore)$citclass)))
colnames(citprobas)<-unique(V(citationcore)$citclass);rownames(citprobas)<-V(citationcore)$name
for(citclass in unique(V(citationcore)$citclass)){
  #show(length(which(V(citationcore)$citclass==citclass)))
  citprobas[V(citationcore)$name[which(V(citationcore)$citclass==citclass)],citclass]=1
}

rho = corrMat(probas,citprobas)
mean(abs(rho))
summary(as.numeric(rho[,2:ncol(rho)]))
summary(as.numeric(abs(rho)))
quantile(as.numeric(rho[,2:ncol(rho)]),(1:10)/10)

##
# citation diversity

citadjacency = get.adjacency(citationcore,sparse=TRUE)#[vertices$name,vertices$name]

citprobas = Matrix(0,nrow=vcount(citationcore),ncol=length(unique(V(citationcore)$citclass)))
colnames(citprobas)<-unique(V(citationcore)$citclass);rownames(citprobas)<-V(citationcore)$name
for(citclass in unique(V(citationcore)$citclass)){
  show(citclass)
  citprobas[,citclass]=colSums(citadjacency[V(citationcore)$name[V(citationcore)$citclass==citclass],])
}
m=Diagonal(nrow(citprobas),1/rowSums(citprobas))
citprobas=m%*%citprobas

subprobas = citprobas[rowSums(citprobas)>0,]
originalities=apply(subprobas,MARGIN = 1,FUN = function(r){if(sum(r)==0){return(0)}else{return(1 - sum(r^2))}})
citclass=V(citationcore)$citclass[rowSums(citprobas)>0]
dat=data.frame(originality=originalities[citclass!='NA'],citclass=citclass[citclass!='NA'])
sdat=as.tbl(dat)%>%group_by(citclass)%>%summarise(mean=mean(originality))
gp=ggplot(dat,aes(x=originality, color=citclass))
gp+geom_density()+geom_vline(data=sdat, aes(xintercept=mean,  colour=citclass),linetype="dashed", size=1)+xlab("Citation originality")+scale_color_discrete(name='Citation class')+stdtheme
ggsave(paste0(figdir,'citation_originalities_citclass.png'),width=30,height=20,units='cm')


##
#  2nd order interdisciplinarities

# OUT

m=Diagonal(nrow(citadjacency),1/rowSums(citadjacency))
citnorm=m%*%citadjacency
citorigsout=data.frame(rownames(probas))
for(j in 1:ncol(probas)){
  citorigsout = cbind(citorigsout,as.numeric(citnorm%*%Matrix(as.matrix(probas[,j]),sparse=TRUE)[,1]))
}
citorigsout=citorigsout[,2:ncol(citorigsout)]
indexes = rowSums(citadjacency)>0

#length(which(rowSums(citadjacency)>0|colSums(citadjacency)>0))
# == size of vertices with links.

outoriginalities=apply(citorigsout[indexes,],MARGIN = 1,FUN = function(r){if(sum(r)==0){return(0)}else{return(1 - sum(r^2))}})

dat=data.frame(orig=outoriginalities,cyb=iscyb[indexes])
sdat=as.tbl(dat)%>%group_by(cyb)%>%summarise(mean=mean(orig))
gp=ggplot(dat, aes(x=orig, fill=cyb))
gp+ geom_density(alpha=.3)+geom_vline(data=sdat, aes(xintercept=mean,  colour=cyb),linetype="dashed", size=1)+
  xlab("out originality")+ggtitle(paste0("N = ",length(which(indexes))))

####
#IN

m=Diagonal(ncol(citadjacency),1/(sapply(colSums(citadjacency),function(x){max(1,x)})))
citnorm=citadjacency%*%m
citorigs=data.frame(rownames(probas))
for(j in 1:ncol(probas)){
  citorigs = cbind(citorigs,as.numeric(Matrix(probas[,j],ncol=nrow(probas),sparse=TRUE)%*%citnorm))
}
citorigs=citorigs[,2:ncol(citorigs)]

indexes = colSums(citadjacency)>0
originalities=apply(citorigs[indexes,],MARGIN = 1,FUN = function(r){if(sum(r)==0){return(0)}else{return(1 - sum(r^2))}})

dat=data.frame(orig=originalities,cyb=iscyb[indexes])
sdat=as.tbl(dat)%>%group_by(cyb)%>%summarise(mean=mean(orig))
gp=ggplot(dat, aes(x=orig, fill=cyb))
gp+ geom_density(alpha=.3)+geom_vline(data=sdat, aes(xintercept=mean,  colour=cyb),linetype="dashed", size=1)+
  xlab("in originality")+ggtitle(paste0("N = ",length(which(indexes))))



# --> corresponds to citation regimes
#  : try nb cit = f(in orig)
#   also with out ?




###
#  IN + OUT

indexes = rowSums(citadjacency)>0|colSums(citadjacency)>0

outoriginalities=apply(citorigsout[indexes,],MARGIN = 1,FUN = function(r){if(sum(r)==0){return(0)}else{return(1 - sum(r^2))}})
originalities=apply(citorigs[indexes,],MARGIN = 1,FUN = function(r){if(sum(r)==0){return(0)}else{return(1 - sum(r^2))}})
originalities = (outoriginalities+originalities)/2

dat=data.frame(orig=originalities,cyb=iscyb[indexes])
sdat=as.tbl(dat)%>%group_by(cyb)%>%summarise(mean=mean(orig))
gp=ggplot(dat, aes(x=orig, fill=cyb))
gp+ geom_density(alpha=.3)+geom_vline(data=sdat, aes(xintercept=mean,  colour=cyb),linetype="dashed", size=1)+
  xlab("in/out originality")+ggtitle(paste0("N = ",length(which(indexes))))








