
library(Matrix)
library(ggplot2)

setwd(paste0(Sys.getenv('CS_HOME'),"/Cybergeo/Models/cybergeo20/CybergeoNetworks"))

stdtheme= theme(axis.title = element_text(size = 22), 
                axis.text.x = element_text(size = 15),axis.text.y = element_text(size = 15),
                strip.text = element_text(size=15),
                legend.text=element_text(size=15), legend.title=element_text(size=15))

# source Overlap_Methods for proba matrices

citationProbas <- justeTerms[justeTerms$CYBERGEOID!=0&!is.na(justeTerms$CYBERGEOID),2:13]
rownames(citationProbas)<-justeTerms$CYBERGEOID[justeTerms$CYBERGEOID!=0&!is.na(justeTerms$CYBERGEOID)]
citationProbas <- citationProbas[rowSums(citationProbas)>0,]
citationProbas <- t(apply(citationProbas,1,function(r){r/sum(r)}))
citationProbas<-Matrix(citationProbas)

keywordProbas <- hadriTerms[,2:11]
rownames(keywordProbas)<-as.character(hadriTerms$ID)
keywordProbas<-Matrix(as.matrix(keywordProbas))

themeProbas <- cybtermsbis[,2:21]
rownames(themeProbas)<-as.character(cybtermsbis$id)
themeProbas<-Matrix(as.matrix(themeProbas))


# comput corr mat between two classifications
# conf int not needed ?
corrMat <- function(p1,p2){
  ids = intersect(rownames(p1),rownames(p2))
  d1 <- p1[ids,];d2<-p2[ids,]
  d1 = Matrix(apply(d1,2,function(col){(col-mean(col))/sd(col)}))
  d2 = Matrix(apply(d2,2,function(col){(col-mean(col))/sd(col)}))
  return(t(d1)%*%d2/nrow(d1))
}

#max(abs(corrMat(citationProbas,keywordProbas)))

bootstrapped<-function(p1,p2){
  minrho=c();maxrho=c();meanabsrho=c()
  minrhosup=c();maxrhosup=c();meanabsrhosup=c()
  for(b in 1:10000){
    if(b%%1000==0){show(b)}
    shuffled=p2;rownames(shuffled)<-sample(rownames(p2),size=nrow(p2),replace = F)
    cors = corrMat(p1,shuffled)
    minrho=append(minrho,min(cors));maxrho=append(maxrho,max(cors));meanabsrho=append(meanabsrho,mean(abs(cors)))
    shuffled=p1;rownames(shuffled)<-sample(rownames(p1),size=nrow(p1),replace = F)
    cors = corrMat(p2,shuffled)
    minrho=append(minrho,min(cors));maxrho=append(maxrho,max(cors));meanabsrho=append(meanabsrho,mean(abs(cors)))
    shuffled=p1;rows=sample.int(n=nrow(p1),size=0.5*nrow(p1),replace = FALSE)
    rownames(shuffled)[rows]<-sample(rownames(shuffled)[rows],size=length(rows),replace=F)
    cors = corrMat(p1,shuffled)
    minrhosup=append(minrhosup,min(cors));maxrhosup=append(maxrhosup,max(cors));meanabsrhosup=append(meanabsrhosup,mean(abs(cors)))
    shuffled=p2;rows=sample.int(n=nrow(p2),size=0.5*nrow(p2),replace = FALSE)
    rownames(shuffled)[rows]<-sample(rownames(shuffled)[rows],size=length(rows),replace=F)
    cors = corrMat(p2,shuffled)
    minrhosup=append(minrhosup,min(cors));maxrhosup=append(maxrhosup,max(cors));meanabsrhosup=append(meanabsrhosup,mean(abs(cors)))
   }
  return(data.frame(minrho,maxrho,meanabsrho,minrhosup,maxrhosup,meanabsrhosup))
}


bcitkws=bootstrapped(citationProbas,keywordProbas)
citkws = corrMat(citationProbas,keywordProbas)
min(citkws);max(citkws);mean(abs(citkws))
apply(bcitkws,2,mean)
apply(bcitkws,2,sd)

bcittheme=bootstrapped(citationProbas,themeProbas)
cittheme=corrMat(citationProbas,themeProbas)
min(cittheme);max(cittheme);mean(abs(cittheme))
apply(bcittheme,2,mean)
apply(bcittheme,2,sd)


bkwstheme=bootstrapped(keywordProbas,themeProbas)
kwstheme=corrMat(keywordProbas,themeProbas)
min(kwstheme);max(kwstheme);mean(abs(kwstheme))
apply(bkwstheme,2,mean)
apply(bkwstheme,2,sd)



##############
## multiclass modularity

overlappingmodularity <- function(probas,adjacency){#,linkfun=function(p1,p2){return(p1*p2)}){
  show(paste0('Computing overlapping modularity : dim(probas)=',dim(probas)[1],' ',dim(probas)[2],' ; dim(adjacency)=',dim(adjacency)[1],' ',dim(adjacency)[2]))
  m = sum(adjacency)
  n=nrow(probas)
  kout=rowSums(adjacency)
  kin=colSums(adjacency)
  res=0
  for(c in 1:ncol(probas)){
    if(sum(probas[,c])>0){
      if(c%%100==0){show(c/ncol(probas))}
      a1 = Diagonal(x=probas[,c])%*%adjacency%*%Diagonal(x=probas[,c])
      a2 = sum(kout*probas[,c])*sum(kin*probas[,c])*((sum(probas[,c])/n)^2)/m
      res = res + sum(a1) - a2
      rm(a1);gc() # loose time to call gc at each step ?
    }
  }
  return(res/m)
}


distMat<-function(probas){
  res = Matrix(0,nrow(probas),nrow(probas))
  for(k in 1:ncol(probas)){
    s = (Matrix(data = rep(probas[,k],nrow(probas)),nrow = nrow(probas),byrow = F)-Matrix(data = rep(probas[,k],nrow(probas)),nrow = nrow(probas),byrow = T))^2
    res = res + s
  }
  return(sqrt(res))
}

probasToAdjacency<-function(probas,theta){
  d = distMat(probas)
  return(Matrix(apply(d,1,function(r){r<theta})))
}

d=distMat(citationProbas)
summary(c(as.matrix(d)))
a = probasToAdjacency(citationProbas,0.2)
sum(probasToAdjacency(citationProbas,0.01))

#overlappingmodularity(citationProbas,probasToAdjacency(themeProbas,0.2))
# need to adjust dimensions

#' 
#' p1 classif within network induced by p2
interClassifModularity<-function(p1,p2,theta){
  ids = intersect(rownames(p1),rownames(p2))
  d1 <- p1[ids,];d2<-p2[ids,]
  return(overlappingmodularity(d1,probasToAdjacency(d2,theta)))
}

interClassifModularity(citationProbas,themeProbas,0.01)
interClassifModularity(citationProbas,citationProbas,0.01)


types=c();mods=c();thetas=c()
for(theta in seq(from=0.05,to=0.5,by=0.05)){
  selfcit = interClassifModularity(citationProbas,citationProbas,theta)
  selftheme = interClassifModularity(themeProbas,themeProbas,theta)
  selfkws = interClassifModularity(keywordProbas,keywordProbas,theta)
  mods=append(mods,interClassifModularity(citationProbas,themeProbas,theta)/selftheme);types=append(types,"citation>theme")
  mods=append(mods,interClassifModularity(citationProbas,keywordProbas,theta)/selfkws);types=append(types,"citation>keyword")
  mods=append(mods,interClassifModularity(themeProbas,citationProbas,theta)/selfcit);types=append(types,"theme>citation")
  mods=append(mods,interClassifModularity(themeProbas,keywordProbas,theta)/selfkws);types=append(types,"theme>keyword")
  mods=append(mods,interClassifModularity(keywordProbas,themeProbas,theta)/selftheme);types=append(types,"keyword>theme")
  mods=append(mods,interClassifModularity(keywordProbas,citationProbas,theta)/selfcit);types=append(types,"keyword>citation")
  thetas=append(thetas,rep(theta,6))
}

g=ggplot(data.frame(type=types,modularity=mods,theta=thetas),aes(x=theta,y=modularity,colour=type,group=type))
g+geom_point()+geom_line()+ylab("Relative modularity") +stdtheme
ggsave(file='../Results/ComparisonMethods/modularities.pdf',width=20,height=10,units = 'cm')






