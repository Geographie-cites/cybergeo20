
library(Matrix)

setwd(paste0(Sys.getenv('CS_HOME'),"/Cybergeo/Models/cybergeo20/CybergeoNetworks"))

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





