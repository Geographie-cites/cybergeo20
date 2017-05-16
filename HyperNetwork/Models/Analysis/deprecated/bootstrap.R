
## convergence of bootstrap estimator for kw relevance

# read kw files from dir

setwd(paste0(Sys.getenv('CS_HOME'),'/Cybergeo/cybergeo20/HyperNetwork/Models/Semantic'))


# basic stats
stats <- read.csv('stats/ref_info.csv.csv',header=FALSE,sep=";")


kwdir = 'res/conv_tm'

# test setdiff
x = c("kw1","ngram az","kw2")
y = c("kw1","ngram az")
2*length(intersect(x,y))/(length(x)+length(y)) # set distance

# construct data frame by hand

# ref corpus is x
corpusDistance<- function(x,y){
  res=0;s=0;
  for(xx in names(x)){
    s=s+(x[[xx]]^2)
    if(xx %in% names(y)){
      res=res+((x[[xx]]-y[[xx]])^2)}else{res=res+(x[[xx]]^2)}
  }
  return(res/s)
  #return(1 - (length(intersect(x,y)))/length(y))
}

corpusUnion<-function(x,y){
  res=list()
  for(xx in names(x)){
    if(xx %in% res){
      res[[xx]]=res[[xx]]+x[[xx]]
    }else{
      res[[xx]]=x[[xx]]
    }
  }
  for(xx in names(y)){
    if(xx %in% res){
      res[[xx]]=res[[xx]]+y[[xx]]
    }else{
      res[[xx]]=y[[xx]]
    }
  }
  return(res)
}

getCorpus<-function(csize,kw,b){
  # kw_50_subCorpusSize100_run0.csv
  d <- read.table(paste0(kwdir,'/kw_',kw,'_subCorpusSize',csize,'_run',b,'.csv'),stringsAsFactors = FALSE,header = FALSE,sep=";")
  res=list()
  for(i in 1:nrow(d)){
    res[[d[i,1]]]=d[i,2]
  }
  res
}

a=getCorpus(500,100,sample.int(n=bootstrapSize,size=1)-1);b=getCorpus(500,100,sample.int(n=bootstrapSize,size=1)-1)
corpusDistance(a,b)
corpusDistance(getCorpus(500,100,sample.int(n=bootstrapSize,size=1)-1),getCorpus(500,100,sample.int(n=bootstrapSize,size=1)-1))
#show(intersect(getCorpus(fullsize,kw,sample.int(n=bootstrapSize,size=1)-1),getCorpus(fullsize,kw,sample.int(n=bootstrapSize,size=1)-1)))

fullsize = 2000
corpusSizes =  c(100,500,1000,2000)
kwlimits = c(50,100,200)

bootstrapSize = 25

mdata = matrix(0,length(corpusSizes)*length(kwlimits)*bootstrapSize^2,5)
colnames(mdata) = c("corpusSize","kwLimit","bsize","distance","rep")

i=1;p=1;k=1
for(csize in corpusSizes){
  for(kw in kwlimits){
    show(kw)
    for(s in 1:bootstrapSize){
      sampling = sample.int(n=bootstrapSize,replace=FALSE)-1
      allkw=list();p=k
      for(b in 1:bootstrapSize){
        allkw=corpusUnion(allkw,getCorpus(csize,kw,sampling[b]))
        mdata[i,] = c(csize,kw,b,corpusDistance(lapply(getCorpus(fullsize,kw,0),function(x){x/bootstrapSize}),lapply(allkw,function(x){x/b})),p)
        i=i+1
        p=p+1
      }
    }
    k=k+bootstrapSize
  }
}


library(dplyr)
library(ggplot2)

data <- as.tbl(data.frame(mdata)) %>% group_by(rep) %>% summarise(
  corpusSize=mean(corpusSize),kwLimit=mean(kwLimit),bsize=mean(bsize),meanDist = mean(distance),sdDist = sd(distance)  
)

#data=data.frame(mdata)

for(kw in kwlimits){
  kw=100
g = ggplot(data = data[which(data[,3]==kw),],aes(x = bsize,y=meanDist,colour=corpusSize))
g+geom_point()+geom_errorbar(aes(ymin=meanDist-sdDist,ymax = meanDist+sdDist))+ggtitle(paste0('kwLimit=',kw))

}





