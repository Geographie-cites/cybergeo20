
# semantic network construction from database

setwd(paste0(Sys.getenv('CS_HOME'),'/Cybergeo/cybergeo20/HyperNetwork/Models/Analysis'))

library(RSQLite)

db = dbConnect(SQLite(),"../Semantic/bootstrap/run_kw1000_csize2000_b20/bootstrap.sqlite3")

relevant = dbReadTable(db,'relevant')
dico = dbReadTable(db,'dico')

dim(relevant)
sort(relevant[,2])

# construct relevant dico : word -> index
rel = list()
for(i in 1:nrow(relevant)){rel[[relevant[i,1]]]=i}

cooccs = matrix(0,nrow(relevant),nrow(relevant))

for(i in 1:nrow(dico)){
  if(i%%1000==0){show(i)}
  kws = strsplit(dico[i,2],";")[[1]]
  if(length(kws)>1){
  for(k in 1:(length(kws)-1)){
    for(l in (k+1):(length(kws))){
      if(nchar(kws[k])>0&nchar(kws[l])>0){
        cooccs[rel[[kws[k]]],rel[[kws[l]]]]=cooccs[rel[[kws[k]]],rel[[kws[l]]]]+1
      }
    }
  }
  }
}


# stats on cooccurrences
length(which(cooccs>100))
co=c(cooccs)
hist(co[co<1000],breaks=100)
sco=sort(co,decreasing=TRUE)
x=log(1:length(sco[sco>0]));y=log(sco[sco>0])
m=summary(lm(y~x,data.frame(x=x[x<10],y=y[x<10])))
plot(x,y,pch='+');
points(x[x<10],x[x<10]*m$coefficients[2]+m$coefficients[1],col='red',type='l')








