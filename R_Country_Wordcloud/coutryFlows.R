library(wordcloud)
library(tm)
library(rgeos)
library(rgdal)
library(ggplot2)
library(RODBC)
library(reshape2)
library(maptools)
library(rCarto)
library(RColorBrewer)
library(shape)

Years = 1996:2015

path=paste0(Sys.getenv('CS_HOME'),'/Cybergeo/Models/cybergeo20/R_Country_Wordcloud');setwd(path)

world = readOGR(dsn=".",layer = "world_withZoom", encoding="utf8", verbose = F)
REG = world
countries = as.character(REG@data$CNTR_ID)

locals = paste0("L_", countries)
authors = paste0("A_", countries)
studies = paste0("S_", countries)

articles = read.csv("articles_Contingency.csv", sep=",", dec=".")

Studied = colSums(articles[,studies])
Authoring = colSums(articles[,authors])
SelfStudied = colSums(articles[,locals])
countryBase = data.frame(countries, Studied, Authoring, SelfStudied)

REG@data = data.frame(REG@data, countryBase[match(REG@data$CNTR_ID,countryBase$countries), ])

country_Mat = data.frame()
i = 0
j = 0
for (c_a in countries) {
  i =i+1
  for (c_s in countries) {
    j = j+1
    f = 0 
    if (1 %in% rownames(table(articles[,paste0("A_",c_a)], articles[,paste0("S_",c_s)]))){
      if (1 %in% colnames(table(articles[,paste0("A_",c_a)], articles[,paste0("S_",c_s)]))){
        f = table(articles[,paste0("A_",c_a)], articles[,paste0("S_",c_s)])[2,2]
      }}
    country_Mat[i,j] = f
  }
  j = 0 }
colnames(country_Mat) = countries
rownames(country_Mat) = countries
country_Mat_df = country_Mat
country_Mat = as.matrix(country_Mat)

rawtabflow<-melt(country_Mat)
tabflow<-melt(country_Mat)
names(tabflow)=c("A","S","Articles")
head(tabflow, 4)

# duplicate european countries in tabflows




x=c();y=c();codes=c();zoom=c()
for (i in 1:length(REG@polygons)) {
  x1 = REG@polygons[[i]]@labpt[[1]];y1 = REG@polygons[[i]]@labpt[[2]]
  x2=gCentroid(REG[i,])@coords[,1];y2=gCentroid(REG[i,])@coords[,2]
  #if(i==1){plot(x[[i]] , y[[i]])}else{points(x[[i]] , y[[i]])}
  dist = sqrt((x1-x2)^2+(y1-y2)^2)
  if(dist>4e6){
    #show(i)
    xz=c();yz=c();nvz=c();xs=c();ys=c();nvs=c()
    for(j in 1:length(REG@polygons[[i]]@Polygons)){
      xm=mean(REG@polygons[[i]]@Polygons[[j]]@coords[,1]);ym=mean(REG@polygons[[i]]@Polygons[[j]]@coords[,2])
      #show(nrow(REG@polygons[[i]]@Polygons[[j]]@coords))
      #show(xm)
      if(ym>5000000){
        xz=append(xz,xm);yz=append(yz,ym);nvz=append(nvz,nrow(REG@polygons[[i]]@Polygons[[j]]@coords))
      }else{
        xs=append(xs,xm);ys=append(ys,ym);nvs=append(nvs,nrow(REG@polygons[[i]]@Polygons[[j]]@coords))
      }
    }
    if(is.null(nvz)){show(as.character(REG$CNTR_ID[i]))}
    if(y1>5000000){
      x=append(x,x1);y=append(y,y1);codes=append(codes,as.character(REG$CNTR_ID[i]));zoom=append(zoom,T)
      x=append(x,xs[nvs==max(nvs)][1]);y=append(y,ys[nvs==max(nvs)][1]);codes=append(codes,as.character(REG$CNTR_ID[i]));zoom=append(zoom,F)  
    }else{
      x=append(x,x1);y=append(y,y1);codes=append(codes,as.character(REG$CNTR_ID[i]));zoom=append(zoom,F)
      x=append(x,xz[nvz==max(nvz)][1]);y=append(y,yz[nvz==max(nvz)][1]);codes=append(codes,as.character(REG$CNTR_ID[i]));zoom=append(zoom,T)
    }
  }else{
    x=append(x,x1);y=append(y,y1);
    codes=append(codes,as.character(REG$CNTR_ID[i]));zoom=append(zoom,F)
  }
  show(as.character(REG$CNTR_ID[i]))
  show(length(x));show(length(y))
}


lookup = data.frame(x,y,codes,zoom)



hist(dists,breaks=100)
plot(x,y);points(x1,y1,col='red')
plot(REG[dists>4e6,]);points(x,y,col='red');
abline(h=6000000,col='blue')
# -> we split the polygons at y=5000000 ; split if dists>4e6
REG[dists>4e6,]@polygons


#tabflow =  data.frame(tabflow, lookup[match(tabflow$A,lookup$countries), ])
#tabflow =  data.frame(tabflow, lookup[match(tabflow$S,lookup$countries), ])

tabflow= tabflow[complete.cases(tabflow$A),]
tabflow= tabflow[complete.cases(tabflow$S),]

coords=data.frame()
for(i in 1:nrow(tabflow)){
  if(tabflow$A[i]%in%lookup$codes[lookup$zoom]&tabflow$S[i]%in%lookup$codes[lookup$zoom]){
    coords=rbind(coords,unlist(c(lookup[as.character(lookup$codes)==as.character(tabflow$A[i])&lookup$zoom==T,c("x","y")],lookup[as.character(lookup$codes)==as.character(tabflow$S[i])&lookup$zoom==T,c("x","y")])))
  }else{
    coords=rbind(coords,unlist(c(lookup[as.character(lookup$codes)==as.character(tabflow$A[i])&lookup$zoom==F,c("x","y")],lookup[as.character(lookup$codes)==as.character(tabflow$S[i])&lookup$zoom==F,c("x","y")])))
  }
  if(nrow(coords)!=i){show(i)}
}

tabflow=cbind(tabflow,coords)

#colnames(tabflow) = c("Authoring", "Studied", "N", "c1", "X_A", "Y_A", "c2",  "X_S", "Y_S")
colnames(tabflow)<-c("Authoring", "Studied", "N", "X_A", "Y_A","X_S", "Y_S")
tabflow = subset(tabflow, N > 0)
tabflow = subset(tabflow, Authoring != Studied)

# find reciprocal flows
recflows = rawtabflow[which(country_Mat>0&t(country_Mat)>0),];
recflows$id = paste0(recflows[,1],recflows[,2])
tabflow$rec = paste0(tabflow$Authoring,tabflow$Studied)%in%recflows$id

#png(filename='who-who.png',width = 30,height = 25,units = 'cm',res = 600)
jpeg(filename='who-who.jpg',width = 30,height = 25,units = 'cm',res = 200,quality = 0.3)
par(mfrow=c(1,1), mar = c(0,0,1,0))
plot(REG, col="lightgrey", border="white")
Arrows(tabflow$X_A[tabflow$rec==F],tabflow$Y_A[tabflow$rec==F],tabflow$X_S[tabflow$rec==F],tabflow$Y_S[tabflow$rec==F], lwd = 0.5, col = "#df691a", code=2, arr.adj = 1 ,arr.type = "curved",arr.width = 0.1,arr.length = 0.2)
Arrows(tabflow$X_A[tabflow$rec==T],tabflow$Y_A[tabflow$rec==T],tabflow$X_S[tabflow$rec==T],tabflow$Y_S[tabflow$rec==T], lwd = 0.8, col = "blue",arr.length = 0)
dev.off()

#title("Who studies who? | 1996-2015", col.main = "white")














