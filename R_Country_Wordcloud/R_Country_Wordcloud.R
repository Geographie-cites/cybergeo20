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


path = "/Users/clementinecottineau/Documents/cybergeo20/R_Country_Wordcloud/"
# REG = readOGR(dsn=paste(path, "FRA_adm/FRA_adm1.shp", sep=""),
#               layer = "FRA_adm1", encoding="utf8")
b
#plot(world)

REG = world
 
countries = as.character(REG@data$CNTR_ID)

par(mfrow=c(4,4), mar = c(0,0,1,0))
for(j in 1:16){
  e = j * 16
  b = e-15
  for (i in b:e){
    print(REG@polygons[[i]]@ID)
    plot(REG[i,], col="grey")
    title(REG@data[i,"CNTR_ID"])
  }}

# articles = read.csv(paste0(path, "articles2.csv"), sep=",", dec=".")
# head(articles)
# articlesJuste = read.csv("/Users/clementinecottineau/Documents/cybergeo20/Data/raw/cybergeo.csv", sep=",", dec=".")
# summary(articlesJuste)
# articles = data.frame(articles, articlesJuste[match(articles$id,articlesJuste$id), ])
# articles = subset(articles, TypeDocument == "Article")
# dim(articles)

# countries$polygonID = rownames(countries)
# countries$polygonID = as.numeric(countries$polygonID) + 1
# write.csv(countries, paste0(path, "countrycodes.csv"))
# REG@bbox


# articles$country = as.character(articles$country)
# articles$textCountry = strsplit(articles$country, split = "_")
# articles$country2 = as.character(articles$country2)
# articles$textCountry2 = strsplit(articles$country2, split = "_")
# articles$firstauthor = as.character(articles$firstauthor)
# articles$textfirstauthor = strsplit(articles$firstauthor, split = "_")
# articles$author2 = as.character(articles$author2)
# articles$textauthor2 = strsplit(articles$author2, split = "_")
# 
# 
# t = 0
# for (i in 1:dim(articles)[1]){
#   for (c in countries){
#       t = t + 1
#   articles[i,paste0("S_",c)] = ifelse(c %in% articles[i,"textCountry"][[1]] == "TRUE" ||
#                                 c %in% articles[i,"textCountry2"][[1]] == "TRUE", 1, 0)
#   articles[i,paste0("A_",c)] = ifelse(c %in% articles[i,"textfirstauthor"][[1]] == "TRUE" ||
#                                        c %in% articles[i,"textauthor2"][[1]] == "TRUE", 1, 0)
#   articles[i,paste0("L_",c)] = ifelse(articles[i,paste0("S_",c)] == 1 && articles[i,paste0("A_",c)] == 1, 1, 0)
#   print(t)
#     }}
# 
# # 
  locals = paste0("L_", countries)
  authors = paste0("A_", countries)
  studies = paste0("S_", countries)
# 
# for (i in 1:dim(articles)[1]){
#   articles[i,"locals"] = sum(articles[i,locals])
#   articles[i,"LocalStudy"] = ifelse(articles[i,"locals"] >= 1, 1, 0)
# }
# summary(articles)
# sum(articles$LocalStudy)
# 
# colnames(articles)
# articlesToSave = articles
# articlesToSave$textCountry = NULL 
# articlesToSave$textCountry2 = NULL 
# articlesToSave$textfirstauthor = NULL 
# articlesToSave$textauthor2 = NULL 
# write.csv(articlesToSave, "/Users/clementinecottineau/Documents/cybergeo20/R_Country_Wordcloud/articles_Contingency.csv")

articles = read.csv("/Users/clementinecottineau/Documents/cybergeo20/R_Country_Wordcloud/articles_Contingency.csv", sep=",", dec=".")
# head(articles)
# 
#Year = 1998
#articles = articles[as.numeric(substr(articles$date.1)) == Year ,]

Studied = colSums(articles[,studies])
Authoring = colSums(articles[,authors])
SelfStudied = colSums(articles[,locals])
countryBase = data.frame(countries, Studied, Authoring, SelfStudied)
tail(countryBase, 10)

REG@data = data.frame(REG@data, countryBase[match(REG@data$CNTR_ID,countryBase$countries), ])

REG@data$StudiedAtAll = ifelse(REG@data$Studied >= 1, "#1C6F91", "lightgrey")
REG@data$AuthoringAtAll = ifelse(REG@data$Authoring >= 1, "orange", "lightgrey")
REG@data$SelfStudiedAtAll = ifelse(REG@data$SelfStudied >= 1,  "grey20", "lightgrey")

Year = "1996-2015"
par(mfrow=c(1,1), mar = c(0,0,1,0))
plot(REG, col=REG@data$StudiedAtAll, border="white")
title(paste0("Countries studied in Cybergeo articles | ", Year))
plot(REG, col=REG@data$AuthoringAtAll, border="white")
title(paste0("Countries authoring Cybergeo articles | ", Year))
plot(REG, col=REG@data$SelfStudiedAtAll, border="white")
title(paste0("Countries studied by locals in Cybergeo articles | ", Year))
# 
# plot(REG, col=alpha(REG@data$StudiedAtAll, alpha = 0.3), border="black")
# plot(REG, col=alpha(REG@data$AuthoringAtAll, alpha = 0.3), add=T, border=F)
# plot(REG, col=alpha(REG@data$SelfStudiedAtAll, alpha = 0.3), add=T, border=F)
# REGQ = SpatialPointsDataFrame(REGP,REG@data)
# plot(REGQ, cex = 0.1 * REGQ@data$Studied, col=alpha("black", alpha = 0.2), add=T, pch = 16)
# text(x,y,w, cex = f)

# head(articles)
#   i = 40
#   print(REG@polygons[[i]]@ID)
#   plot(REG[i,], col="grey")
#   title(REG@data[i,"CNTR_ID"])
# 
#   wt = table(subcountry)
#   LOC=NULL
# ID = c(1:5)
# WORDS = c("Motor", "Food", "Screen", "Cat", "Concept")
# FREQ = c(2, 9, 1, 2, 1)
# IDREG = c(24, 200, 40, 78, 7) #as.numeric(rownames(REG@data))
# LOC = data.frame(ID, WORDS, IDREG, FREQ)
# LOC$IDPOL = LOC$IDREG + 1
# 
# ids = unique(LOC$IDPOL)
# 
# x=data.frame()
# y=data.frame()
# z=data.frame()
# for (i in ids) {
#   x[i,"X"] = REG@polygons[[i]]@labpt[[1]] # Barycentre
#   y[i,"Y"] = REG@polygons[[i]]@labpt[[2]]
#   z[i,"Total"] = sum(LOC[LOC$IDPOL == i,]$FREQ)
# }
# x$IDPOL = rownames(x)
# y$IDPOL = rownames(y)
# z$IDPOL = rownames(z)
# 
# x= x[complete.cases(x),]
# y= y[complete.cases(y),]
# z= z[complete.cases(z),]
# LOC = merge(LOC, x, by.x = "IDPOL", by.y="IDPOL", all.y=F)
# LOC = merge(LOC, y, by.x = "IDPOL", by.y="IDPOL", all.y=F)
# LOC = merge(LOC, z, by.x = "IDPOL", by.y="IDPOL", all.y=F)
# LOC$RFREQ = LOC$FREQ / LOC$Total
# LOC$Random = runif(dim(LOC)[[1]], 0, 1)
# LOC$NewY = ifelse(LOC$RFREQ < 1, LOC$Random * LOC$Ymin + (1-LOC$Random) * LOC$Ymax,LOC$Y)
# LOC
# plot(REG, col="grey", border=F)
# x <- LOC[,"X"]
# y <- LOC[,"NewY"]
# w <- LOC[,"WORDS"]
# f = LOC[,"RFREQ"]
# text(x,y,w, cex = f  * 0.8)


justeProba = "/Users/clementinecottineau/Documents/cybergeo20/HyperNetwork/Models/Analysis/export/relevant_full_50000_eth50_nonfiltdico_kmin0_kmax1000_freqmin100_freqmax10000_eth150/docprobas.csv"
cybterms = read.csv("/Users/clementinecottineau/Documents/cybergeo20/Data/raw/terms.csv", sep=";", dec=".")
head(cybterms)
#cybtermat = table(cybterms$term, cybterms$id)
#tail(cybtermat)
justeTerms =  read.csv(justeProba, sep=",", dec=".") 
head(justeTerms)


lookup = data.frame(countries)
lookup$polyID = as.numeric(rownames(lookup)) - 1

dim(justeTerms)
dim(cybterms)
cybterms = justeTerms[justeTerms$CYBERGEOID != 0,]
cybterms$idterm = rownames(cybterms)
cybterms2 = data.frame(cybterms, articles[match(cybterms$CYBERGEOID,articles$id), ])
cybterms3 = data.frame(cybterms2, lookup[match(cybterms2$firstauthor,lookup$countries), ])
cybterms4 = cybterms3[complete.cases(cybterms3$id.1),]
head(cybterms4)
#summary(cybterms4, 20)
cybterms5 = cybterms4[with(cybterms4, order(-GIS)),]
head(cybterms5)






cybterms5


authors = paste0("A_", countries)
studies = paste0("S_", countries)

summary(cybterms4$countries)


clusterCountriesBasedOnTerms = function(themesFile, themes, numberOfGroups, countries_to_aggregate){
  themes_By_country_bf = data.frame("CountryID" = countries_to_aggregate)
themes_By_country_bf[,themes] = NA

        for (c in countries_to_aggregate){
 #         cybterms4$currentCountry = cybterms4[,c]
          articles_to_aggregate = themesFile[themesFile[,c] == 1,2:13]
          if (!is.null(articles_to_aggregate)){
          nArticles = dim(articles_to_aggregate)[1]
          themes_By_country_bf[themes_By_country_bf$CountryID == c, themes] = colSums(articles_to_aggregate) / nArticles
        }}

themes_By_country_bf = themes_By_country_bf[complete.cases(themes_By_country_bf),]
themes_By_country_bf$CountryID = substr(themes_By_country_bf$CountryID, 3,4)
themesScaled = scale(themes_By_country_bf[,2:13])
rownames(themesScaled) = themes_By_country_bf[,1]
d.themes = dist(themesScaled)
cah.themes = hclust(d.themes, method = "ward.D2")
groups_Country = cutree(cah.themes, k=numberOfGroups)
cahRes = data.frame("ID" = themes_By_country_bf[,1], "group" = groups_Country)
REG=world
REG@data = data.frame(REG@data, cahRes[match(REG@data$CNTR_ID,cahRes$ID), ])
plot(REG, col=REG@data$group)
}
stat.comp<-  function( x,y){
  K <-length(unique(y))
  n <-length(x)
  m <-mean(x)
  TSS <-sum((x-m)^2)
  nk<-table(y)
  mk<-tapply(x,y,mean)
  BSS <-sum(nk* (mk-m)^2)
  result<-c(mk,100.0*BSS/TSS)
  names(result) <-c( paste("G",1:K),"% epl.")
  return(result)
}

countriesDF = themes_By_country_bf[,2:13]
rownames(countriesDF) = themes_By_country_bf[,1]
leg = sapply(countriesDF, stat.comp,y=groups_Country)

clusterCountriesBasedOnTerms(themesFile = cybterms4, themes = colnames(justeTerms)[2:13], 
                             numberOfGroups = 5, countries_to_aggregate = authors)
  

par(mfrow=c(1,1), las=2, mar = c(5,8,4,2), bg="#2b3e50")
barplot(leg[1,], horiz=TRUE, cex.names=0.8, xlab= "Frequency of themes")

groups = 4







keywordsPO = cybterms5
#summary(keywordsPO)

LOC=NULL
ID = as.numeric(keywordsPO$idterm)
WORDS = keywordsPO$term
FREQ = as.numeric(keywordsPO$count)
CountryName = keywordsPO$countries
IDREG = as.numeric(keywordsPO$polyID)
LOC = data.frame(ID, WORDS, IDREG, FREQ, CountryName)
LOC$IDPOL = LOC$IDREG + 1
#LOC

LOC = subset(LOC, !is.na(IDPOL))
#LOC$RFREQ = LOC$FREQ / LOC$Total
isIDmax <- with(LOC, ave(FREQ, CountryName, FUN=function(x) seq_along(x)==which.max(x)))==1
LOC = LOC[isIDmax, ] # most common word by country

ids = unique(LOC$IDPOL)

x=data.frame()
y=data.frame()
z=data.frame()
for (i in ids) {
  x[i,"X"] = REG@polygons[[i]]@labpt[[1]] # Barycentre
  y[i,"Y"] = REG@polygons[[i]]@labpt[[2]]
  z[i,"Total"] = sum(LOC[LOC$IDPOL == i,]$FREQ)
}
x$IDPOL = rownames(x)
y$IDPOL = rownames(y)
z$IDPOL = rownames(z)

x= x[complete.cases(x),]
y= y[complete.cases(y),]
z= z[complete.cases(z),]
LOC = merge(LOC, x, by.x = "IDPOL", by.y="IDPOL", all.y=F)
LOC = merge(LOC, y, by.x = "IDPOL", by.y="IDPOL", all.y=F)
LOC = merge(LOC, z, by.x = "IDPOL", by.y="IDPOL", all.y=F)

LOC
x <- LOC[,"X"]
y <- LOC[,"Y"]
w <- LOC[,"WORDS"]
f = LOC[,"FREQ"]
par(mfrow=c(1,1), mar = c(0,0,1,0))
plot(REG, col="grey", border=F)
text(x,y,w, cex = 0.001 * f)
title(paste0("Frequency of the most frequent word | ", Year))



LOC=NULL
ID = as.numeric(keywordsPO$idterm)
WORDS = keywordsPO$term
FREQ = as.numeric(keywordsPO$count)
CountryName = keywordsPO$countries
IDREG = as.numeric(keywordsPO$polyID)
LOC = data.frame(ID, WORDS, IDREG, FREQ, CountryName)
LOC$IDPOL = LOC$IDREG + 1
#LOC

LOC = subset(LOC, !is.na(IDPOL))
isIDmin <- with(LOC, ave(FREQ, CountryName, FUN=function(x) seq_along(x)==which.min(x)))==1
LOC = LOC[isIDmin, ] # most common word by country
ids = unique(LOC$IDPOL)
x=data.frame()
y=data.frame()
z=data.frame()
for (i in ids) {
  x[i,"X"] = REG@polygons[[i]]@labpt[[1]] # Barycentre
  y[i,"Y"] = REG@polygons[[i]]@labpt[[2]]
  z[i,"Total"] = sum(LOC[LOC$IDPOL == i,]$FREQ)
}
x$IDPOL = rownames(x)
y$IDPOL = rownames(y)
z$IDPOL = rownames(z)
x= x[complete.cases(x),]
y= y[complete.cases(y),]
z= z[complete.cases(z),]
LOC = merge(LOC, x, by.x = "IDPOL", by.y="IDPOL", all.y=F)
LOC = merge(LOC, y, by.x = "IDPOL", by.y="IDPOL", all.y=F)
LOC = merge(LOC, z, by.x = "IDPOL", by.y="IDPOL", all.y=F)
LOC
x <- LOC[,"X"]
y <- LOC[,"Y"]
w <- LOC[,"WORDS"]
f = LOC[,"FREQ"]
par(mfrow=c(1,1), mar = c(0,0,1,0))
plot(REG, col="grey", border=F)
text(x,y,w, cex = 0.6)
title(paste0("The least frequent words | ", Year))





#nettoyage des dev
while( length(dev.list())!=0){
  dev.off()
}

article_maps = function(year, articles) {
  REG = world
  if (year == 2016) {
    articles_y = articles
                     } else {
    articles_y = articles[substr(articles$date.1, 1, 4) == as.character(year),]
  }
  Studied = colSums(articles_y[,studies])
  Authoring = colSums(articles_y[,authors])
  SelfStudied = colSums(articles_y[,locals])
  countryBase = data.frame(countries, Studied, Authoring, SelfStudied)
  REG@data = data.frame(REG@data, countryBase[match(REG@data$CNTR_ID,countryBase$countries), ])
  REG@data$StudiedAtAll = ifelse(REG@data$Studied >= 1, "#1C6F91", "lightgrey")
  REG@data$AuthoringAtAll = ifelse(REG@data$Authoring >= 1, "orange", "lightgrey")
  REG@data$SelfStudiedAtAll = ifelse(REG@data$SelfStudied >= 1,  "grey20", "lightgrey")  
  return(REG)
}
map_studied = function(shp, year) {
  if (year == 2016) year = "1996-2015"
    REG = shp
  par(mfrow=c(1,1), mar = c(0,0,1,0))
  plot(REG, col=REG@data$StudiedAtAll, border="white")
  title(paste0("Countries studied in Cybergeo articles | ", year))
 # plot(REG, col=REG@data$AuthoringAtAll, border="white")
#  title(paste0("Countries authoring Cybergeo articles | ", Year))
 # plot(REG, col=REG@data$SelfStudiedAtAll, border="white")
#  title(paste0("Countries studied by locals in Cybergeo articles | ", Year))
}
make_studied_gif <- function (years, articles) {
  library(animation)
  ani.options(interval=.2)
  saveGIF({
    layout(matrix(c(1, rep(2, 5)), 6, 1))   
    # Adjust the margins a little
    par(mar=c(0,0,1,0))   
    for (j in Years) {
      par(fg=1)
      shp = article_maps(j, articles) 
      map_studied(shp, j)
    }
  } 
  ,movie.name="studied.gif", ani.height=400, ani.width=800
  ) #SaveGIF 
}
Years = 1996:2016
make_studied_gif(Years, articles) 















###### networks author -> study
head(articles)

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

write.csv(country_Mat_df, paste0(path, "matrix_countryAuthoringToCountryStudied.csv"))

tabflow<-melt(country_Mat)
names(tabflow)=c("A","S","Articles")
head(tabflow, 4)


lookup = data.frame(countries)

x=c()
y=c()
for (i in 1:256) {
  x[[i]] = REG@polygons[[i]]@labpt[[1]] # Barycentre
  y[[i]] = REG@polygons[[i]]@labpt[[2]]
}
lookup$x = x
lookup$y = y
tabflow =  data.frame(tabflow, lookup[match(tabflow$A,lookup$countries), ])
tabflow =  data.frame(tabflow, lookup[match(tabflow$S,lookup$countries), ])
tabflow= tabflow[complete.cases(tabflow$A),]
tabflow= tabflow[complete.cases(tabflow$S),]
colnames(tabflow) = c("Authoring", "Studied", "N", "c1", "X_A", "Y_A", "c2",  "X_S", "Y_S")
tabflow = subset(tabflow, N > 0)
tabflow = subset(tabflow, Authoring != Studied)

par(mfrow=c(1,1), mar = c(0,0,1,0))
plot(REG, col="lightgrey", border=F)
Arrows(tabflow$X_A,tabflow$Y_A,tabflow$X_S,tabflow$Y_S, lwd = 0.5, col = "#df691a", code=2, arr.adj = 1 ,arr.type = "curved")
title("Who studies who? | 1996-2015", col.main = "white")
 
write.csv(tabflow, paste0(path, "df_countryAuthoringToCountryStudied.csv"))


  