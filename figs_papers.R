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
paletteCybergeo = c("#1C6F91", "#df691a", "#77c5ba", "orange", "#2db92d", "#e1ff2f", "#ff2313", "#bbab61")
sumNum = function(x){sum(x, na.rm=T)}
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

#plot(world)
world = readOGR(dsn="world_withZoom.shp",
                layer = "world_withZoom", encoding="utf8", verbose = F)
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
locals = paste0("L_", countries)
authors = paste0("A_", countries)
studies = paste0("S_", countries)
articles = read.csv("/Users/clementinecottineau/Documents/cybergeo20/R_Country_Wordcloud/articles_Contingency.csv", sep=",", dec=".")

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
par(mfrow=c(1,1), mar = c(0,0,1,0), bg='white')
plot(REG, col=REG@data$StudiedAtAll, border="white")
#title(paste0("Countries studied in Cybergeo articles | ", Year))
plot(REG, col=REG@data$AuthoringAtAll, border="white")
#title(paste0("Countries authoring Cybergeo articles | ", Year))
plot(REG, col=REG@data$SelfStudiedAtAll, border="white")
#title(paste0("Countries studied by locals in Cybergeo articles | ", Year))


############################################
# Keywords clustering on studied countries
############################################

hadriTerms = read.csv("kwprop.csv", sep=",", dec=".")
cybterms = hadriTerms
colNumbers = 2:11
cybterms2 = data.frame(cybterms, articles[match(cybterms$ID,articles$id), ])
cybterms3 = data.frame(cybterms2, lookup[match(cybterms2$firstauthor,lookup$countries), ])
cybterms4 = cybterms3[complete.cases(cybterms3$id.1),]
themeNames = colnames(hadriTerms)[colNumbers]

### parameters
themesFile = cybterms4
themes = colnames(hadriTerms)[colNumbers]
numberOfGroups = 4
countries_to_aggregate = studies

### clustering
themes_By_country_bf = data.frame("CountryID" = countries_to_aggregate)
themes_By_country_bf[,themes] = NA

for (c in countries_to_aggregate){
  articles_to_aggregate = themesFile[themesFile[,c] == 1,colNumbers]
  if (!is.null(articles_to_aggregate)){
    nArticles = dim(articles_to_aggregate)[1]
    themes_By_country_bf[themes_By_country_bf$CountryID == c, themes] = colSums(articles_to_aggregate) / nArticles
  }}

themes_By_country_bf = themes_By_country_bf[complete.cases(themes_By_country_bf),]
themes_By_country_bf$CountryID = substr(themes_By_country_bf$CountryID, 3,4)
themesScaled = scale(themes_By_country_bf[,colNumbers])
rownames(themesScaled) = themes_By_country_bf[,1]
d.themes = dist(themesScaled)
cah.themes = hclust(d.themes, method = "ward.D2")
groups_Country = cutree(cah.themes, k=numberOfGroups)
cahRes = data.frame("ID" = themes_By_country_bf[,1], "group" = groups_Country)
cahRes$groupColour = as.character(cut(cahRes$group, breaks = c(1:numberOfGroups, numberOfGroups+1),
                                      labels = paletteCybergeo[1:numberOfGroups],include.lowest = TRUE,right = FALSE))
REG=world
REG@data = data.frame(REG@data, cahRes[match(REG@data$CNTR_ID,cahRes$ID), ])

par(mfrow=c(1,1), las=2, mar = c(5,4,4,2), bg="white")
plot(cah.themes)
# Map
par(mfrow=c(1,1), mar = c(0,0,1,0), bg='white')
plot(REG, col=REG@data$groupColour)

summary(cah.themes)

# Legend
countriesDF = themes_By_country_bf[,colNumbers]
rownames(countriesDF) = themes_By_country_bf[,1]
leg = sapply(countriesDF, stat.comp,y=groups_Country)
if(numberOfGroups %% 2 == 0) window = c(numberOfGroups/2,2)
if(numberOfGroups %% 2 == 1) window = c(numberOfGroups/2 + 0.5,2)
themes_By_country_bf$group = cahRes$group
themes_By_country_bf$n = 1
nArticlesByGroup = aggregate(themes_By_country_bf[,"n"], by = list(themes_By_country_bf$group), FUN = sumNum)
colnames(nArticlesByGroup) = c("ID", "n")
nArticlesByGroup = nArticlesByGroup[order(nArticlesByGroup$ID),]

par(mfrow=window, las=2, mar = c(5,10,4,2), bg="white")
for(i in 1:numberOfGroups){
  barplot(leg[i,], col=paletteCybergeo[i], horiz=TRUE, cex.names=0.8, xlab= "Frequency of themes")
  if(nArticlesByGroup[i, "n"] == 1)  title(paste0(nArticlesByGroup[i, "n"], " article"))
  if(nArticlesByGroup[i, "n"] > 1)  title(paste0(nArticlesByGroup[i, "n"], " articles"))
}

str(cah.themes)

sortedHeight <- sort(cah.themes$height, decreasing = TRUE) 
relHeight <- sortedHeight / sum(sortedHeight) * 100 
cumHeight <- cumsum(relHeight)
cumHeight[numberOfGroups]
############################################
# Citation clustering on studied countries
############################################

justeProba = "/Users/clementinecottineau/Documents/cybergeo20/HyperNetwork/Models/Analysis/export/relevant_full_50000_eth50_nonfiltdico_kmin0_kmax1000_freqmin100_freqmax10000_eth150/docprobas.csv"
cybterms = read.csv("/Users/clementinecottineau/Documents/cybergeo20/Data/raw/cybergeo.csv", sep=";", dec=".")
justeTerms =  read.csv(justeProba, sep=",", dec=".") 
lookup = data.frame(countries)
lookup$polyID = as.numeric(rownames(lookup)) - 1
cybterms = justeTerms[justeTerms$CYBERGEOID != 0,]
cybterms$idterm = rownames(cybterms)
cybterms2 = data.frame(cybterms, articles[match(cybterms$CYBERGEOID,articles$id), ])
cybterms3 = data.frame(cybterms2, lookup[match(cybterms2$firstauthor,lookup$countries), ])
cybterms4 = cybterms3[complete.cases(cybterms3$id.1),]
cybterms5 = cybterms4[with(cybterms4, order(-GIS)),]
head(cybterms5)

### parameters
themesFile = cybterms4
themes = colnames(justeTerms)[2:13]
numberOfGroups = 4
countries_to_aggregate = studies

### clustering
  themes_By_country_bf = data.frame("CountryID" = countries_to_aggregate)
  themes_By_country_bf[,themes] = NA
  
  for (c in countries_to_aggregate){
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
  cahRes$groupColour = as.character(cut(cahRes$group, breaks = c(1:numberOfGroups, numberOfGroups+1),
                                        labels = paletteCybergeo[1:numberOfGroups],include.lowest = TRUE,right = FALSE))
  REG=world
  REG@data = data.frame(REG@data, cahRes[match(REG@data$CNTR_ID,cahRes$ID), ])
  
  par(mfrow=c(1,1), las=2, mar = c(5,4,4,2), bg="white")
  plot(cah.themes)
  
  # Map
  par(mfrow=c(1,1), mar = c(0,0,1,0), bg='white')
  plot(REG, col=REG@data$groupColour)
  
  # Legend
countriesDF = themes_By_country_bf[,2:13]
rownames(countriesDF) = themes_By_country_bf[,1]
leg = sapply(countriesDF, stat.comp,y=groups_Country)
if(numberOfGroups %% 2 == 0) window = c(numberOfGroups/2,2)
if(numberOfGroups %% 2 == 1) window = c(numberOfGroups/2 + 0.5,2)
themes_By_country_bf$group = cahRes$group
themes_By_country_bf$n = 1
nArticlesByGroup = aggregate(themes_By_country_bf[,"n"], by = list(themes_By_country_bf$group), FUN = sumNum)
colnames(nArticlesByGroup) = c("ID", "n")
nArticlesByGroup = nArticlesByGroup[order(nArticlesByGroup$ID),]

par(mfrow=window, las=2, mar = c(5,8,4,2), bg="white")
for(i in 1:numberOfGroups){
  barplot(leg[i,], col=paletteCybergeo[i], horiz=TRUE, cex.names=0.8, xlab= "Frequency of themes")
  if(nArticlesByGroup[i, "n"] == 1)  title(paste0(nArticlesByGroup[i, "n"], " article"))
  if(nArticlesByGroup[i, "n"] > 1)  title(paste0(nArticlesByGroup[i, "n"], " articles"))
}

sortedHeight <- sort(cah.themes$height, decreasing = TRUE) 
relHeight <- sortedHeight / sum(sortedHeight) * 100 
cumHeight <- cumsum(relHeight)
cumHeight[numberOfGroups]


############################################
# Full-text clustering on studied countries
############################################
load("/Users/clementinecottineau/Documents/cybergeo20/CybergeoNetworks/data/themesPO.Rdata")
files$name = NULL
files$path = NULL
pattern_list <- c("espace", "territoire", "environnement", "société", "réseau", "interaction", "aménagement", "urbanisme", "carte", "modèle", "système", "SIG", "fractale", "durabilité", "représentation", "migration", "quantitatif", "qualitatif", "post-moderne")
themeDescription = read.csv("20themes20words.csv", sep=",", dec=".")
nameThemes = c(as.character(themeDescription$NAME), "Other")
colnames(document.themes) = nameThemes
files[,3:22] = document.themes 
colnames(files)[3:22] = nameThemes


articlesWithThemes = data.frame(articles, files[match(articles$id,files$id), ])
colNumbers = 2:21
themeNames = nameThemes
cybterms = articlesWithThemes[,c("id",themeNames)]
cybtermsbis = cybterms[complete.cases(cybterms[,themeNames]),]
cybterms2 = data.frame(cybtermsbis, articles[match(cybtermsbis$id,articles$id), ])
cybterms3 = data.frame(cybterms2, lookup[match(cybterms2$firstauthor,lookup$countries), ])
cybterms4 = cybterms3[complete.cases(cybterms3$id.1),]


### parameters
themesFile = cybterms4
themes = nameThemes
numberOfGroups = 4
countries_to_aggregate = studies

### clustering
themes_By_country_bf = data.frame("CountryID" = countries_to_aggregate)
themes_By_country_bf[,themes] = NA

for (c in countries_to_aggregate){
  articles_to_aggregate = themesFile[themesFile[,c] == 1,colNumbers]
  if (!is.null(articles_to_aggregate)){
    nArticles = dim(articles_to_aggregate)[1]
    themes_By_country_bf[themes_By_country_bf$CountryID == c, themes] = colSums(articles_to_aggregate) / nArticles
  }}

themes_By_country_bf = themes_By_country_bf[complete.cases(themes_By_country_bf),]
themes_By_country_bf$CountryID = substr(themes_By_country_bf$CountryID, 3,4)
themesScaled = scale(themes_By_country_bf[,colNumbers])
rownames(themesScaled) = themes_By_country_bf[,1]
d.themes = dist(themesScaled)
cah.themes = hclust(d.themes, method = "ward.D2")
groups_Country = cutree(cah.themes, k=numberOfGroups)
cahRes = data.frame("ID" = themes_By_country_bf[,1], "group" = groups_Country)
cahRes$groupColour = as.character(cut(cahRes$group, breaks = c(1:numberOfGroups, numberOfGroups+1),
                                      labels = paletteCybergeo[1:numberOfGroups],include.lowest = TRUE,right = FALSE))
REG=world
REG@data = data.frame(REG@data, cahRes[match(REG@data$CNTR_ID,cahRes$ID), ])

par(mfrow=c(1,1), las=2, mar = c(5,4,4,2), bg="white")
plot(cah.themes)

# Map
par(mfrow=c(1,1), mar = c(0,0,1,0), bg='white')
plot(REG, col=REG@data$groupColour)

# Legend
countriesDF = themes_By_country_bf[,colNumbers]
rownames(countriesDF) = themes_By_country_bf[,1]
leg = sapply(countriesDF, stat.comp,y=groups_Country)
if(numberOfGroups %% 2 == 0) window = c(numberOfGroups/2,2)
if(numberOfGroups %% 2 == 1) window = c(numberOfGroups/2 + 0.5,2)
themes_By_country_bf$group = cahRes$group
themes_By_country_bf$n = 1
nArticlesByGroup = aggregate(themes_By_country_bf[,"n"], by = list(themes_By_country_bf$group), FUN = sumNum)
colnames(nArticlesByGroup) = c("ID", "n")
nArticlesByGroup = nArticlesByGroup[order(nArticlesByGroup$ID),]

par(mfrow=window, las=2, mar = c(5,8,4,2), bg="white")
for(i in 1:numberOfGroups){
  barplot(leg[i,], col=paletteCybergeo[i], horiz=TRUE, cex.names=0.8, xlab= "Frequency of themes")
  if(nArticlesByGroup[i, "n"] == 1)  title(paste0(nArticlesByGroup[i, "n"], " article"))
  if(nArticlesByGroup[i, "n"] > 1)  title(paste0(nArticlesByGroup[i, "n"], " articles"))
}

sortedHeight <- sort(cah.themes$height, decreasing = TRUE) 
relHeight <- sortedHeight / sum(sortedHeight) * 100 
cumHeight <- cumsum(relHeight)
cumHeight[numberOfGroups]
