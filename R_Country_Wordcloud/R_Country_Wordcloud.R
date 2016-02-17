library(wordcloud)
library(tm)
library(rgeos)
library(rgdal)
library(ggplot2)
library(RODBC)

path = "/Users/clementinecottineau/Documents/cybergeo20/R_Country_Wordcloud/"
REG = readOGR(dsn=paste(path, "FRA_adm/FRA_adm1.shp", sep=""),
              layer = "FRA_adm1", encoding="utf8")


world = readOGR(dsn=paste(path, "world/world.shp", sep=""),
                layer = "world", encoding="utf8")
plot(world)

REG = world
REGP = gCentroid(REG,byid=TRUE, id = REG@data$CNTR_ID)
plot(REGP)

par(mfrow=c(4,4), mar = c(0,0,1,0))
for(j in 1:16){
  e = j * 16
  b = e-15
  for (i in b:e){
    print(REG@polygons[[i]]@ID)
    plot(REG[i,], col="grey")
    title(REG@data[i,"CNTR_ID"])
  }}

articles = read.csv(paste0(path, "articles2.csv"), sep=",", dec=".")
head(articles)
articlesJuste = read.csv("/Users/clementinecottineau/Documents/cybergeo20/Data/raw/cybergeo.csv", sep=",", dec=".")
summary(articlesJuste)
articles = data.frame(articles, articlesJuste[match(articles$id,articlesJuste$id), ])
articles = subset(articles, TypeDocument == "Article")
dim(articles)

countries$polygonID = rownames(countries)
countries$polygonID = as.numeric(countries$polygonID) + 1
write.csv(countries, paste0(path, "countrycodes.csv"))
REG@bbox


articles$country = as.character(articles$country)
articles$textCountry = strsplit(articles$country, split = "_")
articles$country2 = as.character(articles$country2)
articles$textCountry2 = strsplit(articles$country2, split = "_")
articles$firstauthor = as.character(articles$firstauthor)
articles$textfirstauthor = strsplit(articles$firstauthor, split = "_")
articles$author2 = as.character(articles$author2)
articles$textauthor2 = strsplit(articles$author2, split = "_")
# 
 countries = as.character(REG@data$CNTR_ID)
# 
t = 0
for (i in 1:dim(articles)[1]){
  for (c in countries){
      t = t + 1
  articles[i,paste0("S_",c)] = ifelse(c %in% articles[i,"textCountry"] == "TRUE" ||
                                c %in% articles[i,"textCountry2"] == "TRUE", 1, 0)
  articles[i,paste0("A_",c)] = ifelse(c %in% articles[i,"textfirstauthor"] == "TRUE" ||
                                       c %in% articles[i,"textauthor2"] == "TRUE", 1, 0)
  articles[i,paste0("L_",c)] = ifelse(articles[i,paste0("S_",c)] == 1 && articles[i,paste0("A_",c)] == 1, 1, 0)
  print(t)
    }}
head(articles)
# 
 locals = paste0("L_", countries)
 authors = paste0("A_", countries)
 studies = paste0("S_", countries)

for (i in 1:dim(articles)[1]){
  articles[i,"locals"] = sum(articles[i,locals])
  articles[i,"LocalStudy"] = ifelse(articles[i,"locals"] >= 1, 1, 0)
}
summary(articles)
sum(articles$LocalStudy)

colnames(articles)
articlesToSave = articles
articlesToSave$textCountry = NULL 
articlesToSave$textCountry2 = NULL 
articlesToSave$textfirstauthor = NULL 
articlesToSave$textauthor2 = NULL 
write.csv(articlesToSave, "/Users/clementinecottineau/Documents/cybergeo20/R_Country_Wordcloud/articles_Contingency.csv")

articles = read.csv("/Users/clementinecottineau/Documents/cybergeo20/R_Country_Wordcloud/articles_Contingency.csv", sep=",", dec=".")
# head(articles)
# 
Studied = colSums(articles[,studies])
Authoring = colSums(articles[,authors])
SelfStudied = colSums(articles[,locals])
countryBase = data.frame(countries, Studied, Authoring, SelfStudied)
tail(countryBase, 10)

REG@data = data.frame(REG@data, countryBase[match(REG@data$CNTR_ID,countryBase$countries), ])

REG@data$StudiedAtAll = ifelse(REG@data$Studied >= 1, "#1C6F91", "lightgrey")
REG@data$AuthoringAtAll = ifelse(REG@data$Authoring >= 1, "orange", "lightgrey")
REG@data$SelfStudiedAtAll = ifelse(REG@data$SelfStudied >= 1,  "grey20", "lightgrey")

par(mfrow=c(1,1), mar = c(0,0,1,0))
plot(REG, col=REG@data$StudiedAtAll, border="white")
plot(REG, col=REG@data$AuthoringAtAll, border="white")
plot(REG, col=REG@data$SelfStudiedAtAll, border="white")
countries
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



cybterms = read.csv("/Users/clementinecottineau/Documents/cybergeo20/Data/raw/terms.csv", sep=";", dec=".")
head(cybterms)
#cybtermat = table(cybterms$term, cybterms$id)
#tail(cybtermat)

lookup = data.frame(countries)
lookup$polyID = as.numeric(rownames(lookup)) - 1

cybterms$idterm = rownames(cybterms)
cybterms2 = data.frame(cybterms, articles[match(cybterms$id,articles$id), ])
cybterms3 = data.frame(cybterms2, lookup[match(cybterms2$firstauthor,lookup$countries), ])
cybterms4 = cybterms3[complete.cases(cybterms3$id.1),]
#summary(cybterms4, 20)
cybterms5 = cybterms4[with(cybterms4, order(-count)),]



keywordsPO = cybterms5[,]
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
text(x,y,w, cex = 0.5 * f)
#f * 0.01 )





LOC








plot(REG@polygons[[4]])

dim(REG)
REG <- REG[4,]
plot(REG[219,])

REG@data
t = REG[REG@polygons[[4]]@ID]


REG@data[REG@data$ID_1 == 1,]

REG@polygons[[21]]@labpt
REG@polygons[[2]]@labpt[[1]]
REG@polygons[[1]]@ID

p = REG@polygons[[1]]@Polygons

min(REG@polygons[[4]]@Polygons[[1]]@coords[,2])
max(REG@polygons[[4]]@Polygons[[1]]@coords[,2])

REG@polygons[[1]]@Polygons[[1]]@coords

REG@polygons[[2]]@plotOrder


LOC=NULL
ID = c(1,2,3,4)
WORDS = c("Veranda", "Bulle", "Charlie", "Paper")
IDREG = c(1, 11,11, 3)
FREQ = c(3, 1, 2,1)
LOC = data.frame(ID, WORDS, IDREG, FREQ)
LOC$IDPOL = LOC$IDREG + 1

ids = unique(LOC$IDPOL)

x=data.frame()
ymin=data.frame()
ymax=data.frame()
y=data.frame()
z=data.frame()
for (i in ids) {
  x[i,"X"] = REG@polygons[[i]]@labpt[[1]] # Barycentre
  y[i,"Y"] = REG@polygons[[i]]@labpt[[2]]
  ymin[i, "Ymin"] = min(REG@polygons[[i]]@Polygons[[1]]@coords[,2])
  ymax[i, "Ymax"] = max(REG@polygons[[i]]@Polygons[[1]]@coords[,2])
  z[i,"Total"] = sum(LOC[LOC$IDPOL == i,]$FREQ)
}
x$IDPOL = rownames(x)
ymin$IDPOL = rownames(ymin)
ymax$IDPOL = rownames(ymax)
y$IDPOL = rownames(y)
z$IDPOL = rownames(z)

x= x[complete.cases(x),]
ymin= ymin[complete.cases(ymin),]
ymax= ymax[complete.cases(ymax),]
y= y[complete.cases(y),]
z= z[complete.cases(z),]
LOC = merge(LOC, x, by.x = "IDPOL", by.y="IDPOL", all.y=F)
LOC = merge(LOC, ymin, by.x = "IDPOL", by.y="IDPOL", all.y=F)
LOC = merge(LOC, ymax, by.x = "IDPOL", by.y="IDPOL", all.y=F)
LOC = merge(LOC, y, by.x = "IDPOL", by.y="IDPOL", all.y=F)
LOC = merge(LOC, z, by.x = "IDPOL", by.y="IDPOL", all.y=F)
LOC$RFREQ = LOC$FREQ / LOC$Total
LOC
LOC$Random = runif(dim(LOC)[[1]], 0, 1)
LOC$NewY = ifelse(LOC$RFREQ < 1, LOC$Random * LOC$Ymin + (1-LOC$Random) * LOC$Ymax,LOC$Y)
LOC
plot(REG, col="grey", border=F)
x <- LOC[,"X"]
y <- LOC[,"NewY"]
w <- LOC[,"WORDS"]
f = LOC[,"RFREQ"]
text(x,y,w, cex = f)















### Pb: link between IDREG and IDPOLYGON


data(SOTU)
corp <- SOTU
corp <- tm_map(corp, removePunctuation)
corp <- tm_map(corp, content_transformer(tolower))
corp <- tm_map(corp, removeNumbers)
corp <- tm_map(corp, function(x)removeWords(x,stopwords()))
term.matrix <- TermDocumentMatrix(corp)
#term.matrix$ID = 1
term.matrix <- as.matrix(term.matrix)
colnames(term.matrix) <- c("SOTU 2010","SOTU 2011")
comparison.cloud(term.matrix,max.words=50,random.order=FALSE)
commonality.cloud(term.matrix,max.words=50,random.order=FALSE)




withinShape = function (term.matrix, i = 1, scale = c(4, 0.5), max.words = 300, 
                        random.order = FALSE, rot.per = 0.1, 
                        colors = brewer.pal(ncol(term.matrix), "Dark2"), 
                        use.r.layout = FALSE, title.size = 3, ...) 
{
 # ndoc <- ncol(term.matrix)
 # thetaBins <- seq(from = 0, to = 2 * pi, length = ndoc + 1)
    term.matrix[, i] <- term.matrix[, i]/sum(term.matrix[, 
                                                         i])
  mean.rates <- rowMeans(term.matrix)
    term.matrix[, i] <- term.matrix[, i] - mean.rates
  
  group <- apply(term.matrix, 1, function(x) which.max(x))
  words <- rownames(term.matrix)
  freq <- apply(term.matrix, 1, function(x) max(x))
  tails <- "g|j|p|q|y"
  last <- 1
  
  overlap <- function(x1, y1, sw1, sh1) {
    if (!use.r.layout) 
      return(.overlap(x1, y1, sw1, sh1, boxes))
    s <- 0
    if (length(boxes) == 0) 
      return(FALSE)
    for (i in c(last, 1:length(boxes))) {
      bnds <- boxes[[i]]
      x2 <- bnds[1]
      y2 <- bnds[2]
      sw2 <- bnds[3]
      sh2 <- bnds[4]
      if (x1 < x2) 
        overlap <- x1 + sw1 > x2 - s
      else overlap <- x2 + sw2 > x1 - s
      if (y1 < y2) 
        overlap <- overlap && (y1 + sh1 > y2 - s)
      else overlap <- overlap && (y2 + sh2 > y1 - s)
      if (overlap) {
        last <<- i
        return(TRUE)
      }
    }
    FALSE
  }
  ord <- rank(-freq, ties.method = "random")
  words <- words[ord <= max.words]
  freq <- freq[ord <= max.words]
  group <- group[ord <= max.words]
  if (random.order) {
    ord <- sample.int(length(words))
  }
  else {
    ord <- order(freq, decreasing = TRUE)
  }
  words <- words[ord]
  freq <- freq[ord]
  group <- group[ord]
  thetaStep <- 0.05
  rStep <- 0.05
  plot.new()
  op <- par("mar")
  par(mar = c(0, 0, 0, 0))
  plot.window(c(0, 1), c(0, 1), asp = 1)
  normedFreq <- freq/max(freq)
  size <- (scale[1] - scale[2]) * normedFreq + scale[2]
  boxes <- list()
  docnames <- colnames(term.matrix)
  for (i in 1:ncol(term.matrix)) {
    th <- mean(thetaBins[i:(i + 1)])
    word <- docnames[i]
    wid <- strwidth(word, cex = title.size) * 1.2
    ht <- strheight(word, cex = title.size) * 1.2
    x1 <- 0.5 + 0.45 * cos(th)
    y1 <- 0.5 + 0.45 * sin(th)
    rect(x1 - 0.5 * wid, y1 - 0.5 * ht, x1 + 0.5 * wid, y1 + 
           0.5 * ht, col = "grey90", border = "transparent")
    text(x1, y1, word, cex = title.size)
    boxes[[length(boxes) + 1]] <- c(x1 - 0.5 * wid, y1 - 
                                      0.5 * ht, wid, ht)
  }
  for (i in 1:length(words)) {
    rotWord <- runif(1) < rot.per
    r <- 0
    theta <- runif(1, 0, 2 * pi)
    x1 <- 0.5
    y1 <- 0.5
    wid <- strwidth(words[i], cex = size[i], ...)
    ht <- strheight(words[i], cex = size[i], ...)
    if (grepl(tails, words[i])) 
      ht <- ht + ht * 0.2
    if (rotWord) {
      tmp <- ht
      ht <- wid
      wid <- tmp
    }
    isOverlaped <- TRUE
    while (isOverlaped) {
      inCorrectRegion <- theta > thetaBins[group[i]] && 
        theta < thetaBins[group[i] + 1]
      if (inCorrectRegion && !overlap(x1 - 0.5 * wid, y1 - 
                                        0.5 * ht, wid, ht) && x1 - 0.5 * wid > 0 && y1 - 
            0.5 * ht > 0 && x1 + 0.5 * wid < 1 && y1 + 0.5 * 
            ht < 1) {
        text(x1, y1, words[i], cex = size[i], offset = 0, 
             srt = rotWord * 90, col = colors[group[i]], 
             ...)
        boxes[[length(boxes) + 1]] <- c(x1 - 0.5 * wid, 
                                        y1 - 0.5 * ht, wid, ht)
        isOverlaped <- FALSE
      }
      else {
        if (r > sqrt(0.5)) {
          warning(paste(words[i], "could not be fit on page. It will not be plotted."))
          isOverlaped <- FALSE
        }
        theta <- theta + thetaStep
        if (theta > 2 * pi) 
          theta <- theta - 2 * pi
        r <- r + rStep * thetaStep/(2 * pi)
        x1 <- 0.5 + r * cos(theta)
        y1 <- 0.5 + r * sin(theta)
      }
    }
  }
  par(mar = op)
  invisible()
}