##############################
# Shiny App: Cybergeo20
# Packages and functions
##############################



# load packages ----

library(shiny)
library(rgdal)
library(plyr)
library(rgdal) 
library(mapproj) 
library(maptools) 
library(RColorBrewer)
library(RCurl)
library(ggplot2)
library(reshape2)
library(grid)
library(igraph)
library(dplyr)
library(networkD3)





#### Clem
aggregateCountriesBasedOnTerms = function(themesFile, themes, countries_to_aggregate, colNumbers){
  themes_By_country_bf = data.frame("CountryID" = countries_to_aggregate)
  themes_By_country_bf[,themes] = NA
  themes_By_country_bf$n = NA
  
  for (c in countries_to_aggregate){
    articles_to_aggregate = themesFile[themesFile[,c] == 1,colNumbers]
    if (!is.null(articles_to_aggregate)){
      nArticles = dim(articles_to_aggregate)[1]
      themes_By_country_bf[themes_By_country_bf$CountryID == c, themes] = colSums(articles_to_aggregate) / nArticles
      themes_By_country_bf[themes_By_country_bf$CountryID == c, "n"] = nArticles
    }}
  
  themes_By_country_bf = themes_By_country_bf[complete.cases(themes_By_country_bf),]
  themes_By_country_bf$CountryID = substr(themes_By_country_bf$CountryID, 3,4)
  return(themes_By_country_bf)
}

cahCountriesBasedOnTerms = function(themes_By_country_bf, numberOfGroups, colNumbers){
themesScaled = scale(themes_By_country_bf[,colNumbers])
  rownames(themesScaled) = themes_By_country_bf[,1]
  d.themes = dist(themesScaled)
  cah.themes = hclust(d.themes, method = "ward.D2")
  groups_Country = cutree(cah.themes, k=numberOfGroups)
  return(groups_Country)
}

sumNum = function(x){
  y = sum(x, na.rm= T)
  return(y)
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


### Juste ---

load('data/semanticnw.RData')



# plot communities ----

VisuComm <- function(g, comm, vertcol, vertsize, vfacsize, edgesize, efacsize, textsize){
  par(bg = "#4e5d6c")
  # circle layout with sampled coordinates
  oriCoords <- layout_in_circle(g)
  corrCoords <- oriCoords[sample(seq(1, nrow(oriCoords), 1), size = nrow(oriCoords), replace = FALSE), ]
  
  plot(g,
       edge.color = "#df691a",
       edge.width = efacsize * edgesize,
       edge.curved = F,
       edge.arrow.mode = "-",
       edge.arrow.size = 0.01,
       vertex.color = vertcol,
       vertex.frame.color = "#df691a",
       vertex.label = V(g)$name,
       vertex.label.color = "#ebebeb",
       vertex.label.family = "sans-serif",
       vertex.label.cex = textsize / 10,
       vertex.size = vfacsize * vertsize,
       layout = corrCoords
  )
}



# plot semantic field ----

VisuSem <- function(g, kw, textsizemin, textsizemax){
  
  # make theme empty
  theme_empty <- theme_bw() +
    theme(plot.background = element_rect(fill = "#4e5d6c"),
          axis.line = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_rect(fill = "#4e5d6c"),
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          legend.position = "none",
          legend.background = element_rect(fill = "#4e5d6c"))
  
  # graph layout
  tabPoints <- get.data.frame(x = g, what = "vertices")
  tabLinks <- get.data.frame(x = g, what = "edges")
  tabLinks$NODES <- ifelse(tabLinks$from == kw, tabLinks$to, tabLinks$from)
  tabPoints <- tabPoints %>% left_join(x = ., y = tabLinks, by = c("name" = "NODES"))
  
  # compute distance from ego
  tabPoints$DIST <- 1 / tabPoints$relresid
  thresRadis <- seq(0, 0.1 + max(tabPoints$DIST, na.rm = TRUE), 0.1)
  tabPoints$X <- cut(tabPoints$DIST, breaks = thresRadis, labels = thresRadis[-1], include.lowest = TRUE, right = FALSE)
  tabPoints <- tabPoints %>% group_by(X) %>% mutate(NPTS = n())
  
  # get x values
  tabPoints <- tabPoints %>% do(GetXvalues(df = .))
  tabPoints[tabPoints$name == kw, c("XVAL", "DIST")] <- c(0, 0)
  
  # prepare plot
  tabPoints$IDEGO <- ifelse(tabPoints$name == kw, 2, 1)
  tabCircle <- data.frame(XVAL = c(0, 360), DIST = 1)
  
  # draw plot
  circVis <- ggplot() + 
    geom_line(data = tabCircle, aes(x = XVAL, y = DIST), color = "#df691a") + 
    geom_text(data = tabPoints, aes(x = XVAL, y = DIST, label = name, fontface = IDEGO, color = factor(IDEGO), size = nbauth)) +
    scale_colour_manual("Type", values = c("#ebebeb", "#df691a")) +
    scale_size_continuous("Number of articles", range = c(textsizemin, textsizemax)) +
    coord_polar(theta = "x") +
    theme_empty
  
  return(circVis)
}





# Internal functions for VisuSem() ----


# Sample x values for polar coordinates

GetXvalues <- function(df){
  initVal <- sample(x = 0:360, size = 1, replace = FALSE)
  tempRange <- seq(initVal, initVal + 360, 360/unique(df$NPTS))
  tempRange <- tempRange[-length(tempRange)]
  df$XVAL <- ifelse(tempRange > 360, tempRange - 360, tempRange) 
  return(df)
}


# create semantic field network

SemanticField <- function(g, kw){
  
  # list of neighbors
  neiNodes <- unlist(neighborhood(g, order = 1, nodes = V(g)[V(g)$name == kw], mode = "all"))
  pairedNodes <- unlist(paste(which(V(g)$name == kw), neiNodes[-1], sep = ","))
  collapseNodes <- paste(pairedNodes, collapse = ",")
  vecNodes <- as.integer(unlist(strsplit(collapseNodes, split = ",")))
  
  # get edges and create graph
  edgeIds <- get.edge.ids(g, vp = vecNodes)
  gSem <- subgraph.edges(g, eids = edgeIds, delete.vertices = TRUE)
  
  return(gSem)
}
