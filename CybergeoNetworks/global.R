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
library(RSQLite)
library(svgPanZoom)
library(wordcloud)
library(scales)
library(lubridate)
library(stringr)




#### Clem

#'
#' @title Aggregate countries based on terms
#' @name aggregateCountriesBasedOnTerms
#' @description This function summarises the number of articles by theme for each country it is used in the reactive object 'clusterCountries' for every analysis at the country level
#' @param themesFile: a dataframe in which lines represent articles and columns include themes and country codes
#' @param themes: the list of themes of the analysis
#' @param countries_to_aggregate: the list of countries to aggregate articles by (taken from the shapeFile)
#' @value themes_By_country_bf, a dataframe in which lines represent country codes and columns represent the number of articles for each theme
aggregateCountriesBasedOnTerms = function(themesFile, themes, countries_to_aggregate){
  themes_By_country_bf = data.frame("CountryID" = countries_to_aggregate)
  themes_By_country_bf[,themes] = NA
  themes_By_country_bf$n = NA
  
  for (c in countries_to_aggregate){
    articles_to_aggregate = themesFile[themesFile[,c] == 1,themes]
    if (!is.null(articles_to_aggregate)){
      nArticles = dim(articles_to_aggregate)[1]
      themes_By_country_bf[themes_By_country_bf$CountryID == c, themes] = colSums(articles_to_aggregate) / nArticles
      themes_By_country_bf[themes_By_country_bf$CountryID == c, "n"] = nArticles
    }}
  
  themes_By_country_bf = themes_By_country_bf[complete.cases(themes_By_country_bf),]
  themes_By_country_bf$CountryID = substr(themes_By_country_bf$CountryID, 3,4)
  return(themes_By_country_bf)
}



#'
#' @name cahCountriesBasedOnTerms (function)
#' @description This function produces a hierarchical clustering of countries with respect to their frequency of themes it is used in the geosemantic tab to display the groups of countries by themes and the corresponding average profiles of themes
#' @param themes_By_country_bf: dataframe in which lines represent country codes and columns represent the number of articles for each theme
#' @param numberOfGroups: an integer giving the number of classes for the clustering
#' @param themes: the list of themes of the analysis
#' @param groups_Country, a vector of group IDs for each country
cahCountriesBasedOnTerms = function(themes_By_country_bf, numberOfGroups, themes){
themesScaled = scale(themes_By_country_bf[,themes])
  rownames(themesScaled) = themes_By_country_bf[,1]
  d.themes = dist(themesScaled)
  cah.themes = hclust(d.themes, method = "ward.D2")
  groups_Country = cutree(cah.themes, k=numberOfGroups)
  return(groups_Country)
}




#'
#' @name sumNum
#' @description This function ensures that no <NA> is returned for a sum if one element of the sum is <NA> it is used in aggregate and apply functions.
#' @param  x: a vector of elements to sum  
#' Returns: y, a single value (the sum)
sumNum = function(x){
  y = sum(x, na.rm= T)
  return(y)
}



#' @name stat.comp
#' @description This function computes the average frquencies of themes by cah group
#'   it is used in the legend of the cah map 
#' @param x: a dataframe of theme frequency by country (themes_By_country_bf) in which lines represent country codes and columns represent the number of articles for each themes
#' @param y: a vector of group numbers the length of the dataframe rows
#' @value result, a dataframe in which lines represent cah groups of country and columns represent the frequency of articles for each theme
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







# load data ----



#### Keywords network

#'
#' @description contains cyberData = list(NETKW = igraph network of thesaurus keywords, ARTICLES keywords raw data)
load("data/CyberData.RData")

#'
#' @description thesaurus themes probas
hadriTerms = read.csv("data/kwprop.csv", sep=",", dec=".")


#### Semantic network

#'
#' @description themes probas with the semantic network classification
justeTerms = read.csv("data/docprobasJuste2.csv", sep=",", dec=".")


#### LDA analysis

#'
#' @description LDA analysis output : files = ids and raw files ;
#'   themes.termes = keywords of thematics ; document.themes = probability matrix
load("data/themesPO.Rdata")
files$name = NULL
files$path = NULL

#'
#' @description keywords and themes from LDA
poTerms = read.csv("data/20themes20words.csv", sep=",", dec=".")

nameThemes = c(as.character(poTerms$NAME), "Other")
colnames(document.themes) = nameThemes
files[,3:22] = document.themes 
colnames(files)[3:22] = nameThemes




#### Geographical data

world = readOGR(dsn="data/world_withZoom.shp",
                layer = "world_withZoom", encoding="utf8", verbose = F)
countries = as.character(world@data$CNTR_ID)
locals = paste0("L_", countries)
authors = paste0("A_", countries)
studies = paste0("S_", countries)
lookup = data.frame(countries)
lookup$polyID = as.numeric(rownames(lookup)) - 1

articles = data.frame()
paletteCybergeo = c("#1C6F91", "#df691a", "#77c5ba", "orange", "#2db92d", "#e1ff2f", "#ff2313", "#bbab61")
pattern_list <- c("espace", "territoire", "environnement", "société", "réseau", "interaction", "aménagement", "urbanisme", "carte", "modèle", "système", "SIG", "fractale", "durabilité", "représentation", "migration", "quantitatif", "qualitatif", "post-moderne")










######## PO : 
##   Regexp terms in full textes



pattern_list <- c("espace", "territoire", "environnement", "société", "réseau", "interaction", "aménagement", "urbanisme", "carte", "modèle", "système", "SIG", "fractale", "durabilité", "représentation", "migration", "quantitatif", "qualitatif", "post-moderne")
#pattern_list <- c("g[ée]ograph")

#setwd(paste0(Sys.getenv('CS_HOME'),'/Cybergeo/cybergeo20/regexp'))

#-- Loading data --------------------------------------------------------------

terms <- read.table(
  "data/terms.csv", 
  sep = ";", 
  quote = "", 
  comment.char = "", 
  header = TRUE,
  stringsAsFactors = FALSE
) %>% 
  tbl_df() %>%
  dplyr::mutate(
    article_id = id,
    id = row_number()
  ) %>%
  dplyr::select(id, article_id, term, count)

sentences <- read.table(
  "data/sentences.csv", 
  sep = "|", 
  quote = "", 
  comment.char = "", 
  header = TRUE,
  stringsAsFactors=FALSE
) %>% 
  tbl_df() %>%
  dplyr::mutate(
    article_id = id,
    id = row_number()
  )

articles <- read.table(
  "data/cybergeo.csv", 
  sep = ",", 
  quote = "\"", 
  comment.char = "", 
  header = TRUE
) %>% 
  tbl_df() %>%
  dplyr::rename(titre = title_en, auteurs = authors) %>%
  dplyr::mutate(citation = paste(sep = ". ", auteurs, substr(date,1,4), titre)) %>%
  dplyr::select(id, date, citation, langue)

gc() # <- WHY THIS GC HERE ?

#-- Functions -----------------------------------------------------------------

terms_matched <- function(patterns) {
  data <- data_frame()
  for (pattern in patterns) {
    indices <- grep(pattern, terms$term, ignore.case = TRUE, perl = TRUE)
    data <- data_frame(id = indices) %>%
      dplyr::mutate(pattern = pattern) %>%
      dplyr::bind_rows(data)
  }
  data <- data %>%
    dplyr::left_join(terms, by = c("id")) %>%
    dplyr::arrange(id, pattern)
  return(data)
} 

titles_matched <- function(patterns) {
  citations <- terms_matched(patterns) %>%
    dplyr::select(article_id) %>%
    dplyr::unique() %>%
    dplyr::left_join(articles, by = c("article_id" = "id")) %>%
    dplyr::arrange(date) %>%
    dplyr::select(citation)
  return(citations$citation)
}

phrases <- function(patterns) {
  data <- data_frame()
  for (pattern in patterns) {
    indices <- grep(pattern, sentences$sentence, ignore.case = TRUE, perl = TRUE)
    data <- data_frame(id = indices) %>%
      dplyr::bind_rows(data)
  }
  data <- data %>%
    dplyr::left_join(sentences, by = c("id")) %>%
    dplyr::select(sentence)
  return(data$sentence)
}

terms_matched_cloud <- function(patterns) {
  terms_matched(patterns) %>%
    dplyr::group_by(term) %>%
    #    summarise(articles = n_distinct(article_id), terms = sum(count))
    dplyr::summarise(articles = sum(count))
}

articles_matched <- function(patterns) {
  terms_matched(patterns) %>%
    dplyr::group_by(article_id, pattern) %>%
    dplyr::summarise(count = sum(count)) %>%
    dplyr::left_join(articles, by = c("article_id" = "id")) %>%
    dplyr::mutate(ym = str_sub(date, 1, 4)) %>%
    dplyr::group_by(ym, pattern) %>%
    dplyr::summarise(articles=n_distinct(article_id), terms=sum(count)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(date = parse_date_time(ym, "%y")) %>%
    dplyr::select(date, pattern, articles, terms)
}

chronogram <- function(patterns) {
  ggplot(articles_matched(patterns), aes(date, articles)) +
    geom_bar(stat = "identity") +
    facet_grid(pattern ~ ., scales = "free_y", space = "free_y") +
    labs(title="Chronogramme des articles publiés dans Cybergéo", x = "Année de publication", y = "Nombre d'articles publiés")
}

cloud <- function(patterns) {
  words <- terms_matched_cloud(patterns)
  wordcloud(
    words$term,
    words$articles,
    scale = c(10,1),
    rot.per = 0
  ) 
}
















####################
### Juste ---
#
#  --  Archi for cit. nw exploration  --
# 
#   - data/semanticnw.RData is not loaded as huge ; replaced by sqlite
#    -> for performance, can be fully loaded is speed is prefered over memory
#   - load datatable for cybergeo articles ; request in local sqlite db for connections
#   - get the ego nw, and display info for neighbors
#   - display semantic info : keywords, corresponding communities.
#   - one tab with sem nw visu : svg viz
# 
# 


##
#  Notations / id conventions : vars and ids prefixed with "citation"

#' ---- DATA ----

#'
#'  citation nw cybergeo table
load('data/citation_cybergeodata.RData')

#'
#'  kws domains dico
load('data/citation_kwthemdico.RData')

#'
#'  sqlite connection : citation nw
citationdbcit = dbConnect(SQLite(),"data/CitationNetwork.sqlite3")

#'
#'  sqlite connection : keywords
citationdbkws = dbConnect(SQLite(),"data/CitationKeywords.sqlite3")


#' ---- GLOBALS ----

#'
#' list specific colors associated with thematics
#'  -- SHOULD NOT BE HARDCODED --
semanticcolors = list(rgb(204,0,255,maxColorValue=255),rgb(255,102,0,maxColorValue=255), rgb(255,102,0,maxColorValue=255),
                      rgb(255,153,0,maxColorValue=255),rgb(0,204,102,maxColorValue=255),rgb(255,0,0,maxColorValue=255),
                      rgb(153,153,0,maxColorValue=255),rgb(102,204,0,maxColorValue=255),rgb(0,255,255,maxColorValue=255),
                      rgb(255,255,0,maxColorValue=255),rgb(51,102,255,maxColorValue=255),rgb(51,255,51,maxColorValue=255),
                      rgb(0,102,0,maxColorValue=255),rgb(0,0,255,maxColorValue=255),rgb(102,51,0,maxColorValue=255)
)
names(semanticcolors)<-c("complex systems","health","crime",
                         "statistical methods","remote sensing","political sciences/critical geography",
                         "traffic modeling","microbiology","cognitive sciences",
                         "spatial analysis","GIS","biogeography",
                         "environnment/climate","economic geography","physical geography")


# global vars (needed e.g. to avoid numerous db request with reactive functions)
citationGlobalVars <- reactiveValues()
citationGlobalVars$citationSelected = "0"
citationGlobalVars$citationSemanticSelected = "0"


#' ---- FUNCTIONS ----


#'
#' @name citationLoadEdges
#' @description load citation edges given an reference id
citationLoadEdges<-function(id){
  res=data.frame()
  res=rbind(res,dbGetQuery(citationdbcit,paste0("SELECT * FROM edges WHERE `from`='",id,"';")))
  res=rbind(res,dbGetQuery(citationdbcit,paste0("SELECT * FROM edges WHERE `to`='",id,"';")))
  return(res)
}

#' @name  citationLoadKeywords
#' @description load neighbors keywords given an id
citationLoadKeywords<-function(id){
  # load edges
  toids=dbGetQuery(citationdbcit,paste0("SELECT `to` FROM edges WHERE `from`='",id,"';"))[,1]
  fromids=dbGetQuery(citationdbcit,paste0("SELECT `from` FROM edges WHERE `to`='",id,"';"))[,1]
  ids=c(id,toids,fromids)
  req = "SELECT * FROM keywords WHERE "
  for(i in ids[1:(length(ids)-1)]){req=paste0(req,"`id`='",i,"' OR ")}
  req=paste0(req,"`id`='",ids[length(ids)],"';")
  res=dbGetQuery(citationdbkws,req)
  l = sapply(res$keywords,function(s){strsplit(s,";")})
  names(l)<-res$id
  return(l)
}



#'
#' @name citationVisuEgo
#' @description visualize an ego network given edges
citationVisuEgo<-function(edges){
  if(!is.null(edges)){
     if(nrow(edges)>0){
      citsubgraph = graph_from_data_frame(edges,directed=TRUE)
      #show(citsubgraph)
      V(citsubgraph)[head_of(citsubgraph,E(citsubgraph))$name]$cyb = E(citsubgraph)$fromcyb
      V(citsubgraph)[tail_of(citsubgraph,E(citsubgraph))$name]$cyb = E(citsubgraph)$tocyb
      V(citsubgraph)[head_of(citsubgraph,E(citsubgraph))$name]$title = E(citsubgraph)$fromtitle
      V(citsubgraph)[tail_of(citsubgraph,E(citsubgraph))$name]$title = E(citsubgraph)$totitle
      lay=layout_as_tree(citsubgraph,circular=FALSE)
      lay[lay[,2]==0,2]=-sample.int(length(which(lay[,2]==0)),replace=FALSE)-2
      #lay[lay[,2]==2,1]= sample.int(10,size=length(which(lay[,2]==2)))-5#((-length(which(lay[,2]==2))/2):(length(which(lay[,2]==2))/2))*5/length(which(lay[,2]==2))
      lay[lay[,2]==2,1]= sample.int(length(which(lay[,2]==2)))-5
      lay[lay[,2]==2,2]=4+sample.int(length(which(lay[,2]==2)),replace=FALSE)
      palette=c("#df691a","#1C6F91")
      par(bg = "#4e5d6c")
      plot(citsubgraph,edge.color="#df691a",edge.arrow.size = 1,
           vertex.label=V(citsubgraph)$title,vertex.color=palette[V(citsubgraph)$cyb+1],
           vertex.frame.color="#1C6F91",vertex.label.color = "#ebebeb",
           layout=lay
      )
      #16283 22232 23337 23502 26325 24841 26026 22270 24798 25354 26969
    }
  }
}


                      


#'
#' @name citationWordclouds
#' @description plots word clouds, one for the keywords of the ref itself, the other for the provided keywords (neighborhood)
citationWordclouds<-function(id,keywords){
  if(id!="0"&!is.null(keywords)){
    # at least kws for the paper, so no need to check emptyness
    par(mfrow=c(1,2))
    par(bg = "#4e5d6c")
    wordcloud(words=keywords[[id]],
              freq=citationkwfreqs[keywords[[id]]],
              colors=unlist(semanticcolors[citationkwthemdico[keywords[[id]]]]),
              ordered.colors = TRUE
              )
    allkws=unlist(keywords)
    wordcloud(words=allkws,
              freq=citationkwfreqs[allkws],
              colors=unlist(semanticcolors[citationkwthemdico[allkws]]),
              ordered.colors = TRUE
    )
  }
}







#######################
### Hadri

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
