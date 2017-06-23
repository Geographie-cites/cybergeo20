library("rgdal")
library(riverplot)
library(reshape2)
library(igraph)
sumNum = function(x){
  y = sum(x, na.rm= T)
  return(y)
}
setwd("~/Documents/cybergeo20/CybergeoNetworks")
load("data/CyberData.RData")
hadriTerms = read.csv("data/kwprop.csv", sep=",", dec=".")
justeTerms = read.csv("data/docprobasJuste2.csv", sep=",", dec=".")
load("data/themesPO.Rdata")
files$name = NULL
files$path = NULL
poTerms = read.csv("data/20themes20words.csv", sep=",", dec=".")
nameThemes = c(as.character(poTerms$NAME), "Other")
colnames(document.themes) = nameThemes
files[,3:22] = document.themes 
colnames(files)[3:22] = nameThemes
articles = data.frame()
paletteCybergeo = c("#1C6F91", "#df691a", "#77c5ba", "orange", "#2db92d", "#e1ff2f", "#ff2313", "#bbab61")
pattern_list <- c("espace", "territoire", "environnement", "société", "réseau", "interaction", "aménagement", "urbanisme", "carte", "modèle", "système", "SIG", "fractale", "durabilité", "représentation", "migration", "quantitatif", "qualitatif", "post-moderne")
world = readOGR(dsn="data/world_withZoom.shp",
                layer = "world_withZoom", encoding="utf8", verbose = F)

countries = as.character(world@data$CNTR_ID)
locals = paste0("L_", countries)
authors = paste0("A_", countries)
studies = paste0("S_", countries)
lookup = data.frame(countries)
lookup$polyID = as.numeric(rownames(lookup)) - 1

themeNamesJ = colnames(justeTerms)[2:13]
themeNamesH = colnames(hadriTerms)[2:11]
themeNamesP = nameThemes

articles = cyberData$ARTICLES

#Juste
cybterms = justeTerms[justeTerms$CYBERGEOID != 0,]
cybterms$idterm = rownames(cybterms)
cybterms2J = data.frame(cybterms, articles[match(cybterms$CYBERGEOID,articles$id), ])

#Hadri
cybterms = hadriTerms
cybterms2H = data.frame(cybterms, articles[match(cybterms$ID,articles$id), ])

#POC
articlesWithThemes = data.frame(articles, files[match(articles$id,files$id), ])
cybterms = articlesWithThemes[,c("id",themeNamesP)]
cybtermsbis = cybterms[complete.cases(cybterms[,themeNamesP]),]
cybterms2P = data.frame(cybtermsbis, articles[match(cybtermsbis$id,articles$id), ])

#### JUSTE_HADRI
cybterms3 = data.frame(cybterms2J, cybterms2H[match(cybterms2J$id,cybterms2H$id), ])
cybterms4 = cybterms3[complete.cases(cybterms3$id.1),]
n = dim(cybterms4)[1]

multiples_flows = data.frame()
i = 0
for (t1 in themeNamesJ){
  for (t2 in themeNamesH){
    flow = paste(t1, t2, sep=" ")
    print(flow)
    for (r in 1:n){
      i = i+1
      multiples_flows[i, "flow"] = flow
      multiples_flows[i, "share"] = cybterms4[r,t1] * cybterms4[r,t2]
    }}}
summary(multiples_flows)

unique_flows = aggregate(multiples_flows$share, by = list(multiples_flows$flow), FUN = sumNum)

for (r in 1:dim(unique_flows)[1]){
  unique_flows[r,'source'] = strsplit(unique_flows[r,'Group.1'], " ")[[1]][1]
  unique_flows[r,'target'] = strsplit(unique_flows[r,'Group.1'], " ")[[1]][2]
  unique_flows[r,'value'] = unique_flows[r,'x']
}

simple_flow = unique_flows[,c('source', 'target', 'value')]
write.csv(simple_flow, "citations_keywords_overlap.csv")
flow_network = graph_from_data_frame(simple_flow, directed=F)

index_flow = simple_flow
index_flow$N1 = letters[as.numeric(as.factor(index_flow$source))]
index_flow$N2 = LETTERS[as.numeric(as.factor(index_flow$target))]
index_flow$ID = 1:dim(index_flow)[1]
index_flow$Value = index_flow$value

nodes1 = data.frame(unique(index_flow$N1), unique(index_flow$source))
colnames(nodes1) = c('ID', "labels")
nodes1$x = 1
nodes2 = data.frame(unique(index_flow$N2), unique(index_flow$target))
colnames(nodes2) = c('ID', "labels")
nodes2$x = 2
nodes = rbind(nodes1, nodes2)

edges = index_flow[,c("ID", 'N1', 'N2', 'Value')]

ready_flow = makeRiver(nodes, edges)
riverplot(ready_flow, plot_area = 1)






#### HADRI_POC
cybterms3 = data.frame(cybterms2H, cybterms2P[match(cybterms2H$id,cybterms2P$id), ])
cybterms4 = cybterms3[complete.cases(cybterms3$id.1),]
n = dim(cybterms4)[1]

multiples_flows = data.frame()
i = 0
for (t1 in themeNamesH){
  for (t2 in themeNamesP){
    flow = paste(t1, t2, sep=" ")
    print(flow)
    for (r in 1:n){
      i = i+1
      multiples_flows[i, "flow"] = flow
      multiples_flows[i, "share"] = cybterms4[r,t1] * cybterms4[r,t2]
    }}}
summary(multiples_flows)

unique_flows = aggregate(multiples_flows$share, by = list(multiples_flows$flow), FUN = sumNum)

for (r in 1:dim(unique_flows)[1]){
  unique_flows[r,'source'] = strsplit(unique_flows[r,'Group.1'], " ")[[1]][1]
  unique_flows[r,'target'] = strsplit(unique_flows[r,'Group.1'], " ")[[1]][2]
  unique_flows[r,'value'] = unique_flows[r,'x']
}

simple_flow = unique_flows[,c('source', 'target', 'value')]
write.csv(simple_flow, "keywords_themes_overlap.csv")
flow_network = graph_from_data_frame(simple_flow, directed=F)

index_flow = simple_flow
index_flow$N1 = letters[as.numeric(as.factor(index_flow$source))]
index_flow$N2 = LETTERS[as.numeric(as.factor(index_flow$target))]
index_flow$ID = 1:dim(index_flow)[1]
index_flow$Value = index_flow$value

nodes1 = data.frame(unique(index_flow$N1), unique(index_flow$source))
colnames(nodes1) = c('ID', "labels")
nodes1$x = 1
nodes2 = data.frame(unique(index_flow$N2), unique(index_flow$target))
colnames(nodes2) = c('ID', "labels")
nodes2$x = 2
nodes = rbind(nodes1, nodes2)
edges = index_flow[,c("ID", 'N1', 'N2', 'Value')]

ready_flow = makeRiver(nodes, edges)
riverplot(ready_flow, plot_area = 1)





#### POC_JUSTE
cybterms3 = data.frame(cybterms2P, cybterms2J[match(cybterms2P$id,cybterms2J$id), ])
cybterms4 = cybterms3[complete.cases(cybterms3$id.1),]
n = dim(cybterms4)[1]

multiples_flows = data.frame()
i = 0
for (t1 in themeNamesP){
  for (t2 in themeNamesJ){
    flow = paste(t1, t2, sep=" ")
    print(flow)
    for (r in 1:n){
      i = i+1
      multiples_flows[i, "flow"] = flow
      multiples_flows[i, "share"] = cybterms4[r,t1] * cybterms4[r,t2]
    }}}
summary(multiples_flows)

unique_flows = aggregate(multiples_flows$share, by = list(multiples_flows$flow), FUN = sumNum)

for (r in 1:dim(unique_flows)[1]){
  unique_flows[r,'source'] = strsplit(unique_flows[r,'Group.1'], " ")[[1]][1]
  unique_flows[r,'target'] = strsplit(unique_flows[r,'Group.1'], " ")[[1]][2]
  unique_flows[r,'value'] = unique_flows[r,'x']
}

simple_flow = unique_flows[,c('source', 'target', 'value')]
write.csv(simple_flow, "themes_citations_overlap.csv")
flow_network = graph_from_data_frame(simple_flow, directed=F)

index_flow = simple_flow
index_flow$N1 = letters[as.numeric(as.factor(index_flow$source))]
index_flow$N2 = LETTERS[as.numeric(as.factor(index_flow$target))]
index_flow$ID = 1:dim(index_flow)[1]
index_flow$Value = index_flow$value

nodes1 = data.frame(unique(index_flow$N1), unique(index_flow$source))
colnames(nodes1) = c('ID', "labels")
nodes1$x = 1
nodes2 = data.frame(unique(index_flow$N2), unique(index_flow$target))
colnames(nodes2) = c('ID', "labels")
nodes2$x = 2
nodes = rbind(nodes1, nodes2)
edges = index_flow[,c("ID", 'N1', 'N2', 'Value')]

ready_flow = makeRiver(nodes, edges)
riverplot(ready_flow, plot_area = 1)
