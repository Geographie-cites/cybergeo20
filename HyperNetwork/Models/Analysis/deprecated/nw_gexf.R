
library(rgexf)

graph_gexf <- read.gexf(paste0(Sys.getenv('CS_HOME'),'/Cybergeo/cybergeo20/Data/processed/networks/test_citingNW_Wed-Oct-28-23:35:42-CET-2015.gexf'))
summary(graph_gexf)
check.dpl.edges(graph_gexf)

graph = gexf.to.igraph(graph_gexf)


measures <- read.csv(
  paste0(Sys.getenv('CS_HOME'),'/Cybergeo/cybergeo20/Data/processed/networks/test_citingNW_Wed-Oct-28-23:35:42-CET-2015_Nodes.csv'),
  sep=';'
)

cyb = measures[!is.na(measures$cybergeo),]
indeg = measures$Degré.Entrant[!is.na(measures$cybergeo)]
cyb[which(indeg==max(indeg)),]
hist(indeg,breaks=100)

quantile(cyb$Degré.Entrant,0.95)
mean(cyb$Degré.Entrant[cyb$Degré.Entrant<quantile(cyb$Degré.Entrant,0.10)])

# plot quantile vs contributed impact factor (NAME ?)
quantiles = (50:100)/100
impactFactor=mean(cyb$Degré.Entrant)
contrQuantile = sapply(quantiles,function(q){mean(cyb$Degré.Entrant[cyb$Degré.Entrant<=quantile(cyb$Degré.Entrant,q)])/impactFactor})
# 'Gini' curve ?
plot(quantiles,contrQuantile,type="l")


