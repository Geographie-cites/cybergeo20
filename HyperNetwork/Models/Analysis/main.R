
##

setwd(paste0(Sys.getenv('CS_HOME'),'/Cybergeo/Models/cybergeo20/HyperNetwork/Models/Analysis'))

source('networkConstruction.R')
source('citationNWConstruction.R')



#####
## Construct the citation nw from raw data
#  csv -> RData
citnwedgefile = paste0(Sys.getenv('CS_HOME'),'/Cybergeo/Models/cybergeo20/HyperNetwork/Data/nw/full_edges.csv')
citnwnodefile = paste0(Sys.getenv('CS_HOME'),'/Cybergeo/Models/cybergeo20/HyperNetwork/Data/nw/full_nodes.csv')
citnwoutput=paste0(Sys.getenv('CS_HOME'),'/Cybergeo/Models/cybergeo20/HyperNetwork/Data/nw/citationNetwork.RData')

constructCitationNetwork(citnwedgefile,citnwnodefile,citnwoutput)


cybnodes = V(gcitation)[V(gcitation)$name%in%cybnames]
citingcyb=c();citedbycyb=c()
for(i in 1:length(cybnodes)){
  if(i %% 10==0){show(i)}
  citingcyb = append(citingcyb,neighbors(gcitation,v=cybnodes[i],mode="in")$name)
  citedbycyb = append(citedbycyb,neighbors(gcitation,v=cybnodes[i],mode="out")$name)
}

citingcited=c()
for(i in 1:length(citedbycyb)){ 
  if(i %% 10==0){show(i)}
  citingcited = append(citingcited,neighbors(gcitation,v=citedbycyb[i],mode="in")$name)
}

save(cybnodes,citingcyb,citedbycyb,citingcited,paste0(Sys.getenv('CS_HOME'),'/Cybergeo/Models/cybergeo20/HyperNetwork/Data/nw/citationNetworkStats.RData'))





# load(citnwoutput)


####
## Construct the semantic nw
#   mongo -> RData

relevantCollection = 'relevant.relevant_full_50000'
kwcollection = 'cybergeo.keywords'
nwcollection = 'relevant.network_full_50000_eth10'
edge_th = 50
target = 'processed/relevant_full_50000_eth50_nonfiltdico'
mongohost = '127.0.0.1:27017'
constructSemanticNetwork(relevantcollection,kwcollection,nwcollection,edge_th,target,mongohost)


####
## Sensitivity of the semantic nw

source('semsensitivity.R')

db='relevant_full_50000_eth50_nonfiltdico'
filters=c('data/filter.csv','data/french.csv')
freqmaxvals=c(5000,10000,20000)
freqminvals=c(50,75,100,125,200)
kmaxvals=seq(from=300,to=1500,by=50)
ethvals=seq(from=50,to=200,by=10)
outputfile=paste0('sensitivity/',db,'_ext_local_2.RData')

networkSensitivity(db,filters,freqmaxvals,freqminvals,kmaxvals,ethvals,outputfile)

#load('sensitivity/relevant_full_50000_eth50_nonfiltdico_ext_local.RData')
load(outputfile)
names(d)[ncol(d)-2]="balance"

library(ggplot2)
source(paste0(Sys.getenv('CN_HOME'),'/Models/Utils/R/plots.R'))
figdir=paste0(Sys.getenv('CS_HOME'),'/Cybergeo/Models/cybergeo20/HyperNetwork/Results/Networks/Sensitivity/relevant_full50000_eth50/')

freqmin=50;freqmax=10000
g = ggplot(d[d$freqmin==freqmin&d$freqmax==freqmax,]) + scale_fill_gradient(low="yellow",high="red")#+ geom_raster(hjust = 0, vjust = 0) 
plots=list()
for(indic in c("modularity","communities","vertices","balance")){#,"components","density")){
  #plots[[indic]] = g+geom_raster(aes_string("degree_max","edge_th",fill=indic))+xlab(expression(k[max]))+ylab(expression(theta[w]))#+facet_grid(freqmax~freqmin)
  g+geom_raster(aes_string("degree_max","edge_th",fill=indic))+xlab(expression(k[max]))+ylab(expression(theta[w]))+stdtheme#+facet_grid(freqmax~freqmin)
  ggsave(file=paste0(figdir,'sensitivity_',indic,'_freqmin',freqmin,'_freqmax',freqmax,'.png'),width=20,height=18,units = 'cm')
}
#multiplot(plotlist = plots,cols=2)

g = ggplot(d,aes(x=edge_th,y=modularity,color=degree_max,group=degree_max))
g+geom_line()+xlab(expression(theta[w]))+facet_grid(freqmax~freqmin)+stdtheme

g = ggplot(d,aes(x=edge_th,y=vertices,color=degree_max,group=degree_max))
g+geom_line()+xlab(expression(theta[w]))+facet_grid(freqmax~freqmin)+stdtheme

g = ggplot(d,aes(x=modularity,y=vertices,color=edge_th))
g+geom_point()+facet_grid(freqmax~freqmin)+stdtheme

g = ggplot(d[d$freqmin==50&d$freqmax==10000,],aes(x=components,y=modularity,color=degree_max))
g+geom_point()+geom_point(aes(x=d$components[d$freqmin==50&d$freqmax==10000&d$edge_th==100&d$degree_max==1200],y=d$modularity[d$freqmin==50&d$freqmax==10000&d$edge_th==100&d$degree_max==1200]),col='red')+stdtheme





# -> etablish the optimal parameters
# relevant_full_50000_eth50_nonfiltdico_kmin0_kmax1200_freqmin50_freqmax10000_eth100


######
#  export

source('semexport.R')

nkws='50000'
eth_0 = '50'
eth = '100'
kmin = '0'
kmax = '1200'
freqmin = '50'
freqmax = '10000'
eth = '100'

exportData(nkws,eth_0,eth,kmin,kmax,freqmin,freqmax,eth)






