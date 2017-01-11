
##

setwd(paste0(Sys.getenv('CS_HOME'),'/Cybergeo/cybergeo20/HyperNetwork/Models/Analysis'))

source('networkConstruction.R')
source('citationNWConstruction.R')




#####
## Construct the citation nw from raw data
#  csv -> RData
citnwedgefile = paste0(Sys.getenv('CS_HOME'),'/Cybergeo/cybergeo20/HyperNetwork/Data/nw/full_edges.csv')
citnwnodefile = paste0(Sys.getenv('CS_HOME'),'/Cybergeo/cybergeo20/HyperNetwork/Data/nw/full_nodes.csv')
citnwoutput=paste0(Sys.getenv('CS_HOME'),'/Cybergeo/cybergeo20/HyperNetwork/Data/nw/citationNetwork.RData')

constructCitationNetwork(citnwedgefile,citnwnodefile,citnwoutput)



####
## Construct the semantic nw
#   mongo -> RData

importNetwork('relevant.relevant_full_50000','cybergeo.keywords','relevant.network_full_50000_eth10',50,'processed/relevant_full_50000_eth50_nonfiltdico','127.0.0.1:27017')



####
## Sensitivity of the semantic nw

source('semsensitivity.R')

db='relevant_full_50000_eth50_nonfiltdico'
filters=c('data/filter.csv','data/french.csv')
freqmaxvals=c(5000,10000,20000)
freqminvals=c(50,75,100,125,200)
kmaxvals=seq(from=300,to=1500,by=50)
ethvals=seq(from=140,to=300,by=20)
outputfile=paste0('sensitivity/',db,'_ext_local.RData')

networkSensitivity(db,filters,freqmaxvals,freqminvals,kmaxvals,ethvals,outputfile)

load('sensitivity/relevant_full_50000_eth50_nonfiltdico_ext_local.RData')
names(d)[ncol(d)-2]="balance"
g = ggplot(d) + scale_fill_gradient(low="yellow",high="red")#+ geom_raster(hjust = 0, vjust = 0) 
plots=list()
for(indic in c("modularity","communities","components","vertices","density","balance")){
  plots[[indic]] = g+geom_raster(aes_string("degree_max","edge_th",fill=indic))+facet_grid(freqmax~freqmin)
}
multiplot(plotlist = plots,cols=3)


# -> etablish the optimal parameters
# relevant_full_50000_eth50_nonfiltdico_kmin0_kmax1200_freqmin50_freqmax10000_eth100


######
# 





