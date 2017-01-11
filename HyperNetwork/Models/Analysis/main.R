
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


