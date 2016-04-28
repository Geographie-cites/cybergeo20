
# full semantic nw construction


setwd(paste0(Sys.getenv('CS_HOME'),'/Cybergeo/cybergeo20/HyperNetwork/Models/Analysis'))
library(dplyr)
library(igraph)

source('networkConstruction.R')

importNetwork('relevant.relevant_full_50000','cybergeo.keywords','relevant.network_full_50000_eth10',50,'processed/relevant_full_50000_eth50_nonfiltdico')

