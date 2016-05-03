

setwd(paste0(Sys.getenv('CS_HOME'),'/Cybergeo/cybergeo20/HyperNetwork/Models/Analysis'))

# export thematics for Clem mappping

list.files("probas")
list.files("export/comunitiesnames")

db='relevant_full_50000_eth50_nonfiltdico'
dbparams = 'relevant_full_50000_eth50_nonfiltdico_kmin0_kmax1200_freqmin50_freqmax10000_eth100'

load(paste0('probas/',dbparams,'.RData'))
load(paste0('processed/',db,'.RData'))
keyword_dico=res$keyword_dico;g=res$g;rm(res);gc()
them_probas = probas

# define comunities names
# com
thematics = communities(sub$com)
thematics[[1]]
# define names by hand

themnames = as.character(read.csv(file=paste0('export/comunitiesnames/',dbparams,'.csv'),header=FALSE,stringsAsFactors = FALSE)[,1])

names(thematics)<-themnames

# construct kws df
kws=c()
for(i in 1:length(thematics)){
  if(!is.na(names(thematics)[i])){
    for(kw in thematics[[i]]){
      show(c(kw,names(thematics)[i]))
      kws=append(kws,c(kw,names(thematics)[i]))
    }
  }
}

# load them probas
#  -> precomputed in semthem_probas

# select existing thematics
export_probas = them_probas[,!is.na(names(thematics))]
colnames(export_probas) = names(thematics)[!is.na(names(thematics))]

# need iscyb and cybindexes
# -> load from consolidated db
#export_probas = cbind(data.frame(export_probas),as.character(names(keyword_dico)))
#colnames(export_probas)[13]="ID"
export_probas = cbind(cybid,data.frame(export_probas))
names(export_probas)[1] = "CYBERGEOID"

cybprobas = as.tbl(export_probas[export_probas$CYBERGEOID>0,])
cybprobas[cybprobas$cognitive.sciences>0.2,]
intersect(keyword_dico[[cybergeo$SCHID[cybergeo$id==4994]]],thematics[['cognitive sciences']])

#res = left_join(as.tbl(export_probas),as.tbl(cybergeo),by=c("ID","SCHID"))
# export into dbparams
exdir=paste0('export/',dbparams)
dir.create(exdir)

write.table(export_probas,col.names = TRUE,row.names = FALSE,file = paste0(exdir,'/docprobas.csv'),sep=",")
write.table(data.frame(matrix(kws,ncol=2,byrow=TRUE)),col.names = FALSE,row.names = FALSE,file = paste0(exdir,'/thematics.csv'),sep=",")



