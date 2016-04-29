
# export thematics for Clem mappping


# define comunities names
# com
thematics = communities(sub$com)
# define names by hand
# names(thematics) =c("climate","NA","socio-economic","NA","agriculture","commerce","NA","NA","health","NA","NA",
#                      "biology","communication/politics","spatial analysis","NA","GIS","biogeography","physical geography"
#                      )

names(thematics) = c("NA","NA","GIS","NA","socio-economic","NA","agriculture",
                     "NA","health","NA","communication/politics","biology","spatial analysis",
                     "NA","neuroscience","biogeography","NA","climate","commerce","physical geography"
                     )

# construct kws df
kws=c()
for(i in 1:length(thematics)){
  if(names(thematics)[i]!="NA"){
    for(kw in thematics[[i]]){
      show(c(kw,names(thematics)[i]))
      kws=append(kws,c(kw,names(thematics)[i]))
    }
  }
}

# load them probas
#  -> precomputed in semthem_probas

# select existing thematics
export_probas = them_probas[,names(thematics)!="NA"]
colnames(export_probas) = names(thematics)[names(thematics)!="NA"]

# need iscyb and cybindexes
# -> load from consolidated db
#export_probas = cbind(data.frame(export_probas),as.character(names(keyword_dico)))
#colnames(export_probas)[13]="ID"
export_probas = cbind(cybid,data.frame(export_probas))
names(export_probas)[1] = "CYBERGEOID"

#res = left_join(as.tbl(export_probas),as.tbl(cybergeo),by=c("ID","SCHID"))
# export into dbparams
exdir=paste0('export/',dbparams)
dir.create(exdir)

write.table(export_probas,col.names = TRUE,row.names = FALSE,file = paste0(exdir,'/docprobas.csv'),sep=",")
write.table(data.frame(matrix(kws,ncol=2,byrow=TRUE)),col.names = FALSE,row.names = FALSE,file = paste0(exdir,'/thematics.csv'),sep=",")



