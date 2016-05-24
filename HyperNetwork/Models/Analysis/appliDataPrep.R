
# data prep for shiny app

# source semexport.R

# PB : cybnames has 927 rows ; here 885
# with kws : 584

citation_cybergeodata<-as.tbl(cybergeo[nchar(cybergeo$SCHID)>0,])

save(citation_cybergeodata,file=paste0(Sys.getenv('CS_HOME'),'/Cybergeo/cybergeo20/Cybergeo20/data/citation_cybergeodata.RData'))

