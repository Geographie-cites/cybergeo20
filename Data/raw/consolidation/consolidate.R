
setwd(paste0(Sys.getenv('CS_HOME'),'/Cybergeo/cybergeo20/Data'))

load('statsvisu//statsvisu.RData')
#write.csv(statsCyber,'prov.csv')
write.table(data.frame(statsCyber$UNIQID,statsCyber$Titre),'raw/prov_ids.csv',sep='\t',col.names = FALSE,row.names = FALSE,quote = FALSE)

raw <- read.table('raw/cybergeo_withFR.csv',sep='\t',header=TRUE,quote="\"",stringsAsFactors=FALSE)


library(dplyr)

prov<-as.tbl(statsCyber) %>% group_by(UNIQID) %>% summarise (
  Title=Titre[1],DatePublication=DatePublication[1],Rubrique=Rubrique[1],Auteur=Auteur[1],TypeDocument=TypeDocument[1],Disponibilite=Disponibilite[1],
  VisuTot = sum(NombreVisualisations),
  Visu07 = sum(NombreVisualisations[Annee==2007]),Visu08 = sum(NombreVisualisations[Annee==2008]),Visu09 = sum(NombreVisualisations[Annee==2009]),Visu10 = sum(NombreVisualisations[Annee==2010]),
  Visu11 = sum(NombreVisualisations[Annee==2011]),Visu12 = sum(NombreVisualisations[Annee==2012]),Visu13 = sum(NombreVisualisations[Annee==2013]),Visu14 = sum(NombreVisualisations[Annee==2014])
)

write.csv(prov,'raw/prov.csv')

joined = inner_join(as.tbl(raw),prov,by='Title')
dim(joined)

# -> 924 refs

which(is.na(joined$VisuTot))
data.frame(raw[118,])

sqlmerged <- read.table('raw/merged.csv',sep='\t',header=TRUE,quote="\"",stringsAsFactors=FALSE)
colnames(sqlmerged)[2]="UNIQID"

# -> 936 : 12 refs beneficied from hashconsing merging -> shit...

# remerged in a left way 
joined = left_join(as.tbl(sqlmerged),prov,by='UNIQID')


# now get citations columns
cit <- read.table('raw/cit.csv',sep='\t',header=TRUE,quote="\"",stringsAsFactors=FALSE)
colnames(cit)=c("SCHID",'numciting','numcited')

final = left_join(joined,cit,by="")

write.csv(final,'raw/cybergeo_final.csv')







