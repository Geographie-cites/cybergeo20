
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

final = left_join(joined,as.tbl(cit),by="SCHID")
res=final[,c(1,3:11,15:30)]
colnames(res)[2:4]=c("schid","title","title_en")
colnames(res)[25:26]=c("citedby","citing")

write.csv(res,'raw/cybergeo_final.csv',row.names=FALSE)

cybergeo=read.csv('raw/cybergeo.csv',header=TRUE)
cited=cybergeo$citedby
mean(cited,na.rm=TRUE)
sd(cited,na.rm=TRUE)
cited[is.na(cited)]=0
mean(cited)
1-length(which(is.na(cybergeo$citedby)))/length(cybergeo$citedby)


#
# bootstrap with distribution to have an estimate of mean with IC ?
# -> missing data as mean ~ draw missing data from distrib ?
#

# Treatment mean imputation
#cited[is.na(cited)]=mean(cited,na.rm=TRUE)
#mean(cited)
# IDIOT !
# idem if draws from distrib.

cited=cybergeo$citedby
cited=cited[!is.na(cited)]
s=0;count=0;for(i in 1:(length(cited)-1)){for(j in (i+1):length(cited)){s=s+(cited[i]-cited[j])^2;count=count+1}}
sqrt((s/count)/(length(cited)*(length(cybergeo$cited)-length(cited))))

##
N=length(cited)
vars=c();ths=c()
Ks=seq(from=0.5,to=1,by=0.02)*N
for(K in Ks){
  show(K)
B=50000
v=0;th=0
for(b in 1:B){
  s=cited[sample.int(n = N,size = K,replace=FALSE)]
  v=v+((mean(cited)-mean(s))^2)
  th=th+(1.96*sd(s))/sqrt(K)
}
vars=append(vars,v/B);ths=append(ths,th/B)
}
plot(Ks,vars);
plot(Ks,ths,col='red')
plot(1/sqrt(Ks),vars)

summary(lm(y~x,data.frame(x=1/sqrt(Ks),y=vars)))



