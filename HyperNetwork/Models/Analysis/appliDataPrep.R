
# data prep for shiny app

# source semexport.R

datadir=paste0(Sys.getenv('CS_HOME'),'/Cybergeo/cybergeo20/CybergeoNetworks/data/')

# PB : cybnames has 927 rows ; here 885
# with kws : 584

citation_cybergeodata<-as.tbl(cybergeo[nchar(cybergeo$SCHID)>0,])
citation_cybergeodata$authors= sapply(citation_cybergeodata$authors,function(s){substr(s,2,nchar(s))})

#kws=c();
kwcount=c()
for(ref in citation_cybergeodata$SCHID){
  effkws="";inds=c()
   if(ref%in%names(keyword_dico)){
  show(ref)
  refkws=keyword_dico[ref][[1]]
  inds=which(refkws%in%kwdf[,1])
  #if(length(inds)>1){for(i in 1:(length(inds)-1)){
  #  effkws=paste0(effkws,refkws[inds[i]],";")
  #}
  #}
  #if(length(inds)>=1){effkws=paste0(effkws,refkws[inds[length(inds)]])}
   }
  #kws=append(kws,effkws);
  kwcount=append(kwcount,length(inds))
}




# export citation nw

# shit its the same as colSum(adj)+rowSums(adj)
linknum=c()
for(ref in citation_cybergeodata$SCHID){
  show(ref)
  #show(length(neighbors(gcitation,V(gcitation)[ref],mode="all")))
  lnum=0
  if(ref %in% V(gcitation)$name){lnum=length(neighbors(gcitation,V(gcitation)[ref],mode="all"))}
  linknum=append(linknum,lnum)
}

# TODO : correct authors (first ;) OK - idem keywords ? -- not needed -> global sqlite base

citation_cybergeodata = cbind(citation_cybergeodata,linknum,kwcount)

save(citation_cybergeodata,file=paste0(datadir,'citation_cybergeodata.RData'))

citationkwthemdico=kwthemdico
citationkwfreqs=kwfreqs
save(citationkwthemdico,citationkwfreqs,file=paste0(datadir,'citation_kwthemdico.RData'))



library(RSQLite)
db = dbConnect(SQLite(),paste0(datadir,"CitationNetwork.sqlite3"))

# write attributes in edgelist to avoid double lookup when loading data
#  (optimizing time performance at the price of disk memory size)
from=head_of(gcitation,E(gcitation));to=tail_of(gcitation,E(gcitation))
edges = data.frame(
  from=from$name,to=to$name,
  fromtitle=from$title,totitle=to$title,
  fromyear=from$year,toyear=to$year,
  fromcyb=from$cyb,tocyb=to$cyb
)

dbWriteTable(db,"edges",edges)


## export kw reduced dico as sqlite

citationdbkws = dbConnect(SQLite(),"data/CitationKeywords.sqlite3")

redkws = sapply(keyword_dico,function(l){
  s=Reduce(function(s1,s2){paste0(s1,";",s2)},l[l%in%ckws])
  if(is.null(s)){return("")}else{return(s)}
})

keywords=data.frame(id=names(keyword_dico),keywords=unlist(redkws))
dbWriteTable(citationdbkws,"keywords",keywords)

######
#

#svgs=""
#rows=scan('data/synththemcyb.svg',what = "character")
#for(l in rows){svgs=paste0(svgs,l)}

#library(svglite)

#svgPanZoom('data/semantic.svg')

