
#  functions for netowrk export


importDicoCsv<-function(kwFile){
  res=list()
  relevant = read.table(paste0("../Semantic/res/cybergeo/kw_",kwFile,".csv"),header=FALSE,sep=";",stringsAsFactors = FALSE)
  colnames(relevant)=c("keyword","cumtermhood")
  dico = scan(paste0("../Semantic/res/cybergeo/relevantDico_kwLimit",kwFile,".csv"),what="character",sep="\n")
  relevant$keyword=sapply(relevant$keyword,FUN=enc2utf8)
  res$relevant=relevant
  res$dico=dico
  return(res)
}




exportNetwork<-function(data,kwthreshold = 2000,linkthreshold =15,connex=TRUE,export=FALSE,exportPrefix="",filterFile="",kwFile="2000"){
  
  relevant = data$relevant
  dico = data$dico
  # dico can be of two types : output of a scanned pseudo-csv or structured output of sqlite table
  # must take that into account
  kwCol=2
  if(is.null(dim(dico))){dico=data.frame(keywords=dico);kwCol=1}
  # dirty as keep id in kws, but needed for perf to not split strings twice
  
  srel = as.tbl(relevant) %>% arrange(desc(cumtermhood))
  if(filterFile!=""){
    forbidden = read.csv(filterFile)
    srel = srel %>% filter(!(keyword %in% forbidden))
  }
  
  
  srel = srel[1:min(kwthreshold,nrow(srel)),]
  
  # construct relevant dico : word -> index
  rel = list()
  for(i in 1:length(srel$keyword)){rel[[srel$keyword[i]]]=i}
  
  cooccs = matrix(0,nrow(srel),nrow(srel))
  
  for(i in 1:nrow(dico)){
    if(i%%100==0){show(i)}
    kws=strsplit(enc2utf8(dico[i,kwCol]),";")[[1]]
    if(kwCol==1){kws=kws[-1]}
    if(length(kws)>1){
      for(k in 1:(length(kws)-1)){
        for(l in (k+1):(length(kws))){
          if(nchar(kws[k])>0&nchar(kws[l])>0){
            cooccs[rel[[kws[k]]],rel[[kws[l]]]]=cooccs[rel[[kws[k]]],rel[[kws[l]]]]+1
          }
        }
      }
    }
  }
  
  colnames(cooccs) = names(unlist(rel))
  # filter edges
  adjacency=cooccs;adjacency[adjacency<linkthreshold]=0
  g = graph_from_adjacency_matrix(adjacency,weighted=TRUE,mode="undirected")
  
  
  # keep giant component
  if(connex==TRUE){
    clust = clusters(g);cmax = which(clust$csize==max(clust$csize))
    g = induced.subgraph(g,which(clust$membership==cmax))
  }
  
  if(export==TRUE){
    if(kwCol==1){
    filename=paste0(exportPrefix,"_kw",kwFile,"_kwth",kwthreshold,"_th",linkthreshold,"_connex",connex)
    }else{
      filename=paste0(exportPrefix,"_kwth",kwthreshold,"_th",linkthreshold,"_connex",connex)
    }
    if(filterFile!=""){filename=paste0(filename,"_filtered")}
    filename=paste0(filename,".gml")
    write.graph(g,filename,"gml")
  }
  
  return(g)
}


