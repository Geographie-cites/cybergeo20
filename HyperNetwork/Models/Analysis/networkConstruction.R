
#  functions for netowrk export

exportNetwork<-function(kwFile,kwthreshold = 2000,linkthreshold =15,connex=TRUE,export=FALSE){
  relevant = read.table(paste0("../Semantic/res/cybergeo/kw_",kwFile,".csv"),header=FALSE,sep=";",stringsAsFactors = FALSE)
  colnames(relevant)=c("keyword","cumtermhood")
  dico = scan(paste0("../Semantic/res/cybergeo/relevantDico_kwLimit",kwFile,".csv"),what="character",sep="\n")
  relevant$keyword=sapply(relevant$keyword,FUN=enc2utf8)
  
  srel = as.tbl(relevant) %>% arrange(desc(cumtermhood))
  
  
  srel = srel[1:kwthreshold,]
  
  # construct relevant dico : word -> index
  rel = list()
  for(i in 1:length(srel$keyword)){rel[[srel$keyword[i]]]=i}
  
  cooccs = matrix(0,nrow(srel),nrow(srel))
  
  for(i in 1:length(dico)){#nrow(dico)){
    if(i%%100==0){show(i)}
    #kws = strsplit(dico[i,2],";")[[1]]
    kws=strsplit(enc2utf8(dico[i]),";")[[1]][-1]
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
  
  if(export==TRUE){write.graph(g,paste0("graphs/cybergeo_kw",kwFile,"_kwth",kwthreshold,"_th",linkthreshold,"_connex",connex,".gml"),"gml")}
  
  return(g)
}


