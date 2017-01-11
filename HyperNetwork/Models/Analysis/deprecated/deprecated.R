
## deprecated functions



##
#  generic function to create and export nw
#  also compute kws dictionary (used later in originality computation)
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
  
  res=list()
  
  keyword_dico = list()
  
  cooccs = matrix(0,nrow(srel),nrow(srel))
  
  for(i in 1:nrow(dico)){
    if(i%%100==0){show(i)}
    kws=strsplit(enc2utf8(dico[i,kwCol]),";")[[1]]
    id=""
    if(kwCol==1){id=kws[1];kws=kws[-1];}else{id=dico[i,1]}
    if(length(kws)>1){
      for(k in 1:(length(kws)-1)){
        for(l in (k+1):(length(kws))){
          if(nchar(kws[k])>0&nchar(kws[l])>0){
            cooccs[rel[[kws[k]]],rel[[kws[l]]]]=cooccs[rel[[kws[k]]],rel[[kws[l]]]]+1
          }
        }
      }
    }
    keyword_dico[[id]]=kws
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
  
  res=list()
  res$g=g
  res$keyword_dico=keyword_dico
  
  return(res)
}





##
#  construct coocs graph and kw dico
computeNetwork<-function(db,target){
  mongo <- mongo.create()
  relevant <-mongo.find.all(mongo,paste0(db,'.relevant'))
  dico <- mongo.find.all(mongo,'keywords.keywords')
  relevant = data.frame(keyword=sapply(relevant,function(d){d$keyword}),cumtermhood=sapply(relevant,function(d){d$cumtermhood}))
  
  srel = as.tbl(relevant)
  srel$keyword = as.character(srel$keyword)
  srel = srel %>% arrange(desc(cumtermhood))
  
  #srel = srel[1:min(kwthreshold,nrow(srel)),]
  
  # construct relevant dico : word -> index
  rel = list()
  for(i in 1:length(srel$keyword)){rel[[srel$keyword[i]]]=i}
  
  res=list()
  
  keyword_dico = list()
  
  for(i in 1:length(dico)){
    if(i%%100==0){show(i)}
    kws = unique(dico[[i]]$keywords)
    #show(kws)
    if(length(kws)>0){
      kws = kws[sapply(kws,function(w){w %in% srel$keyword})]
      keyword_dico[[dico[[i]]$id]]=kws
    }
  }
  
  keyword_dico_keys = names(keyword_dico)
  
  cooccs = matrix(0,nrow(srel),nrow(srel))
  
  for(i in 1:length(keyword_dico)){
    if(i%%100==0){show(i)}
    #kws=strsplit(enc2utf8(dico[i,kwCol]),";")[[1]]
    #kws = dicoSplitFunction(dico[i,kwCol])
    kws = keyword_dico[[i]]
    #id=keyword_dico_keys[i]
    #if(kwCol==1){id=kws[1];kws=kws[-1];}else{id=dico[i,1]}
    if(length(kws)>1){
      for(k in 1:(length(kws)-1)){
        for(l in (k+1):(length(kws))){
          if(nchar(kws[k])>0&nchar(kws[l])>0){
            cooccs[rel[[kws[k]]],rel[[kws[l]]]]=cooccs[rel[[kws[k]]],rel[[kws[l]]]]+1
          }
        }
      }
    }
    #keyword_dico[[id]]=kws
  }
  
  colnames(cooccs) = names(unlist(rel))
  #g = graph_from_adjacency_matrix(adjacency,weighted=TRUE,mode="undirected")
  
  res$g=g
  res$keyword_dico=keyword_dico
  
  save(res,file=paste0(target,'.RData'))
  
}








################
##  Citation Network


#setwd(paste0(Sys.getenv('CS_HOME'),'/Cybergeo/cybergeo20/HyperNetwork/Data/nw'))


# issue in csv file with semicolon delimiter -> use tabulation instead.
#edges <- read.csv('provnw2_edges.csv',header=FALSE,sep="\t",colClasses=c("character","character"))
#nodes <- read.csv('provnw2_nodes.csv',header=FALSE,sep="\t",colClasses=c("character","character","numeric","numeric"),blank.lines.skip=FALSE)
#colnames(nodes)=c("name","title","year","cyb")
#colnames(edges)=c("from","to")
#vedges = unique(c(edges[,1],edges[,2]))
#vertices = merge(x=data.frame(v=vedges),y=nodes[!duplicated(nodes[,1]),],by.x=1,by.y=1,all.x=FALSE,all.y=TRUE)
#colnames(vertices)=c("name","title","year","cyb")

# filters 36 edges
#vindexes=list()
#for(i in 1:nrow(nodes)){vindexes[[as.character(nodes$name[i])]]=i-1}
#citationedges = data.frame(source=sapply(e$from,function(s){vindexes[[s]]}),target=sapply(e$to,function(s){vindexes[[s]]}),value=rep(1,nrow(e)))


#e = merge(x=edges,y=data.frame(vertices$name),by.x=1,by.y=1,all.x=FALSE,all.y=TRUE)
#e = merge(x=data.frame(e$to,e$from),y=data.frame(vertices$name),by.x=1,by.y=1,all.x=FALSE,all.y=TRUE)

# need to filter ? -> one failed ref
#length(setdiff(nodes$name,vedges))
#setdiff(vedges,nodes$name)












