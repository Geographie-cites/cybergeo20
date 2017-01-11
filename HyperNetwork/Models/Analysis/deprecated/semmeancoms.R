




edges=list()
gg=NULL
for(freqmax in c(10000,20000)){
  for(freqmin in c(100,200)){
    for(kmax in seq(from=1000,to=1500,by=500)){
      for(edge_th in seq(from=100,to=200,by=50)){
        show(paste0('kmax : ',kmax,' e_th : ',edge_th,' ; freqmin : ',freqmin,' ; freqmax : ',freqmax))
        sub = extractSubGraphCommunities(ggiant,kmin,kmax,freqmin,freqmax,edge_th);
        if(!is.null(gg)){gg = gg+sub$gg
          E(gg)$weight_1[which(is.na(E(gg)$weight_1))]=0;E(gg)$weight_2[which(is.na(E(gg)$weight_2))]=0
          E(gg)$weight = E(gg)$weight_1 + E(gg)$weight_2
        }else{gg=sub$gg}
        # for(i in 1:length(sizes(sub$com))){
        #   show(sizes(sub$com)[i])
        #   currentcom=V(sub$gg)$name[membership(sub$com)==i]
        #   show(currentcom)
        #   for(k in 1:length(currentcom)){
        #     for(l in 1:length(currentcom)){
        #       if(k!=l){
        #         k1=currentcom[k];k2=currentcom[l];linkkey=paste0(k1,";",k2)
        #         if(linkkey %in% names(edges)){edges[[linkkey]]=edges[[linkkey]]+1}else{edges[[linkkey]]=1}
        #       }
        #     }
        #   }
        # }
      }
    }
  }
}

meang = graph_from_data_frame(data.frame(matrix(unlist(strsplit(names(edges),";")),ncol=2,byrow=TRUE),weight=unlist(edges)),directed=FALSE)
meancom=cluster_louvain(meang)
