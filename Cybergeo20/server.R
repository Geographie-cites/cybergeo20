##############################
# Shiny App: Cybergeo20
# Server
##############################


# load data ----

load("data/CyberData.RData")
articles = data.frame()


# set server ----

shinyServer(function(input, output, session) {
  
  ### CLEM ----
  
  yearValues <- reactiveValues(years= NULL)
  
  observe({
    if (length(input$dateRange) == 1) {
      yearValues$years = input$dateRange
    } else {
      dates = input$dateRange
      yearValues$years = dates[[1]] : dates[[length(dates)]]
    }
  })
  
  subsetArticles <- reactive({
    allArticles <- cyberData$ARTICLES
    years = yearValues$years
    articles = allArticles[substr(allArticles$date.1, 1, 4) %in% as.character(years),]
    return(articles)   
  })
  
  output$statArticles = renderDataTable({
    tab = data.frame()
    articlesDF = subsetArticles()
    nPapers = dim(articlesDF)[1]
    
    tab[1,1] = "Number of scientific articles"
    tab[1,2] = nPapers
    
    articlesDF$authorsLists = strsplit(articlesDF$authors, split = ",")
    for (i in 1:dim(articlesDF)[1]) {
      articlesDF[i,"Nauthors"] = ifelse(is.na(articlesDF[i,"authorsLists"]), 1, length(articlesDF[i,"authorsLists"][[1]]))
    }
    Nauthors = sum(articlesDF$Nauthors)
    
    tab[2,1] = "Number of authors"
    tab[2,2] = Nauthors
    
    colnames(tab) = c("Indicator", "Value")
    return(tab)
  }, options = list(paging = FALSE, searching = FALSE))
  
  
  output$statAuthoring = renderDataTable({
    tab = data.frame()
    articlesDF = subsetArticles()
    nPapers = dim(articlesDF)[1]
    
    tab[1,1] = "Number of scientific articles"
    tab[1,2] = nPapers
    
    articlesDF$authorsLists = strsplit(articlesDF$authors, split = ",")
    for (i in 1:dim(articlesDF)[1]) {
      articlesDF[i,"Nauthors"] = ifelse(is.na(articlesDF[i,"authorsLists"]), 1, length(articlesDF[i,"authorsLists"][[1]]))
    }
    Nauthors = sum(articlesDF$Nauthors)
    
    tab[2,1] = "Number of authors"
    tab[2,2] = Nauthors
    
    colnames(tab) = c("Indicator", "Value")
    return(tab)
  }, options = list(paging = FALSE, searching = FALSE))
  
  output$Npapers = renderText({
    articlesDF = subsetArticles()
    nPapers = dim(articlesDF)[1]
    return(nPapers)
  })
  output$Nauthors = renderText({
    articlesDF = subsetArticles()
    articlesDF$authorsLists = strsplit(articlesDF$authors, split = ",")
    for (i in 1:dim(articlesDF)[1]) {
      articlesDF[i,"Nauthors"] = ifelse(is.na(articlesDF[i,"authorsLists"]), 1, length(articlesDF[i,"authorsLists"][[1]]))
    }
    Nauthors = sum(articlesDF$Nauthors)
    return(Nauthors)
  })
  
  
  
  
  
  ### Juste ----
  
  
  #output$citationNetwork <- renderForceNetwork({
  #  forceNetwork(Links = edf, Nodes = vdf,
  #               Source = "source", Target = "target",
  #               Value = "value", NodeID = "name",
  #               Group = "community", opacity = 0.8,zoom=TRUE)
  #  
  #})
  
  
  output$semanticNetwork <- renderForceNetwork({
    forceNetwork(Links = edf, Nodes = vdf,
                 Source = "source", Target = "target",
                 Value = "value", NodeID = "name",
                 Group = "community", opacity = 0.8,zoom=TRUE)
 
   })
  
  
  
  
  
  
  ### HADRI ----
  
  # select level and other variables
  SelectYearRes <- reactive({
    fullGraph <- cyberData$NETKW
    return(fullGraph)
  })
  
  # select level and other variables
  SelectYearCom <- reactive({
    fullGraph <- cyberData$NETKW
    commId <- sort(unique(V(fullGraph)$clus))
    updateSelectInput(session = session, inputId = "commid", choices = commId)
    return(fullGraph)
  })
  
  # select level and other variables
  SelectYearSem <- reactive({
    fullGraph <- cyberData$NETKW
    kwId <- sort(unique(V(fullGraph)$name))
    updateSelectInput(session = session, inputId = "kwid2", choices = kwId)
    return(fullGraph)
  })
  
  
  # create information table for nodes
  InfoTableNodes <- reactive({
    g <- SelectYearRes()
    infoNodes <- data.frame(KEYWORDS = V(g)$name, NB_ARTICLES = V(g)$nbauth, DEGREE = V(g)$degbeg)
    return(infoNodes)
  })
  
  # create information table for edges
  InfoTableEdges <- reactive({
    g <- SelectYearRes()
    infoEdges <- get.data.frame(g)
    tabEdges <- data.frame(KEYWORD1 = infoEdges[ , 1],
                           KEYWORD2 = infoEdges[ , 2],
                           OBSERVED_WEIGHT = infoEdges[ , 3],
                           EXPECTED_WEIGHT = round(infoEdges[ , 4], 2),
                           RESIDUALS = round(infoEdges[ , 5], 2))
    return(tabEdges)
  })
  
  # select community
  SelectComm <- reactive({
    fullGraph <- SelectYearCom()
    commSubgraph <- induced.subgraph(fullGraph, vids=V(fullGraph)[V(fullGraph)$clus == input$commid])
  })
  
  # create semantic field
  SelectSemField <- reactive({
    fullGraph <- SelectYearSem()
    SemSubgraph <- SemanticField(fullGraph, kw = input$kwid2)
  })
  
  
  # outputs ----
  
  # panel "Data summary"
  
  output$textfull <- renderText({
    summarytext <- paste("<strong>Description :</strong> The network has <strong>",
                         vcount(SelectYearRes()), 
                         " vertices</strong> (number of keywords) and <strong>",
                         ecount(SelectYearRes()),
                         " edges</strong> (number of edges between keywords).",
                         sep = "")
  })
  
  output$contentsnodes <- renderDataTable({
    InfoTableNodes()
  })
  
  output$contentsedges <- renderDataTable({
    InfoTableEdges()
  })
  
  
  # panel "Communities"
  
  output$plotcomm <- renderPlot({
    if(input$vsizecom == "uni" && input$esizecom == "rel"){
      vertsize <- 1
      edgesize <- E(SelectComm())$relresid
    } else if(input$vsizecom == "uni" && input$esizecom == "nbl"){
      vertsize <- 1
      edgesize <- E(SelectComm())$obsfreq
    } else if(input$vsizecom == "poi" && input$esizecom == "rel"){
      vertsize <- V(SelectComm())$nbauth
      edgesize <- E(SelectComm())$relresid
    } else if(input$vsizecom == "poi" && input$esizecom == "nbl"){
      vertsize <- V(SelectComm())$nbauth
      edgesize <- E(SelectComm())$obsfreq
    } else if(input$vsizecom == "deg" && input$esizecom == "rel"){
      vertsize <- V(SelectComm())$degbeg
      edgesize <- E(SelectComm())$relresid
    } else if(input$vsizecom == "deg" && input$esizecom == "nbl"){
      vertsize <- V(SelectComm())$degbeg
      edgesize <- E(SelectComm())$obsfreq
    }
    
    VisuComm(SelectComm(), 
             comm = input$commid, 
             vertcol = "#2b3e50", 
             vertsize = vertsize, 
             vfacsize = input$vfacsizecom,
             edgesize = edgesize,
             efacsize = input$efacsizecom, 
             textsize = input$tsizecom)
    
  })
  
  output$downcomm <- downloadHandler(
    filename = "Community.svg",
    content = function(file) {
      if(input$vsizecom == "uni" && input$esizecom == "rel"){
        vertsize <- 30
        edgesize <- E(SelectComm())$relresid
      } else if(input$vsizecom == "uni" && input$esizecom == "nbl"){
        vertsize <- 30
        edgesize <- E(SelectComm())$obsfreq
      } else if(input$vsizecom == "poi" && input$esizecom == "rel"){
        vertsize <- V(SelectComm())$nbauth
        edgesize <- E(SelectComm())$relresid
      } else if(input$vsizecom == "poi" && input$esizecom == "nbl"){
        vertsize <- V(SelectComm())$nbauth
        edgesize <- E(SelectComm())$obsfreq
      } else if(input$vsizecom == "deg" && input$esizecom == "rel"){
        vertsize <- V(SelectComm())$degbeg
        edgesize <- E(SelectComm())$relresid
      } else if(input$vsizecom == "deg" && input$esizecom == "nbl"){
        vertsize <- V(SelectComm())$degbeg
        edgesize <- E(SelectComm())$obsfreq
      }
      
      svg(file, width = 20 / 2.54, height = 20 / 2.54, pointsize = 8)
      VisuComm(SelectComm(),
               comm = input$commid, 
               vertcol = "#2b3e50", 
               vertsize = vertsize, 
               vfacsize = input$vfacsizecom,
               edgesize = edgesize,
               efacsize = input$efacsizecom, 
               textsize = input$tsizecom)
      dev.off()
    })
  
  
  # panel "Semantic area"
  
  output$plotsem <- renderPlot({
    VisuSem(SelectSemField(), kw = input$kwid2, textsizemin = input$tsizesemmin, textsizemax = input$tsizesemmax)
  })
  
  output$downsem <- downloadHandler(
    filename = "Semantic.svg",
    content = function(file) {
      svg(file, width = 20 / 2.54, height = 20 / 2.54, pointsize = 8)
      VisuSem(SelectSemField(), kw = input$kwid2, textsizemin = input$tsizesemmin, textsizemax = input$tsizesemmax)
      dev.off()
    })
  
  
})

