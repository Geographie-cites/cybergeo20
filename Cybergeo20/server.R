##############################
# Shiny App: Cybergeo20
# Server
##############################


# load data ----

load("data/CyberData.RData")
load("data/themesPO.RData")
files$name = NULL
files$path = NULL
files[,3:22] = document.themes 
colnames(files)[3:22] = paste0("T_", 1:20)
themeDescription = read.csv("data/20themes20words.csv", sep=",", dec=".")
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
    articles = merge(articles, files, by = "id" , all.x = T, all.y = F)
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
    
     cols = colnames(articlesDF)
    authoring = cols[substr(cols,1, 2) == "A_"]
    sumsAByCountry = colSums(articlesDF[,authoring])
    authoringCountries = sumsAByCountry[sumsAByCountry>0]
    nAuthoringCountries = length(authoringCountries)
    
    tab[3,1] = "Number of countries authoring"
    tab[3,2] = nAuthoringCountries
    
    names(authoringCountries) = substr(names(authoringCountries), 3, 4)
    AC = sort(authoringCountries, decreasing = T)
    AC5 = paste0(names(AC[1]), " (", AC[1], "), ", names(AC[2]), " (",  AC[2], "), ", 
                 names(AC[3]), " (", AC[3], "), ", names(AC[4]), " (",  AC[4], "), ", 
                 names(AC[4]), " (", AC[4], ")")
    
    tab[4,1] = "Top countries authoring"
    tab[4,2] = AC5
    
    studied = cols[substr(cols,1, 2) == "S_"]
    sumsSByCountry = colSums(articlesDF[,studied])
    studiedCountries = sumsSByCountry[sumsSByCountry>0]
    nStudiedCountries = length(studiedCountries)
    
    tab[5,1] = "Number of countries studied"
    tab[5,2] = nStudiedCountries
    
    names(studiedCountries) = substr(names(studiedCountries), 3, 4)
    SC = sort(studiedCountries, decreasing = T)
    SC5 = paste0(names(SC[1]), " (", SC[1], "), ", names(SC[2]), " (",  SC[2], "), ", 
                 names(SC[3]), " (", SC[3], "), ", names(SC[4]), " (",  SC[4], "), ", 
                 names(SC[4]), " (", SC[4], ")")
    
    tab[6,1] = "Top countries studied"
    tab[6,2] = SC5
    
    sumsCitations = colSums(articlesDF[,c("citedby", "citing")], na.rm = T)
    
    tab[7,1] = "Number of citations from other articles"
    tab[7,2] = as.numeric(sumsCitations[1])
    tab[8,1] = "Number of citations of other articles"
    tab[8,2] = as.numeric(sumsCitations[2])
    
    
   themes =  paste0("T_", 1:20)
   sumsByTheme = colSums(articlesDF[,themes], na.rm = T)
   names(sumsByTheme) = 
   sortedThemes = sort(sumsByTheme, decreasing = T)
   topTheme = themeDescription[as.numeric(substr(names(sortedThemes)[1], 3, 3)),2]
  
   tab[9,1] = "Top theme described with 20 words"
   tab[9,2] = as.character(topTheme)
   
    colnames(tab) = c("Indicator", "Value")
    
    
    return(tab)
  }, options = list(paging = FALSE, searching = FALSE))
  
  # 
  # output$statAuthoring = renderDataTable({
  #   tab = data.frame()
  #   articlesDF = subsetArticles()
  #   nPapers = dim(articlesDF)[1]
  #   
  #   tab[1,1] = "Number of scientific articles"
  #   tab[1,2] = nPapers
  #   
  #   articlesDF$authorsLists = strsplit(articlesDF$authors, split = ",")
  #   for (i in 1:dim(articlesDF)[1]) {
  #     articlesDF[i,"Nauthors"] = ifelse(is.na(articlesDF[i,"authorsLists"]), 1, length(articlesDF[i,"authorsLists"][[1]]))
  #   }
  #   Nauthors = sum(articlesDF$Nauthors)
  #   
  #   tab[2,1] = "Number of authors"
  #   tab[2,2] = Nauthors
  #   
  #   colnames(tab) = c("Indicator", "Value")
  #   
  #     
  #   return(tab)
  # }, options = list(paging = FALSE, searching = FALSE))
  # 
  # output$Npapers = renderText({
  #   articlesDF = subsetArticles()
  #   nPapers = dim(articlesDF)[1]
  #   return(nPapers)
  # })
  # output$Nauthors = renderText({
  #   articlesDF = subsetArticles()
  #   articlesDF$authorsLists = strsplit(articlesDF$authors, split = ",")
  #   for (i in 1:dim(articlesDF)[1]) {
  #     articlesDF[i,"Nauthors"] = ifelse(is.na(articlesDF[i,"authorsLists"]), 1, length(articlesDF[i,"authorsLists"][[1]]))
  #   }
  #   Nauthors = sum(articlesDF$Nauthors)
  #   return(Nauthors)
  # })
  # 
  # 
  # 
  
  
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

