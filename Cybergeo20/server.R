library(shiny)
library(rgdal)
library(plyr)
library(rgdal) 
library(dplyr) 
library(mapproj) 
library(maptools) 
library(RColorBrewer)
library(RCurl)
library(ggplot2)
library(reshape2)
library(grid)

AllArticles = read.csv("data/articles_Contingency.csv", sep=",", dec=".", encoding="UTF-8")
AllArticles$authors = as.character(AllArticles$authors)
articles = data.frame()

shinyServer(function(input, output) {
  
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
    allArticles <- AllArticles
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
})