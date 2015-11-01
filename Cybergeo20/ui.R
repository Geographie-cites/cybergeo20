library(shiny)

shinyUI(fluidPage(theme = "darkBlue.css",
  #tags$head(tags$link(rel="shortcut icon", href="favicon.png")),
  
  tags$head(
    tags$style(HTML("
      @import url('//fonts.googleapis.com/css?family=Orbitron|Cabin:400,700');
    "))
  ),
  headerPanel(
    h1("Cybergeo | 1996-2016", 
       style = "font-family: 'Orbitron', sans-serif;
        font-weight: 500; line-height: 1.1; 
        color: #ffffff;")),
  
  
 # titlePanel(""),
  titlePanel(h4("  20 years of Cybergeo, European Journal of Geography")),
  
  navlistPanel(
    #"Looking back",
    tabPanel("The Project",
             fluidRow(
               column(3, img(src = "favicon.png",class="img-responsive")),
               column(9, h2("20 Years of Cybergeo"),
               tags$p(class="text-justify",
              "Cybergeo turns 20 this years. It's time to look back ... 
              [blablabla]", br(), 
              "This app aims at reviewing the evolution of geography as revealed by 20 years of publication in Cybergeo.")
               ))
             )
    )
                             
  
))