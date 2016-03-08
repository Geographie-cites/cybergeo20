library(shiny)

shinyUI(fluidPage(theme = "darkBlue.css",
  #tags$head(tags$link(rel="shortcut icon", href="favicon.png")),
  
  tags$head(
    tags$style(HTML("
      @import url('//fonts.googleapis.com/css?family=Orbitron|Cabin:400,700');
    "))
  ),
  headerPanel(
    h1("Cybergeo  |  1996-2016", 
       style = "font-family: 'Orbitron', sans-serif;
        font-weight: 500; line-height: 1.1; 
        color: #ffffff;")),
  titlePanel(h4("20 years of Cybergeo, European Journal of Geography")),
  
  navlistPanel(
    #"Looking back",
    tabPanel("The Project",
             fluidRow(
               
               column(9, h2("20 Years of Cybergeo"),
               tags$p(class="text-justify",
              "Cybergeo turns 20: It's time to look back ... ", br(), "[blablabla]", br())) ,
              column(3, img(src = "favicon.png",class="img-responsive")),
              column(12,h4("The journal philosophy"), 
              "[blablabla]", br(), 
               h4("An app to look back"), "This app aims at reviewing the evolution of geography as revealed by 20 years of publication in Cybergeo.")
               )
             ),
    "-----",
    tabPanel("Global Stats",
             fluidRow(h2("Global Stats"),
                      column(12, sliderInput("dateRange", label = "Time Range",
                                            min = 1996, max = 2015, value = c(1996,2015), step = 1)),
                      column(12, dataTableOutput('statArticles')),
                      div(dataTableOutput("tableName"), style = "font-size:120%")
                      #h4("Number of papers / Authors / Editos / Reviews",br()), 
                      
                       #  h4("Number of readers / countries / citations",br()), 
                        #    h4("Number of staff")
             )),
    
    tabPanel("Citation network",
             fluidRow(h2("Exploring the citation network"),
                      h4("...",br()))
    )
   ,
       
  "-----",
  tabPanel("About",
           fluidRow(
             
             column(9, h2("About Cybergeo"),
                    a("http://cybergeo.revues.org/",href="http://cybergeo.revues.org/"), br(),
                    "[blablabla]", br()),
             column(3, img(src = "favicon.png",class="img-responsive")),
             column(12,h4("The Cybergeo20 team"), 
                    "[blablabla]", br())
             )
  )
  
  )
))