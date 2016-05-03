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
                    tabPanel("Overview",
                             fluidRow(h2("Global Stats"),
                                      column(6, sliderInput("dateRange", label = "Time Range",
                                                             min = 1996, max = 2015, value = c(1996,2015), step = 1), animate=T),
                                      column(6, selectInput("whatMapped", label = "Indicator to map",
                                                            choices=c("Authoring countries" = "A", 
                                                                      "Countries Studied"= "S",  
                                                                      "Countries Studies by Locals"= "L"), multiple=F)),
                                      column(12, plotOutput("cybMap")),
                                      column(12, dataTableOutput('statArticles')),
                                      div(dataTableOutput("tableName"), style = "font-size:120%")
                             )),
                    
                   
                    
                    # JUSTE ----
                    
                    tabPanel("Citation network",
                             fluidRow(h2("Exploring the citation network"),
                                      #h4("...",br())
                                      forceNetworkOutput("citationNetwork")
                             )
                    ),
                    
                    
                    tabPanel("Semantic network",
                             fluidRow(h2("Exploring the semantic network"),
                                      #h4("...",br())
                                      forceNetworkOutput("semanticNetwork")
                                      )
                    ),
                   
                    # HADRI ----
                    
                    tabPanel("Keyword network",
                             
                             tabsetPanel(
                               
                               tabPanel("Data summary",
                                        fluidRow(
                                          htmlOutput("textfull"),
                                          tags$hr(),
                                          dataTableOutput("contentsnodes"),
                                          dataTableOutput("contentsedges")
                                        )),
                               
                               tabPanel("Communities",
                                        fluidRow(
                                          column(8, selectInput(inputId = "commid", 
                                                                label = "Choose a community", 
                                                                choices = "",
                                                                selected = "",
                                                                multiple = FALSE))),
                                        fluidRow(
                                          column(12, 
                                                 fluidRow(
                                                   column(3, wellPanel(
                                                     tags$strong("Graphic options"),
                                                     radioButtons("vsizecom", "Nodes proportional to:",
                                                                  list("Uniforme" = "uni",
                                                                       "Number of articles" = "poi",
                                                                       "Nodes degree" = "deg")),
                                                     radioButtons("esizecom", "Edges proportional to:",
                                                                  list("Observed weight" = "nbl",
                                                                       "Residuals" = "rel")),
                                                     sliderInput(inputId = "vfacsizecom", 
                                                                 label = "Nodes size", 
                                                                 min = 0, 
                                                                 max = 1, 
                                                                 value = 0.5, 
                                                                 step = 0.05),
                                                     sliderInput(inputId = "efacsizecom", 
                                                                 label = "Edges size", 
                                                                 min = 0, 
                                                                 max = 2, 
                                                                 value = 1, 
                                                                 step = 0.1),
                                                     sliderInput(inputId = "tsizecom",
                                                                 label = "Font size", 
                                                                 min = 1, 
                                                                 max = 15, 
                                                                 value = 10, 
                                                                 step = 1),
                                                     downloadButton("downcomm", "Download plot"))),
                                                   
                                                   column(9, plotOutput("plotcomm", width = "100%", height = "800px")))))
                               ),
                               
                               
                               tabPanel("Semantic area",
                                        fluidRow(
                                          column(8, selectInput(inputId = "kwid2", 
                                                                label = "Choose a keyword", 
                                                                choices = "", 
                                                                selected = "", 
                                                                multiple = FALSE))),
                                        fluidRow(
                                          column(12, 
                                                 fluidRow(
                                                   column(3, wellPanel(
                                                     tags$strong("Graphic options"),
                                                     sliderInput(inputId = "tsizesemmin", 
                                                                 label = "Font size (min)", 
                                                                 min = 1, 
                                                                 max = 10, 
                                                                 value = 4, 
                                                                 step = 0.5),
                                                     sliderInput(inputId = "tsizesemmax", 
                                                                 label = "Font size (max)", 
                                                                 min = 1, 
                                                                 max = 10, 
                                                                 value = 6, 
                                                                 step = 0.5),
                                                     downloadButton("downsem", "Download plot"))),
                                                   
                                                   column(9, plotOutput("plotsem", width = "100%", height = "800px")))))
                               ),
                               
                               
                               
                            
                               
                               
                               tabPanel("User guide",
                                        withMathJax(), 
                                        includeMarkdown("README.md"))
                             )
                    ),
                    
                    
                    ############## CLEM
                    tabPanel("Geo-semantic Networks",
                             fluidRow(h2("Geo-semantic Networks"),
                                      
                                      column(4, selectInput("semanticMethod", label = "Semantic Method",
                                                            choices = c("Juste"), multiple = F)),
                                      column(4, selectInput("aggregationMethod", label = "Set of Countries",choices = c("Authoring", "Studied"), 
                                                            selected = "Studied", multiple = F)),
                                      column(4, sliderInput("nClassifGroups", label = "Number of Clusters",
                                                            min = 1, max = 8, value = 4, step = 1), animate=T),
                                      column(12, plotOutput("termsXCountriesMap")),
                                      column(12, plotOutput("termsXCountriesLegend"))
                             )),
                    ############## 
                    
                    
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
                    
                  )))
