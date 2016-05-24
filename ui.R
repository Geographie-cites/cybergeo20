fluidPage(
  column(
    2,
    selectInput("mode", "Mode", c(
        "Analyse d'un motif" = "one", 
        "Analyse multi-motifs" = "multi",
        "Paramétrage" = "param"
      )
    ),
    conditionalPanel(
      "input.mode == 'multi' | input.mode == 'one'",
      textInput("pattern_input", "Motif recherché")
    ),
    conditionalPanel(
      "input.mode == 'multi'",
      actionButton("add_pattern", "Ajout à la sélection"),
      p(),
      checkboxGroupInput("patterns_selection", "Sélection de motifs", pattern_list)
    )
  ),
  column(
    10,
    tabsetPanel(
      tabPanel(
        "Chronogramme",
        plotOutput("chronogram", height = "700px")
      ),
      tabPanel(
        "Nuage de mots",
        plotOutput("cloud", height = "700px")
      ),
      tabPanel("Phrases", verbatimTextOutput("phrases")),
      tabPanel("Citations", verbatimTextOutput("citations"))
    )
  )
)
