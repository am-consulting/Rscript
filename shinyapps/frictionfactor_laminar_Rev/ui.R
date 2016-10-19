library(shiny)
library(DT)

powerSelect.Re_04 <- c("10^1", "10^2")
shinyUI(
  fluidPage(
    tabsetPanel(
      tabPanel(
        "Laminar",
        headerPanel(    "Calculation of Loss coefficient for straight pipe(λ , friction factor):Laminar"),
        fluidRow(column(12,
                        withMathJax("$$\\text{Laminar } \\frac {1}{ \\sqrt{\\lambda}}=\\frac{Re\\times\\sqrt{\\lambda}}{64}$$"),textOutput("datachecktime_04"))
        ),
        fluidRow(column(10,fluidRow(column(4,
                                           wellPanel(
                                             sliderInput(
                                               "slider.Re_04",
                                               label = "Reynolds number",
                                               min = 1,
                                               max = 100,
                                               value = 1
                                             ),
                                             selectInput(
                                               "power.Re_04",
                                               label = "Select",
                                               powerSelect.Re_04  ,
                                               selectize = F
                                             ),
                                             verbatimTextOutput("Reynolds_04"),tags$hr(),
                                             strong("Friction Factor(λ , Loss coefficient for straight pipe)"),
                                             verbatimTextOutput("Result_04")
                                           )
        ),
        column(8, wellPanel(DT::dataTableOutput("dtTable_04")))),
        fluidRow(column(12, htmlOutput("remarktext_04")))
        ),
        column(2,a(
          "@AMC2_Japan",
          class = "twitter-timeline",
          href = "https://twitter.com/AMC2_Japan",
          "data-widget-id" = "449799943780200448",
          width = "100%",
          height = "1000"
        )
        )
        )
      )
    )
  )
)