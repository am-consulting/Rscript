library(shiny)
library(DT)

powerSelect.Re_03 <- c("10^3", "10^4", "10^5", "10^6")
shinyUI(
  tabsetPanel(
    tabPanel(
      "Smooth pipe",
      fluidPage(
        headerPanel("Calculation of Loss coefficient for straight pipe(λ , friction factor):Smooth pipe"),
        fluidRow(column(12,
                        withMathJax("$$\\text{Smooth pipe } \\frac {1}{ \\sqrt{\\lambda}}=2\\times \\log_{10}(Re\\times\\sqrt{\\lambda})  -0.8$$"),
                        textOutput("datachecktime_03")
        )),
        fluidRow(column(10,fluidRow(column(4,
                                           wellPanel(
                                             sliderInput(
                                               "slider.Re_03",
                                               label = "Reynolds number",
                                               min = 1,
                                               max = 100,
                                               value = 1
                                             ),
                                             selectInput(
                                               "power.Re_03",
                                               label = "Select",
                                               powerSelect.Re_03,
                                               selectize = F
                                             ),
                                             verbatimTextOutput("Reynolds_03"),tags$hr(),
                                             strong("Friction Factor(λ , Loss coefficient for straight pipe)"),
                                             verbatimTextOutput("Result_03")
                                           )
        ),
        column(8, wellPanel(DT::dataTableOutput("dtTable_03")))),
        fluidRow(column(12, htmlOutput("remarktext_03")))
        ),
        column(
          2,
          a("@AMC2_Japan",
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