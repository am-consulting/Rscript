library(shiny)
library(DT)

powerSelect.ed_02 <- c("10^-3", "10^-4", "10^-5", "10^-6")
shinyUI(
  tabsetPanel(
    tabPanel(
      "Rough pipe",
      fluidPage(
        headerPanel("Calculation of Loss coefficient for straight pipe(λ , friction factor):Rough pipe"),
        fluidRow(column(12,
                        withMathJax(
                          "$$\\text{Rough pipe }
                      \\frac {1}{ \\sqrt{\\lambda}}=2\\times \\log_{10}( \\frac{d  }{\\varepsilon })  +1.14 \\;,\\;
                      Re\\times\\sqrt{\\lambda }\\times \\frac{\\varepsilon } {d} =200 \\;,\\;
                      Re=\\frac{3500}{\\frac{\\varepsilon } {d}}$$"),
                        textOutput("datachecktime_02")

        )
        ),
        fluidRow(column(10,
                        fluidRow(column(3,
                                        wellPanel(
                                          sliderInput(
                                            "slider.ed_02",
                                            label = "ε/d. Roughness height：ε , Pipe diameter：d ",
                                            min = 1,
                                            max = 100,
                                            value = 1
                                          ),
                                          selectInput(
                                            "power.ed_02",
                                            label = "Select",
                                            powerSelect.ed_02,
                                            selectize = F
                                          ),
                                          verbatimTextOutput("orig_ed_02"), tags$hr(),
                                          strong("Friction Factor(λ , Loss coefficient for straight pipe)"),
                                          verbatimTextOutput("Resultlamnda_02"), tags$hr(),
                                          withMathJax("$$\\text{Re(1).}\\;  Re\\times\\sqrt{\\lambda }\\times \\frac{\\varepsilon } {d} =200$$"),
                                          verbatimTextOutput("ResultRe01_02"), tags$hr(),
                                          withMathJax("$$\\text{Re(2).}\\;  Re=\\frac{3500}{\\frac{\\varepsilon } {d}}$$"),
                                          verbatimTextOutput("ResultRe02_02")
                                        )
                        ),
                        column(9,
                               wellPanel(DT::dataTableOutput("dtTable_02")))
                        ),
                        fluidRow(column(12, htmlOutput("remarktext_02")))),
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