library(shiny)
library(DT)
powerSelect.ed_01 <- c("10^-5", "10^-6")
powerSelect.Re_01 <- c("10^3", "10^4", "10^5", "10^6")
powerSelect.ed_02 <- c("10^-3", "10^-4", "10^-5", "10^-6")
powerSelect.Re_03 <- c("10^3", "10^4", "10^5", "10^6")
powerSelect.Re_04 <- c("10^1", "10^2")
shinyUI(
  tabsetPanel(
    tabPanel(
      "Colebrook-White Equation",
      fluidPage(
        tags$head(
          tags$link(rel = "stylesheet", href = "http://fonts.googleapis.com/css?family=Roboto+Condensed"),
          tags$link(rel = "shortcut icon", href = "http://knowledgevault.saecanet.com/components/saecanet.png"),
          tags$style("body{font-family: 'Roboto Condensed', sans-serif;}"),
          tags$script('!function(d,s,id){
                  var js,
                  fjs=d.getElementsByTagName(s)[0],
                  p=/^http:/.test(d.location)?\'http\':\'https\';
                  if(!d.getElementById(id)){js=d.createElement(s);
                  js.id=id;js.src=p+"://platform.twitter.com/widgets.js";
                  fjs.parentNode.insertBefore(js,fjs);}}(document,"script","twitter-wjs");'
          )
        ),
        headerPanel("Calculation of Loss coefficient for straight pipe(λ , friction factor):Colebrook-White Equation"),
        fluidRow(column(12,
                        withMathJax(
                          "$$\\text{Colebrook-White Equation }
                      \\frac {1}{ \\sqrt{\\lambda}}=-2\\times \\log_{10}( \\frac{\\varepsilon /d  }{3.71}  + \\frac{ 2.51} {Re\\sqrt{\\lambda }})$$")
        )
        ),
        textOutput("datachecktime_01"),
        fluidRow(column(10,
                        fluidRow(column(4,
                                        wellPanel(
                                          sliderInput(
                                            "slider.ed_01",
                                            label = "ε/d. Roughness height:ε , Pipe diameter:d ",
                                            min = 1,
                                            max = 100,
                                            value = 1),
                                          selectInput(
                                            "power.ed_01",
                                            label = "Select",
                                            powerSelect.ed_01,
                                            selectize = F),
                                          verbatimTextOutput("orig_ed_01"),
                                          tags$hr(),
                                          sliderInput(
                                            "slider.Re_01",
                                            label = "Reynolds number",
                                            min = 1,
                                            max = 100,
                                            value = 1),
                                          selectInput(
                                            "power.Re_01",
                                            label = "Select",
                                            powerSelect.Re_01,
                                            selectize = F),
                                          verbatimTextOutput("orig_Reynolds_01"),
                                          tags$hr(),
                                          strong("Friction Factor(λ , Loss coefficient for straight pipe)"),
                                          verbatimTextOutput("Result_01")
                                        )
                        ),
                        column(4, wellPanel(DT::dataTableOutput("table1_01"))),
                        column(4, wellPanel(DT::dataTableOutput("table2_01")))),
                        fluidRow(column(12, htmlOutput("remarktext_01")))
        ),
        column( 2,
                a("@AMC2_Japan",
                  class = "twitter-timeline",
                  href = "https://twitter.com/AMC2_Japan",
                  "data-widget-id" = "449799943780200448",
                  width = "100%",
                  height = "1000")
        )
        )
      )
    ),
# Rough pipe
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
                                        label = "ε/d. Roughness height:ε , Pipe diameter:d ",
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
  ),
# Rough pipe
# smooth pipe
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
),
# smooth pipe
# laminar
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
# laminar
  )
)