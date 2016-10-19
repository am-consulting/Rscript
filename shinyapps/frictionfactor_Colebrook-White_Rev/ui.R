library(shiny)
library(DT)
# Colebrook-White Equation: _01
powerSelect.ed_01 <- c("10^-5", "10^-6")
powerSelect.Re_01 <- c("10^3", "10^4", "10^5", "10^6")
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
                                            label = "ε/d. Roughness height：ε , Pipe diameter：d ",
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
    )
  )
)