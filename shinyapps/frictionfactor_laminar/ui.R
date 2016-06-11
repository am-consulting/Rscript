library(shiny)
library(DT)
powerSelect.Re <<- c("10^1", "10^2")
shinyUI(fluidPage(
  tags$head(
    tags$link(rel = "stylesheet",
              href = "http://fonts.googleapis.com/css?family=Roboto+Condensed"),
# favicon    
    tags$link(rel = "apple-touch-icon-precomposed",
              href = "http://www.saecanet.com/subfolder/ico/apple-touch-icon-144-precomposed.png",
              sizes = "144x144"),
    tags$link(rel = "apple-touch-icon-precomposed",
              href = "http://www.saecanet.com/subfolder/ico/apple-touch-icon-114-precomposed.png",
              sizes = "114x114"),
    tags$link(rel = "apple-touch-icon-precomposed",
              href = "http://www.saecanet.com/subfolder/ico/apple-touch-icon-72-precomposed.png",
              sizes = "72x72"),
    tags$link(rel = "apple-touch-icon-precomposed",
              href = "http://www.saecanet.com/subfolder/ico/apple-touch-icon-52-precomposed.png",
              sizes = "52x52"),
    tags$link(rel = "shortcut icon",
              href = "http://www.saecanet.com/subfolder/ico/favicon.png"),
# favicon    
    tags$style("body{font-family: 'Roboto Condensed', sans-serif;}"),
    tags$script(
      '!function(d,s,id){var js,fjs=d.getElementsByTagName(s)[0],p=/^http:/.test(d.location)?\'http\':\'https\';if(!d.getElementById(id)){js=d.createElement(s);js.id=id;js.src=p+"://platform.twitter.com/widgets.js";fjs.parentNode.insertBefore(js,fjs);}}(document,"script","twitter-wjs");'
    )
  ),
  headerPanel(
    "Calculation of Loss coefficient for straight pipe(λ , friction factor):Laminar"
  ),
  fluidRow(column(
    12,
    withMathJax(
      "$$\\text{Laminar } \\frac {1}{ \\sqrt{\\lambda}}=\\frac{Re\\times\\sqrt{\\lambda}}{64}$$"
      ,
      textOutput("datachecktime")
    )
  )),
  fluidRow(column(
    4,
    wellPanel(
      sliderInput(
        "slider.Re",
        label = "Reynolds number",
        min = 1,
        max = 100,
        value = 1
      ),
      selectInput(
        "power.Re",
        label = "Select",
        powerSelect.Re  ,
        selectize = FALSE
      ),
      verbatimTextOutput("Reynolds"),
      hr(),
      strong("Friction Factor(λ , Loss coefficient for straight pipe)"),
      verbatimTextOutput("Result")
    )
  ),
  column(6, wellPanel(
    DT::dataTableOutput("dtTable")
  )),
  column(
    2,
    a(
      "@AMC2_Japan",
      class = "twitter-timeline"
      ,
      href = "https://twitter.com/AMC2_Japan"
      ,
      "data-widget-id" = "449799943780200448",
      width = "100%",
      height = "650"
    )
  )),
  fluidRow(column(12, htmlOutput("remarktext"))),
  fluidRow(column(12, htmlOutput("history")))
))