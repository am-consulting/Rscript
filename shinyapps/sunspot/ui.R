library(shiny)
library(RCurl)
library(DT)
script <-
  getURL(
    "https://raw.githubusercontent.com/am-consulting/Rscript/master/Rscript_SunspotNumber.r",
    ssl.verifypeer = FALSE
  )
eval(parse(text = script))
sunSpot()
downloadTime <<- as.POSIXlt(Sys.time(), "GMT")
datatitle <<-
  c(
    "Monthly mean total sunspot number",
    "North monthly mean sunspot number",
    "South monthly mean sunspot number"
  )
shinyUI(
  fluidPage(
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
    headerPanel("Sunspot Number. Raw Data source: Royal Observatory of Belgium"),
    fluidRow(column(
      12,
      textOutput("datachecktime"),
      selectInput("region", label = "Select",   datatitle  ,     selectize = FALSE)
    )),
    fluidRow(column(12,
                    h1(
                      textOutput("datatitle")
                    ))),
    
    fluidRow(column(
      12,
      column(
        10,
        fluidRow(
          column(3, wellPanel(plotOutput("plot1"))),
          column(3, wellPanel(plotOutput("plot2"))),
          column(3, wellPanel(plotOutput("plot3"))),
          column(3, wellPanel(plotOutput("plot4")))
        ),
        fluidRow(
          column(6, DT::dataTableOutput("table1")),
          column(6, DT::dataTableOutput("table2"))
        )
      )
      ,
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
          height = "750"
        )
      )
    )),
    fluidRow(
      div(
        "Caution: Removed rows containing missing values or  non-finite values.",
        style = "color:black",
        align = "center"
      )
    ),
    fluidRow(column(12, htmlOutput("remarktext"))),
    fluidRow(column(12, htmlOutput("history"))),
    fluidRow(column(12, htmlOutput("gitcode")))
  )
)