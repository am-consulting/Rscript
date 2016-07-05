library(shiny)
library(RCurl)
script <-
  getURL(
    "https://raw.githubusercontent.com/am-consulting/Rscript/master/Rscript_JGBInterestRate.r",
    ssl.verifypeer = FALSE
  )
eval(parse(text = script))

latestDataDownloadTime <<- as.POSIXlt(Sys.time(), "GMT")
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
    
    headerPanel(
      "Japanese Government Bonds Interest Rate. Raw Data source:Ministry of Finance Japan"
    ),
    textOutput("latestDataDownloadTime"),
    
    fluidRow(
      column(
        2,
        wellPanel(
          selectInput(
            "year",
            label = "Year",
            colnames(jgbData)[-1],
            selectize = FALSE
          ),
          dateRangeInput(
            "dateRange",
            label = "Date Range Input",
            start = Sys.Date() - 365,
            end = jgbData[nrow(jgbData), 1]
          ),
          radioButtons(
            "charttype",
            label = "Chart Type",
            choices = list(
              `Time series( Line )` = "l",
              `Time series( Bar )` = "h",
              Histogram = "hist",
              `Q-Q plot` = "QQ" ,
              `Arima` = "arima"
            ),
            selected = "l"
          ),
          radioButtons(
            "datatype",
            label = "Data Type",
            choices = list(
              Level = 1,
              `1st difference` = 2  ,
              `2nd difference` = 3
            ),
            selected = 1
          )
        )
      ),
      column(4, wellPanel(plotOutput("plot1"))),
      column(4, DT::dataTableOutput("table1")),
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
          height = "500"
        )
      )
    ),
    tags$hr(),
    fluidRow(column(12, DT::dataTableOutput("table2"))),
    fluidRow(column(12, htmlOutput("remarktext"))),
    fluidRow(column(12, htmlOutput("history"))),
    fluidRow(column(12, htmlOutput("gitcode")))
  )
)