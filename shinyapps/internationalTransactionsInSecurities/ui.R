library(shiny)
library(RCurl)
script <-
  getURL(
    "https://raw.githubusercontent.com/am-consulting/Rscript/master/internationalTransactionsInSecurities.r",
    ssl.verifypeer = FALSE
  )
eval(parse(text = script))

script <-
  getURL(
    "https://raw.githubusercontent.com/am-consulting/Rscript/master/amccLinkList.r",
    ssl.verifypeer = FALSE
  )
eval(parse(text = script))

latestDataDownloadTime <<- as.POSIXlt(Sys.time(), "GMT")
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
    tags$style("table{white-space: nowrap;}"),
    tags$script(
      '!function(d,s,id){var js,fjs=d.getElementsByTagName(s)[0],p=/^http:/.test(d.location)?\'http\':\'https\';if(!d.getElementById(id)){js=d.createElement(s);js.id=id;js.src=p+"://platform.twitter.com/widgets.js";fjs.parentNode.insertBefore(js,fjs);}}(document,"script","twitter-wjs");'
    )
  ),
  headerPanel(
    "International Transactions in Securities. Raw Data source:Ministry of Finance Japan"
  ),
  fluidRow(column(12,
                  textOutput("DataDownloadTime"))),
  fluidRow(column(
    12,
    column(
      10,
      fluidRow(
        column(
          2,
          wellPanel(
            selectInput(
              "item",
              label = "Item",
              colnames(origData)[-1],
              selectize = F
            ),
            sliderInput(
              "selectedRow",
              label = "Select Rows",
              min = 1,
              max = nrow(origData),
              value =  c(1, nrow(origData)),
              step = 1
            ),
            uiOutput("startPeriod"),
            uiOutput("endPeriod"),
            radioButtons(
              "charttype",
              label = "Chart Type",
              choices = list(
                `Time series( Bar )` = "h",
                Histogram = "hist",
                `Q-Q plot` = "QQ" ,
                `Arima` = "arima"
              ),
              selected = "h"
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
        column(5,
               DT::dataTableOutput("table1")),
        column(5,
               div(style = "overflow-x:scroll;", DT::dataTableOutput("table2")))
      )
      ,
      tags$hr(),
      fluidRow(column(
        12, div(style = "height:500px;background-color:white;", plotOutput("plot1")),
        downloadButton(outputId = "Download1", label = "Download TimeSeries Plot")
      )),
      fluidRow(column(
        12,
        htmlOutput("remarktext"),
        htmlOutput("history"), 
        htmlOutput("gitcode"),
        htmlOutput("linkList")
      ))
    ),
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
        height = "2500"
      )
    )
  ))
))