library(shiny)
library(RCurl)
script <-
  getURL(
    "https://raw.githubusercontent.com/am-consulting/Rscript/master/importCustomExportImportData.r",
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
    tags$link(rel = "stylesheet",
              href = "http://archive.am-consulting.co.jp/paceCSS.css"),
    tags$script(src = "http://archive.am-consulting.co.jp/pace.min.js"),
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
    "Trade Statistics of Japan. Raw Data source:Ministry of Finance Japan"
  ),
  fluidRow(column(12,
                  textOutput("DataDownloadTime"))),
  fluidRow(column(10,
                  fluidRow(
                    column(
                      2,
                      wellPanel(
                        selectInput(
                          "objective1",
                          label = "By Region:World",
                          colnames(origData1)[-1],
                          selectize = FALSE
                        ),
                        sliderInput(
                          "dataRange1",
                          label = "Data Range",
                          min = 1,
                          max = nrow(origData1),
                          value = c(1, nrow(origData1))
                        ),
                        tags$hr(),
                        selectInput(
                          "objective2",
                          label = " By product category:Export",
                          colnames(origData2)[-1],
                          selectize = FALSE
                        ),
                        sliderInput(
                          "dataRange2",
                          label = "Data Range",
                          min = 1,
                          max = nrow(origData2),
                          value = c(1, nrow(origData2))
                        ),
                        tags$hr(),
                        selectInput(
                          "objective3",
                          label = "By product category:Import",
                          colnames(origData3)[-1],
                          selectize = FALSE
                        ),
                        sliderInput(
                          "dataRange3",
                          label = "Data Range",
                          min = 1,
                          max = nrow(origData3),
                          value = c(1, nrow(origData3))
                        ),
                        tags$hr(),
                        selectInput(
                          "objective4",
                          label = "By Region:World",
                          colnames(origData4)[-1],
                          selectize = FALSE
                        ),
                        sliderInput(
                          "dataRange4",
                          label = "Data Range",
                          min = 1,
                          max = nrow(origData4),
                          value = c(1, nrow(origData4))
                        ),
                        tags$hr(),
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
                        tags$hr(),
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
                    column(
                      10,
                      fluidRow(column(6, wellPanel(
                        tags$h4(textOutput("title01"), paste("Unit:trillion JPY")),
                        plotOutput("plot01", height = "500px")
                      )),
                      column(6, wellPanel(
                        tags$h4(textOutput("title02"), "Unit:100 million JPY"),
                        plotOutput("plot02", height = "500px")
                      ))),
                      fluidRow(column(6, wellPanel(
                        tags$h4(textOutput("title03"), "Unit:100 million JPY"),
                        plotOutput("plot03", height = "500px")
                      )),
                      column(6, wellPanel(
                        tags$h4(textOutput("title04"), paste("Unit:trillion JPY")),
                        plotOutput("plot04", height = "500px")
                      ))),
                      tags$hr(),
                      fluidRow(
                        column(
                          6,
                          tags$h4(
                            textOutput("title001"),
                            paste("Unit of Monetary values:trillion JPY")
                          ),
                          DT::dataTableOutput("statisticTable01")
                        ),
                        column(
                          6,
                          tags$h4(
                            textOutput("title002"),
                            "Unit of Monetary values:100 million JPY"
                          ),
                          DT::dataTableOutput("statisticTable02")
                        )
                      ),
                      tags$hr(),
                      fluidRow(
                        column(
                          6,
                          tags$h4(
                            textOutput("title003"),
                            "Unit of Monetary values:100 million JPY"
                          ),
                          DT::dataTableOutput("statisticTable03")
                        ),
                        column(
                          6,
                          tags$h4(textOutput("title004"), paste("Unit:trillion JPY")),
                          DT::dataTableOutput("statisticTable04")
                        )
                      ),
                      tags$hr(),
                      fluidRow(
                        column(
                          6,
                          tags$h4(
                            textOutput("title0001"),
                            paste("Unit of Monetary values:trillion JPY")
                          ),
                          DT::dataTableOutput("timeseriesTable01")
                        ),
                        column(
                          6,
                          tags$h4(
                            textOutput("title0002"),
                            "Unit of Monetary values:100 million JPY"
                          ),
                          DT::dataTableOutput("timeseriesTable02")
                        )
                      ),
                      tags$hr(),
                      fluidRow(
                        column(
                          6,
                          tags$h4(
                            textOutput("title0003"),
                            "Unit of Monetary values:100 million JPY"
                          ),
                          DT::dataTableOutput("timeseriesTable03")
                        ),
                        column(
                          6,
                          tags$h4(textOutput("title0004"),
                                  "Unit of Monetary values:trillion JPY"),
                          DT::dataTableOutput("timeseriesTable04")
                        )
                      ),
                      fluidRow(
                        column(
                          12,
                          tags$hr(),
                          paste("Unit of Monetary values:trillion JPY"),
                          DT::dataTableOutput("allData"),
                          htmlOutput("remarktext"),
                          htmlOutput("history"),
                          htmlOutput("gitcode"),
                          htmlOutput("linkList")
                        )
                      )
                    )
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
               height = "5500"
             )
           ))
))