library(shiny)
library(RCurl)
script <-
  getURL(
    "https://raw.githubusercontent.com/am-consulting/Rscript/master/importJapansBalanceofPayments.r",
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
    "Japan's Balance of Payments. Raw Data source:Ministry of Finance Japan"
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
                          label = "Summary of Balance of Payments",
                          colnames(origData1)[-1],
                          selectize = FALSE
                        ),
                        selectInput(
                          "objective2",
                          label = "Services(Not seasonally adjusted)",
                          colnames(origData2)[-1],
                          selectize = FALSE
                        ),
                        selectInput(
                          "objective3",
                          label = "Income(Not seasonally adjusted)",
                          colnames(origData3)[-1],
                          selectize = FALSE
                        ),
                        selectInput(
                          "objective4",
                          label = "Financial Account(Not seasonally adjusted)",
                          colnames(origData4)[-1],
                          selectize = FALSE
                        ),
                        sliderInput(
                          "dataRange",
                          label = "Data Range",
                          min = 1,
                          max = nrow(origData1),
                          value = c(1, nrow(origData1))
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
                    column(
                      10,
                      fluidRow(column(6, wellPanel(
                        tags$h4(textOutput("title01"), "Unit:100 million JPY"),
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
                        tags$h4(textOutput("title04"), "Unit:100 million JPY"),
                        plotOutput("plot04", height = "500px")
                      ))),
                      tags$hr(),
                      fluidRow(
                        column(
                          6,
                          tags$h4(
                            textOutput("title001"),
                            "Unit of Monetary values:100 million JPY"
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
                          tags$h4(
                            textOutput("title004"),
                            "Unit of Monetary values:100 million JPY"
                          ),
                          DT::dataTableOutput("statisticTable04")
                        )
                      ),
                      tags$hr(),
                      fluidRow(
                        column(
                          6,
                          tags$h4(
                            textOutput("title0001"),
                            "Unit of Monetary values:100 million JPY"
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
                          tags$h4(
                            textOutput("title0004"),
                            "Unit of Monetary values:100 million JPY"
                          ),
                          DT::dataTableOutput("timeseriesTable04")
                        )
                      ),
                      fluidRow(column(
                        12,
                        htmlOutput("remarktext"),
                        htmlOutput("history"),
                        htmlOutput("gitcode"),
                        htmlOutput("linkList")
                      ))
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
               height = "4000"
             )
           ))
))