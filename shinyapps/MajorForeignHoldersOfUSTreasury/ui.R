library(shiny)
library(RCurl)
script <-
  getURL(
    "https://raw.githubusercontent.com/am-consulting/Rscript/master/importMajorForeignHoldersUSTreasury.r",
    ssl.verifypeer = FALSE
  )
eval(parse(text = script))

tmp1 <- which(regexpr("Japan", colnames(allData), ignore.case = T) != -1)
tmp2 <- which(regexpr("china", colnames(allData), ignore.case = T) != -1)
tmp3 <- which(regexpr("grand", colnames(allData), ignore.case = T) != -1)
origData1 <- allData[, c(1, tmp1)]
origData2 <- allData[, c(1, tmp2)]
origData3 <- allData[, c(1, tmp3)]
origData4 <- allData[, -c(tmp1, tmp2, tmp3)]
origData1$Ratio <- origData1[, 2] / origData3[, 2] * 100
origData2$Ratio <- origData2[, 2] / origData3[, 2] * 100
origData5 <- merge(origData1, origData2, by = "Date")
colnames(origData5)[c(3, 5)] <- c("Japan:Ratio(%)", "China:Ratio(%)")
origData1 <<- origData1
origData2 <<- origData2
origData3 <<- origData3
origData4 <<- origData4
origData5 <<- origData5

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
    "Major Foreign Holders Of U.S.Treasury. Raw Data source:U.S. Department of the Treasury"
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
                          label = "Japan",
                          colnames(origData1)[-1],
                          selectize = FALSE
                        ),
                        sliderInput(
                          "dataRange1",
                          label = "Data Range",
                          min = 1,
                          max = nrow(origData1),
                          value = c(1, nrow(origData1)),
                          step = 1
                        ),
                        tags$hr(),
                        selectInput(
                          "objective2",
                          label = " China",
                          colnames(origData2)[-1],
                          selectize = FALSE
                        ),
                        sliderInput(
                          "dataRange2",
                          label = "Data Range",
                          min = 1,
                          max = nrow(origData2),
                          value = c(1, nrow(origData2)),
                          step = 1
                        ),
                        tags$hr(),
                        selectInput(
                          "objective3",
                          label = "Grand Total",
                          colnames(origData3)[-1],
                          selectize = FALSE
                        ),
                        sliderInput(
                          "dataRange3",
                          label = "Data Range",
                          min = 1,
                          max = nrow(origData3),
                          value = c(1, nrow(origData3)),
                          step = 1
                        ),
                        tags$hr(),
                        selectInput(
                          "objective4",
                          label = "Others",
                          colnames(origData4)[-1],
                          selectize = FALSE
                        ),
                        sliderInput(
                          "dataRange4",
                          label = "Data Range",
                          min = 1,
                          max = nrow(origData4),
                          value = c(1, nrow(origData4)),
                          step = 1
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
                        tags$h4(textOutput("title01"), paste("Unit:Millions of dollars")),
                        plotOutput("plot01", height = "500px")
                      )),
                      column(6, wellPanel(
                        tags$h4(textOutput("title02"), paste("Unit:Millions of dollars")),
                        plotOutput("plot02", height = "500px")
                      ))),
                      fluidRow(column(6, wellPanel(
                        tags$h4(textOutput("title03"), paste("Unit:Millions of dollars")),
                        plotOutput("plot03", height = "500px")
                      )),
                      column(6, wellPanel(
                        tags$h4(textOutput("title04"), paste("Unit:Millions of dollars")),
                        plotOutput("plot04", height = "500px")
                      ))),
                      tags$hr(),
                      fluidRow(
                        column(
                          6,
                          tags$h4(
                            textOutput("title001"),
                            paste("Unit of Monetary values:Millions of dollars")
                          ),
                          DT::dataTableOutput("statisticTable01")
                        ),
                        column(
                          6,
                          tags$h4(
                            textOutput("title002"),
                            paste("Unit of Monetary values:Millions of dollars")
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
                            paste("Unit of Monetary values:Millions of dollars")
                          ),
                          DT::dataTableOutput("statisticTable03")
                        ),
                        column(
                          6,
                          tags$h4(
                            textOutput("title004"),
                            paste("Unit of Monetary values:Millions of dollars")
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
                            paste("Unit of Monetary values:Millions of dollars")
                          ),
                          DT::dataTableOutput("timeseriesTable01")
                        ),
                        column(
                          6,
                          tags$h4(
                            textOutput("title0002"),
                            paste("Unit of Monetary values:Millions of dollars")
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
                            paste("Unit of Monetary values:Millions of dollars")
                          ),
                          DT::dataTableOutput("timeseriesTable03")
                        ),
                        column(
                          6,
                          tags$h4(
                            textOutput("title0004"),
                            paste("Unit of Monetary values:Millions of dollars")
                          ),
                          DT::dataTableOutput("timeseriesTable04")
                        )
                      ),
                      fluidRow(
                        column(
                          12,
                          tags$hr(),
                          paste("Unit of Monetary values:Millions of dollars"),
                          DT::dataTableOutput("allData"),
                          htmlOutput("remarktext"),
                          htmlOutput("history"),
                          htmlOutput("gitcode")
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
               height = "4500"
             )
           ))
))