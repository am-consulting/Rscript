library(shiny)
library(RCurl)
script <-
  getURL(
    "https://raw.githubusercontent.com/am-consulting/Rscript/master/laborForceSurveyofJapan.r",
    ssl.verifypeer = FALSE
  )
eval(parse(text = script))
laborforcesurvey <<- origData

script <-
  getURL(
    "https://raw.githubusercontent.com/am-consulting/Rscript/master/consumerPriceIndexofJapan.r",
    ssl.verifypeer = FALSE
  )
eval(parse(text = script))
consumerpriceindex <<- origData0

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
    "Phillips Curve in Japan. Labor Force Survey & Consumer Price Index. Raw Data source:Statistics Bureau, Ministry of Internal Affairs and Communications"
  ),
  fluidRow(column(12,
                  textOutput("DataDownloadTime"))),
  fluidRow(column(10,
                  fluidRow(
                    column(
                      2,
                      wellPanel(
                        selectInput(
                          "lfs",
                          label = "Labor Force Survey",
                          colnames(laborforcesurvey)[-1],
                          selectize = FALSE
                        ),
                        sliderInput(
                          "lfsRange",
                          label = "Labor Force Survey",
                          min = 1,
                          max = nrow(laborforcesurvey),
                          value = c(1, nrow(laborforcesurvey))
                        ),
                        selectInput(
                          "cpi",
                          label = "Consumer Price Index",
                          colnames(consumerpriceindex)[-1],
                          selectize = FALSE
                        ),
                        sliderInput(
                          "cpiRange",
                          label = "Consumer Price Index",
                          min = 1,
                          max = nrow(consumerpriceindex),
                          value = c(1, nrow(consumerpriceindex))
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
                        plotOutput("plot01", height = "400px")
                      )),
                      column(6, wellPanel(
                        plotOutput("plot02", height = "400px")
                      ))),
                      fluidRow(column(12, wellPanel(
                        plotOutput("plotmerge", height = "500px")
                      ))),
                      fluidRow(column(12, wellPanel(
                        plotOutput("PhillipsCurve", height = "500px")
                      ))),
                      tags$hr(),
                      fluidRow(
                        column(6,
                               DT::dataTableOutput("statisticTable01")),
                        column(6,
                               DT::dataTableOutput("statisticTable02"))
                      ),
                      tags$hr(),
                      fluidRow(
                        column(6,
                               DT::dataTableOutput("timeseriesTable01")),
                        column(6,
                               DT::dataTableOutput("timeseriesTable02"))
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
               height = "3500"
             )
           ))
))