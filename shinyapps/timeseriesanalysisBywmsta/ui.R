library(shiny)
completionTime <<- as.POSIXlt(Sys.time(), "GMT")
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
  headerPanel("Time Series Analysis by Wavelet Methods"),
  fluidRow(column(12,
                  textOutput("completionTime"))),
  fluidRow(column(10,
                  fluidRow(
                    column(
                      2,
                      wellPanel(
                        fileInput(
                          'file',
                          'Choose CSV File',
                          accept = c('text/csv',
                                     'text/comma-separated-values,text/plain',
                                     '.csv')
                        ),
                        tags$hr(),
                        uiOutput("objList"),
                        uiOutput("dataRange"),
                        uiOutput("lag"),
                        uiOutput("differences"),
                        uiOutput("n.scale"),
                        uiOutput("shift"),
                        uiOutput("variance"),
                        uiOutput("wavelet")
                      )
                    ),
                    column(10,
                           fluidRow(
                             column(6, plotOutput(
                               "Plot01", width = "100%", height = "550px"
                             )),
                             column(6, DT::dataTableOutput("dataSet01"))
                           ),
                           tags$hr(),
                           fluidRow(column(
                             12, plotOutput("Plot02", width = "100%", height = "500px")
                           )))
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
               height = "1000"
             )
           )),
  fluidRow(column(
    12,
    htmlOutput("remarktext"),
    htmlOutput("history"),
    htmlOutput("gitcode")
  ))
))