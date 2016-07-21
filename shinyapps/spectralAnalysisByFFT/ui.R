library(shiny)
library(RCurl)
script <-
  getURL(
    "https://raw.githubusercontent.com/am-consulting/Rscript/master/amccLinkList.r",
    ssl.verifypeer = FALSE
  )
eval(parse(text = script))

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
  headerPanel("Spectral Analysis By  Fast Fourier Transform"),
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
                        tags$h5("CSV file sampe"),
                        tags$a(href="https://raw.githubusercontent.com/am-consulting/CSVDataAtGitHub/master/sample-MYG0041103111446.NS.csv","･sample data",target="_blank"),           
                        tags$hr(),
                        textInput("dataTitle", label = "Data Title"),
                        uiOutput("timeslider"),
                        uiOutput("freqslider"),
                        textInput("sf", label = "Sampling Frequency(Hz)", value = 100),
                        textInput("dataScale", label = "Scale(Only Numerical Value)", value = 0.001),
                        radioButtons(
                          "inverse",
                          label = "Inverse",
                          choices = list("FALSE" = 1),
                          selected = 1
                        ),
                        tags$hr(),
                        tags$h5("Mean of Sampling Data*Scale"),
                        verbatimTextOutput("zmean")
                      )
                    ),
                    column(
                      10,
                      fluidRow(
                      conditionalPanel(
                      condition = "output.completionTime!=''",
                      column(
                        6, plotOutput("Plot01", width = "100%", height = "500px"),
                          downloadButton(outputId = "Download1", label = "Download")
                      )),
                      column(6, DT::dataTableOutput("dataSet01"))),
                      fluidRow(
                      conditionalPanel(
                      condition = "output.completionTime!=''",
                      tags$hr(),
                      column(
                        6, plotOutput("Plot02", width = "100%", height = "500px"),
                          downloadButton(outputId = "Download2", label = "Download")
                      )),
                      column(6, DT::dataTableOutput("dataSet02"))),
                      fluidRow(
                      conditionalPanel(
                      condition = "output.completionTime!=''",
                      tags$hr(),
                      column(
                        6, plotOutput("Plot03", width = "100%", height = "500px"),
                          downloadButton(outputId = "Download3", label = "Download")
                      )),
                      column(6, DT::dataTableOutput("dataSet03"))),
                      fluidRow(
                      conditionalPanel(
                      condition = "output.completionTime!=''",
                      tags$hr(),
                        column(
                        6, plotOutput("Plot04", width = "100%", height = "500px"),
                          downloadButton(outputId = "Download4", label = "Download")
                      )),
                      column(6, DT::dataTableOutput("dataSet04"))),
#                      tags$hr(),
                      htmlOutput("remarktext"),
                      htmlOutput("history"),
                      htmlOutput("gitcode"),
                      htmlOutput("linkList")
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