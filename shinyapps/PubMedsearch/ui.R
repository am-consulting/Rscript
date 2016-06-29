library(shiny)
library(lubridate)
shinyUI(fluidPage(
  tags$head(
    tags$link(rel = "stylesheet",
              href = "http://fonts.googleapis.com/css?family=Roboto+Condensed"),
    tags$link(rel = "stylesheet", href = "http://cdn.datatables.net/plug-ins/1.10.7/features/searchHighlight/dataTables.searchHighlight.css"),
    tags$script(src = "http://cdn.datatables.net/plug-ins/1.10.7/features/searchHighlight/dataTables.searchHighlight.min.js"),
    tags$script(src = "http://bartaz.github.io/sandbox.js/jquery.highlight.js"),
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
    "PubMed Search. Raw Data source:PubMed http://www.ncbi.nlm.nih.gov/pubmed"
  ),
  fluidRow(column(12,
                  textOutput(
                    "searchStatusFinish"
                  ))),
  fluidRow(column(10,
                  fluidRow(
                    column(
                      2,
                      wellPanel(
                        textInput("searchWord", label = "Search Word", value = ""),
                        dateRangeInput(
                          "dateRange",
                          label = "Date Range Input",
                          start = Sys.Date() - 100,
                          end = Sys.Date()
                        ),
                        radioButtons(
                          "returnMax",
                          label = "Returen Max",
                          choices = list("5" = 5, "10" = 10,   "50" = 50),
                          selected = 5
                        ),
                        actionButton("searchAction", label = "Search")
                      )
                    ),
                    column(10,
                           fluidRow(column(
                             12, DT::dataTableOutput("resultTable")
                           ))
                           ,
                           fluidRow(
                             column(
                               12,
                               htmlOutput("remarktext"),
                               htmlOutput("history"),
                               htmlOutput("gitcode")
                             )
                           ))
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
               height = "1500"
             )
           ))
))