library(shiny)
library(XML)
library(RCurl)
script <-
  getURL(
    "https://raw.githubusercontent.com/am-consulting/Rscript/master/amccLinkList.r",
    ssl.verifypeer = FALSE
  )
eval(parse(text = script))

origData <<- list()
sourceURL <<- c(
  "http://www.data.jma.go.jp/cpdinfo/temp/list/mon_wld.html",
  "http://www.data.jma.go.jp/cpdinfo/temp/list/mon_jpn.html"
)
datatitle <<- c(
  "Monthlyt Mean Temperature Anomaly:World(Celsius,Base Line:30 years mean from Y1981 to Y2010)",
  "Monthlyt Mean Temperature Anomaly:Japan(Celsius,Base Line:30 years mean from Y1981 to Y2010)"
)
for (iii in 1:length(sourceURL)) {
  Sys.sleep(1)
  buf <-
    readHTMLTable(
      doc = sourceURL[iii],
      header = T,
      trim = T,
      stringsAsFactors = F,
      as.data.frame = T,
      which = 1
    )
  buf[, -1] <-
    sapply(buf[, -1], function(x) {
      gsub("[^0-9|-|+|.]*?", "", x)
    })
  buf <-
    data.frame(Year = buf[, 1],
               sapply(buf[, -1], as.numeric),
               check.names = FALSE)
  origData[[iii]] <<-
    buf # not work in list2env(origData[[iii]], globalenv())
}
downloadTime <<- as.POSIXlt(Sys.time(), "GMT")
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
      "Monthly Mean Temperature Anomaly. Raw Data source: Japan Meteorological Agency"
    ),
    fluidRow(column(
      12,
      textOutput("datachecktime"),
      selectInput("region", label = "Select Region",   datatitle  ,     selectize = FALSE)
    )),
    fluidRow(column(12,
                    h2(
                      textOutput("datatitle")
                    ))),
    fluidRow(column(12,
                    fluidRow(
                      column(
                        10,
                        fluidRow(
                          tags$div(
                            "Caution: Removed rows containing missing values or  non-finite values.",
                            style = "color:black",
                            align = "center"
                          ),
                          column(6, wellPanel(plotOutput("plot1")), DT::dataTableOutput("table1")),
                          column(6, wellPanel(plotOutput("plot2")), DT::dataTableOutput("table1NA"))
                        ),
                        fluidRow(column(12, htmlOutput("remarktext"))),
                        fluidRow(column(12, htmlOutput("history"))),
                        fluidRow(column(12, htmlOutput("gitcode"))),
                        fluidRow(column(12, htmlOutput("linkList")))
                      ),
                      column(
                        2,
                        a(
                          "@AMC2_Japan",
                          class = "twitter-timeline"
                          ,
                          href = "https://twitter.com/AMC2_Japan",
                          "data-widget-id" = "449799943780200448",
                          width = "100%",
                          height = "1500"
                        )
                      )
                    )))
  )
)