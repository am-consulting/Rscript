library(shiny)
library(rvest)
origData <<- list()
sourceURL <-
  "http://www.data.jma.go.jp/gmd/cpd/data/elnino/index/dattab.html"
url <- c(
  "http://www.data.jma.go.jp/gmd/cpd/data/elnino/index/nino3abs.html",
  "http://www.data.jma.go.jp/gmd/cpd/data/elnino/index/ninowabs.html",
  "http://www.data.jma.go.jp/gmd/cpd/data/elnino/index/iobwabs.html"
)
datatitle <<- c(
  "NINO.3 Sea Surface Temperature",
  "NINO.WEST Sea Surface Temperature",
  "IOBW Sea Surface Temperature"
)
for (rrr in 1:length(url)) {
  source <- read_html(url[rrr])
  tmp01 <-
    source %>% html_nodes(xpath = '//pre') %>% html_text %>% iconv(from = "UTF-8")
  buf <- strsplit(tmp01, "\n")
  for (fff in 3:length(buf[[1]])) {
    tmp02 <- buf[[1]][fff]
    tmp02 <- gsub("  ", " ", tmp02)
    tmp02 <- strsplit(tmp02, " ")
    tmp03 <- t(tmp02[[1]][-1])
    if (fff == 3) {
      dataset <-
        as.data.frame(tmp03)
    } else{
      dataset <- rbind(dataset, as.data.frame(tmp03))
    }
  }
  dataset <-
    data.frame(apply(dataset, 2, as.numeric), stringsAsFactors = FALSE)
  datasetDF <-
    data.frame(Date = seq(
      as.Date(paste(dataset[1, 1], "-1-1", sep = "")),
      by = "month",
      length = nrow(dataset) * 12
    ),
    value = as.vector(t(dataset)[-1, ]))
  datasetDF[datasetDF == 99.9] <- NA
  colnames(datasetDF)[2] <- datatitle[[rrr]]
  origData[[rrr]] <<- datasetDF
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
      "Sea Surface Temperature. Raw Data source: Japan Meteorological Agency"
    ),
    fluidRow(column(
      6,
      textOutput("datachecktime"),
      selectInput("region", label = "Select",   datatitle  ,     selectize = FALSE)
    )),
    fluidRow(column(
      12,
      column(
        10,
        fluidRow(h1(textOutput("datatitle")), htmlOutput("figure01")),
        fluidRow(column(6, wellPanel(
          plotOutput("plot1")
        )),
        column(6, wellPanel(
          plotOutput("plot2")
        ))),
        fluidRow(DT::dataTableOutput("table1"))
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
          height = "1000"
        )
      )
    )),
    fluidRow(
      div(
        "Caution: Removed rows containing missing values or  non-finite values.",
        style = "color:black",
        align = "center"
      )
    ),
    fluidRow(column(12, htmlOutput("remarktext"))),
    fluidRow(column(12, htmlOutput("history"))),
    fluidRow(column(12, htmlOutput("gitcode")))
  )
)