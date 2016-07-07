library(shiny)
library(DT)
library(XML)
library(tseries)
library(lubridate)
library(ggplot2)
library(scales)
library(dplyr)
library(reshape2)
library(xtable)
library(RCurl)
script <-
  getURL(
    "https://raw.githubusercontent.com/am-consulting/Rscript/master/amccLinkList.r",
    ssl.verifypeer = FALSE
  )
eval(parse(text = script))

dataset <<- list()
datatitle <<- c(
  "Sea Ice Index Data:North",
  "Sea Ice Index Data:South",
  "Sea Ice Index Data:Total(North+South)"
)
dataUrl <- c(
  "ftp://sidads.colorado.edu/DATASETS/NOAA/G02135/north/daily/data/NH_seaice_extent_final_v2.csv",
  "ftp://sidads.colorado.edu/DATASETS/NOAA/G02135/north/daily/data/NH_seaice_extent_nrt_v2.csv",
  "ftp://sidads.colorado.edu/DATASETS/NOAA/G02135/south/daily/data/SH_seaice_extent_final_v2.csv",
  "ftp://sidads.colorado.edu/DATASETS/NOAA/G02135/south/daily/data/SH_seaice_extent_nrt_v2.csv"
)
chkNH <- 0
chkSH <- 0
for (ddd in 1:length(dataUrl)) {
  Sys.sleep(1)
  buf <<-
    read.csv(
      dataUrl[ddd],
      header = T,
      skip = 0,
      stringsAsFactor = F,
      na.strings = c("")
    )
  tmp <- apply(buf[-1, -6], 2, as.numeric)
  if (regexpr("NH_seaice", dataUrl[ddd]) != -1) {
    ppp <- 1
    if (chkNH == 0) {
      dataset[[ppp]] <-
        tmp
      chkNH <- 1
    } else{
      dataset[[ppp]] <- rbind(dataset[[ppp]], tmp)
    }
  } else{
    ppp <- 2
    if (chkSH == 0) {
      dataset[[ppp]] <-
        tmp
      chkSH <- 1
    } else{
      dataset[[ppp]] <- rbind(dataset[[ppp]], tmp)
    }
  }
}
for (ppp in 1:2) {
  colnames(dataset[[ppp]])
  Date <-
    as.Date(paste(dataset[[ppp]][, 1], "-", dataset[[ppp]][, 2], "-", dataset[[ppp]][, 3], sep =
                    ""))
  if (ppp == 1) {
    colnames(dataset[[ppp]]) <-
      paste("NH_", colnames(dataset[[ppp]]), sep = "")
  } else{
    colnames(dataset[[ppp]]) <-
      paste("SH_", colnames(dataset[[ppp]]), sep = "")
  }
  dataset[[ppp]] <-
    data.frame(dataset[[ppp]], Date, check.names = FALSE)
  dataset[[ppp]] <<- dataset[[ppp]]
}
dataset[[3]] <- merge(dataset[[1]], dataset[[2]])
dataset[[3]]$TotalExtent <-
  as.numeric(dataset[[3]][, 5]) + as.numeric(dataset[[3]][, 10])
dataset[[3]] <<- dataset[[3]]
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
      "Sea Ice Index Data. Raw Data source: National Snow and Ice Data Center"
    )
    ,
    fluidRow(column(
      12,
      textOutput("datachecktime"),
      selectInput("region", label = "Select",   datatitle  ,     selectize = FALSE)
    )),
    fluidRow(column(12,
                    h2(
                      textOutput("datatitle")
                    )))
    ,
    fluidRow(column(12,
                    fluidRow(
                      column(
                        10,
                        fluidRow(
                          div(
                            "Caution: Removed rows containing missing values or  non-finite values.",
                            style = "color:black",
                            align = "center"
                          ),
                          column(4, wellPanel(plotOutput("plot1"))),
                          column(4, wellPanel(plotOutput("plot2"))),
                          column(4, wellPanel(plotOutput("plot3")))
                        ),
                        fluidRow(column(12, wellPanel(
                          div(style = "height:700px;",  plotOutput("plot4"))
                        ))),tags$hr(),
                        fluidRow(column(12, DT::dataTableOutput("table1"))),
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
                          href = "https://twitter.com/AMC2_Japan"
                          ,
                          "data-widget-id" = "449799943780200448",
                          width = "100%",
                          height = "2500"
                        )
                      )
                    )))
  )
)