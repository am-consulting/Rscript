library(shiny)
library(RCurl)
options(download.file.method = "libcurl")
script <-
  getURL(
    "https://raw.githubusercontent.com/am-consulting/Rscript/master/amccLinkList.r",
    ssl.verifypeer = FALSE
  )
eval(parse(text = script))

Sys.sleep(1)
data.file <- "all_month.csv"
dataset <-
  read.csv(
    paste(
      "http://earthquake.usgs.gov/earthquakes/feed/v1.0/summary/",
      data.file,
      sep = ""
    ),
    header = T,
    as.is = T,
    skip = 0,
    stringsAsFactor = F,
    na.strings = c(""),
    check.names = F
  )
downloadTime <<- as.POSIXlt(Sys.time(), "GMT")
dataset$GMT <-
  as.POSIXlt(sub("Z", "", sub("T", " ", dataset$time)), "GMT")
dataset$JST <-
  as.POSIXct(sub("Z", "", sub("T", " ", dataset$time))) + 3600 * 9 #UTC+9
dataset$latlong <-
  paste(dataset$latitude, dataset$longitude, sep = ":")
dataset$info <- paste(
  "Time(UTC)=",
  dataset$GMT,
  ", Time(UTC+9)=",
  dataset$JST,
  ", Depth(km)=",
  dataset$depth,
  ", Mag=",
  dataset$mag,
  ", MagType=",
  dataset$magType,
  ", Place=",
  dataset$place
  ,
  sep = ""
)
borderM <<- 5
dataset24hour <<-
  subset(dataset, (downloadTime - 60 * 60 * 24) <= dataset$GMT)
datasetM <<- subset(dataset, borderM <= dataset$mag)
datasetJapan <<- dataset[grep("Japan", dataset$place),]
dataset <<- dataset
titlegvis24hour <<-
  paste(
    "Earthquakes in a 24-hour period.\nPeriod(UTC): ",
    downloadTime - 60 * 60 * 24,
    "-",
    downloadTime ,
    ". n=",
    nrow(dataset24hour)
  )
titlegvisM <<-
  paste("Earthquakes over Magnitude ",
        borderM,
        " in past 30 days. n=",
        nrow(datasetM))
titlegvisJapan <<-
  paste("Earthquakes around Japan in past 30 days. n=", nrow(datasetJapan))
selectList <<-
  c(
    "Earthquakes in a 24-hour period.",
    paste("Earthquakes over Magnitude ", borderM , " in past 30 days."),
    "Earthquakes around Japan in past 30 days."
  )
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
    headerPanel("Earthquake. Raw Data source: United States Geological Survey"),
    fluidRow(column(
      12,
      textOutput("datachecktime"),
      selectInput("maptype", label = "Select map", selectList, selectize = FALSE)
    )) ,
    fluidRow(column(10,fluidRow(
      column(6, wellPanel(
        textOutput("titlegvis"), htmlOutput("gvis")
      )),
      column(6, DT::dataTableOutput("dtTable"))
    ),
    tags$hr(),
    fluidRow(
      div(
        "Caution: Differences in magnitude type are unconsidered in magnitude histgram.",
        style = "color:black",
        align = "center"
      )
    ),
    fluidRow(column(6, wellPanel(plotOutput("plot1"))),
             column(6, wellPanel(plotOutput("plot2")))),
    fluidRow(column(6, wellPanel(plotOutput("plot3"))),
             column(6, wellPanel(plotOutput("plot4")))),
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
          height = "2000"
        )
      )
    )
  )
)