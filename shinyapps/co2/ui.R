library(shiny)
sourceURL <- "\nhttp://ds.data.jma.go.jp/ghg/kanshi/info_co2.html"
datatitle <<- list()
origData <<- list()
url <- c(
  "http://ds.data.jma.go.jp/ghg/kanshi/obs/co2_monthave_ryo.csv",
  "http://ds.data.jma.go.jp/ghg/kanshi/obs/co2_monthave_mnm.csv",
  "http://ds.data.jma.go.jp/ghg/kanshi/obs/co2_monthave_yon.csv"
)
datatitle[[1]] <<- "Monthly average of CO2 concentration(ppm):Ryōri"
datatitle[[2]] <<-
  "Monthly average of CO2 concentration(ppm):Minami-Tori-shima"
datatitle[[3]] <<-
  "Monthly average of CO2 concentration(ppm):Yonaguni-jima"
for (rrr in 1:length(url)) {
  switch(rrr,
         station <- "Ryori",
         station <- "Minamitorishima",
         station <- "Yonagunijima")
  tmp <-
    read.csv(
      url[rrr],
      header = T,
      skip = 0,
      stringsAsFactor = F,
      check.names = F
    )
  tmp <- tmp[, -(4:5)]                #列4と列5 , 単位 ppm
  tmp[, 3] <- as.numeric(tmp[, 3])     #ppm列を数値化
  tmp <- subset(tmp, tmp[, 2] != "NA")   #コメント(csv最下から4行分)削除
  tmp <-
    data.frame(Date = as.Date(paste(tmp[, 1], "-", tmp[, 2], "-1", sep = "")), co2 =
                 tmp[, 3])
  colnames(tmp)[2] <- paste(station, ".co2(ppm)", sep = "")
  origData[[rrr]] <<- tmp
}
downloadTime <<- as.POSIXlt(Sys.time(), "GMT")
selectList <<- c("Ryori",   "Minami-Tori-shima", "Yonaguni-jima")
stations <-
  data.frame(
    lat = c(39.031333, 24.289418, 24.466518),
    long = c(141.821463 , 153.983041 , 123.010643),
    info = selectList
  )
stations$latlong <- paste(stations[, 1], stations[, 2], sep = ":")
latlong <<- paste(stations[1, 1], stations[2, 1], sep = ":")
stations$info <- paste(stations$info, "-", stations$latlong)
stations <<- stations
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
      "Carbon Dioxide Concentration. Raw Data source: Japan Meteorological Agency"
    ),
    fluidRow(column(
      12,
      textOutput("datachecktime"),
      selectInput(
        "stationname",
        label = "Select Station",
        selectList,
        selectize = FALSE
      )
    )),
    fluidRow(
      column(4, wellPanel(htmlOutput("gvis"))),
      column(3, DT::dataTableOutput("table1")),
      column(3, DT::dataTableOutput("table2")),
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
          height = "700"
        )
      )
    ),
    fluidRow(
      div(
        "Caution: Removed rows containing missing values or  non-finite values.",
        style = "color:black",
        align = "center"
      ),
      column(6, wellPanel(plotOutput("plot1"))),
      column(6, wellPanel(plotOutput("plot2")))
    ),
    fluidRow(column(12, htmlOutput("remarktext"))),
    fluidRow(column(12, htmlOutput("history")))
  )
)