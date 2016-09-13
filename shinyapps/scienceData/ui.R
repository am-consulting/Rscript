require(shiny)
require(RCurl)
require(googleVis)
require(DT)
options(download.file.method = "libcurl")
shinyUI(
  tabsetPanel(
    tabPanel(
      "Earthquake",
      fluidPage(
        tags$head(
          tags$link(rel = "stylesheet", href = "http://fonts.googleapis.com/css?family=Roboto+Condensed"),
          tags$link(rel = "shortcut icon", href = "http://knowledgevault.saecanet.com/components/saecanet.png"),
          tags$style("body{font-family: 'Roboto Condensed', sans-serif;}"),
          tags$script('!function(d,s,id){
                      var js,
                      fjs=d.getElementsByTagName(s)[0],
                      p=/^http:/.test(d.location)?\'http\':\'https\';
                      if(!d.getElementById(id)){js=d.createElement(s);
                      js.id=id;js.src=p+"://platform.twitter.com/widgets.js";
                      fjs.parentNode.insertBefore(js,fjs);}}(document,"script","twitter-wjs");'
          )
        ),
        fluidRow(
          column(10,
                 headerPanel('Earthquake. Raw Data source: United States Geological Survey'),
                 fluidRow(
                   column(6,actionButton('searchActionEarthquake', label = 'Import Raw Data(take a few seconds to import.)')),
                   column(6,align="right",textOutput('latestDataDownloadTimeEarthquake'))
                 ),
                 conditionalPanel(condition = "output.latestDataDownloadTimeEarthquake!=''",  tags$hr()),
                 fluidRow(column(
                   12,
                   textOutput("datachecktimeEarthquake"),
                   uiOutput("maptypeEarthquake")
                 )
                 ) ,
                 fluidRow(
                   column(12,                   
                          conditionalPanel(condition = "output.latestDataDownloadTimeEarthquake!=''",
                                           fluidRow(
                                             column(6, 
                                                    textOutput("titlegvisEarthquake"), htmlOutput("gvisEarthquake")
                                             ),
                                             column(6, DT::dataTableOutput("dtTableEarthquake"))
                                           ),
                                           tags$hr(),
                                           fluidRow(
                                             div(
                                               "Caution: Differences in magnitude type are unconsidered in magnitude histgram.",
                                               style = "color:black",
                                               align = "center"
                                             )
                                           ),
                                           fluidRow(column(6, plotOutput("plot1Earthquake")),
                                                    column(6, plotOutput("plot2Earthquake"))),
                                           fluidRow(column(6, plotOutput("plot3Earthquake")),
                                                    column(6, plotOutput("plot4Earthquake"))),
                                           fluidRow(column(12, htmlOutput("remarktextEarthquake")))
                          ),
                          fluidRow(column(12, htmlOutput("disclaimerEarthquake")))
                   )
                 )
          ), 
          column(2,
                 a("@AMC2_Japan", class = "twitter-timeline",  href = "https://twitter.com/AMC2_Japan",
                   "data-widget-id" = "449799943780200448", width = "100%", height = "1200"
                 )
          )
        )
      )
    )
  )
)