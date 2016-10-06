require(shiny)
require(RCurl)
require(googleVis)
require(DT)
library(ggplot2)
library(scales)
library(lubridate)
library(XML)
library(tseries)
library(dplyr)
library(reshape2)
library(xtable)
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
    # SeaIceIndex
    ,
    tabPanel(      
      "Sea Ice Index Data",
      fluidPage(
        fluidRow(
          column(10,
                 headerPanel(
                   "Sea Ice Index Data. Raw Data source: National Snow and Ice Data Center"
                 ),
                 fluidRow(
                   column(6,actionButton('searchAction_SeaIceIndex', label = 'Import Data(take a few seconds to import.)')),
                   column(6,align="right",textOutput('latestDataDownloadTime_SeaIceIndex'))
                 ),
                 conditionalPanel(condition = "output.latestDataDownloadTime_SeaIceIndex != ''",  tags$hr()), 
                 fluidRow(column(12, align='center', uiOutput("regionSeaIceIndex"))),
                 fluidRow(column(12,
                                 conditionalPanel(
                                   condition = "output.latestDataDownloadTime_SeaIceIndex != ''",	
                                   column(12,
                                          h2(textOutput("datatitleSeaIceIndex")),
                                          column(4, wellPanel(plotOutput("plot1_SeaIceIndex"))),
                                          column(4, wellPanel(plotOutput("plot2_SeaIceIndex"))),
                                          column(4, wellPanel(plotOutput("plot3_SeaIceIndex")))
                                   )
                                 )   
                 )
                 ),
                 fluidRow(column(12, 
                                 conditionalPanel(condition = "output.latestDataDownloadTime_SeaIceIndex != ''",  tags$hr(), 
                                                  wellPanel(div(style = "height:660px;", plotOutput("plot4_SeaIceIndex"))))
                 )
                 ),
                 conditionalPanel(condition = "output.latestDataDownloadTime_SeaIceIndex != ''", tags$hr(),
                                  fluidRow(column(12, wellPanel(DT::dataTableOutput("table1_SeaIceIndex")))), tags$hr(),
                                  tags$h3(textOutput("title01_SeaIceIndex")),
                                  verbatimTextOutput("summary01_SeaIceIndex"),
                                  verbatimTextOutput("psych01_SeaIceIndex"), 
                                  verbatimTextOutput("pastecs01_SeaIceIndex"),
                                  fluidRow(column(12, htmlOutput("remarktext_SeaIceIndex")))
                 ),  
                 fluidRow(column(12, htmlOutput("disclaimer_SeaIceIndex")))
          ),
          column(2,
                 a("@AMC2_Japan", class = "twitter-timeline",  href = "https://twitter.com/AMC2_Japan",
                   "data-widget-id" = "449799943780200448", width = "100%", height = "1200"
                 )
          )
        ) 
      )
    )
    # SeaIceIndex
    # Sunspot
    ,
    tabPanel(      
      "Sunspot Number",
      fluidPage(
        fluidRow(
          column(10,
                 headerPanel("Sunspot Number. Raw Data source: Royal Observatory of Belgium - http://www.astro.oma.be/"),
                 fluidRow(
                   column(6,actionButton('searchAction_Sunspot', label = 'Import Data(take a few seconds to import.)')),
                   column(6,align="right",textOutput('latestDataDownloadTime_Sunspot'))
                 ),
                 conditionalPanel(condition = "output.latestDataDownloadTime_Sunspot!=''",  tags$hr()), 
                 fluidRow(column(12, align='center', uiOutput("region_Sunspot"))),
                 fluidRow(column(12,
                                 conditionalPanel(
                                   condition = "output.latestDataDownloadTime_Sunspot!=''",	
                                   column(12, h3(textOutput("datatitle_Sunspot"))),
                                   column(12,
                                          column(6, wellPanel(plotOutput("plot1_Sunspot"))),
                                          column(6, wellPanel(plotOutput("plot2_Sunspot")))
                                   ),
                                   column(12,
                                          column(6, wellPanel(plotOutput("plot3_Sunspot"))),
                                          column(6, wellPanel(plotOutput("plot4_Sunspot")))
                                   ),
                                   column(12,tags$hr(),
                                          column(6, DT::dataTableOutput("table1_Sunspot")),
                                          column(6, DT::dataTableOutput("table2_Sunspot"))
                                   ),
                                   column(12,tags$hr(),
                                          verbatimTextOutput("summary01_Sunspot"),
                                          verbatimTextOutput("psych01_Sunspot"), 
                                          verbatimTextOutput("pastecs01_Sunspot")         
                                   ),
                                   div(
                                     "Caution: Rows containing missing values or non-finite values are removed.",
                                     style = "color:black",
                                     align = "center"
                                   )
                                 )   
                 )
                 ),
                 conditionalPanel(condition = "output.latestDataDownloadTime_Sunspot!=''",  
                                  fluidRow(column(12, htmlOutput("remarktext_Sunspot")))),  
                 fluidRow(column(12, htmlOutput("disclaimer_Sunspot")))
          ),
          column(2,
                 a("@AMC2_Japan", class = "twitter-timeline",  href = "https://twitter.com/AMC2_Japan",
                   "data-widget-id" = "449799943780200448", width = "100%", height = "1200"
                 )
          )
        ) 
      )
    )
    # Sunspot  
    # Carbon Dioxide Concentration
    ,
    tabPanel(      
      "CO2",
      fluidPage(
        fluidRow(
          column(10,
                 headerPanel("Carbon Dioxide Concentration. Raw Data source: Japan Meteorological Agency"),
                 fluidRow(
                   column(6,actionButton('searchAction_CO2', label = 'Import Data(take a few seconds to import.)')),
                   column(6,align="right",textOutput('latestDataDownloadTime_CO2'))
                 ),
                 conditionalPanel(condition = "output.latestDataDownloadTime_CO2!=''",  tags$hr()), 
                 fluidRow(column(12, align='center', uiOutput("stationname_CO2"))),
                 fluidRow(column(12,
                                 conditionalPanel(
                                   condition = "output.latestDataDownloadTime_CO2!=''",	
                                   column(12,
                                          column(6, wellPanel(htmlOutput("gvis_CO2"))),
                                          column(6, DT::dataTableOutput("table1_CO2"))
                                   ),
                                   column(12,
                                          column(6, wellPanel(plotOutput("plot1_CO2"))),
                                          column(6, wellPanel(plotOutput("plot2_CO2")))
                                          
                                   ),
                                   column(12,tags$hr(),
                                          column(6, wellPanel(plotOutput("plot3_CO2"))),
                                          column(6, wellPanel(plotOutput("plot4_CO2")))
                                   ),
                                   div(
                                     "Caution: Rows containing missing values or non-finite values are removed.",
                                     style = "color:black",
                                     align = "center"
                                   )
                                 )   
                 )
                 ),
                 conditionalPanel(condition = "output.latestDataDownloadTime_CO2!=''",  
                                  fluidRow(column(12, htmlOutput("remarktext_CO2")))),  
                 fluidRow(column(12, htmlOutput("disclaimer_CO2")))
          ),
          column(2,
                 a("@AMC2_Japan", class = "twitter-timeline",  href = "https://twitter.com/AMC2_Japan",
                   "data-widget-id" = "449799943780200448", width = "100%", height = "1200"
                 )
          )
        ) 
      )
    )
    # Carbon Dioxide Concentration
  )
)