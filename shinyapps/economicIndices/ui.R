library(shiny)
library(tseries)
library(forecast)
library(DT)
library(nortest)
library(RCurl)
library(e1071)
library(dplyr)
library(pastecs)
library(psych)
library(quantmod)
shinyUI(
  tabsetPanel(
    tabPanel(
      "Japanese Government Bonds",
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
                 headerPanel('Japanese Government Bonds Interest Rate. Data source:Ministry of Finance Japan'),
                 fluidRow(
                   column(6,actionButton('searchAction', label = 'Import Raw Data from MOF(take a few seconds to import.)')),
                   column(6,align="right",textOutput('latestDataDownloadTime'))
                 ),
                 conditionalPanel(condition = "output.latestDataDownloadTime!=''",  tags$hr()),
                 fluidRow(
                   column(2,
                          uiOutput("year"),
                          uiOutput("dataRange"),
                          uiOutput("lineType"),
                          uiOutput("dataType")
                   ),
                   conditionalPanel(
                     condition = "output.latestDataDownloadTime!=''",
                     column(5, plotOutput("plot1", height = "650px")),
                     column(5, DT::dataTableOutput("table1"))
                   )
                 ),
                 conditionalPanel(condition = "output.latestDataDownloadTime!=''",  tags$hr(),
                                  fluidRow(column(12, DT::dataTableOutput("table2"))),
                                  fluidRow(column(12, htmlOutput("remarktext")))
                 ),
                 fluidRow(column(12, htmlOutput("disclaimer")))
          ),
          column(2,
                 a("@AMC2_Japan", class = "twitter-timeline",  href = "https://twitter.com/AMC2_Japan",
                   "data-widget-id" = "449799943780200448", width = "100%", height = "1200"
                 )
          )
        ) 
      )
    )
    ,
# historical FX    
    tabPanel(
      "Historical Foreign Exchange Rate",
      fluidPage(
        fluidRow(
          column(10,
                 headerPanel(
                   "Historical Foreign Exchange Rate(exchange quotation,middle rate). Raw Data source:Mizuho Bank, Ltd."
                 ),
                 fluidRow(
                   column(6,actionButton('searchActionFX', label = 'Import Historical FX Data(take a few seconds to import.)')),
                   column(6,align="right",textOutput('latestDataDownloadTimeFX'))
                 ),
                 conditionalPanel(condition = "output.latestDataDownloadTimeFX!=''",  tags$hr()), 
                 fluidRow(
                   column(2,
                          uiOutput("currencyFX"), 
                          uiOutput("dataRangeFX"), 
                          uiOutput("charttypeFX"), 
                          uiOutput("datatypeFX") 
                   ),
                   conditionalPanel(
                     condition = "output.latestDataDownloadTimeFX!=''",	
                     column(5,
                            plotOutput("plot1FX", height = "600px")),
                     column(5,
                            DT::dataTableOutput("table1FX"))
                   )   
                 ),
                 conditionalPanel(condition = "output.latestDataDownloadTimeFX!=''",  tags$hr(),
                                  fluidRow(column(12, DT::dataTableOutput("table2FX"))),
                                  fluidRow(column(12, htmlOutput("remarktextFX")))
                 ),  
                 fluidRow(column(12, htmlOutput("disclaimerFX")))
          ),
          column(2,
                 a("@AMC2_Japan", class = "twitter-timeline",  href = "https://twitter.com/AMC2_Japan",
                   "data-widget-id" = "449799943780200448", width = "100%", height = "1200"
                 )
          )
        ) 
          )
      )   
# historical FX 
# currency analysis
,
tabPanel(
  "Currency Analysis",
  fluidPage(
    fluidRow(
      column(10,
             headerPanel(
               "Currency Analysis. Raw Data source:OANDA Corporation - https://www.oanda.com/"
             ),
             fluidRow(
               column(6,actionButton('searchAction_FXOanda', label = 'Import FX Data(take a few seconds to import.)')),
               column(6,align="right",textOutput('latestDataDownloadTime_FXOanda'))
             ),
             conditionalPanel(condition = "output.latestDataDownloadTime_FXOanda!=''",  tags$hr()), 
             fluidRow(column(12, align='center', uiOutput("dataRange_FXOanda"))),
             fluidRow(column(12,
                             conditionalPanel(
                               condition = "output.latestDataDownloadTime_FXOanda!=''",	
                               column(12,
                                      plotOutput("plot1_FXOanda", height = "300px"),tags$hr(),
                                      plotOutput("plot2_FXOanda", height = "300px")
                               )
                             )   
             )
             ),
             conditionalPanel(condition = "output.latestDataDownloadTime_FXOanda!=''",  tags$hr(),
                              fluidRow(column(12, 
                                              DT::dataTableOutput("table1_FXOanda"),  tags$hr(),
                                              tags$h3(textOutput("title01_FXOanda")),
                                              verbatimTextOutput("summary01_FXOanda"),
                                              verbatimTextOutput("psych01_FXOanda"), 
                                              verbatimTextOutput("pastecs01_FXOanda"),
                                              verbatimTextOutput("adf01_FXOanda"),
                                              tags$h3(textOutput("title011corr_FXOanda")),
                                              verbatimTextOutput("cor01_FXOanda"),
                                              tags$h3(textOutput("title021corr_FXOanda")),
                                              verbatimTextOutput("cor03_FXOanda"),
                                              tags$hr(),
                                              tags$h3(textOutput("title02_FXOanda")),
                                              verbatimTextOutput("summary02_FXOanda"),
                                              verbatimTextOutput("psych02_FXOanda"), 
                                              verbatimTextOutput("pastecs02_FXOanda"),
                                              verbatimTextOutput("adf02_FXOanda"),
                                              tags$hr(),
                                              tags$h3(textOutput("title012corr_FXOanda")),
                                              verbatimTextOutput("cor02_FXOanda"),
                                              tags$h3(textOutput("title022corr_FXOanda")),
                                              verbatimTextOutput("cor04_FXOanda")
                              )
                              ),
                              fluidRow(column(12, htmlOutput("remarktextFX_FXOanda")))
             ),  
             fluidRow(column(12, htmlOutput("disclaimerFX_FXOanda")))
      ),
      column(2,
             a("@AMC2_Japan", class = "twitter-timeline",  href = "https://twitter.com/AMC2_Japan",
               "data-widget-id" = "449799943780200448", width = "100%", height = "1200"
             )
      )
    )    
  )
)
# currency analysis
  )
)