library(shiny)
shinyUI(
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