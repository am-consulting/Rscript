library(shiny)
shinyUI(
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