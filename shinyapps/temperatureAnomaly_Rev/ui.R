library(shiny)
shinyUI(
  fluidPage(
    fluidRow(
      column(10,
             headerPanel("Monthly Mean Temperature Anomaly. Raw Data source: Japan Meteorological Agency"),
             fluidRow(
               column(6,actionButton('searchAction_MMT', label = 'Import Data(take a few seconds to import.)')),
               column(6,align="right",textOutput('latestDataDownloadTime_MMT'))
             ),
             conditionalPanel(condition = "output.latestDataDownloadTime_MMT!=''",  tags$hr()), 
             fluidRow(column(12, align='center', uiOutput("region_MMT"))),
             fluidRow(column(12,
                             conditionalPanel(
                               condition = "output.latestDataDownloadTime_MMT!=''",	
                               column(12,
                                      h2(textOutput("datatitle_MMT"))
                               ),
                               column(12,
                                      column(6, plotOutput("plot1_MMT")),
                                      column(6, plotOutput("plot2_MMT"))
                               ),
                               column(12,
                                      column(6, plotOutput("plot4_MMT")),
                                      column(6, plotOutput("plot5_MMT"))
                                      
                               ),
                               column(12,tags$hr(),
                                      plotOutput("plot3_MMT")
                               ),
                               column(12,tags$hr(),
                                      column(6, DT::dataTableOutput("table1_MMT")),
                                      column(6, DT::dataTableOutput("table1NA_MMT"))
                               ),
                               div(
                                 "Caution: Rows containing missing values or non-finite values are removed.",
                                 style = "color:black",
                                 align = "center"
                               )
                             )   
             )
             ),
             conditionalPanel(condition = "output.latestDataDownloadTime_MMT!=''",  
                              fluidRow(column(12, htmlOutput("remarktext_MMT")))),  
             fluidRow(column(12, htmlOutput("disclaimer_MMT")))
      ),
      column(2,
             a("@AMC2_Japan", class = "twitter-timeline",  href = "https://twitter.com/AMC2_Japan",
               "data-widget-id" = "449799943780200448", width = "100%", height = "1200"
             )
      )
    ) 
  )
)