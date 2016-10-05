library(shiny)
shinyUI(
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