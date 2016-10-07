library(shiny)
shinyUI(
  # Sea Surface Temperature
  tabPanel(      
    "Sea Surface Temperature",  
  fluidPage(
    fluidRow(
      column(10,
             headerPanel("Sea Surface Temperature. Raw Data source: Japan Meteorological Agency"),
             fluidRow(column(12, htmlOutput("figure01_SST"))),
             fluidRow(
               column(6,actionButton('searchAction_SST', label = 'Import Data(take a few seconds to import.)')),
               column(6,align="right",textOutput('latestDataDownloadTime_SST'))
             ),
             conditionalPanel(condition = "output.latestDataDownloadTime_SST!=''",  tags$hr()), 
             fluidRow(column(12, align='center', uiOutput("region_SST"))),
             fluidRow(column(12,
                             conditionalPanel(
                               condition = "output.latestDataDownloadTime_SST!=''",	
                               column(12,
                                      h2(textOutput("datatitle_SST"))
                               ),
                               column(12,
                                      column(6, plotOutput("plot1_SST")),
                                      column(6, plotOutput("plot2_SST"),tags$hr())
                               ),
                               column(12,
                                      column(6, plotOutput("plot4_SST")),
                                      column(6, plotOutput("plot5_SST"))
                               ),
                               column(12,tags$hr(),
                                      plotOutput("plot3_SST")
                               ),
                               column(12,tags$hr(),
                                      column(6, DT::dataTableOutput("table1_SST"), 
                                             verbatimTextOutput("adf_SST"), verbatimTextOutput("psych_SST")),
                                      column(6, verbatimTextOutput("summary_SST"), 
                                             verbatimTextOutput("summary_fit_SST"), verbatimTextOutput("confint_SST"))
                               ),
                               div(
                                 "Caution: Rows containing missing values or non-finite values are removed.",
                                 style = "color:black",
                                 align = "center"
                               )
                             )   
             )
             ),
             conditionalPanel(condition = "output.latestDataDownloadTime_SST!=''",  
                              fluidRow(column(12, htmlOutput("remarktext_SST")))),  
             fluidRow(column(12, htmlOutput("disclaimer_SST")))
      ),
      column(2,
             a("@AMC2_Japan", class = "twitter-timeline",  href = "https://twitter.com/AMC2_Japan",
               "data-widget-id" = "449799943780200448", width = "100%", height = "1200"
             )
      )
    ) 
  )
  )
  # Sea Surface Temperature
)