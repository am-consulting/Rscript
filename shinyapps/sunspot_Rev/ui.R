library(shiny)
shinyUI(
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