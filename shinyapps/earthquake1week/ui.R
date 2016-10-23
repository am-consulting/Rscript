library(shiny)
shinyUI(
  # Earthquakes for the last week in Japan
  tabPanel(
    "Earthquakes for the last week in Japan",
    fluidPage(
      fluidRow(
        column(10,
               headerPanel("Earthquakes for the last week in Japan. Raw Data source: Japan Meteorological Agency"),
               fluidRow(
                 column(6,actionButton('searchAction_1weekEQ', label = 'Import Data(take a few seconds to import.)')),
                 column(6,align="right",textOutput('latestDataDownloadTime_1weekEQ'))
               ),
               conditionalPanel(condition = "output.latestDataDownloadTime_1weekEQ!=''",  tags$hr()),
               fluidRow(column(12,
                               conditionalPanel(
                                 condition = "output.latestDataDownloadTime_1weekEQ!=''",
                                 column(12,
                                        column(6, DT::dataTableOutput("table1_1week01EQ")),
                                        column(6, DT::dataTableOutput("table2_1week02EQ"))
                                 )
                               )
               )
               ),
               conditionalPanel(condition = "output.latestDataDownloadTime_1weekEQ!=''",
                                fluidRow(column(12, htmlOutput("remarktext_1weekEQ")))),
               fluidRow(column(12, htmlOutput("disclaimer_1weekEQ")))
        ),
        column(2,
               a("@AMC2_Japan", class = "twitter-timeline",  href = "https://twitter.com/AMC2_Japan",
                 "data-widget-id" = "449799943780200448", width = "100%", height = "1000"
               )
        )
      )
    )
  )
  # Earthquakes for the last week in Japan
)