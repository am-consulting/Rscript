library(shiny)
shinyUI(fluidPage(
  tags$head(
    tags$link(rel = "stylesheet",
              href = "http://fonts.googleapis.com/css?family=Roboto+Condensed"),
    # favicon
    tags$link(rel = "apple-touch-icon-precomposed",
              href = "http://www.saecanet.com/subfolder/ico/apple-touch-icon-144-precomposed.png",
              sizes = "144x144"),
    tags$link(rel = "apple-touch-icon-precomposed",
              href = "http://www.saecanet.com/subfolder/ico/apple-touch-icon-114-precomposed.png",
              sizes = "114x114"),
    tags$link(rel = "apple-touch-icon-precomposed",
              href = "http://www.saecanet.com/subfolder/ico/apple-touch-icon-72-precomposed.png",
              sizes = "72x72"),
    tags$link(rel = "apple-touch-icon-precomposed",
              href = "http://www.saecanet.com/subfolder/ico/apple-touch-icon-52-precomposed.png",
              sizes = "52x52"),
    tags$link(rel = "shortcut icon",
              href = "http://www.saecanet.com/subfolder/ico/favicon.png"),
    # favicon
    tags$style("body{font-family: 'Roboto Condensed', sans-serif;}"),
    tags$script(
      '!function(d,s,id){var js,fjs=d.getElementsByTagName(s)[0],p=/^http:/.test(d.location)?\'http\':\'https\';if(!d.getElementById(id)){js=d.createElement(s);js.id=id;js.src=p+"://platform.twitter.com/widgets.js";fjs.parentNode.insertBefore(js,fjs);}}(document,"script","twitter-wjs");'
    )
  ),
  headerPanel("Statistic Analysis for Two Time Series Data."),
  textOutput("completiontime"),
  fluidRow(
    column(
      2,
      wellPanel(
        fileInput(
          'file',
          'Choose CSV File',
          accept = c('text/csv',
                     'text/comma-separated-values,text/plain',
                     '.csv')
        ),
        tags$script('$( "#file" ).on( "click", function() {
                    this.value = null;
                    });'),
         uiOutput("datax"),
        uiOutput("datay"),
        uiOutput("selectrow"),
        uiOutput("selectlag"),
        checkboxInput('header', 'Header', TRUE),
        radioButtons('sep', 'Separator',
                     c(
                       Comma = ',',
                       Semicolon = ';',
                       Tab = '\t'
                     ),
                     ','),
        radioButtons(
          'quote',
          'Quote',
          c(
            'None' = '',
            'Double Quote' = '"',
            'Single Quote' = "'"
          ),
          ''
        ),
        radioButtons('chkname', 'Check Names',
                     c('True' = 'T',
                       'False' = 'F'),
                     'F')
      )
    )  ,
    column(
      8,
      tabsetPanel(
        tabPanel(
          "Plot",
          fluidRow(
            column(4, plotOutput("tsplot")),
            column(4, plotOutput("ccf")),
            column(4, plotOutput("scatter"))
          ),
          tags$hr(),
          fluidRow(column(6, plotOutput("varresult")),
                   column(6, plotOutput("vecmresult"))),
          tags$hr(),
          fluidRow(
            column(4, plotOutput("acfx")),
            column(4, plotOutput("arimax")),
            column(4, plotOutput("histx"))
          ),
          tags$hr(),
          fluidRow(
            column(4, plotOutput("acfy")),
            column(4, plotOutput("arimay")),
            column(4, plotOutput("histy"))
          )
        ),
        tabPanel("Statistic Analysis",
                 fluidRow(
                   conditionalPanel(
                     condition = "output.completiontime!=''",
                     column(
                       12,
                       tags$h4("Summary"),
                       DT::dataTableOutput("summaryDF"),
                       tags$hr(),
                       
                       tags$h4("Linear Regression Model"),
                       DT::dataTableOutput("fitDF"),
                       tags$hr(),
                       
                       tags$h4("1st Step:Unit Root Test"),
                       DT::dataTableOutput("adfDF"),
                       tags$hr(),
                       
                       tags$h4("2nd Step:Cointegration Test"),
                       tags$h5("ADF test for Residuals of OLS"),
                       DT::dataTableOutput("fitResidualsDF"),
                       tags$hr(),
                       tags$h5("dwtest{lmtest}"),
                       DT::dataTableOutput("dwDF"),
                       tags$hr(),
                       tags$h5("po.test{tseries}"),
                       DT::dataTableOutput("potestDF"),
                       tags$hr(),
                       
                       tags$h4("3rd Step:Impulse Responce"),
                       DT::dataTableOutput("varselectResult"),
                       tags$hr(),
                       DT::dataTableOutput("varDF"),
                       tags$hr(),
                       DT::dataTableOutput("vecmDF")
                     )
                   )
                 )),
        tabPanel("Table", DT::dataTableOutput("dt"))
      ),
      htmlOutput("remarktext"),
      htmlOutput("history")
    ),
    column(
      2,
      a(
        "@AMC2_Japan",
        class = "twitter-timeline"
        ,
        href = "https://twitter.com/AMC2_Japan"
        ,
        "data-widget-id" = "449799943780200448",
        width = "100%",
        height = "1000"
      )
    )
  )
))