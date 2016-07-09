library(shiny)
library(RCurl)
script <-
  getURL(
    "https://raw.githubusercontent.com/am-consulting/Rscript/master/amccLinkList.r",
    ssl.verifypeer = FALSE
  )
eval(parse(text = script))

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
        tags$h5("CSV file sampe(at least two numeric data columns)"),
        tags$a(href="https://raw.githubusercontent.com/am-consulting/CSVDataAtGitHub/master/example-USDJPY-NIKKEI.csv",
               "･USD/JPY & NIKKEI",target="_blank"),
        tags$hr(), 
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
      conditionalPanel(
        condition = "output.completiontime!=''",
          fluidRow(
            column(4, plotOutput("tsplot"),
        downloadButton(outputId = "Download1", label = "Download TimeSeries Plot")),
            column(4, plotOutput("ccf"),
        downloadButton(outputId = "Download2", label = "Download Cross Correlation Plot")),
            column(4, plotOutput("scatter"),
        downloadButton(outputId = "Download3", label = "Download Scatter Plot"))
          ),
          tags$hr(),
          fluidRow(column(6, tags$b("without unit root:VAR{vars}→irf{vars}"), plotOutput("varresult"),
        downloadButton(outputId = "Download4", label = "Download VAR without unit root")),
                   column(6, tags$b("with unit root:ca.jo{urca}→vec2var{vars}→irf{vars}"), plotOutput("vecmresult"),
        downloadButton(outputId = "Download5", label = "Download VAR with unit root"))
          ),
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
      )          
        ),
        tabPanel("Statistic Analysis",
                   conditionalPanel(
                     condition = "output.completiontime!=''",
                 fluidRow(
                     column(
                       12,fluidRow(
                       column(6,
                       tags$h4("Summary"),
                       DT::dataTableOutput("summaryDF")
                       ),
                       column(6,
                       tags$h4("Linear Regression Model"),
                       DT::dataTableOutput("fitDF")
                       )
                       ),tags$hr(),
                       tags$h4("1st Step:Unit Root Test"),
                       DT::dataTableOutput("adfDF"),
                       tags$hr(),
                       fluidRow(column(12,
                       tags$h4("2nd Step:Cointegration Test")
                       )
                       ),
                       fluidRow(
                       column(4,   
                       tags$h5("ADF test for Residuals of OLS"),
                       DT::dataTableOutput("fitResidualsDF")
                       ),
                       column(4,
                       tags$h5("dwtest{lmtest}"),
                       DT::dataTableOutput("dwDF")
                       ),
                       column(4,
                       tags$h5("po.test{tseries}"),
                       DT::dataTableOutput("potestDF")
                       )
                       ),tags$hr(),
                       tags$h4("3rd Step:Impulse Responce"),
                       DT::dataTableOutput("varselectResult"),
                       tags$hr(),
                       fluidRow(column(6,
                       DT::dataTableOutput("varDF")
                       ),
                       column(6,
                       DT::dataTableOutput("vecmDF")
                       )
                       )
                     )
                   )
                 )),
        tabPanel("Table",conditionalPanel(
                     condition = "output.completiontime!=''", DT::dataTableOutput("dt")))
      ),
      htmlOutput("remarktext"),
      htmlOutput("history"), 
      htmlOutput("gitcode"),
      htmlOutput("linkList")
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
        height = "3000"
      )
    )
  )
))