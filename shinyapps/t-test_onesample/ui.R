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
  headerPanel("t-test:one sample."),
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
         uiOutput("targetData"),
        uiOutput("selectrow"),
        uiOutput("mu"),
        uiOutput("alternative"),
        tags$hr(),
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
      fluidRow(
        column(4, plotOutput("hist")),
        column(4, plotOutput("scatter")),
        column(4, plotOutput("qq"))
      ),
      tags$hr()
      ,
      fluidRow(
        conditionalPanel(
          condition = "output.completiontime!=''",
          column(
            12,
            DT::dataTableOutput("summaryDF"),
            tags$hr(),
            DT::dataTableOutput("ttestDF"),
            tags$hr(),
            DT::dataTableOutput("DF")
            
          )
        )
      ),
      DT::dataTableOutput("dt")
      ,
      htmlOutput("remarktext"),
      htmlOutput("history"), 
      htmlOutput("gitcode")
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
        height = "2000"
      )
    )
  )
))