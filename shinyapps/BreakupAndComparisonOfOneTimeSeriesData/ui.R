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
  headerPanel("Breakup and Comparison of One Time Series Data."),
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
      uiOutput("datadate"),
      uiOutput("datay"),
      uiOutput("selectrow"),
      uiOutput("startDate"),
      uiOutput("endDate"),
      tags$hr(),
      tags$b("Breakup Date"),
      dateInput('date01',
                label = 'Date:1st',
                value = as.Date("1989-4-1")),
      dateInput('date02',
                label = 'Date:2nd',
                value = as.Date("1997-4-1")),
      dateInput('date03',
                label = 'Date:3rd',
                value = as.Date("2014-4-1")),
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
    ),
    column(
      8,
      plotOutput("tsplot"),
      tags$br(),
      tags$br(),
      DT::dataTableOutput("dtResult"),
      tags$br(),
      tags$br(),
      DT::dataTableOutput("dt"),
      htmlOutput("remarktext"),
      htmlOutput("history"), 
      htmlOutput("gitcode")
    ),
    column(
      2,
      a(
        "@AMC2_Japan",
        class = "twitter-timeline",
        href = "https://twitter.com/AMC2_Japan",
        "data-widget-id" = "449799943780200448",
        width = "100%",
        height = "3000"
      )
    )
  )
))