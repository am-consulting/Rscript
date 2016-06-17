# Reference http://stackoverflow.com/questions/29378493/update-on-file-upload-shiny-r?answertab=active#tab-top
library(shiny)
library(RCurl)
script <-
  getURL(
    "https://raw.githubusercontent.com/am-consulting/Rscript/master/Rscript_HighestAuthority.r",
    ssl.verifypeer = FALSE
  )
eval(parse(text = script))
highestAuthority()
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
  headerPanel("Time Series Plot & Boxplot Chart with Highest Authority"),
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
      uiOutput("xAxis"),
      uiOutput("yAxis"),
      uiOutput("selectrow"),
      uiOutput("startDate"),
      uiOutput("endDate"),
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
      ,
      radioButtons(
        "HA",
        label = "Highest Authority",
        choices = indextitle,
        selected = indextitle[1]
      ),
      radioButtons(
        "dateType",
        label = "Date Axis",
        choices = list(
          "YYYY-MM-DD" = "%Y-%m-%d",
          "YYYY-MM" = "%Y-%m",
          "YYYY" = "%Y"
        ),
        selected = "%Y-%m"
      )
      )
    ),
    column(
      8,
      fluidRow(
        conditionalPanel(
          condition = "output.timenow!=''",
          plotOutput("plottimeseries", width = "100%", height = "500px")
        )
      ),
      fluidRow(
        conditionalPanel(
          condition = "output.timenow!=''",
          plotOutput("plotboxplot", width = "100%", height = "500px")
        )
      ),
      conditionalPanel(condition = "output.table1!=''",
                       DT::dataTableOutput("table1")),
      conditionalPanel(condition = "output.table1!=''",
                       DT::dataTableOutput("table2")),
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
)
)