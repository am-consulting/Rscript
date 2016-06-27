library(shiny)
library(DT)
library(wmtsa)
# Reference: https://cran.r-project.org/web/packages/wmtsa/wmtsa.pdf
options(shiny.maxRequestSize = 0.5 * 1024 ^ 2)
shinyServer(function(input, output, session) {
  importData <- reactive({
    inFile <- input$file
    if (is.null(inFile)) {
      return(NULL)
    } else{
      tmp <<- read.csv(
        inFile$datapath,
        header = T,
        sep =  ',',
        quote = '',
        check.names = F,
        na.strings = c(""),
        stringsAsFactors = F
      )
    }
  })
  
  selectData <- reactive({
    inFile <- input$file
    if (is.null(inFile)) {
      return(NULL)
    } else{
      output$objList <- renderUI({
        if (nrow(tmp) == 0) {
          return(NULL)
        } else{
          selectInput("obj",
                      "Select Data",
                      colnames(tmp),
                      selected = colnames(tmp)[1])
        }
      })
      
      buf <-
        na.omit(tmp[, which(input$obj == colnames(tmp)), drop = F])
      buf <<- data.frame(ID = seq(1, nrow(buf)), buf, check.names = F)
      
      output$dataRange <- renderUI({
        sliderInput(
          "dataRange",
          label = "Data Range",
          min = 1,
          max = nrow(buf),
          value = c(1, nrow(buf)),
          step = 1
        )
      })
      
      output$lag <- renderUI({
        numericInput("lag", label = "lag", value = 0)
      })
      
      output$differences <- renderUI({
        numericInput("differences", label = "differences", value = 1)
      })
      
      output$n.scale <- renderUI({
        numericInput("n.scale", label = "n.scale", value = 100)
      })
      
      output$shift <- renderUI({
        numericInput("shift", label = "shift", value = 5)
      })
      
      output$variance <- renderUI({
        numericInput("variance", label = "variance", value = 1)
      })
      
      output$wavelet <- renderUI({
        radioButtons(
          "wavelet",
          label = "wavelet",
          choices = list(
            "haar" = "haar",
            "gaussian1" = "gaussian1",
            "gaussian2" = "gaussian2",
            "morlet" = "morlet"
          ),
          selected = "gaussian2"
        )
      })
    }
  })
  
  reactiveData <- reactive({
    if (!is.null(input$obj)) {
      dataSet <-
        buf[c(input$dataRange[1]:input$dataRange[2]), ]
      if (0 < input$lag & 0 < input$differences) {
        dataSet <-
          data.frame(ID = dataSet[, 1][-(1:(input$lag * input$differences))],
                     diff(
                       dataSet[, 2],
                       lag = input$lag,
                       differences = input$differences
                     ))
      }
      colnames(dataSet)[2] <- colnames(buf)[2]
      dataSet$Time <- c(1:nrow(dataSet))
      x <- as.double(as.vector(dataSet[, 2]))
      waveletData <-
        wavCWT(
          x ,
          n.scale = input$n.scale,
          shift = input$shift,
          variance = input$variance ,
          wavelet = input$wavelet
        )
      
      output$Plot02 <- renderPlot({
        plot(waveletData, series = T)
      })
      
      output$Plot01 <- renderPlot({
        par(mar = c(5, 4, 3, 3), family = "Noto Sans Japanese")
        plot(
          dataSet[, 1],
          dataSet[, 2],
          type = "l",
          xlab = "ID" ,
          ylab = colnames(dataSet)[2] ,
          panel.first = grid(
            nx = NULL,
            ny = NULL,
            equilogs = T
          ) ,
          main = paste(colnames(dataSet)[2],
                       "\nTime Series Data"),
          cex.axis = 1,
          cex.lab = 1,
          cex.main = 1
        )
      })
      
      output$dataSet01 <- DT::renderDataTable(
        dataSet,
        rownames = F,
        caption = paste("Table 1:Time Series Data."),
        options = list(
          autoWidth = T,
          info = T,
          lengthChange = T,
          ordering = T,
          searching = T,
          scrollX = T,
          lengthMenu = list(c(10, 1), c("10", "1")),
          orderClasses = TRUE,
          order = list(list(0, "asc")),
          paging = T
        )
      )
    } else{
      print("something wrong")
    }
  })
  
  output$remarktext <- renderUI({
    str <- "<hr>
    <b>Remarks</b><br>
    <ol>
    <li><a href=\"http://www.saecanet.com\" target=\"_blank\">SaECaNet</a></li>
    <li>Other apps <a href=\"http://webapps.saecanet.com\" target=\"_blank\">SaECaNet - Web Applications</a></li>
    <li><a href=\"http://am-consulting.co.jp\" target=\"_blank\">Asset Management Consulting Corporation</a></li>
    <li><a href=\"http://www.saecanet.com/subfolder/disclaimer.html\" target=\"_blank\">Disclaimer</a></li>
    </ol>"
    HTML(str)
  })
  
  output$history <- renderUI({
    str <- "<hr>
    <b>History</b><br>
    <ol>
    <li>2016-06-27:ver.1.0.0</li>
    </ol>"
    HTML(str)
  })
  
  output$gitcode <- renderUI({
    str <- "<hr>
    <b>Code</b><br>
    <ol>
    <li><a href=\"https://github.com/am-consulting/Rscript/tree/master/shinyapps/timeseriesanalysisBywmsta\" target=\"_blank\">Move to GitHub</a></li>
    </ol>"
    HTML(str)
  })
  
  output$completionTime <- renderText({
    importData()
    selectData()
    reactiveData()
    paste("Completion(UTC):" ,
          as.character(completionTime))
  })
  
})