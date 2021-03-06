library(shiny)
library(DT)
library(wmtsa)
# Reference: https://cran.r-project.org/web/packages/wmtsa/wmtsa.pdf
options(shiny.maxRequestSize = 0.25 * 1024 ^ 2)
options(download.file.method="libcurl")
shinyServer(function(input, output, session) {
  importData <- reactive({
    inFile <- input$file
    if (is.null(inFile)) {
      return(NULL)
    } else{
      tmp <<- read.csv(
        inFile$datapath,
        header = F,
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
      tmp0 <- sapply(tmp,as.numeric)
      buf <- c(t(tmp0))
      buf <- data.frame(ID = seq(1, length(buf)), Data=buf, check.names = F)

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
      buf<<-buf
    }
  })
  
  reactiveData <- reactive({
      if (is.null(input$dataRange[1])) {
        return(NULL)
      } else{
        
      dataTitle<-input$dataTitle
      if(dataTitle==""){dataTitle<-"Data"}
      
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
      colnames(dataSet)[2] <- dataTitle
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
        par(mar = c(5, 4, 10, 3), family = "Noto Sans Japanese")
        plot(waveletData, series = T)
        mtext(paste(colnames(dataSet)[2],"- Continuous Wavelet Transform"), side = 3, line = 0, cex=1.5)
      })

      output$Download1 <- downloadHandler(
        filename = function() {
          paste("SaECaNet-",
                format(as.POSIXlt(Sys.time(), "GMT"), "%Y-%m-%d-%H-%M-%S"),
                ".png",
                sep = "")
        },
      content = function(file) {
          png(file, width = 1500, height = 800)
          plot(waveletData, series = T)
          mtext(paste(colnames(dataSet)[2],"- Continuous Wavelet Transform"), side = 3, line = 0, cex=1.5)
          dev.off()
        }
      )

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
          cex.main = 1.5
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
    }
  })
  
  output$remarktext <- renderUI({
    str <- "<hr>
    <b>Remarks</b><br>
    <ol>
    <li>Sample Data Original Source:<a href=\"http://strongmotioncenter.org/\" target=\"_blank\">Center for Engineering Strong Motion Data</a></li>
    <li><a href=\"http://www.saecanet.com\" target=\"_blank\">SaECaNet</a></li>
    <li>Other apps <a href=\"http://webapps.saecanet.com\" target=\"_blank\">SaECaNet - Web Applications</a></li>
    <li><a href=\"http://am-consulting.co.jp\" target=\"_blank\">Asset Management Consulting Corporation / アセット･マネジメント･コンサルティング株式会社</a></li>
    <li><a href=\"http://www.saecanet.com/subfolder/disclaimer.html\" target=\"_blank\">Disclaimer</a></li>
    </ol>"
    HTML(str)
  })
  
  output$history <- renderUI({
    str <- "<hr>
    <b>History</b><br>
    <ol>
    <li>2016-06-27:ver.1.0.0</li>
    <li>2016-07-22:ver.1.0.1</li>
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
        inFile <- input$file
    if (is.null(inFile)) {
      return(NULL)
    } else{
    importData()
    selectData()
    reactiveData()
    paste("Completion(UTC):" ,
          as.character(completionTime))
    }
  })
  
  output$linkList <- renderUI({
    str <- linkList
    HTML(str)
  })
      
})