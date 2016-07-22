library(shiny)
library(DT)
options(shiny.maxRequestSize = 0.3 * 1024 ^ 2)
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

  reactiveData <- reactive({
    inFile <- input$file
    if (!is.null(inFile)) {
      if (input$inverse == 1) {
        selectInverse <- "F"
      } else{
        selectInverse <- "T"
      }
      tmp0 <- sapply(tmp,as.numeric)
      dataSet <- c(t(tmp0))
      if (is.null(input$dataScale) |
          input$dataScale == 0 |
          input$dataScale == "") {
        dataScale <-
          1
      } else{
        dataScale <- as.numeric(input$dataScale)
      }# ex. 3920(gal)/6182761
      dataTitle<-input$dataTitle
      z <- dataSet
      z <- z * dataScale
      zOffset <- mean(z)
      z <- z - zOffset
      n <- length(z) # Data number
      if (is.null(input$sf) |
          input$sf == 0 |
          input$sf == "") {
        sf <-
          1
      } else{
        sf <- as.numeric(input$sf)
      } # Sampling frequency(Hz)
      si <- 1 / sf # Sampling interval(s)
      i <- 0:(n - 1)
      f <- i * sf / n
      nf <- sf / 2 #Nyquist frequency
      dataSet <- data.frame(Time = i * si, dataSet, z)
      spec <- fft(z, inverse = selectInverse)
      
      output$freqslider <- renderUI({
        sliderInput(
          "freqslider",
          label = "Frequency(Hz)",
          min = 0,
          max = nf,
          value = c(0, nf),
          step = 0.1
        )
      })
      
      output$timeslider <- renderUI({
        sliderInput(
          "timeslider",
          label = "Time(s)",
          min = 0,
          max = si * (n - 1),
          value = c(0, si * (n - 1)),
          step = 1
        )
      })

      funPlot1 <- function(){
        par(mar = c(5, 4, 3, 3), family = "Noto Sans Japanese")
        plot(
          si * i,
          z,
          type = "h",
          xlab = "Time(s)" ,
          ylab = "z" ,
          panel.first = grid(
            nx = NULL,
            ny = NULL,
            equilogs = T
          ) ,
          main = paste(
            dataTitle,
            "\nz=Sampling Data*Scale-mean(Sampling Data*Scale)"
          ),
          cex.axis = 1,
          cex.lab = 1,
          cex.main = 1.5,
          xlim = c(input$timeslider[1], input$timeslider[2])
        )
      }
      
      output$Plot01 <- renderPlot({funPlot1()}, height = 500)
  
      output$Download1 <- downloadHandler(
        filename = function() {
          paste("SaECaNet-",
                format(as.POSIXlt(Sys.time(), "GMT"), "%Y-%m-%d-%H-%M-%S"),
                ".png",
                sep = "")
        },
      content = function(file) {
          png(file, width = 1500, height = 800)
          funPlot1()
          dev.off()
        }
      )         

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
      
      modspec <- (Mod(spec)) ^ 2 / n
      funPlot2 <- function(){
        par(mar = c(5, 4, 3, 3), family = "Noto Sans Japanese")
        plot(
          f,
          modspec,
          type = "l",
          xlab = "Frequency(Hz)" ,
          ylab = "" ,
          panel.first = grid(
            nx = NULL,
            ny = NULL,
            equilogs = T
          ) ,
          main = paste(dataTitle, "\nMod(fft(z))^2/n"),
          cex.axis = 1,
          cex.lab = 1,
          cex.main = 1.5,
          xlim = c(input$freqslider[1], input$freqslider[2])
        )
      }

      output$Plot02 <- renderPlot({funPlot2()}, height = 500)
  
      output$Download2 <- downloadHandler(
        filename = function() {
          paste("SaECaNet-",
                format(as.POSIXlt(Sys.time(), "GMT"), "%Y-%m-%d-%H-%M-%S"),
                ".png",
                sep = "")
        },
      content = function(file) {
          png(file, width = 1500, height = 800)
          funPlot2()
          dev.off()
        }
      )         

      moddataSet <- data.frame(f, modspec)
      moddataSet <- subset(moddataSet, moddataSet[, 1] <= nf)
      colnames(moddataSet) <- c("Frequency(Hz)", "Mod(fft(z))^2/n")
      output$dataSet02 <- DT::renderDataTable(
        moddataSet,
        rownames = F,
        caption = paste("Table 2:FFT-Absolute value."),
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
      
      respec <- (Re(spec)) ^ 2 / n
      funPlot3 <- function(){
        par(mar = c(5, 4, 3, 3), family = "Noto Sans Japanese")
        plot(
          f,
          respec,
          type = "l",
          xlab = "Frequency(Hz)" ,
          ylab = "" ,
          col = "blue",
          panel.first = grid(
            nx = NULL,
            ny = NULL,
            equilogs = T
          ) ,
          main = paste(dataTitle, "\nRe(fft(z))^2/n"),
          cex.axis = 1,
          cex.lab = 1,
          cex.main = 1.5,
          xlim = c(input$freqslider[1], input$freqslider[2])
        )
      }

      output$Plot03 <- renderPlot({funPlot3()}, height = 500)
  
      output$Download3 <- downloadHandler(
        filename = function() {
          paste("SaECaNet-",
                format(as.POSIXlt(Sys.time(), "GMT"), "%Y-%m-%d-%H-%M-%S"),
                ".png",
                sep = "")
        },
      content = function(file) {
          png(file, width = 1500, height = 800)
          funPlot3()
          dev.off()
        }
      )         

      redataSet <- data.frame(f, respec)
      redataSet <- subset(redataSet, redataSet[, 1] <= nf)
      colnames(redataSet) <- c("Frequency(Hz)", "Re(fft(z))^2/n")
      output$dataSet03 <- DT::renderDataTable(
        redataSet,
        rownames = F,
        caption = paste("Table 3:FFT-Real part."),
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
      
      imspec <- (Im(spec)) ^ 2 / n
      funPlot4 <- function(){
        par(mar = c(5, 4, 3, 3), family = "Noto Sans Japanese")
        plot(
          f,
          imspec,
          type = "l",
          xlab = "Frequency(Hz)" ,
          ylab = "" ,
          col = "red",
          panel.first = grid(
            nx = NULL,
            ny = NULL,
            equilogs = T
          ) ,
          main = paste(dataTitle, "\nIm(fft(z))^2/n"),
          cex.axis = 1,
          cex.lab = 1,
          cex.main = 1.5,
          xlim = c(input$freqslider[1], input$freqslider[2])
        )
      }

      output$Plot04 <- renderPlot({funPlot4()}, height = 500)
  
      output$Download4 <- downloadHandler(
        filename = function() {
          paste("SaECaNet-",
                format(as.POSIXlt(Sys.time(), "GMT"), "%Y-%m-%d-%H-%M-%S"),
                ".png",
                sep = "")
        },
      content = function(file) {
          png(file, width = 1500, height = 800)
          funPlot4()
          dev.off()
        }
      )         

      imdataSet <- data.frame(f, imspec)
      imdataSet <- subset(imdataSet, imdataSet[, 1] <= nf)
      colnames(imdataSet) <- c("Frequency(Hz)", "Im(fft(z))^2/n")
      output$dataSet04 <- DT::renderDataTable(
        imdataSet,
        rownames = F,
        caption = paste("Table 4:FFT-Imaginary part."),
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
      
      output$zmean <- renderPrint({
        zOffset
      })
    } else{
      return(NULL)
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
    <li>2016-06-24:ver.1.0.0</li>
    <li>2016-06-26:ver.1.0.1</li>
    <li>2016-07-21:ver.1.0.2</li>
    </ol>"
    HTML(str)
  })
  
  output$gitcode <- renderUI({
    str <- "<hr>
    <b>Code</b><br>
    <ol>
    <li><a href=\"https://github.com/am-consulting/Rscript/tree/master/shinyapps/spectralAnalysisByFFT\" target=\"_blank\">Move to GitHub</a></li>
    </ol>"
    HTML(str)
  })
  
  output$completionTime <- renderText({
    inFile <- input$file
    if (is.null(inFile)) {
      return(NULL)
    } else{
    importData()
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