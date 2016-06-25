library(shiny)
library(DT)
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
    }
  })
  
  reactiveData <- reactive({
    if (!is.null(input$obj)) {
      if (input$inverse == 1) {
        selectInverse <- "F"
      } else{
        selectInverse <- "T"
      }
      dataSet <- tmp[, which(input$obj == colnames(tmp)), drop = F]
      if (is.null(input$dataScale) |
          input$dataScale == 0 |
          input$dataScale == "") {
        dataScale <-
          1
      } else{
        dataScale <- as.numeric(input$dataScale)
      }# ex. 3920(gal)/6182761
      z <- as.vector(dataSet[, 1])
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
      
      output$Plot01 <- renderPlot({
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
            colnames(dataSet)[2],
            "\nz=Sampling Data*Scale-mean(Sampling Data*Scale)"
          ),
          cex.axis = 1,
          cex.lab = 1,
          cex.main = 1,
          xlim = c(input$timeslider[1], input$timeslider[2])
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
          lengthMenu = list(c(5, 1), c("5", "1")),
          orderClasses = TRUE,
          order = list(list(0, "asc")),
          paging = T
        )
      )
      
      modspec <- (Mod(spec)) ^ 2 / n
      output$Plot02 <- renderPlot({
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
          main = paste(colnames(dataSet)[2], "\nMod(fft(z))^2/n"),
          cex.axis = 1,
          cex.lab = 1,
          cex.main = 1,
          xlim = c(input$freqslider[1], input$freqslider[2])
        )
      })
      
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
          lengthMenu = list(c(5, 1), c("5", "1")),
          orderClasses = TRUE,
          order = list(list(0, "asc")),
          paging = T
        )
      )
      
      respec <- (Re(spec)) ^ 2 / n
      output$Plot03 <- renderPlot({
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
          main = paste(colnames(dataSet)[2], "\nRe(fft(z))^2/n"),
          cex.axis = 1,
          cex.lab = 1,
          cex.main = 1,
          xlim = c(input$freqslider[1], input$freqslider[2])
        )
      })
      
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
          lengthMenu = list(c(5, 1), c("5", "1")),
          orderClasses = TRUE,
          order = list(list(0, "asc")),
          paging = T
        )
      )
      
      imspec <- (Im(spec)) ^ 2 / n
      output$Plot04 <- renderPlot({
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
          main = paste(colnames(dataSet)[2], "\nIm(fft(z))^2/n"),
          cex.axis = 1,
          cex.lab = 1,
          cex.main = 1,
          xlim = c(input$freqslider[1], input$freqslider[2])
        )
      })
      
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
          lengthMenu = list(c(5, 1), c("5", "1")),
          orderClasses = TRUE,
          order = list(list(0, "asc")),
          paging = T
        )
      )
      
      output$zmean <- renderPrint({
        zOffset
      })
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
    <li>2016-06-24:ver.1.0.0</li>
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
    importData()
    selectData()
    reactiveData()
    paste("Completion(UTC):" ,
          as.character(completionTime))
  })
  
})