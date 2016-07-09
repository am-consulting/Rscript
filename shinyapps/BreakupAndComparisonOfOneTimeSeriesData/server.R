library(shiny)
library(zoo)
library(dplyr)
library(lubridate)
options(shiny.maxRequestSize = 0.5 * 1024 ^ 2)
options(download.file.method="libcurl")
options(scipen = 999)
tsData <- list()
shinyServer(function(input, output , session) {
  uploadFile <- reactive({
    inFile <- input$file
    if (is.null(inFile)) {
      return(NULL)
    } else{
      origData <<- read.csv(
        inFile$datapath,
        header = input$header,
        sep = input$sep,
        quote = input$quote,
        check.names = input$chkname,
        stringsAsFactors = F,
        na.strings = c(".", ""),
        fileEncoding = "cp932"
      )
    }
  })
  
  output$datecolumn <- renderUI({
    uploadFile()
    if (is.null(input$file))
      return(NULL)
    selectInput("datecolumn",
                "Select as Date",
                colnames(origData),
                selected = colnames(origData)[1])
  })
  
  output$datacolumn <- renderUI({
    uploadFile()
    if (is.null(input$file))
      return(NULL)
    selectInput("datacolumn",
                "Select as Data",
                colnames(origData),
                selected = colnames(origData)[2])
  })
  
  output$selectrow <- renderUI({
    uploadFile()
    if (is.null(input$file))
      return(NULL)
    sliderInput(
      "selectedRow",
      label = "Select Rows",
      min = 0,
      max = nrow(origData),
      value =  c(1, nrow(origData)),
      step = 1
    )
  })
  
  resultOutput <- reactive({
    if (is.null(input$datecolumn) | is.null(input$datacolumn))
      return(NULL)
    colx <- which(colnames(origData) == input$datecolumn)
    coly <- which(colnames(origData) == input$datacolumn)  
    dataset <-
      data.frame(origData[, c(colx, coly)], check.names = F)
    dataset <- na.omit(dataset)
    dataset[, 1] <- as.Date(dataset[, 1])
    dataset <-
      dataset %>% filter(input$selectedRow[1] <= index(dataset[, 1]),
                         index(dataset[, 1]) <= input$selectedRow[2])
    colnames(dataset)[1]<-"DATE"
    dateFormat = "%Y-%m-%d"
    if (length(unique(day(dataset[, 1]))) == 1) {
      dateFormat = "%Y-%m"
    }
    if (length(unique(month(dataset[, 1]))) == 1) {
      dateFormat = "%Y"
    }
    
    output$startDate <- renderPrint({
      dataset[1, 1]
    })
    
    output$endDate <- renderPrint({
      dataset[nrow(dataset), 1]
    })
    
    dateRange01 <-
      data.frame(ID = seq(-24, 23),
                 DATE =  input$date01 %m+% months(-24:23))
    dateRange02 <-
      data.frame(ID = seq(-24, 23),
                 DATE =  input$date02 %m+% months(-24:23))
    dateRange03 <-
      data.frame(ID = seq(-24, 23),
                 DATE =  input$date03 %m+% months(-24:23))
    maxValue <- -10 ^ 100
    minValue <- 10 ^ 100
    for (yyy in 1:3) {
      switch (
        yyy,
        breakupDate <- input$date01,
        breakupDate <- input$date02,
        breakupDate <- input$date03
      )
      bufdateRange <- get(paste("dateRange0", yyy, sep = ""))
      buf <-
        dataset %>% filter(bufdateRange[1, 2] <= dataset[, 1], dataset[, 1] <= bufdateRange[nrow(bufdateRange), 2])
      if (nrow(buf) != 0) {
        buf <- merge(bufdateRange, buf, by = "DATE", all = T)
      } else{
        buf <-
          data.frame(bufdateRange,
                     VALUE = rep(NA, nrow(bufdateRange)),
                     check.names = F)
      }
      colnames(buf)[(grep("[^DATE|ID]", colnames(buf)))] <-
        as.character(breakupDate)
      buf[, 3] <- as.numeric(buf[, 3])
      tsData[[yyy]] <- buf
      if (nrow(na.omit(tsData[[yyy]])) != 0) {
        maxValue <-
          max(maxValue, max(as.numeric(na.omit(tsData[[yyy]][, 3]))))
        minValue <-
          min(minValue, min(as.numeric(na.omit(tsData[[yyy]][, 3]))))
      }
    }
    allData <-
      merge(merge(tsData[[1]], tsData[[2]], by = "ID", all = T),
            tsData[[3]],
            by = "ID",
            all = T)
    colnames(allData)[(grep("DATE", colnames(allData)))] <- "DATE"
    obj <- vector()
    if (sum(is.na(allData[, 3])) != nrow(allData)) {
      obj <- c(obj, 3)
    }
    if (sum(is.na(allData[, 5])) != nrow(allData)) {
      obj <- c(obj, 5)
    }
    if (sum(is.na(allData[, 7])) != nrow(allData)) {
      obj <- c(obj, 7)
    }
    statisticData <- allData[, c(1, obj)]

    output$tsplot <- renderPlot({tsPlot()})

    tsPlot<<-function(){
      if (length(obj) != 0) {
        collist <- c("", "black", "red", "blue")
        labellist <- colnames(statisticData)[-1]
        par(mar = c(5, 4, 2, 4))
        cnt <- 1
        for (ccc in 2:ncol(statisticData)) {
          if (sum(is.na(statisticData[, ccc])) != nrow(statisticData)) {
            if (cnt == 1) {
              yRange <- c(minValue, maxValue)
              yaxtType<-"s"
              mainTitle<-colnames(dataset)[2]
              xTitle<-"months ago or later"
            } else{
              yaxtType<-"n"
              mainTitle<-""
              xTitle<-""
            }
            cnt <- cnt + 1
            par(new = T)
            plot(
              statisticData[, 1],
              statisticData[, ccc],
              type = "l",
              col = collist[ccc],
              xlab = xTitle ,
              ylab = "" ,
              panel.first = grid(
                nx = NULL,
                ny = NULL,
                lty = 2,
                equilogs = T
              ) ,
              main = mainTitle,
              cex.axis = 1.2,
              cex.lab = 1.2,
              cex.main = 1.2,
              ylim = yRange,
              xaxt = "n",
              yaxt = yaxtType
            )
          }
        }
        axis(
          1,
          at = statisticData[, 1],
          labels = statisticData[, 1],
          col.axis = "black",
          las = 2,
          cex = 1.2
        )
        abline(v = 0)
        legend(
          "topleft",
          legend = labellist,
          col = collist[-1] ,
          ncol = length(labellist),
          lty = 1,
          lwd = 2
        )
      } else{
        plot.new()
      }
    }
    
    resultDF <- data.frame()
    periodDF <- vector()
    if (length(obj) != 0) {
      for (iii in 2:ncol(statisticData)) {
        buf <- na.omit(statisticData[, c(1, iii)])
        for (yyy in 1:3) {
          if (yyy == 1) {
            buData <- buf
            dftitle <- paste(colnames(buData)[2], "-24 ~ +23")
          } else if (yyy == 2) {
            buData <- subset(buf, buf[, 1] < 0)
            dftitle <- paste(colnames(buData)[2], "-24")
          } else{
            buData <- subset(buf, buf[, 1] >= 0)
            dftitle <- paste(colnames(buData)[2], "+23")
          }
          if (nrow(buData) != 0) {
            meanDF <- mean(buData[, 2])
            medianDF <- median(buData[, 2])
            minDF <- min(buData[, 2])
            maxDF <- max(buData[, 2])
          }
          if (3 <= nrow(buData)) {
            result <- lm(buData[, 2] ~ buData[, 1])
            slope <- result$coefficients[2]
            f <- summary(result)$fstatistic
            p <- pf(f[1], f[2], f[3], lower.tail = F)
          } else{
            slope <- p <-  ""
          }
          resultDF[(iii - 2) * 3 + yyy, 1] <-
            format(slope, digits = 3,  scientific = T)
          resultDF[(iii - 2) * 3 + yyy, 2] <-
            format(p, digits = 3, scientific = T)
          resultDF[(iii - 2) * 3 + yyy, 3] <- meanDF
          resultDF[(iii - 2) * 3 + yyy, 4] <- medianDF
          resultDF[(iii - 2) * 3 + yyy, 5] <- minDF
          resultDF[(iii - 2) * 3 + yyy, 6] <- maxDF
          periodDF <- c(periodDF, dftitle)
        }
      }
      colnames(resultDF) <-
        c("Slope", "p.value", "Mean", "Median", "Min", "Max")
    }
    resultDF <-
      data.frame(ID = index(resultDF), Period = periodDF, resultDF)
    
    output$dtResult <- DT::renderDataTable(
      resultDF,
      rownames = F,
      caption = "Table 1: Statistic Result by Period.",
      options = list(
        autoWidth = T,
        info = T,
        lengthChange = T,
        ordering = T,
        searching = T,
        scrollX = T,
        lengthMenu = list(c(-1, 1), c("All", "1")),
        orderClasses = TRUE,
        order = list(list(0, "asc"))
      )
    )
    
    output$dt <- DT::renderDataTable(
      allData,
      rownames = FALSE,
      caption = "Table 2: Breakup Time Series Data.",
      options = list(
        autoWidth = T,
        info = T,
        lengthChange = T,
        ordering = T,
        searching = T,
        scrollX = T,
        lengthMenu = list(c(-1, 12), c("All", "12")),
        orderClasses = TRUE,
        order = list(list(0, "asc"))
      )
    )
  })
  
  output$completiontime <- renderText({
    if (is.null(input$datecolumn) & is.null(input$datacolumn))
      return(NULL)
    resultOutput()
    paste("Completion(UTC):" ,
          as.character(as.POSIXlt(Sys.time(), "GMT")))
  })

  output$Download1 <- downloadHandler(
    filename = function() {
      paste("SaECaNet-",
            format(as.POSIXlt(Sys.time(), "GMT"), "%Y-%m-%d-%H-%M-%S"),
            ".png",
            sep = "")
    },
  content = function(file) {
      png(file, width = 1200, height = 800)
      tsPlot()
      dev.off()
    }
  )

  output$remarktext <- renderUI({
    str <- "<hr>
    <b>Remarks</b><br>
    <ol>
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
    <li>2016-06-13:ver.1.0.0</li>
    <li>2016-06-17:ver.1.0.1</li>
    <li>2016-07-09:ver.1.0.2</li>
    </ol>"
    HTML(str)
  })

  output$gitcode <- renderUI({
    str <- "<hr>
    <b>Code</b><br>
    <ol>
    <li><a href=\"https://github.com/am-consulting/Rscript/tree/master/shinyapps/BreakupAndComparisonOfOneTimeSeriesData\" target=\"_blank\">Move to GitHub</a></li>
    </ol>"
    HTML(str)
  })
  
  output$linkList <- renderUI({
    str <- linkList
    HTML(str)
  })
  
})