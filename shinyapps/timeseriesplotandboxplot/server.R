library(shiny)
library(ggplot2)
library(scales)
library(lubridate)
library(reshape)
library(DT)
library(dplyr)
library(zoo)
options(download.file.method="libcurl")
options(shiny.maxRequestSize = 0.5 * 1024 ^ 2)
shinyServer(function(input, output) {
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
        na.strings = c(".",""),
        fileEncoding = "cp932"
      )
    }
  })
  
  output$xAxis <- renderUI({
    uploadFile()
    if (is.null(input$file))
      return(NULL)
    selectInput("datecolumn",
                "Date column",
                colnames(origData),
                selected = colnames(origData)[1]) #default:column1
  })
  
  output$yAxis <- renderUI({
    uploadFile()
    if (is.null(input$file))
      return(NULL)
    selectInput("datacolumn",
                "Data column",
                colnames(origData),
                selected = colnames(origData)[2]) #default:column2
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
    iii <- grep(paste("\\b", input$HA , "\\b", sep = "") ,
                 indextitle)
    colx <- which(colnames(origData) == input$datecolumn)
    coly <- which(colnames(origData) == input$datacolumn)    
    dataset <- origData[, c(colx, coly)]
    dataset <- na.omit(dataset)
    dataset[, 1] <- as.Date(dataset[, 1])
    dataset[, 2] <- as.numeric(dataset[, 2])
    dataset <-
      dataset %>% filter(input$selectedRow[1] <= index(dataset[, 1]),
                         index(dataset[, 1]) <= input$selectedRow[2])
    dateFormat = "%Y-%m-%d"
    if (length(unique(day(dataset[, 1]))) == 1) {
      dateFormat = "%Y-%m"
    }
    if (length(unique(month(dataset[, 1]))) == 1) {
      dateFormat = "%Y"
    }
    highestData <- switch (iii,
                            get(titleAbb[1]),
                            get(titleAbb[2]),
                            get(titleAbb[3]),
                            get(titleAbb[4]))
    FUN <- function(x) {
      highestData[which(highestData[1] <= x &
                          highestData[2] >= x, arr.ind = F), 3]
    }
    dataset <-
      data.frame(dataset, apply(dataset[1], 1, FUN), check.names = F)
    colnames(dataset)[3] <- indextitle[iii]

    output$startDate <- renderPrint({
      dataset[1, 1]
    })
    
    output$endDate <- renderPrint({
      dataset[nrow(dataset), 1]
    })
    
    chartTitle <-
      paste(
        colnames(dataset)[2],
        format(dataset[1, 1], dateFormat),
        "-",
        format(dataset[nrow(dataset), 1], dateFormat)
      )
    borderdate <- max(highestData[1, 1], dataset[1, 1])
    
    output$table1 <- DT::renderDataTable(
      dataset,
      rownames = FALSE,
      caption = "Table 1: Time Series Data.",
      options = list(
        autoWidth = T,
        info = T,
        lengthChange = T,
        ordering = T,
        searching = T,
        scrollX = T,
        lengthMenu = list(c(5, 10, -1), c("5", "10", "All")),
        pageLength = 5,
        orderClasses = TRUE,
        order = list(list(0, "desc"))
      )
    )
    
    output$table2 <- DT::renderDataTable(
      highestData,
      rownames = FALSE,
      caption = "Table 2: Boxplot with Highest Authority.",
      options = list(
        autoWidth = T,
        info = T,
        lengthChange = T,
        ordering = T,
        searching = T,
        scrollX = T,
        lengthMenu = list(c(5, 10, -1), c("5", "10", "All")),
        pageLength = 5,
        orderClasses = TRUE,
        order = list(list(0, "desc"))
      )
    )

    output$plottimeseries  <- renderPlot({
      dataset <- na.omit(subset(dataset, borderdate <= dataset[, 1]))
      colnames(dataset)[2] <- "value"
      for (rrrUL in seq(1, nrow(highestData), by = 1)) {
        if (highestData[rrrUL, 1] <= borderdate &
            borderdate <= highestData[rrrUL, 2]) {
          break
        }
      }
      for (rrrLL in seq(rrrUL, nrow(highestData), by = 1)) {
        if (highestData[rrrLL, 1] <= dataset[nrow(dataset), 1] &
            dataset[nrow(dataset), 1] <= highestData[rrrLL, 2]) {
          break
        }
      }
      segmentData <- highestData[c(rrrUL:rrrLL), ]
      segmentData[1, 1] <- borderdate
      segmentData[nrow(segmentData), 2] <- dataset[nrow(dataset), 1]
      x0 <-
        segmentData[, 1][seq(1, length(segmentData[, 1]), by = 2)]
      x1 <-
        segmentData[, 2][seq(1, length(segmentData[, 2]), by = 2)]
      y0 <- min(dataset[, 2])
      if (length((dataset$value)[dataset$value < 0]) != 0 &
          length((dataset$value)[0 < dataset$value]) != 0) {
        level.type <- 2
      } else{
        level.type <- 1
      }
      if (0 < y0 & level.type == 2) {
        y0 <- 0
      }
      y1 <- max(dataset[, 2])
      shade <- data.frame(x0, x1, y0, y1)
      value.y <- (max(dataset[, 2]) + min(dataset[, 2])) / 2
      value.y <- value.y
      level.type <- level.type

      g1 <- ggplot()
      g1 <-
        g1 + geom_rect(
          data = shade,
          aes(
            xmin = x0,
            xmax = x1,
            ymin = y0,
            ymax = y1
          ),
          fill = 'gray80',
          alpha = 0.5
        )
      g1 <-
        g1 + geom_segment(
          data = segmentData,
          aes(
            x = segmentData[, 1],
            xend = segmentData[, 2],
            y = value.y,
            yend = value.y,
            colour = segmentData[, 3]
          ),
          size = 8
        )
      if (level.type == 1) {
        g1 <-
          g1 + geom_line(data = dataset, aes(x = dataset[, 1], y = dataset[, 2]))
      } else{
        g1 <-
          g1 + geom_bar(
            data = dataset,
            aes(x = dataset[, 1], y = dataset[, 2]),
            stat = "identity",
            position = "identity",
            fill = "blue",
            alpha = 0.5
          )
      }
      g1 <-
        g1 + geom_smooth(
          data = dataset,
          aes(x = dataset[, 1], y = dataset[, 2]),
          method = loess,
          color = "red"
        )
      g1 <- g1 + ggtitle(chartTitle)
      g1 <- g1 + xlab("")
      g1 <- g1 + ylab("")
      g1 <-
        g1 + theme(axis.text = element_text(size = 13, face = "plain"))
      g1 <-
        g1 + theme(axis.title = element_text(size = 13, face = "plain"))
      g1 <-
        g1 + theme(title = element_text(size = 13, face = "plain"))
      g1 <- g1 + theme(legend.position = "right")
      g1 <-
        g1 + theme(legend.text = element_text(
          colour = "black",
          size = 13,
          face = "plain"
        ))
      g1 <-
        g1 + theme(legend.title = element_text(
          colour = "white",
          size = 0,
          face = "plain"
        ))
      g1 <- g1 + scale_x_date(labels = date_format(dateFormat))
      g1 <- g1 + scale_y_continuous(labels = comma)
      print(g1)
      g1<<-g1
    })

    output$plotboxplot  <- renderPlot({
      par(mar = c(5, 4, 3, 3))
      subset(dataset, borderdate <= dataset[, 1])
      boxplotData <- dataset[, c(2, 3)]
      colnames(boxplotData)[2] <- "Person"
      boxplotData <- melt(boxplotData, id = "Person")
      boxplotData <- boxplotData[, -2]
      boxplotData[, 2] <- as.numeric(boxplotData[, 2])
      colnames(boxplotData)[1] <- "variable"
      g2 <- ggplot(data = boxplotData, aes(x = variable, y = value))
      g2 <- g2 + geom_boxplot(lwd = 1)
      g2 <- g2 + ggtitle(chartTitle)
      g2 <-
        g2 + theme(axis.text.x = element_text(
          size = 13,
          angle = 90,
          hjust = 1,
          vjust = 0.5,
          face = "plain"
        ))
      g2 <-
        g2 + theme(axis.text.y = element_text(size = 13, face = "plain"))
      g2 <-
        g2 + theme(axis.title = element_text(size = 13, face = "plain"))
      g2 <-
        g2 + theme(title = element_text(size = 13, face = "plain"))
      g2 <- g2 + xlab("")
      g2 <- g2 + ylab("")
      g2 <- g2 + scale_y_continuous(labels = comma)
      print(g2)
      g2<<-g2
    })
  })
  
  output$completiontime <- renderText({
    if (is.null(input$datecolumn) | is.null(input$datacolumn))
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
      print(g1)
      dev.off()
    }
  )
  
  output$Download2 <- downloadHandler(
    filename = function() {
      paste("SaECaNet-",
            format(as.POSIXlt(Sys.time(), "GMT"), "%Y-%m-%d-%H-%M-%S"),
            ".png",
            sep = "")
    },
  content = function(file) {
      png(file, width = 1200, height = 800)
      print(g2)
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
  
  output$gitcode <- renderUI({
      str <- "<hr>
      <b>Code</b><br>
      <ol>
      <li><a href=\"https://github.com/am-consulting/Rscript/tree/master/shinyapps/timeseriesplotandboxplot\" target=\"_blank\">Move to GitHub</a></li>
      </ol>"
      HTML(str)
  })
  
  output$history <- renderUI({
    str <- "<hr>
    <b>History</b><br>
    <ol>
    <li>2016-06-12:ver.1.0.0</li>
    <li>2016-06-13:ver.1.0.1</li>
    <li>2016-06-17:ver.1.0.2</li>
    <li>2016-06-17:ver.1.0.3</li>
    <li>2016-07-08:ver.1.0.4</li>
    <li>2016-07-08:ver.1.0.5</li>
    </ol>"
    HTML(str)
  })

  output$linkList <- renderUI({
    str <- linkList
    HTML(str)
  })

})