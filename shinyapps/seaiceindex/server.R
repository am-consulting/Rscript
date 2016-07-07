library(shiny)
library(ggplot2)
library(scales)
library(lubridate)
shinyServer(function(input, output, session)
{
  reactiveData <- reactive({
    if (input$region == datatitle[1]) {
      ccc <- 1
    } else if (input$region == datatitle[2]) {
      ccc <- 2
    } else{
      ccc <- 3
    }
    ccc <<- ccc
    output$datatitle <- renderText({
      paste(datatitle[ccc],
            "\nPeriod:",
            first(dataset[[ccc]]$Date),
            "-",
            last(dataset[[ccc]]$Date),
            sep = "")
    })
    if (ccc <= 2) {
      valueColumn <<- 4
    } else{
      valueColumn <<- 12
    }
    mainTitle <-
      paste(datatitle[ccc],
            "\nPeriod:",
            first(dataset[[ccc]]$Date),
            "-",
            last(dataset[[ccc]]$Date),
            sep = "")
    rp1  <- renderPlot({
      par(mar = c(5, 4, 3, 3))
      g <- ggplot()
      g <-
        g + geom_line(
          data = dataset[[ccc]],
          aes(x = Date, y = dataset[[ccc]][, valueColumn]),
          size = 0.5,
          col = "black"
        )
      g <-
        g + geom_smooth(
          data = dataset[[ccc]],
          aes(x = Date, y = dataset[[ccc]][, valueColumn]),
          method = lm,
          lwd = 1,
          col = "red"
        )
      g <- g + scale_x_date(labels = date_format("%Y-%m-%d"))
      g <- g + theme(plot.title = element_text(size = 15))
      g <- g + theme(axis.title.x = element_text(size = 15))
      g <- g + theme(axis.title.y = element_text(size = 15))
      g <-
        g + theme(axis.text.x = element_text(size = 15, angle = 0))
      g <-
        g + theme(axis.text.y = element_text(size = 15, angle = 90))
      g <- g + xlab("")
      g <-
        g + ylab(paste(colnames(buf)[4], "(", unlist(buf[1,][4]), ")"))
      lmResult <-
        lm(dataset[[ccc]][, valueColumn] ~ dataset[[ccc]]$Date)
      g <- g + ggtitle(
        paste(
          mainTitle,
          "\nLiner Model Slope:",
          lmResult$coefficients[2],
          "\nUnit Root Test p-value:",
          adf.test(dataset[[ccc]][, valueColumn])$p.value
        )
      )
      print(g)
    })
    output$plot1 <- rp1
    
    rp2  <- renderPlot({
      par(mar = c(5, 4, 3, 3))
      seasonData <-
        data.frame(dataset[[ccc]], month = month.abb[month(dataset[[ccc]]$Date)])
      seasonData$month <-
        factor(seasonData$month, levels = month.abb)
      seasonData <<- seasonData
      g1 <-
        ggplot(data = seasonData, aes(x = month, y = seasonData[, valueColumn]))
      g1 <- g1 + geom_boxplot(lwd = 1)
      g1 <- g1 + theme(plot.title = element_text(size = 15))
      g1 <- g1 + theme(axis.title.x = element_text(size = 15))
      g1 <- g1 + theme(axis.title.y = element_text(size = 15))
      g1 <-
        g1 + theme(axis.text.x = element_text(
          size = 15,
          angle = 0,
          hjust = 0.5,
          vjust = 0.5
        ))
      g1 <- g1 + theme(axis.text.y = element_text(size = 15))
      g1 <- g1 + ggtitle(paste(mainTitle))
      g1 <- g1 + xlab("")
      g1 <-
        g1 + ylab(paste(colnames(buf)[4], "(", unlist(buf[1,][4]), ")"))
      print(g1)
    })
    output$plot2 <- rp2
    
    rp3  <- renderPlot({
      par(mar = c(3, 5, 5, 3))
      acf(
        dataset[[ccc]][, valueColumn],
        lag = 1000,
        panel.first = grid(
          nx = NULL,
          ny = NULL,
          lty = 2,
          equilogs = F
        ),
        main = mainTitle
      )
    })
    output$plot3 <- rp3
    
    rp4  <- renderPlot({
      dataset[[ccc]]$yearMonth <-
        as.Date(paste(
          year(dataset[[ccc]]$Date),
          "-",
          month(dataset[[ccc]]$Date),
          "-1",
          sep =
            ""
        ))
      functionList <- c("mean", "median", "max", "min", "sd")
      par(
        mfrow = c(2, 3),
        ps = 18,
        mar = c(5, 5, 7, 3),
        cex.main = 1.0,
        cex.lab = 1,
        cex.axis = 1.0
      )
      for (iii in 1:length(functionList)) {
        tmp <-
          aggregate(
            x = list(value = dataset[[ccc]][, valueColumn]),
            by = list(Date = dataset[[ccc]]$yearMonth),
            FUN = get(functionList[iii])
          )
        histTitle <-
          paste(mainTitle,
                "\n",
                format(first(tmp[, 1]), "%Y/%m"),
                "-",
                format(last(tmp[, 1]), "%Y/%m"),
                sep = "")
        hist(
          tmp[, 2],
          col = "grey",
          breaks = "scott",
          main = paste(
            histTitle,
            "\naggregated by Year&Month\n",
            functionList[iii],
            sep = ""
          ),
          xlab = unlist(buf[1,][4]),
          freq = T
        )
      }
      
    }, height = 650)
    output$plot4 <- rp4
  })
  
  datatableData <- dataset[[3]][, c(1, 5, 6, 10, 11, 12)]
  datatableData <- tail(datatableData, 100)
  rdt <- DT::renderDataTable(
    datatableData,
    rownames = FALSE,
    caption = "Table 1:NH,SH and Total. Last 100 dates.",
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
  output$table1 <- rdt
  
  output$datachecktime <- renderText({
    reactiveData()
    paste("Data downloaded time(UTC):" ,
          as.character(downloadTime))
  })
  
  output$remarktext <- renderUI({
    str <- "<hr>
    <b>Remarks</b><br>
    <ol>
    <li><a href=\"http://www.saecanet.com\" target=\"_blank\">SaECaNet</a></li>
    <li>Other apps <a href=\"http://webapps.saecanet.com\" target=\"_blank\">SaECaNet - Web Applications</a></li>
    <li><a href=\"http://am-consulting.co.jp\" target=\"_blank\">Asset Management Consulting Corporation / アセット･マネジメント･コンサルティング株式会社</a></li>
    <li><a href=\"http://www.saecanet.com/subfolder/disclaimer.html\" target=\"_blank\">Disclaimer</a></li>
    <li><a href=\"https://nsidc.org/data/seaice_index/archives.html\" target=\"_blank\">Raw Data Source</a></li>
    <li><a href=\"http://nsidc.org/data/docs/noaa/g02135_seaice_index/#jul-2016\" target=\"_blank\">Raw Data Source</a></li>
    </ol>"
    HTML(str)
  })
  
  output$history <- renderUI({
    str <- "<hr>
    <b>History</b><br>
    <ol>
    <li>2016-06-08:ver.1.0.0</li>
    <li>2016-06-17:ver.1.0.1</li>
    <li>2016-07-07:ver.1.0.2</li>
    </ol>"
    HTML(str)
  })
  
  output$gitcode <- renderUI({
    str <- "<hr>
    <b>Code</b><br>
    <ol>
    <li><a href=\"https://github.com/am-consulting/Rscript/tree/master/shinyapps/seaiceindex\" target=\"_blank\">Move to GitHub</a></li>
    </ol>"
    HTML(str)
  })
  
  output$linkList <- renderUI({
    str <- linkList
    HTML(str)
  })
})