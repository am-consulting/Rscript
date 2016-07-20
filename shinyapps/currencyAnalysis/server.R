library(shiny)
library(tseries)
library(forecast)
library(DT)
library(nortest)
library(lubridate)
library(ggplot2)
library(dplyr)
shinyServer(function(input, output, session)
{
  reactiveData <- reactive({
    tmp <- origData[, c(1, which(input$currency == colnames(origData)))]
    tmp <- na.omit(tmp)
    tmp <-
      tmp %>% filter(input$selectedRow[1] <= index(tmp),
                     index(tmp) <= input$selectedRow[2])
    buf <- colnames(tmp)
    if (input$datatype == 2)
      #1st diff
    {
      tmp <-
        data.frame(tmp[-1, 1], diff(tmp[, 2], lag = 1, differences = 1),
                   check.names = F)
    }
    if (input$datatype == 3)
      #2nd diff
    {
      tmp <-
        data.frame(tmp[-c(1, 2), 1], diff(tmp[, 2], lag = 1, differences = 2),
                   check.names = F)
    }
    colnames(tmp) <- buf
    plotData <<- tmp
    subtitle <-
      paste(plotData[1, 1], " - ", plotData[nrow(plotData), 1])
    x <- plotData[, 2]
    SV <- var(x) * (length(x) - 1) / length(x) #sample variance
    UV <- var(x)#unbiased variance
    SSD <- sqrt(SV)#sample standard deviation
    minData <- tail(subset(plotData, min(x) == x), 1)
    maxData <- tail(subset(plotData, max(x) == x), 1)
    statistic <-
      c(
        "n",
        "mean( x )",
        "median( x )",
        "min( x )",
        "max( x )",
        "sd( x )",
        "Sample Standard Deviation",
        "var( x )",
        "Sample Variance",
        "adf.test( x )",
        "ad.test( x )",
        "Variation Coefficient"
      )
    result <-
      c(length(x), round(
        c(
          mean(x),
          median(x),
          minData[, 2],
          maxData[, 2],
          sd(x),
          SSD  ,
          UV ,
          SV ,
          adf.test(x)$p.value,
          ad.test(x)$p.value,
          SSD / mean(x) * 100
        ),
        3
      ))
    remark <-
      c(
        "-",
        "-",
        "-",
        paste(format(minData[, 1], "%Y-%m-%d"), ""),
        paste(format(maxData[, 1], "%Y-%m-%d"), ""),
        "-",
        "-",
        "-",
        "-",
        "p-value",
        paste("p-value:", ad.test(x)$method),
        "%"
      )
    statisticTable <-
      data.frame(
        Statistic = statistic,
        Result = result,
        Remark = remark,
        stringsAsFactors = F,
        check.names = F
      )
    
    yearList <- unique(year(plotData[, 1]))
    yyData <- list()
    for (iii in 1:length(yearList)) {
      yyData[[iii]] <- subset(plotData, year(plotData[, 1]) == yearList[iii])
    }
    
    yymmData <- list()
    cnt <- 0
    for (iii in 1:length(yyData)) {
      for (mm in 1:12) {
        cnt <- cnt + 1
        yymmData[[cnt]] <-
          subset(yyData[[iii]], month(yyData[[iii]][, 1]) == mm)
      }
    }
    
    colnamesDF <-
      c("n", "Mean", "Median", "Min", "Max", "Variation Coefficient")
    yearDF <- data.frame()
    for (iii in 1:length(yyData)) {
      if (nrow(yyData[[iii]]) != 0) {
        x <- yyData[[iii]][, 2]
        SV <- var(x) * (length(x) - 1) / length(x)
        SSD <- sqrt(SV)
        vaData <- SSD / mean(x) * 100
        yearDF[iii, 1] <- year(yyData[[iii]][1, 1])
        yearDF[iii, 2] <- length(x)
        yearDF[iii, 3] <- mean(x)
        yearDF[iii, 4] <- median(x)
        yearDF[iii, 5] <- min(x)
        yearDF[iii, 6] <- max(x)
        yearDF[iii, 7] <- vaData
      }
    }
    colnames(yearDF) <- c("Year", colnamesDF)
    
    yearmonthDF <- data.frame()
    for (iii in 1:length(yymmData)) {
      if (nrow(yymmData[[iii]]) != 0) {
        x <- yymmData[[iii]][, 2]
        SV <- var(x) * (length(x) - 1) / length(x)
        SSD <- sqrt(SV)
        vaData <- SSD / mean(x) * 100
        yearmonthDF[iii, 1] <- format(yymmData[[iii]][1, 1], "%Y-%m")
        yearmonthDF[iii, 2] <- length(x)
        yearmonthDF[iii, 3] <- mean(x)
        yearmonthDF[iii, 4] <- median(x)
        yearmonthDF[iii, 5] <- min(x)
        yearmonthDF[iii, 6] <- max(x)
        yearmonthDF[iii, 7] <- vaData
      }
    }
    yearmonthDF <- na.omit(yearmonthDF)
    colnames(yearmonthDF) <- c("Year-Month", colnamesDF)
    
    seasonData <-
      data.frame(yearmonthDF, month = month.abb[as.numeric(substr(yearmonthDF[, 1], 6, 7))])
    seasonData$month <-
      factor(seasonData$month, levels = month.abb)
    seasonData <<- seasonData
    g1 <-
      ggplot(data = seasonData, aes(x = month, y = seasonData[, 7]))
    g1 <- g1 + geom_boxplot(lwd = 1)
    g1 <- g1 + theme(plot.title = element_text(size = 15))
    g1 <- g1 + theme(axis.title.x = element_text(size = 15))
    g1 <- g1 + theme(axis.title.y = element_text(size = 15))
    g1 <-
      g1 + theme(axis.text.x = element_text(
        size = 15,
        angle = 90,
        hjust = 0.5,
        vjust = 0.5
      ))
    g1 <-
      g1 + theme(axis.text.y = element_text(size = 15, angle = 90))
    g1 <-
      g1 + ggtitle(paste("Variation Coefficient by month\n", subtitle))
    g1 <- g1 + xlab("")
    g1 <- g1 + ylab("Variation Coefficient(%)")
    
    timeseriesData <- data.frame(plotData,
                                 c(NA, round(((plotData[-1, -1] / plotData[-nrow(plotData), -1]) - 1
                                 ) * 100, 2)))
    colnames(timeseriesData)[3] <- "Day to day change(%)"
    
    output$statisticTable <- DT::renderDataTable(
      statisticTable,
      rownames = F,
      caption = paste("Table 1: Statistical Summary.", subtitle),
      options = list(
        autoWidth = T,
        info = F,
        lengthChange = F,
        ordering = F,
        searching = F,
        scrollX = T,
        paging = F
      )
    )
    
    output$yearDF <- DT::renderDataTable(
      yearDF,
      rownames = F,
      caption = paste("Table 2:Statistical Summary by Year.", subtitle),
      options = list(
        autoWidth = T,
        info = T,
        lengthChange = T,
        ordering = T,
        searching = T,
        scrollX = T,
        paging = T,
        lengthMenu = list(c(10, -1, 1), c("10", "All", "1")),
        orderClasses = TRUE,
        order = list(list(0, "desc"))
      )
    )
    
    output$yearmonthDF <- DT::renderDataTable(
      yearmonthDF,
      rownames = F,
      caption = paste("Table 3:Statistical Summary by Year-Month.", subtitle),
      options = list(
        autoWidth = T,
        info = T,
        lengthChange = T,
        ordering = T,
        searching = T,
        scrollX = T,
        paging = T,
        lengthMenu = list(c(10, -1, 1), c("10", "All", "1")),
        orderClasses = TRUE,
        order = list(list(0, "desc"))
      )
    )
    
    output$timeseriesData <- DT::renderDataTable(
      timeseriesData,
      rownames = F,
      caption = paste("Table 4:Time Series Data.", subtitle),
      options = list(
        autoWidth = T,
        info = T,
        lengthChange = T,
        ordering = T,
        searching = T,
        scrollX = T,
        lengthMenu = list(c(10, -1, 1), c("10", "All", "1")),
        orderClasses = TRUE,
        order = list(list(0, "desc"))
      )
    )
    
    output$currencyName <- DT::renderDataTable(
      currencyName,
      rownames = F,
      caption = paste("Table 5:Symbols."),
      options = list(
        autoWidth = T,
        info = F,
        lengthChange = F,
        ordering = F,
        searching = F,
        scrollX = T,
        paging = F
      )
    )
    
    output$plot1 <- renderPlot({
      par(mar = c(5, 4, 3, 3), family = "Noto Sans Japanese")
      
      if (input$charttype == "hist")
      {
        hist(
          plotData[, 2],
          breaks = 50,
          cex.axis = 1,
          cex.lab = 1,
          cex.main = 1,
          main = paste(colnames(plotData)[2], "\n", subtitle),
          xlab = ""
        )
      }
      if (input$charttype == "arima")
      {
        ci = c(90, 99)
        forecastN <- 30
        result.arima <-
          auto.arima(plotData[, 2],
                     ic = "aic",
                     trace = F,
                     stepwise = T)
        result.forecast <-
          forecast(result.arima, level = ci, h = forecastN)
        plot(
          result.forecast,
          main = paste(
            colnames(plotData)[2],
            "\n",
            subtitle,
            " CI=",
            ci[1],
            ",",
            ci[2]
          ),
          ylab = "",
          panel.first = grid(
            nx = NULL,
            ny = NULL,
            lty = 2,
            equilogs = T
          )
        )
      }
      if (input$charttype == "QQ")
      {
        qqnorm(plotData[, 2],
               panel.first = grid(
                 nx = NULL,
                 ny = NULL,
                 lty = 2,
                 equilogs = T
               ))
        qqline(plotData[, 2], col = "red")
      }
      if (input$charttype == "l" || input$charttype == "h")
      {
        plot(
          plotData[, 1],
          plotData[, 2],
          type = input$charttype,
          xlab = "",
          ylab = colnames(plotData)[2],
          panel.first = grid(
            nx = NULL,
            ny = NULL,
            lty = 2,
            equilogs = T
          ),
          xaxt = "n",
          main = paste(colnames(plotData)[2], "\n", subtitle),
          cex.axis = 1,
          cex.lab = 1,
          cex.main = 1
        )
        abline(h = 0, col = "red")
        axis.Date(
          side = 1,
          at = plotData[, 1],
          format = "%Y-%m-%d",
          padj = 1,
          cex.axis = 1
        )
      }
      if (input$charttype == "boxplot")
      {
        print(g1)
      }
    })
  })
  
  output$remarktext <- renderUI({
    str <- "<hr>
    <b>Remarks</b><br>
    <ol>
    <li>Sample Variance = var(x)*(length(x)-1)/length(x)</li>
    <li>Sample Standard Deviation = √Sample Variance</li>
    <li>adf.test( x ) { tseries } , ad.test( x ) { nortest }(Normality test)</li>
    <li>Variation Coefficient(VC,%)=(Sample Standard Deviation/Mean)*100</li>
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
    <li>2016-06-20:ver.1.0.0</li>
    <li>2016-06-21:ver.1.0.1</li>
    <li>2016-07-20:ver.1.0.2</li>
    </ol>"
    HTML(str)
  })
  
  output$gitcode <- renderUI({
    str <- "<hr>
    <b>Code</b><br>
    <ol>
    <li><a href=\"https://github.com/am-consulting/Rscript/tree/master/shinyapps/currencyAnalysis\" target=\"_blank\">Move to GitHub</a></li>
    </ol>"
    HTML(str)
  })
  
  output$DataDownloadTime <- renderText({
    reactiveData()
    paste("Data downloaded time(UTC):" ,
          as.character(latestDataDownloadTime))
  })

  output$linkList <- renderUI({
    str <- linkList
    HTML(str)
  })
        
})