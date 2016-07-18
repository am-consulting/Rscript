library(shiny)
library(tseries)
library(forecast)
library(DT)
library(nortest)
library(dplyr)
shinyServer(function(input, output, session)
{
  reactiveData <- reactive({
    tmp <- origData[, c(1, which(input$item == colnames(origData)))]
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
    
    sDate <- format(plotData[1, 1], "%Y-%m")
    eDate <- format(plotData[nrow(plotData), 1], "%Y-%m")    

    output$startPeriod <- renderPrint({
      as.character(sDate)
    })
    output$endPeriod <- renderPrint({
      as.character(eDate)
    })
    
    output$plot1 <- renderPlot({
      par(mar = c(5, 4, 3, 3))
      subtitle <-
        paste(sDate, " - ", eDate)
      if (input$charttype == "hist")
      {
        hist(
          plotData[, 2],
          breaks = 50,
          cex.axis = 1,
          cex.lab = 1,
          cex.main = 1,
          main = colnames(plotData)[2],
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
        plotData$color <- "blue"
        plotData[plotData[, 2] < 0, ncol(plotData)] <- "red"
        loessResult <-
          loess(plotData[, 2] ~ as.numeric(plotData[, 1]))
        if(input$charttype == "h"){plotcol<-plotData$color}else{plotcol<-"black"}
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
          col = plotcol,
          main = paste(colnames(plotData)[2], "\n", subtitle),
          cex.axis = 1,
          cex.lab = 1,
          cex.main = 1
        )
        lines(plotData[, 1],
              predict(loessResult),
              col = 'red',
              lwd = 1)
        axis.Date(
          side = 1,
          at = plotData[, 1],
          format = "%Y-%m",
          padj = 1,
          cex.axis = 1
        )
      }
    }, height = 500)
    
    x <- tmp[, 2]
    SV <- var(x) * (length(x) - 1) / length(x) #sample variance
    UV <- var(x)#unbiased variance
    SSD <- sqrt(SV)#sample standard deviation
    minData <- tail(subset(tmp, min(x) == x), 2)
    maxData <- tail(subset(tmp, max(x) == x), 2)
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
        "Period"
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
          ad.test(x)$p.value
        ),
        3
      ),
      paste(sDate,"-",eDate)
      )
    remark <-
      c(
        "-",
        "-",
        "-",
        format(minData[, 1], "%Y-%m"),
        format(maxData[, 1], "%Y-%m"),
        "-",
        "-",
        "-",
        "-",
        "p-value",
        paste(ad.test(x)$method, ". p-value"),
        "-"
      )
    statisticTable <-
      data.frame(
        Statistic = statistic,
        Result = result,
        Remark = remark,
        stringsAsFactors = F,
        check.names = F
      )
    
    output$table1 <- DT::renderDataTable(
      statisticTable,
      rownames = F,
      caption = paste("Table 1: Statistical Summary -", colnames(tmp)[2]),
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
    
    dtDF <- plotData
    dtDF[, 1] <- format(plotData[, 1], "%Y-%m")
    output$table2 <- DT::renderDataTable(
      dtDF,
      rownames = F,
      caption = paste("Table 2: Time Series Data"),
      options = list(
        autoWidth = T,
        info = T,
        lengthChange = T,
        ordering = T,
        searching = T,
        scrollX = T,
        lengthMenu = list(c(10, 5, -1), c("10", "5", "All")),
        orderClasses = TRUE,
        order = list(list(0, "desc"))
      )
    )
  })
  
  output$remarktext <- renderUI({
    str <- "<hr>
    <b>Remarks</b><br>
    <ol>
    <li>Sample Variance = var(x)*(length(x)-1)/length(x)</li>
    <li>Sample Standard Deviation = √Sample Variance</li>
    <li>adf.test( x ) { tseries } , ad.test( x ) { nortest }(Normality test)</li>
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
    <li>2016-06-17:ver.1.0.0</li>
    <li>2016-06-17:ver.1.0.1</li>
    <li>2016-07-18:ver.1.0.1</li>
    </ol>"
    HTML(str)
  })

  output$gitcode <- renderUI({
    str <- "<hr>
    <b>Code</b><br>
    <ol>
    <li><a href=\"https://github.com/am-consulting/Rscript/tree/master/shinyapps/IMFPrimaryCommodityPrices\" target=\"_blank\">Move to GitHub</a></li>
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