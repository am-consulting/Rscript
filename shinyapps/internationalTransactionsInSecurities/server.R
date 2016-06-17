library(shiny)
library(tseries)
library(forecast)
library(DT)
library(nortest)
library(dplyr)
shinyServer(function(input, output, session)
{
  reactiveData <- reactive({
    tmp <- origData[, c(1, 2, which(input$item == colnames(origData)))]
    tmp <- na.omit(tmp)
    buf <- colnames(tmp)
    tmp <-
      tmp %>% filter(input$selectedRow[1] <= tmp[, 1],
                     tmp[, 1] <= input$selectedRow[2])
    if (input$datatype == 2)
      #1st diff
    {
      tmp <-
        data.frame(tmp[-1, 1:2], diff(tmp[, 3], lag = 1, differences = 1),
                   check.names = F)
    }
    if (input$datatype == 3)
      #2nd diff
    {
      tmp <-
        data.frame(tmp[-c(1, 2), 1:2], diff(tmp[, 3], lag = 1, differences = 2),
                   check.names = F)
    }
    colnames(tmp) <- buf
    plotData <<- tmp
    output$startPeriod <- renderPrint({
      as.character(plotData[1, 2])
    })
    output$endPeriod <- renderPrint({
      as.character(plotData[nrow(plotData), 2])
    })

    output$plot1 <- renderPlot({
      par(mar = c(5, 4, 3, 3))
      subtitle <-
        paste(plotData[1, 1], " - ", plotData[nrow(plotData), 1])
      if (input$charttype == "hist")
      {
        hist(
          plotData[, 3],
          breaks = 50,
          cex.axis = 1,
          cex.lab = 1,
          cex.main = 1,
          main = colnames(plotData)[3],
          xlab = ""
        )
      }
      if (input$charttype == "arima")
      {
        ci = c(90, 99)
        forecastN <- 30
        result.arima <-
          auto.arima(plotData[, 3],
                     ic = "aic",
                     trace = F,
                     stepwise = T)
        result.forecast <-
          forecast(result.arima, level = ci, h = forecastN)
        plot(
          result.forecast,
          main = paste(
            colnames(plotData)[3],
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
        qqnorm(plotData[, 3],
               panel.first = grid(
                 nx = NULL,
                 ny = NULL,
                 lty = 2,
                 equilogs = T
               ))
        qqline(plotData[, 3], col = "red")
      }
      if (input$charttype == "h")
      {
        plotData <- tail(plotData, 60)
        plotData$color <- "blue"
        plotData[plotData[, 3] < 0, ncol(plotData)] <- "red"
        par(mar = c(15, 5, 2, 2))
        plotbuf <- barplot(
          plotData[, 3],
          names.arg = plotData[, 2],
          las = 3,
          main = colnames(plotData)[3],
          col = plotData$color,
          density = 20,
          ylab = "100million JPY",
          cex.main = 1,
          cex.sub = 1,
          cex.lab = 1,
          cex.names = 1,
          cex.axis = 1
        )
        text(
          plotbuf,
          y = plotData[, 3],
          labels = plotData[, 3],
          srt = 90,
          cex = 1,
          adj = c(-0.1, 0.5),
          col = "black"
        )
      }
    }, height = 500)
    
    x <- tmp[, 3]
    SV <- var(x) * (length(x) - 1) / length(x) #sample variance
    UV <- var(x)#unbiased variance
    SSD <- sqrt(SV)#sample standard deviation
    minData <- tail(subset(tmp, min(tmp[, 3]) == tmp[, 3]), 2)
    maxData <- tail(subset(tmp, max(tmp[, 3]) == tmp[, 3]), 2)
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
        "ad.test( x )"
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
      ))
    remark <-
      c(
        "-",
        "-",
        "-",
        format(minData[, 1]),
        format(maxData[, 1]),
        "-",
        "-",
        "-",
        "-",
        "p-value",
        paste(ad.test(x)$method, ". p-value")
      )
    statisticTable <-
      data.frame(
        Statistic = statistic,
        Result = result,
        Remark = remark,
        stringsAsFactors = F,
        check.names = F
      )
    
    variance <- function(x) {
      tmpx <- sqrt(var(x) * (length(x) - 1) / length(x))
      value <- floor(1000 * as.numeric(tmpx) + 0.5) / 1000
      return(value)
    }
    
    output$table1 <- DT::renderDataTable(
      statisticTable,
      rownames = F,
      caption = paste("Table 1: Statistical Summary -", colnames(tmp)[3]),
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
    
    output$table2 <- DT::renderDataTable(
      plotData,
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
    </ol>"
    HTML(str)
  })
  
  output$history <- renderUI({
    str <- "<hr>
    <b>History</b><br>
    <ol>
    <li>2016-06-16:ver.1.0.0</li>
    <li>2016-06-17:ver.1.0.1</li>
    </ol>"
    HTML(str)
  })

  output$gitcode <- renderUI({
    str <- "<hr>
    <b>Code</b><br>
    <ol>
    <li><a href=\"https://github.com/am-consulting/Rscript/tree/master/shinyapps/internationalTransactionsInSecurities\" target=\"_blank\">Move to GitHub</a></li>
    </ol>"
    HTML(str)
  })
  
  output$DataDownloadTime <- renderText({
    reactiveData()
    paste("Data downloaded time(UTC):" ,
          as.character(latestDataDownloadTime))
  })
  
})