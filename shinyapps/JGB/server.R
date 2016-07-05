library(shiny)
library(tseries)
library(forecast)
library(DT)
library(nortest)
shinyServer(function(input, output, session)
{
  reactiveData <- reactive({
    tmp <-
      jgbData[, c(1, grep(paste("\\b", input$year , "\\b", sep = ""), colnames(jgbData)))]
    tmp <- na.omit(tmp)
    startDate <- input$dateRange[1]
    endDate <-
      max(min(input$dateRange[2], tmp[nrow(tmp), 1]), tmp[1, 1] + 365)
    if (startDate >= endDate) {
      startDate <- endDate - 365
    }
    tmp <- subset(tmp, startDate <= tmp[, 1] & tmp[, 1] <=
                    endDate)
    buf <- colnames(tmp)
    if (input$datatype == 2)
      #1st diff
    {
      tmp <-
        data.frame(tmp[-1, 1], diff(tmp[, 2], lag = 1, differences = 1),
                   check.names = F)
      colnames(tmp) <- buf
    }
    if (input$datatype == 3)
      #2nd diff
    {
      tmp <-
        data.frame(tmp[-c(1, 2), 1], diff(tmp[, 2], lag = 1, differences = 2),
                   check.names = F)
      colnames(tmp) <- buf
    }
    x <- tmp[, 2]
    SV <- var(x) * (length(x) - 1) / length(x) #sample variance
    UV <- var(x)#unbiased variance
    SSD <- sqrt(SV)#sample standard deviation
    minData <- tail(subset(tmp, min(tmp[, 2]) == tmp[, 2]), 1)
    maxData <- tail(subset(tmp, max(tmp[, 2]) == tmp[, 2]), 1)
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
          maxData[,         2],
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
        format(minData[, 1], "%Y-%m-%d"),
        format(maxData[,        1], "%Y-%m-%d"),
        "-",
        "-",
        "-",
        "-",
        "p-value",
        "p-value"
      )
    statisticTable <<-
      data.frame(
        Statistic = statistic,
        Result = result,
        Remark = remark,
        stringsAsFactors = F,
        check.names = F
      )
    tabledata <<-
      data.frame(Date = format(jgbData[, 1], "%Y-%m-%d"),
                 jgbData[, -1],
                 check.names = F)
    plotData <<- tmp
    
    output$table1 <-
      DT::renderDataTable(
        statisticTable,
        rownames = FALSE,
        caption = "Table 1: Statistics.",
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
      tail(tabledata, 100),
      rownames = FALSE,
      caption = "Table 2: Japanese Government Bonds Interest Rate(%). Last 100 dates.",
      options = list(
        autoWidth = T,
        info = T,
        lengthChange = T,
        ordering = T,
        searching = T,
        scrollX = T,
        lengthMenu = list(c(5,
                            15, -1), c("5", "15", "All")),
        pageLength = 5,
        orderClasses = TRUE,
        order = list(list(0, "desc"))
      )
    )
    
    output$remarktext <- renderUI({
      str <- "<hr>
      <b>Remarks</b><br>
      <ol>
      <li>Sample Variance = var(x)*(length(x)-1)/length(x)</li>
      <li>Sample Standard Deviation = √Sample Variance</li>
      <li>adf.test( x ) { tseries } , ad.test( x ) { nortest }(Normality test)</li>
      <li>Other apps <a href=\"http://www.saecanet.com\" target=\"_blank\">SaECaNet</a></li>
      </ol>"
      HTML(str)
    })
    
    output$gitcode <- renderUI({
      str <- "<hr>
      <b>Code</b><br>
      <ol>
      <li><a href=\"https://github.com/am-consulting/Rscript/tree/master/shinyapps/JGB\" target=\"_blank\">Move to GitHub</a></li>
      </ol>"
      HTML(str)
    })
    
    output$history <- renderUI({
      str <- "<hr>
      <b>History</b><br>
      <ol>
      <li>2016-06-08:ver.1.0.0</li>
      <li>2016-06-11:ver.1.0.1</li>
      <li>2016-06-17:ver.1.0.2</li>
      <li>2016-07-05:ver.1.0.3</li>
      </ol>"
      HTML(str)
    })
  })
  
  output$plot1 <- renderPlot({
    par(mar = c(5, 4, 3, 3))
    reactiveData()
    subtitle <-
      paste(plotData[1, 1], " - ", plotData[nrow(plotData), 1])
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
        main = paste(colnames(plotData)[2], "\n", subtitle, " CI=", ci[1], ",", ci[2]),
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
  })
  
  output$latestDataDownloadTime <- renderText({
    paste("Data downloaded time(UTC):" ,
          as.character(latestDataDownloadTime))
  })
  
  })