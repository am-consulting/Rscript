library(shiny)
library(tseries)
library(forecast)
library(DT)
library(nortest)
library(lubridate)
library(ggplot2)
shinyServer(function(input, output, session)
{
  funGDP <- function(sss) {
    gdpData <<- get(paste("gdp", sss, sep = ""))
    tmp <-
      gdpData[, c(1, which(input$objective == colnames(gdpData)))]
    tmp <- na.omit(tmp)
    if (nrow(tmp) != 0) {
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
      }
      if (input$datatype == 3)
        #2nd diff
      {
        tmp <-
          data.frame(tmp[-c(1, 2), 1], diff(tmp[, 2], lag = 1, differences = 2),
                     check.names = F)
      }
      colnames(tmp) <- buf
      plotData <- tmp
      subtitle <<-
        paste(format(plotData[1, 1], "%Y-%m"),
              " - ",
              format(plotData[nrow(plotData), 1], "%Y-%m"))
      
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
          paste(format(minData[, 1], "%Y-%m"), ""),
          paste(format(maxData[, 1], "%Y-%m"), ""),
          "-",
          "-",
          "-",
          "-",
          "p-value",
          paste("p-value:", ad.test(x)$method),
          "%"
        )
      
      assign(
        paste("statisticTable", sss, sep = ""),
        data.frame(
          Statistic = statistic,
          Result = result,
          Remark = remark,
          stringsAsFactors = F,
          check.names = F
        ),
        envir = .GlobalEnv
      )
      
      assign(
        paste("stDT", sss, sep = ""),
        DT::renderDataTable(
          get(paste("statisticTable", sss, sep = "")),
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
        ),
        envir = .GlobalEnv
      )
      
      bufTSData <-
        data.frame(c(NA, round(((plotData[-1, -1] / plotData[-nrow(plotData), -1]) - 1
        ) * 100, 2)))
      colnames(bufTSData) <- "Changes from previous data(%)"
      bufTSData[, 1] <- bufTSData[, 1]
      bufplotData <- plotData
      bufplotData[, 1] <- format(bufplotData[, 1], "%Y-%m")
      assign(
        paste("timeseriesData", sss, sep = ""),
        data.frame(bufplotData, bufTSData, check.names = F),
        envir = .GlobalEnv
      )
      assign(
        paste("tsDT", sss, sep = ""),
        DT::renderDataTable(
          get(paste("timeseriesData", sss, sep = "")),
          rownames = F,
          caption = paste("Table 1: Statistical Summary.", subtitle),
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
        ),
        envir = .GlobalEnv
      )
      
      assign(paste("plot", sss, sep = ""), renderPlot({
        par(mar = c(5, 4, 3, 3), family = "Noto Sans Japanese")
        if (nrow(na.omit(plotData)) == 0) {
          plot.new()
        } else{
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
              auto.arima(
                plotData[, 2],
                ic = "aic",
                trace = F,
                stepwise = T
              )
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
              format = "%Y-%m",
              padj = 1,
              cex.axis = 1
            )
          }
        }
      })
      , envir = .GlobalEnv)
    } else{
      assign(paste("plot", sss, sep = ""), renderPlot({
        plot.new()
      }), envir = .GlobalEnv)
      
      assign(paste("statisticTable", sss, sep = ""),
             data.frame(),
             envir = .GlobalEnv)
      assign(
        paste("stDT", sss, sep = ""),
        DT::renderDataTable(
          get(paste("statisticTable", sss, sep = "")),
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
        ),
        envir = .GlobalEnv
      )
      
      assign(paste("timeseriesData", sss, sep = ""),
             data.frame(),
             envir = .GlobalEnv)
      assign(
        paste("tsDT", sss, sep = ""),
        DT::renderDataTable(
          get(paste("timeseriesData", sss, sep = "")),
          rownames = F,
          caption = paste("Table 1: Statistical Summary.", subtitle),
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
        ),
        envir = .GlobalEnv
      )
    }
  }
  
  reactiveData <- reactive({
    sss <- 1
    funGDP(sss)
    output$plot01 <- get(paste("plot", sss, sep = ""))
    output$statisticTable01 <- get(paste("stDT", sss, sep = ""))
    output$timeseriesTable01 <- get(paste("tsDT", sss, sep = ""))
    
    sss <- 2
    funGDP(sss)
    output$plot02 <- get(paste("plot", sss, sep = ""))
    output$statisticTable02 <- get(paste("stDT", sss, sep = ""))
    output$timeseriesTable02 <- get(paste("tsDT", sss, sep = ""))
    
    sss <- 3
    funGDP(sss)
    output$plot03 <- get(paste("plot", sss, sep = ""))
    output$statisticTable03 <- get(paste("stDT", sss, sep = ""))
    output$timeseriesTable03 <- get(paste("tsDT", sss, sep = ""))
    
    sss <- 4
    funGDP(sss)
    output$plot04 <- get(paste("plot", sss, sep = ""))
    output$statisticTable04 <- get(paste("stDT", sss, sep = ""))
    output$timeseriesTable04 <- get(paste("tsDT", sss, sep = ""))
  })
  
  output$remarktext <- renderUI({
    str <- "<hr>
    <b>Remarks</b><br>
    <ol>
    <li>Sample Variance = var(x)*(length(x)-1)/length(x)</li>
    <li>Sample Standard Deviation = √Sample Variance</li>
    <li>adf.test( x ) { tseries } , ad.test( x ) { nortest }(Normality test)</li>
    <li>Variation Coefficient(VC,%)=(Sample Standard Deviation/Mean)*100</li>
    <li><a href=\"http://github.hubspot.com/pace/docs/welcome/\" target=\"_blank\">PACE - http://github.hubspot.com/pace/docs/welcome/</a></li>
    <li><a href=\"http://www.saecanet.com\" target=\"_blank\">SaECaNet</a></li>
    <li>Other apps <a href=\"http://webapps.saecanet.com\" target=\"_blank\">SaECaNet - Web Applications</a></li>
    <li><a href=\"http://am-consulting.co.jp\" target=\"_blank\">Asset Management Consulting Corporation</a></li>
    </ol>"
    HTML(str)
  })
  
  output$history <- renderUI({
    str <- "<hr>
    <b>History</b><br>
    <ol>
    <li>2016-06-23:ver.1.0.0</li>
    </ol>"
    HTML(str)
  })
  
  output$gitcode <- renderUI({
    str <- "<hr>
    <b>Code</b><br>
    <ol>
    <li><a href=\"https://github.com/am-consulting/Rscript/tree/master/shinyapps/GDPofJapan\" target=\"_blank\">Move to GitHub</a></li>
    </ol>"
    HTML(str)
  })
  
  output$DataDownloadTime <- renderText({
    reactiveData()
    paste("Data downloaded time(UTC):" ,
          as.character(latestDataDownloadTime))
  })
  
})