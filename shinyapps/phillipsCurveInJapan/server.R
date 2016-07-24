library(shiny)
library(tseries)
library(forecast)
library(DT)
library(nortest)
library(lubridate)
library(ggplot2)
shinyServer(function(input, output, session)
{
  funPC <- function(sss) {
    tmp <-
      bufData[, c(1, which(objData == colnames(bufData)))]
    tmp <- na.omit(tmp)
    if (nrow(tmp) != 0) {
      tmp <- subset(tmp, startData <= index(tmp) & index(tmp) <= endData)
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
      
      assign(paste("plotData", sss, sep = ""), plotData, envir = .GlobalEnv)
      
      subtitle <-
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
      
      bufTSData <-
        data.frame(c(NA, diff(plotData[, -1])))
      colnames(bufTSData) <- "Changes from previous data(pp)"
      #      bufTSData[, 1] <- bufTSData[, 1]
      bufplotData <- plotData
      bufplotData[, 1] <- format(bufplotData[, 1], "%Y-%m")
      assign(
        paste("timeseriesData", sss, sep = ""),
        data.frame(bufplotData, bufTSData, check.names = F),
        envir = .GlobalEnv
      )
      
      assign(paste("plot", sss, sep = ""), renderPlot({
        par(mar = c(5, 4, 3, 3), family = "Noto Sans Japanese")
        if (nrow(na.omit(plotData)) != 0) {
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
      
      assign(paste("timeseriesData", sss, sep = ""),
             data.frame(),
             envir = .GlobalEnv)
      
    }
    assign(
      paste("stDT", sss, sep = ""),
      DT::renderDataTable(
        get(paste("statisticTable", sss, sep = "")),
        rownames = F,
        caption = paste(
          "Table ",
          sss + 1,
          colnames(plotData)[2],
          ": Statistical Summary.",
          subtitle
        ),
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
    
    assign(
      paste("tsDT", sss, sep = ""),
      DT::renderDataTable(
        get(paste("timeseriesData", sss, sep = "")),
        rownames = F,
        caption = paste("Table ", sss + 2, ": Time Series Data.", subtitle),
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
  
  reactiveData <- reactive({
    sss <- 0
    bufData <<- laborforcesurvey
    objData <<- input$lfs
    startData <<- input$lfsRange[1]
    endData <<- input$lfsRange[2]
    funPC(sss)
    output$plot01 <- get(paste("plot", sss, sep = ""))
    output$statisticTable01 <- get(paste("stDT", sss, sep = ""))
    output$timeseriesTable01 <- get(paste("tsDT", sss, sep = ""))
    
    sss <- 2
    bufData <<- consumerpriceindex
    objData <<- input$cpi
    startData <<- input$cpiRange[1]
    endData <<- input$cpiRange[2]
    funPC(sss)
    output$plot02 <- get(paste("plot", sss, sep = ""))
    output$statisticTable02 <- get(paste("stDT", sss, sep = ""))
    output$timeseriesTable02 <- get(paste("tsDT", sss, sep = ""))
    
    mergeData <- na.omit(merge(plotData0, plotData2, by = "Date"))
    
    output$plotmerge <- renderPlot({
      par(mar = c(5, 5, 3, 5), family = "Noto Sans Japanese")
      plot(
        mergeData[, 1],
        mergeData[, 2],
        type = "o",
        col = "blue",
        xlab = "" ,
        ylab = "" ,
        panel.first = grid(
          nx = NULL,
          ny = NULL,
          lty = 2,
          equilogs = T
        ) ,
        xaxt = "n",
        main = paste(colnames(mergeData)[2], "×", colnames(mergeData)[3]),
        cex.axis = 1,
        cex.lab = 1,
        cex.main = 1
      )
      par(new = T)
      plot(
        mergeData[, 1],
        mergeData[, 3],
        col = "red",
        type = "o",
        xlab = "" ,
        ylab = "" ,
        panel.first = grid(
          nx = NULL,
          ny = NULL,
          lty = 2,
          equilogs = T
        ) ,
        xaxt = "n",
        yaxt = "n",
        cex.axis = 1,
        cex.lab = 1,
        cex.main = 1
      )
      axis(4, cex.axis = 1, cex.lab = 1)
      legend(
        "topleft",
        col = c("blue", "red"),
        pch = 1,
        legend = c(colnames(mergeData)[2], colnames(mergeData)[3]),
        cex = 1
      )
      mtext(
        colnames(mergeData)[2],
        side = 2,
        line = 3.2,
        cex = 1
      )
      mtext(
        colnames(mergeData)[3],
        side = 4,
        line = 3.2,
        cex = 1
      )
      axis.Date(
        side = 1,
        at = mergeData[, 1],
        format = "%Y-%m",
        padj = 1,
        cex.axis = 1
      )
    })
    
    output$PhillipsCurve <- renderPlot({
      dataSet <- mergeData
      g <- ggplot()
      g <-
        g + geom_point(data = dataSet,
                       aes(x = dataSet[, 2], y = dataSet[, 3]),
                       size = 1)
      g <-
        g + ggtitle(paste(colnames(dataSet)[2], "×", colnames(dataSet)[3]))
      g <-
        g + xlab(colnames(dataSet)[2]) + ylab(colnames(dataSet)[3])
      g <- g + theme(plot.title = element_text(size = 17))
      g <- g + theme(axis.title.x = element_text(size = 17))
      g <- g + theme(axis.title.y = element_text(size = 17))
      g <-
        g + theme(axis.text.x = element_text(size = 17,
                                             angle = 0))
      g <-
        g + theme(axis.text.y = element_text(size = 17,
                                             angle = 90))
      g <-
        g + geom_text(
          data = dataSet,
          aes(
            x = dataSet[, 2],
            y = dataSet[, 3],
            label = format(dataSet[, 1], "%Y-%m")
          ),
          size = 4,
          vjust = -0.5,
          hjust = 0.5
        )
      g <-
        g + geom_path(
          data = dataSet,
          aes(x = dataSet[, 2], y = dataSet[, 3]),
          col = "blue",
          size = 0.5
        )
      print(g)
    })
  })
  
  output$remarktext <- renderUI({
    str <- "<hr>
    <b>Remarks</b><br>
    <ol>
    <li>Unit: 10 thousand people , percent</li>
    <li>Sample Variance = var(x)*(length(x)-1)/length(x)</li>
    <li>Sample Standard Deviation = √Sample Variance</li>
    <li>adf.test( x ) { tseries } , ad.test( x ) { nortest }(Normality test)</li>
    <li>Variation Coefficient(VC,%)=(Sample Standard Deviation/Mean)*100</li>
    <li><a href=\"http://github.hubspot.com/pace/docs/welcome/\" target=\"_blank\">PACE - http://github.hubspot.com/pace/docs/welcome/</a></li>
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
    <li>2016-06-28:ver.1.0.0</li>
    <li>2016-07-24:ver.1.0.1</li>
    </ol>"
    HTML(str)
  })
  
  output$gitcode <- renderUI({
    str <- "<hr>
    <b>Code</b><br>
    <ol>
    <li><a href=\"https://github.com/am-consulting/Rscript/tree/master/shinyapps/phillipsCurveInJapan\" target=\"_blank\">Move to GitHub</a></li>
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