library(shiny)
library(tseries)
library(forecast)
library(DT)
library(nortest)
shinyServer(function(input, output, session)
{
  getOrigData <- reactive({
    tmp0 <- origData[, c(1, which(input$currency==colnames(origData)))]
    tmp1 <<- na.omit(tmp0)
  })

    output$dataRange <- renderUI({
      getOrigData()
      if (length(nrow(tmp1)) == 0) {
        return(NULL)
      } else{
        sliderInput(
          "dataRange",
          label = "Data Range",
          min = 1,
          max = nrow(tmp1),
          value = c(1, nrow(tmp1)),
          step = 1
        )
      }
    })

    reactiveData <- reactive({
    if(is.null(input$dataRange[1])){return(NULL)}else{  
    tmp <- subset(tmp1, input$dataRange[1] <= index(tmp1) & index(tmp1) <=
                    input$dataRange[2])
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
        "ad.test( x )",
        "Date Range"
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
      paste(tmp[1,1],"-",tmp[nrow(tmp),1])
      )
    remark <-
      c(
        "-",
        "JPY",
        "JPY",
        paste(format(minData[, 1], "%Y-%m-%d"),"JPY"),
        paste(format(maxData[, 1], "%Y-%m-%d"),"JPY"),
        "JPY",
        "JPY",
        "JPY",
        "JPY",
        "p-value",
        paste(ad.test(x)$method, ": p-value"),
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
    
    variance <- function(x) {
      tmpx <- sqrt(var(x) * (length(x) - 1) / length(x))
      value <- floor(1000 * as.numeric(tmpx) + 0.5) / 1000
      return(value)
    }
    
    buffxData <- tail(origData, 100)
    buffxData <- buffxData[, colSums(is.na(buffxData)) == 0]
    meanValue = as.vector(colMeans(buffxData[, -1]), mode = 'numeric')
    sdValue = as.vector(apply(buffxData[, -1], 2, variance), mode = 'numeric')
    latest1Data <-
      as.vector(buffxData[nrow(buffxData) - 0, -1], mode = 'numeric')
    latest2Data <-
      as.vector(buffxData[nrow(buffxData) - 1, -1], mode = 'numeric')
    tmpDataset <- data.frame(
      Currency = colnames(buffxData[, -1]),
      latest1 = latest1Data,
      latest2 = latest2Data,
      Median = as.vector(apply(buffxData[, -1], 2, median), mode = 'numeric'),
      SampleStandardDeviation = sdValue,
      Max = as.vector(apply(buffxData[, -1], 2, max), mode = 'numeric'),
      Min = as.vector(apply(buffxData[, -1], 2, min), mode = 'numeric'),
      VariationCoefficient = sdValue / meanValue * 100,
      FromTheDayBefore = (latest1Data - latest2Data) / latest2Data * 100,
      row.names = NULL
    )
    tmpDataset[, c(5, 8, 9)] <-
      floor(1000 * (tmpDataset[, c(5, 8, 9)]) + 0.5) / 1000
    colnames(tmpDataset)[2:3] <-
      c(as.character(buffxData[nrow(buffxData) - 0, 1]), as.character(buffxData[nrow(buffxData) -
                                                                                  1, 1]))
    #一旦挟まないとdatatablesのエラーが出る
    #https://datatables.net/manual/tech-notes/1
    tmpDataset[, 1] <- gsub("-", "-", tmpDataset[, 1])
    tmpDataset <<- tmpDataset
    dataset <<-
      data.frame(ID = seq(1, nrow(tmpDataset)),
                 tmpDataset,
                 check.names = F)
    
    output$table1 <- DT::renderDataTable(
      statisticTable,
      rownames = F,
      caption = "Table 1: Statistical Summary",
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
      dataset,
      rownames = F,
      caption = paste("Table 2: Statistical Summary for last 100 days.Unit:VariationCoefficient,FromTheDayBefore - %,others - JPY"),
      options = list(
        autoWidth = T,
        info = T,
        lengthChange = T,
        ordering = T,
        searching = F,
        scrollX = T,
        lengthMenu = list(c(-1, 1), c("All", "1")),
        orderClasses = TRUE,
        order = list(list(0, "asc"))
      )
    )
    
    output$plot1 <- renderPlot({
      par(mar = c(5, 4, 3, 3),family="Noto Sans Japanese")
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
          main = paste(colnames(plotData)[2],"Unit:JPY\n",subtitle),
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
            "Unit:JPY\n",
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
          main = paste(colnames(plotData)[2], "Unit:JPY\n", subtitle),
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
    }
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
    <li>2016-06-15:ver.1.0.0</li>
    <li>2016-06-17:ver.1.0.1</li>
    <li>2016-06-19:ver.1.0.2</li>
    <li>2016-07-12:ver.1.0.3</li>
    </ol>"
    HTML(str)
  })

  output$gitcode <- renderUI({
    str <- "<hr>
    <b>Code</b><br>
    <ol>
    <li><a href=\"https://github.com/am-consulting/Rscript/tree/master/shinyapps/historicalFXData\" target=\"_blank\">Move to GitHub</a></li>
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