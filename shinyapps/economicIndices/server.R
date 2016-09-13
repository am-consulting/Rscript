library(shiny)
shinyServer(function(input, output)
{
  tabJGB <- 0
  makeReactiveBinding('tabJGB')
  
  observeEvent(input$searchAction, {
    script <-
      getURL(
        "https://raw.githubusercontent.com/am-consulting/Rscript/master/Rscript_JGBInterestRate.r",
        ssl.verifypeer = F
      )
    eval(parse(text = script))
    
    latestDataDownloadTime <<- as.POSIXlt(Sys.time(), "GMT")    
    
    jgbData <- na.omit(jgbData)
    
    output$year <- renderUI({
      selectInput(
        "year",
        label = "Year",
        colnames(jgbData)[-1],
        selected = colnames(jgbData)[11], # default 10Y
        selectize = F
      )
    })
    
    output$dataRange <- renderUI({
      sliderInput(
        "dataRange",
        label = "Period",
        min = 1,
        max = nrow(jgbData),
        value = c(1, nrow(jgbData)),
        step = 1
      )
    })
    
    output$lineType <- renderUI({
      radioButtons(
        "charttype",
        label = "Chart Type",
        choices = list(
          `Time series( Line )` = "l",
          `Time series( Bar )` = "h",
          `Histogram` = "hist",
          `Q-Q plot` = "QQ" ,
          `Arima` = "arima"
        ),
        selected = "l"
      )
    })
    
    output$dataType <- renderUI({
      radioButtons(
        "datatype",
        label = "Data Type",
        choices = list(
          `Level` = 1,
          `1st difference` = 2,
          `2nd difference` = 3
        ),
        selected = 1
      )
    })
    
    
    tabJGB <<- 1
    jgbData <<- jgbData
  })
  
  observe({
    if (tabJGB == 0) {
      return(NULL)
    } else{
      tmp0 <-
        jgbData[, c(1, grep(
          paste("\\b", input$year , "\\b", sep = ""),
          colnames(jgbData)
        ))]
      if (is.null(input$dataRange[1])) { # dataRangeの読み込みを待つために必要
        return(NULL)
      } else{
        dateRange <- input$dataRange[2] - input$dataRange[1] + 1
        if (dateRange < 10) { # adf.test: sample size must be greater than 8
          if (input$dataRange[2] < 10) {
            lowerRange <- 1
            upperRange <- 10
          } else{
            lowerRange <- input$dataRange[2] - 9
            upperRange <- input$dataRange[2]
          }
        }
        else{
          lowerRange <- input$dataRange[1]
          upperRange <- input$dataRange[2]
        }
        dataSet <-
          subset(tmp0,
                 lowerRange <= index(tmp0) & index(tmp0) <= upperRange)
        buf <- colnames(dataSet)
        if (input$datatype == 2)
          #1st diff
        {
          dataSet <-
            data.frame(dataSet[-1, 1],
                       diff(dataSet[, 2], lag = 1, differences = 1),
                       check.names = F)
        }
        if (input$datatype == 3)
          #2nd diff
        {
          dataSet <-
            data.frame(dataSet[-c(1, 2), 1],
                       diff(dataSet[, 2], lag = 1, differences = 2),
                       check.names = F)
        }
        colnames(dataSet) <- buf
        x <- dataSet[, 2]
        SV <- var(x) * (length(x) - 1) / length(x) # sample variance
        UV <- var(x) # unbiased variance
        SSD <- sqrt(SV) # sample standard deviation
        latestData <- tail(dataSet, 1)
        minData <-
          tail(subset(dataSet, min(dataSet[, 2]) == dataSet[, 2]), 1)
        maxData <-
          tail(subset(dataSet, max(dataSet[, 2]) == dataSet[, 2]), 1)
        statistic <-
          c(
            "n",
            "Last Data within The Period",
            "Mean",
            "Median",
            "Min",
            "Max",
            "Unbiassed Standard Deviation",
            "Sample Standard Deviation",
            "Unbiassed Variance",
            "Sample Variance",
            "ADF test",
            ad.test(x)$method,
            "Skewness",
            "Kurtosis"
          )
        result <-
          c(length(x), round(
            c(
              latestData[,2],
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
              skewness(x),
              kurtosis(x)
            ),
            3
          ))
        remark <-
          c(
            "-",
            format(latestData[, 1], "%Y-%m-%d"),
            "-",
            "-",
            format(minData[, 1], "%Y-%m-%d"),
            format(maxData[, 1], "%Y-%m-%d"),
            "-",
            "-",
            "-",
            "-",
            "p-value",
            "p-value",
            "-",
            "-"
          )
        statisticTable <-
          data.frame(
            Item  = statistic,
            Value  = result,
            Notes  = remark,
            stringsAsFactors = F,
            check.names = F
          )
        
        plotData <- dataSet
        subtitle <-
          paste(plotData[1, 1], " - ", plotData[nrow(plotData), 1])
        
        output$table1 <-
          DT::renderDataTable(
            statisticTable,
            rownames = FALSE,
            caption = paste("Table 1: Statistics Data. Period:", subtitle),
            options = list(
              autoWidth = F,
              info = F,
              lengthChange = F,
              ordering = F,
              searching = F,
              scrollX = F,
              paging = F
            )
          )
        
        tabledata <- subset(jgbData, lowerRange <= index(jgbData) & index(jgbData) <= upperRange)
        tabledata[, 1] <- format(tabledata[, 1], "%Y-%m-%d")

        output$table2 <- DT::renderDataTable(
          tabledata,
          rownames = F,
          caption = paste0(
            "Table 2: Japanese Government Bonds Interest Rate(%). Period:", 
            tabledata[1, 1], " - ", tabledata[nrow(tabledata), 1]),
          options = list(
            autoWidth = F,
            info = T,
            lengthChange = F,
            ordering = T,
            searching = T,
            scrollX = F,
            lengthMenu = list(c(5, 15, -1), c("5", "15", "All")),
            pageLength = 5,
            orderClasses = T,
            order = list(list(0, "desc"))
          )
        )

        output$plot1 <- renderPlot({
          par(mar = c(5, 4, 4, 3))
          if (input$charttype == "hist")
          {
            hist(
              plotData[, 2],
              breaks = 50,
              cex.axis = 1,
              cex.lab = 1,
              cex.main = 1.5,
              main = colnames(plotData)[2],
              xlab = "",
              col = "#808000"
            )
          }
          if (input$charttype == "arima")
          {
            ci = c(95, 99)
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
                ci[2],
                "\n",
                "Number of periods for forecasting=",forecastN
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
              cex.main = 1.5
            )
            abline(h = 0, col = "red")
            lo <- loess(plotData[, 2]~as.numeric(plotData[, 1]), degree = 2)
            lines(plotData[, 1] , predict(lo) , col='red', lwd=2,lty=2)            
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
    }
  })
  
  output$latestDataDownloadTime <- renderText({
    if (tabJGB == 1) {
      paste("Data imported time(UTC):" ,
            as.character(latestDataDownloadTime))
    }
  })
  
  output$remarktext <- renderUI({
    str <- "<hr>
    <b>Note</b><br>
    <ol>
    <li>Sample Variance = var(x)*(length(x)-1)/length(x)</li>
    <li>Sample Standard Deviation = √Sample Variance</li>
    <li>The histogram cells are right-closed (left open) intervals.</li>
    </ol>"
    HTML(str)
  })
  
  output$disclaimer <- renderUI({
    str <- "<hr>
    <b>Operating Company / Disclaimer</b><br>
    <ol>
    <li>
    <a href=\"http://am-consulting.co.jp\" target=\"_blank\">Asset Management Consulting Corporation -  http://am-consulting.co.jp</a>
    </li>
    <li><a href=\"http://am-consulting.co.jp/disclaimer/\" target=\"_blank\">Disclaimer</a>
    </li>
    </ol>"
    HTML(str)
  })

# historical FX  
  tabHistoricalFX <- 0
  makeReactiveBinding('tabHistoricalFX')
  
  observeEvent(input$searchActionFX, {
    script <-
      getURL(
        "https://raw.githubusercontent.com/am-consulting/Rscript/master/getFXhistoricalData.r",
        ssl.verifypeer = F
      )
    eval(parse(text = script))
    latestDataDownloadTimeFX <<- as.POSIXlt(Sys.time(), "GMT")    
    
    origData <- na.omit(origData)
    
    output$currencyFX <- renderUI({
      selectInput("currencyFX",
                  label = "Currency",
                  colnames(origData)[-1],
                  selectize = F)
    })
    
    output$dataRangeFX <- renderUI({
      sliderInput(
        "dataRangeFX",
        label = "Data Range",
        min = 1,
        max = nrow(origData),
        value = c(1, nrow(origData)),
        step = 1
      )
    })
    
    output$charttypeFX <- renderUI({
      radioButtons(
        "charttypeFX",
        label = "Chart Type",
        choices = list(
          `Time series( Line )` = "l",
          `Time series( Bar )` = "h",
          Histogram = "hist",
          `Q-Q plot` = "QQ" ,
          `Arima` = "arima"
        ),
        selected = "l"
      )
    })
    
    output$datatypeFX <- renderUI({
      radioButtons(
        "datatypeFX",
        label = "Data Type",
        choices = list(
          Level = 1,
          `1st difference` = 2  ,
          `2nd difference` = 3
        ),
        selected = 1
      )
    })
    
    tabHistoricalFX <<- 1
    
    origData <<- origData
  })
  
  observe({
    if (tabHistoricalFX == 0) {
      return(NULL)
    } else{
      if (is.null(input$dataRangeFX[1])) { # dataRangeFXの読み込みを待つために必要
        return(NULL)
      } else{
        tmp0 <- origData[, c(1, which(input$currencyFX == colnames(origData)))]
        dateRange <- input$dataRangeFX[2] - input$dataRangeFX[1] + 1
        if (dateRange < 10) { # adf.test: sample size must be greater than 8
          if (input$dataRangeFX[2] < 10) {
            lowerRange <- 1
            upperRange <- 10
          } else{
            lowerRange <- input$dataRangeFX[2] - 9
            upperRange <- input$dataRangeFX[2]
          }
        }
        else{
          lowerRange <- input$dataRangeFX[1]
          upperRange <- input$dataRangeFX[2]
        }
        dataSet <-
          subset(tmp0,
                 lowerRange <= index(tmp0) &
                   index(tmp0) <= upperRange)
        buf <- colnames(dataSet)
        if (input$datatypeFX == 2)
          #1st diff
        {
          dataSet <-
            data.frame(dataSet[-1, 1],
                       diff(dataSet[, 2], lag = 1, differences = 1),
                       check.names = F)
        }
        if (input$datatypeFX == 3)
          #2nd diff
        {
          dataSet <-
            data.frame(dataSet[-c(1, 2), 1],
                       diff(dataSet[, 2], lag = 1, differences = 2),
                       check.names = F)
        }
        colnames(dataSet) <- buf
        plotData <- dataSet
        x <- dataSet[, 2]
        SV <- var(x) * (length(x) - 1) / length(x) # sample variance
        UV <- var(x) # unbiased variance
        SSD <- sqrt(SV) # sample standard deviation
        latestData <- tail(dataSet, 1)
        minData <-
          tail(subset(dataSet, min(dataSet[, 2]) == dataSet[, 2]), 1)
        maxData <-
          tail(subset(dataSet, max(dataSet[, 2]) == dataSet[, 2]), 1)
        statistic <-
          c(
            "n",
            "Mean",
            "Median",
            "Min",
            "Max",
            "Unbiassed Standard Deviation",
            "Sample Standard Deviation",
            "Unbiassed Variance",
            "Sample Variance",
            "ADF test",
            ad.test(x)$method,
            "Last Data within The Period"
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
              tail(x, 1)
            ), 3 ))
        remark <-
          c(
            "-",
            "JPY",
            "JPY",
            paste(format(minData[, 1], "%Y-%m-%d"), "JPY"),
            paste(format(maxData[, 1], "%Y-%m-%d"), "JPY"),
            "JPY",
            "JPY",
            "JPY",
            "JPY",
            "p-value",
            "p-value",
            paste("JPY ,", as.Date(dataSet[nrow(dataSet), 1]))
          )
        statisticTable <-
          data.frame(
            Item = statistic,
            Value = result,
            Notes = remark,
            stringsAsFactors = F,
            check.names = F
          )
        
        variance <- function(x) {
          tmpx <- sqrt(var(x) * (length(x) - 1) / length(x))
          value <- floor(1000 * as.numeric(tmpx) + 0.5) / 1000
          return(value)
        }
        
        buffxData <-
          subset(origData, lowerRange <= index(origData) & index(origData) <= upperRange)
        buffxData   <- buffxData[, colSums(is.na(buffxData)) == 0]
        meanValue   <-
          as.vector(colMeans(buffxData[, -1]), mode = 'numeric')
        sdValue     <-
          as.vector(apply(buffxData[, -1], 2, variance), mode = 'numeric')
        latest1Data <-
          as.vector(buffxData[nrow(buffxData), -1], mode = 'numeric')
        latest2Data <-
          as.vector(buffxData[1, -1], mode = 'numeric')
        tmpDataset  <- data.frame(
          Currency = colnames(buffxData[, -1]),
          latest1 = latest1Data,
          latest2 = latest2Data,
          Median = as.vector(apply(buffxData[, -1], 2, median), mode = 'numeric'),
          SampleStandardDeviation = sdValue,
          Max = as.vector(apply(buffxData[, -1], 2, max), mode = 'numeric'),
          Min = as.vector(apply(buffxData[, -1], 2, min), mode = 'numeric'),
          VariationCoefficient = sdValue / meanValue * 100,
          ChangeWithinThePeriod = (latest1Data - latest2Data) / latest2Data * 100,
          row.names = NULL
        )
        tmpDataset[, c(5, 8, 9)] <-
          floor(1000 * (tmpDataset[, c(5, 8, 9)]) + 0.5) / 1000
        colnames(tmpDataset)[2:3] <-
          c(as.character(buffxData[nrow(buffxData), 1]), as.character(buffxData[1, 1]))
        colnames(tmpDataset)[8:9] <-
          paste0(colnames(tmpDataset)[8:9], '(%)')
        #一旦挟まないとdatatablesのエラーが出る
        #https://datatables.net/manual/tech-notes/1
        tmpDataset[, 1] <- gsub("-", "-", tmpDataset[, 1])
        currencyDataTable <-
          data.frame(ID = seq(1, nrow(tmpDataset)),
                     tmpDataset,
                     check.names = F)
        
        output$table1FX <- DT::renderDataTable(
          statisticTable,
          rownames = F,
          caption = paste0(
            "Table 1: Statistical Summary. Currency:",
            colnames(dataSet)[2],
            ". Period:",
            dataSet[1, 1],
            "-",
            dataSet[nrow(dataSet), 1]
          ),
          options = list(
            autoWidth = F,
            info = F,
            lengthChange = F,
            ordering = F,
            searching = F,
            scrollX = F,
            paging = F
          )
        )
        
        output$table2FX <- DT::renderDataTable(
          currencyDataTable,
          rownames = F,
          caption = paste(
            "Table 2: Statistical Summary. Period:",
            dataSet[1, 1],
            "-",
            dataSet[nrow(dataSet), 1]
          ),
          options = list(
            autoWidth = F,
            info = T,
            lengthChange = T,
            ordering = T,
            searching = F,
            scrollX = F,
            lengthMenu = list(c(10, -1), c(10, "All")),
            orderClasses = T,
            order = list(list(0, "asc")),
            columnDefs = list(
              list(width = '15%', targets = 1),
              list(width = '10%', targets = 2),
              list(width = '10%', targets = 3)
            )
          )
        )
        
        output$plot1FX <- renderPlot({
          par(mar = c(5, 4, 4, 3), family = "Noto Sans Japanese")
          subtitle <-
            paste(plotData[1, 1], " - ", plotData[nrow(plotData), 1])
          if (input$charttypeFX == "hist")
          {
            hist(
              plotData[, 2],
              breaks = 50,
              cex.axis = 1,
              cex.lab = 1,
              cex.main = 1.5,
              main = paste(colnames(plotData)[2], "Unit:JPY\n", subtitle),
              xlab = "",
              col = "#808000"
            )
          }
          if (input$charttypeFX == "arima")
          {
            ci = c(80, 95)
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
                " , ",
                subtitle,
                " , CI=",
                ci[1],
                ",",
                ci[2],
                "\n",
                "Number of periods for forecasting=",forecastN
              ),
              ylab = "",
              panel.first = grid(
                nx = NULL,
                ny = NULL,
                lty = 2,
                equilogs = T
              ),
              cex.main=1.5
            )
          }
          if (input$charttypeFX == "QQ")
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
          if (input$charttypeFX == "l" || input$charttypeFX == "h")
          {
            plot(
              plotData[, 1],
              plotData[, 2],
              type = input$charttypeFX,
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
              cex.main = 1.5
            )
            abline(h = 0, col = "red")
            lo <-
              loess(plotData[, 2] ~ as.numeric(plotData[, 1]), degree = 2)
            lines(
              plotData[, 1] ,
              predict(lo) ,
              col = 'red',
              lwd = 2,
              lty = 2
            )
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
    }
  })
  
  output$latestDataDownloadTimeFX <- renderText({
    if (tabHistoricalFX == 1) {
      paste("Data imported time(UTC):" ,
            as.character(latestDataDownloadTimeFX))
    }
  })
  
  output$remarktextFX <- renderUI({
    str <- "<hr>
    <b>Note</b><br>
    <ol>
    <li>Sample Variance = var(x)×(length(x)-1)/length(x)</li>
    <li>Sample Standard Deviation = √Sample Variance</li>
    <li>Variation Coefficient(VC,%)=(Sample Standard Deviation/Mean)×100</li>
    <li>The histogram cells are right-closed (left open) intervals.</li>
    </ol>"
    HTML(str)
  })
  
  output$disclaimerFX <- renderUI({
    str <- "<hr>
    <b>Operating Company / Disclaimer</b><br>
    <ol>
    <li>
    <a href=\"http://am-consulting.co.jp\" target=\"_blank\">Asset Management Consulting Corporation -  http://am-consulting.co.jp</a>
    </li>
    <li><a href=\"http://am-consulting.co.jp/disclaimer/\" target=\"_blank\">Disclaimer</a>
    </li>
    </ol>"
    HTML(str)
  })
# historical FX
})