library(shiny)
library(tseries)
library(forecast)
library(DT)
library(nortest)
library(RCurl)
library(e1071)
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
    
    jgbData <- na.omit(jgbData)
    
    output$year <- renderUI({
      selectInput(
        "year",
        label = "Year",
        colnames(jgbData)[-1],
        selected = colnames(jgbData)[11],
        #default 10Y
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
          `1st difference` = 2  ,
          `2nd difference` = 3
        ),
        selected = 1
      )
    })
    
    tabledata <<-
      data.frame(Date = format(jgbData[, 1], "%Y-%m-%d"),
                 jgbData[, -1],
                 check.names = F)
    
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
      if (is.null(input$dataRange[1])) {
        return(NULL)
      } else{
        dateRange <- input$dataRange[2] - input$dataRange[1] + 1
        if (dateRange < 10) {
          # adf.test: sample size must be greater than 8
          if (input$dataRange[2] < 10) {
            lowerRange <- 1
            upperRange <- 10
          } else if (nrow(tmp0) - 9 < (input$dataRange[1])) {
            lowerRange <- input$dataRange[2] - 9
            upperRange <- input$dataRange[2]
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
                 lowerRange <= index(tmp0) & index(tmp0) <=
                   upperRange)
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
        SV <- var(x) * (length(x) - 1) / length(x) #sample variance
        UV <- var(x)#unbiased variance
        SSD <- sqrt(SV)#sample standard deviation
        latestData <- tail(dataSet, 1)
        minData <-
          tail(subset(dataSet, min(dataSet[, 2]) == dataSet[, 2]), 1)
        maxData <-
          tail(subset(dataSet, max(dataSet[, 2]) == dataSet[, 2]), 1)
        statistic <-
          c(
            "n",
            "Last Data within Period",
            "Mean",
            "median",
            "Min",
            "Max",
            "Unbiassed Standard Deviation",
            "Sample Standard Deviation",
            "Unbiassed Variance",
            "Sample Variance",
            "ADF test",
            "Normality test",
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
            paste(ad.test(x)$method, ": p-value"),
            "-",
            "-"
          )
        statisticTable <<-
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
        
        output$table2 <- DT::renderDataTable(
          tail(tabledata, 100),
          rownames = FALSE,
          caption = "Table 2: Japanese Government Bonds Interest Rate(%). Last 100 dates",
          options = list(
            autoWidth = F,
            info = T,
            lengthChange = F,
            ordering = T,
            searching = T,
            scrollX = F,
            lengthMenu = list(c(5,
                                15, -1), c("5", "15", "All")),
            pageLength = 5,
            orderClasses = TRUE,
            order = list(list(0, "desc"))
          )
        )

        output$plot1 <- renderPlot({
          par(mar = c(5, 4, 3, 3))
          if (input$charttype == "hist")
          {
            hist(
              plotData[, 2],
              breaks = 50,
              cex.axis = 1,
              cex.lab = 1,
              cex.main = 1.5,
              main = colnames(plotData)[2],
              xlab = ""
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
              cex.main = 1.5
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
})