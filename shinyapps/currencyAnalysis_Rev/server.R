library(shiny)
library(tseries)
library(dplyr)
library(RCurl)
library(PerformanceAnalytics)
shinyServer(function(input, output, session)
{
  tab_FXOanda <- 0
  makeReactiveBinding('tab_FXOanda')
  
  observeEvent(input$searchAction_FXOanda, {
    script <-
      getURL(
        "https://raw.githubusercontent.com/am-consulting/Rscript/master/importFXRateBygetFX_major.r",
        ssl.verifypeer = F
      )
    eval(parse(text = script))

    latestDataDownloadTime_FXOanda <<- as.POSIXlt(Sys.time(), "GMT")    

    output$dataRange_FXOanda <- renderUI({
      sliderInput(
        "dataRange_FXOanda",
        label = "Period",
        min = 1,
        max = nrow(origData),
        value = c(1, nrow(origData)),
        step = 1
      )
    })

    tab_FXOanda <<- 1
    origData <<- origData
  })  
  
  observe({
    if (tab_FXOanda == 0) {
      return(NULL)
    } else{
      if (is.null(input$dataRange_FXOanda[1])) { # dataRangeの読み込みを待つために必要
        return(NULL)
      } else{
        dateFormat <- "%Y-%m-%d"
        dateRange <- input$dataRange_FXOanda[2] - input$dataRange_FXOanda[1] + 1
        if (dateRange < 10) { # adf.test: sample size must be greater than 8
          if (input$dataRange_FXOanda[2] < 10) {
            lowerRange <- 1
            upperRange <- 10
          } else{
            lowerRange <- input$dataRange_FXOanda[2] - 9
            upperRange <- input$dataRange_FXOanda[2]
          }
        }
        else{
          lowerRange <- input$dataRange_FXOanda[1]
          upperRange <- input$dataRange_FXOanda[2]
        }
        
        dataSet <-
          subset(origData,
                 lowerRange <= index(origData) & index(origData) <= upperRange)

        dataSet01P <- data.frame(
          Date = dataSet[-1,1],  
          round(diff(as.matrix(dataSet[,-1]),lag = 1,differences = 1)/as.matrix(dataSet[-nrow(dataSet),-1])*100,2),
          check.names = F
        )
        
        colnames(dataSet01P)[-1]<-paste0(colnames(dataSet01P)[-1],'-DoD')
        subtitle01 <- paste0(first(dataSet[,1]) ,'~', last(dataSet[,1]))
        subtitle02 <- paste0(first(dataSet01P[,1]) ,'~', last(dataSet01P[,1]))
        
        output$plot1_FXOanda <- renderPlot({
          par(mar = c(5, 5, 4, 3), mfrow=c(1, 3), oma=c(0, 0, 0, 0))
          for(iii in 2:4){
          plot(
            dataSet[, 1],
            dataSet[, iii],
            type = 'l',
            xlab = "",
            ylab = colnames(dataSet)[iii],
            panel.first = grid(nx = NULL, ny = NULL, lty = 2, equilogs = T),
            xaxt = "n",
            main = paste(colnames(dataSet)[iii], "\n", subtitle01),
            cex.axis = 2,
            cex.lab = 2,
            cex.main = 2
          )
          lo <- loess(dataSet[, iii]~as.numeric(dataSet[, 1]), degree = 2)
          lines(dataSet[, 1] , predict(lo) , col='red', lwd=2, lty=2)            
          axis.Date(side = 1, at = dataSet[, 1], format = dateFormat, padj = 1, cex.axis = 2)
          } 
        })
          
        output$plot2_FXOanda <- renderPlot({
          par(mar = c(5, 5, 4, 3), mfrow=c(1, 3), oma=c(0, 0, 4, 0))
          for(iii in 2:4){
            plot(
              dataSet01P[, 1],
              dataSet01P[, iii],
              type = 'h',
              xlab = "",
              ylab = colnames(dataSet01P)[iii],
              panel.first = grid(nx = NULL, ny = NULL, lty = 2, equilogs = T),
              xaxt = "n",
              main = paste(colnames(dataSet01P)[iii], "\n", subtitle02),
              cex.axis = 2,
              cex.lab = 2,
              cex.main = 2
            )
            axis.Date(side = 1, at = dataSet01P[, 1], format = dateFormat, padj = 1, cex.axis = 2)
            abline(h=0, col='red', lwd=2)
          }
          mtext('Changes From Previous Day(%)',side = 3,outer = T,cex = 2)
        })

        correlationMethod<- c("pearson", "kendall", "spearman")

        output$title01_FXOanda <-   renderText({subtitle01})      
        output$summary01_FXOanda <- renderPrint({summary(dataSet[,-1])})
        output$psych01_FXOanda <- renderPrint({psych::describe(dataSet[,-1])})
        output$pastecs01_FXOanda <- renderPrint({pastecs::stat.desc(dataSet[,-1])})
        output$adf01_FXOanda <- renderPrint({apply(dataSet[,-1], 2, adf.test)})
        
        output$title02_FXOanda <-   renderText({subtitle02})      
        output$summary02_FXOanda <- renderPrint({summary(dataSet01P[,-1])})
        output$psych02_FXOanda <- renderPrint({psych::describe(dataSet01P[,-1])})
        output$pastecs02_FXOanda <- renderPrint({pastecs::stat.desc(dataSet01P[,-1])}) 
        output$adf02_FXOanda <- renderPrint({apply(dataSet01P[,-1], 2, adf.test)})
        
        selectedMethod01 <- 1
        output$title011corr_FXOanda <- output$title012corr_FXOanda <- renderText({correlationMethod[selectedMethod01]})
        output$cor01_FXOanda <- renderPrint({cor(dataSet[,-1],    method = correlationMethod[selectedMethod01])})
        output$cor02_FXOanda <- renderPrint({cor(dataSet01P[,-1], method = correlationMethod[selectedMethod01])})
        
        selectedMethod02 <- 3
        output$title021corr_FXOanda <- output$title022corr_FXOanda <- renderText({correlationMethod[selectedMethod02]})
        output$cor03_FXOanda <- renderPrint({cor(dataSet[,-1],    method = correlationMethod[selectedMethod02])})
        output$cor04_FXOanda <- renderPrint({cor(dataSet01P[,-1], method = correlationMethod[selectedMethod02])})

        mergeData_FXOanda<- merge(dataSet,dataSet01P,by = 'Date',all=T)
        
        output$table1_FXOanda <- DT::renderDataTable(
          mergeData_FXOanda,
          rownames = F,
          caption = paste0(
            "Table: Foreign Excahnge Rate & Day over Day(%). Period:", 
            mergeData_FXOanda[1, 1], " - ", mergeData_FXOanda[nrow(mergeData_FXOanda), 1]),
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
      }
    }
  })      
  
  output$latestDataDownloadTime_FXOanda <- renderText({
    if (tab_FXOanda == 1) {
      paste("Data imported time(UTC):" ,
            as.character(latestDataDownloadTime_FXOanda))
    }
  })
  
  output$remarktextFX_FXOanda <- renderUI({
    str <- "<hr>
  <b>Note</b><br>
  <ol>
  <li>-</li>
  </ol>"
    HTML(str)
  })
  
  output$disclaimerFX_FXOanda<- renderUI({
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