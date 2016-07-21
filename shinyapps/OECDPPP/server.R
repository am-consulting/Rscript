library(shiny)
library(tseries)
library(forecast)
library(DT)
library(nortest)
library(lubridate)
library(ggplot2)
shinyServer(function(input, output, session)
{
  reactiveData <- reactive({
    tmp <-
      origData[, c(1, which(input$location == colnames(origData)))]
    tmp <- na.omit(tmp)
    startYear <- input$yearRange[1]
    
    print(startYear)
    
    endYear <-input$yearRange[2]
    tmp <- subset(tmp, startYear <= tmp[, 1] & tmp[, 1] <=
                    endYear)
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
        minData[, 1],
        maxData[, 1],
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

    timeseriesData <- data.frame(plotData,
                                 c(NA, round(((plotData[-1, -1] / plotData[-nrow(plotData), -1]) - 1
                                 ) * 100, 2)), check.names = F)
    colnames(timeseriesData)[3] <- "Changes from previous data(%)"
    
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

    output$timeseriesData <- DT::renderDataTable(
      timeseriesData,
      rownames = F,
      caption = paste("Table 2:Time Series Data.", subtitle),
      options = list(
        autoWidth = T,
        info = T,
        lengthChange = T,
        ordering = T,
        searching = T,
        scrollX = T,
        lengthMenu = list(c(10, -1, 1), c("10", "All", "1")),
        orderClasses = TRUE,
        order = list(list(0, "desc")),
        paging = T
      )
    )
    
    output$locationList <- DT::renderDataTable(
      locationList,
      rownames = F,
      caption = paste("Table 3:Symbols."),
      options = list(
        autoWidth = T,
        info = T,
        lengthChange = T,
        ordering = T,
        searching = T,
        scrollX = T,
        lengthMenu = list(c(10, -1, 1), c("10", "All", "1")),
        orderClasses = TRUE,
        order = list(list(0, "asc")),
        paging = T
      )
    )

    #JPY
    JPY01<-dailyFXrate[,c(1,2,5)]
    JPY02<-origData[,c(1,16)]
    chimeraPlotJPY<-merge(JPY01,JPY02,by="TIME",all=T)
    chimeraPlotJPY<-na.omit(chimeraPlotJPY)
    chimeraPlotJPY$TIME <-factor(chimeraPlotJPY$TIME)
    tsDataJPY<-unique(chimeraPlotJPY[,c(1,4)])
    tsDataJPY[,1]<-as.integer(tsDataJPY[,1]) # important
    output$chimeraPlot01<-renderPlot({
    g1<-ggplot()
    g1<-g1+geom_boxplot(data = chimeraPlotJPY, aes(x = TIME, y = chimeraPlotJPY[, 3]))
    g1<-g1+geom_line(data = tsDataJPY,aes(x=TIME, y=tsDataJPY[,2]),col="blue")
    g1<-g1+geom_point(data = tsDataJPY,aes(x=TIME, y=tsDataJPY[,2]),size=3,col="blue")
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
    g1 <-      g1 + theme(axis.text.y = element_text(size = 15, angle = 90))
        g1 <-
      g1 + ggtitle(paste(colnames(chimeraPlotJPY)[3],"×",colnames(chimeraPlotJPY)[4],"\nBoxplot:Daily Exchange Rate , Blue Line:PPP\n", subtitle))
    g1 <- g1 + xlab("Year")
    g1 <- g1 + ylab("")
    print(g1)
      })
    
    #EURO
    EUR01<-dailyFXrate[,c(1,3,5)]
    EUR02<-origData[,c(1,48)]
    chimeraPlotEUR<-merge(EUR01,EUR02,by="TIME",all=T)
    chimeraPlotEUR<-na.omit(chimeraPlotEUR)
    chimeraPlotEUR$TIME <-factor(chimeraPlotEUR$TIME)
    tsDataEUR<-unique(chimeraPlotEUR[,c(1,4)])
    tsDataEUR[,1]<-as.integer(tsDataEUR[,1]) # important
    output$chimeraPlot02<-renderPlot({
    g2<-ggplot()
    g2<-g2+geom_boxplot(data = chimeraPlotEUR, aes(x = TIME, y = chimeraPlotEUR[, 3]))
    g2<-g2+geom_line(data = tsDataEUR,aes(x=TIME, y=tsDataEUR[,2]),col="blue")
    g2<-g2+geom_point(data = tsDataEUR,aes(x=TIME, y=tsDataEUR[,2]),size=3,col="blue")
    g2 <- g2 + theme(plot.title = element_text(size = 15))
    g2 <- g2 + theme(axis.title.x = element_text(size = 15))
    g2 <- g2 + theme(axis.title.y = element_text(size = 15))
    g2 <-
      g2 + theme(axis.text.x = element_text(
        size = 15,
        angle = 90,
        hjust = 0.5,
        vjust = 0.5
      ))
    g2 <-      g2 + theme(axis.text.y = element_text(size = 15, angle = 90))
        g2 <-
      g2 + ggtitle(paste(colnames(chimeraPlotEUR)[3],"×",colnames(chimeraPlotEUR)[4],"\nBoxplot:Daily Exchange Rate , Blue Line:PPP\n", subtitle))
    g2 <- g2 + xlab("Year")
    g2 <- g2 + ylab("")
    print(g2)
      })    
    
    #GBR
    GBR01<-dailyFXrate[,c(1,4,5)]
    GBR02<-origData[,c(1,30)]
    chimeraPlotGBR<-merge(GBR01,GBR02,by="TIME",all=T)
    chimeraPlotGBR<-na.omit(chimeraPlotGBR)
    chimeraPlotGBR$TIME <-factor(chimeraPlotGBR$TIME)
    tsDataGBR<-unique(chimeraPlotGBR[,c(1,4)])
    tsDataGBR[,1]<-as.integer(tsDataGBR[,1]) # important
    output$chimeraPlot03<-renderPlot({
    g3<-ggplot()
    g3<-g3+geom_boxplot(data = chimeraPlotGBR, aes(x = TIME, y = chimeraPlotGBR[, 3]))
    g3<-g3+geom_line(data = tsDataGBR,aes(x=TIME, y=tsDataGBR[,2]),col="blue")
    g3<-g3+geom_point(data = tsDataGBR,aes(x=TIME, y=tsDataGBR[,2]),size=3,col="blue")
    g3 <- g3 + theme(plot.title = element_text(size = 15))
    g3 <- g3 + theme(axis.title.x = element_text(size = 15))
    g3 <- g3 + theme(axis.title.y = element_text(size = 15))
    g3 <-
      g3 + theme(axis.text.x = element_text(
        size = 15,
        angle = 90,
        hjust = 0.5,
        vjust = 0.5
      ))
    g3 <-      g3 + theme(axis.text.y = element_text(size = 15, angle = 90))
        g3 <-
      g3 + ggtitle(paste(colnames(chimeraPlotGBR)[3],"×",colnames(chimeraPlotGBR)[4],"\nBoxplot:Daily Exchange Rate , Blue Line:PPP\n", subtitle))
    g3 <- g3 + xlab("Year")
    g3 <- g3 + ylab("")
    print(g3)
      })

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
          main = paste(colnames(plotData)[2], "\n", subtitle),
          cex.axis = 1,
          cex.lab = 1,
          cex.main = 1
        )
        abline(h = 0, col = "red")
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
    <li>2016-06-22:ver.1.0.0</li>
    <li>2016-06-29:ver.1.0.1</li>
    <li>2016-07-21:ver.1.0.2</li>
    </ol>"
    HTML(str)
  })
  
  output$gitcode <- renderUI({
    str <- "<hr>
    <b>Code</b><br>
    <ol>
    <li><a href=\"https://github.com/am-consulting/Rscript/tree/master/shinyapps/OECDPPP\" target=\"_blank\">Move to GitHub</a></li>
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