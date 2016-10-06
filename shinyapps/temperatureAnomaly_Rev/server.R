library(shiny)
library(XML)
library(RCurl)
library(ggplot2)
library(scales)
library(lubridate)
library(DT)
shinyServer(function(input, output, session)
{
  tabMMT <- 0
  makeReactiveBinding('tabMMT') 
  
  observeEvent(input$searchAction_MMT, {
    latestDataDownloadTime_MMT <<- as.POSIXlt(Sys.time(), "GMT")
    
    sourceURL_MMT <- c(
      "http://www.data.jma.go.jp/cpdinfo/temp/list/mon_wld.html",
      "http://www.data.jma.go.jp/cpdinfo/temp/list/mon_jpn.html"
    )
    datatitle_MMT <<- c(
      "Monthly Mean Temperature Anomaly:World(Celsius,Base Line:30 years mean from Y1981 to Y2010)",
      "Monthly Mean Temperature Anomaly:Japan(Celsius,Base Line:30 years mean from Y1981 to Y2010)"
    )
    for (iii in 1:length(sourceURL_MMT)) {
      Sys.sleep(1)
      buf_MMT <-
        readHTMLTable(
          doc = sourceURL_MMT[iii],
          header = T,
          trim = T,
          stringsAsFactors = F,
          as.data.frame = T,
          which = 1
        )
      buf_MMT[, -1] <- sapply(buf_MMT[, -1], function(x) {gsub("[^0-9|-|+|.]*?", "", x)})
      buf_MMT <- data.frame(Year = buf_MMT[, 1], sapply(buf_MMT[, -1], as.numeric), check.names = F,stringsAsFactors = F)
      assign(paste0('origData_MMT_',iii), buf_MMT, envir = .GlobalEnv)
    }  
    
    output$region_MMT <- renderUI({
      selectInput("region_MMT", label = "Select Region",   datatitle_MMT  ,     selectize = F)   
    })      
    
    tabMMT <<- 1
  })  
  
  observe({
    if (tabMMT == 0) {return(NULL)} else{
      if (is.null(input$region_MMT)) { # stationname_CO2の読み込みを待つために必要
        return(NULL)} else { 
        if (input$region_MMT == datatitle_MMT[1]) {
          iii <- 1
        } else{
          iii <- 2
        }
        output$datatitle_MMT <- renderText({datatitle_MMT[iii]})        

        obj_MMT<-get(paste0('origData_MMT_',iii))
        start <- as.numeric(gsub("年", "", obj_MMT[1, 1]))
        tmpValue <- as.numeric(as.vector(t(obj_MMT[, -1])))
        tmpDate <- seq(as.Date(paste(start, "/1/1", sep = "")), by = "month", length.out = length(tmpValue))
        dataset_MMT <- tdData_MMT <- tdDataNA_MMT <- data.frame(Date = tmpDate, value = tmpValue)
        colnames(dataset_MMT)[2] <- "value"
        tdData_MMT[, 1] <- format(tdData_MMT[, 1], "%Y-%m")
        tdData_MMT <- na.omit(tdData_MMT)
        tdDataNA_MMT[, 1] <- format(tdDataNA_MMT[, 1], "%Y-%m")
        tdDataNA_MMT <- tdDataNA_MMT[is.na(tdDataNA_MMT[, 2]) == T, ]
        tdDataNA_MMT[, 2] <- as.character("Not Available")
        chartTitle <-
          paste(datatitle_MMT[iii],'\n',
                format(dataset_MMT[1, 1], "%Y-%m"), "-", format(dataset_MMT[nrow(dataset_MMT), 1], "%Y-%m"),"\nSource:JMA")  
        colnames(tdData_MMT)[2] <- colnames(tdDataNA_MMT)[2] <- datatitle_MMT[iii]

        rdt_MMT <- DT::renderDataTable(
          tdData_MMT,
          rownames = F,
          caption = "Table 1: without NA.",
          options = list(
            autoWidth = F,
            info = T,
            lengthChange = T,
            ordering = T,
            searching = T,
            scrollX = F,
            lengthMenu = list(c(5, 10, -1), c("5", "10", "All")),
            pageLength = 5,
            orderClasses = T,
            order = list(list(0, "desc")),
            columnDefs = list(list(width = '15%', targets = 0))
          )
        )
        output$table1_MMT <- rdt_MMT
        
        if (nrow(tdDataNA_MMT) != 0) {
          rdtNA_MMT <- DT::renderDataTable(
            tdDataNA_MMT,
            rownames = F,
            caption = "Table 2: Only NA.",
            options = list(
              autoWidth = F,
              info = T,
              lengthChange = T,
              ordering = T,
              searching = T,
              scrollX = F,
              lengthMenu = list(c(5, 10, -1), c("5", "10", "All")),
              pageLength = 5,
              orderClasses = T,
              order = list(list(0, "desc")),
              columnDefs = list(list(width = '15%', targets = 0))
            )
          )
          output$table1NA_MMT <- rdtNA_MMT
        }
        
        rp1_MMT  <- renderPlot({
          par(mar = c(5, 4, 3, 3))
          g1 <- ggplot(data = dataset_MMT, aes(x = Date, y = value))
          g1 <- g1 + geom_line(size = 0.5, colour = "blue", alpha = 0.5)
          g1 <- g1 + geom_smooth(aes(group = 1), method = loess, color = "red",se = T)
          g1 <- g1 + theme(plot.title = element_text(size = 15))
          g1 <- g1 + theme(axis.title.x = element_text(size = 15))
          g1 <- g1 + theme(axis.title.y = element_text(size = 15))
          g1 <- g1 + theme(axis.text.x = element_text(size = 15, angle = 0, hjust = 0.5, vjust = 0.5))
          g1 <- g1 + theme(axis.text.y = element_text(size = 15))
          g1 <- g1 + scale_x_date(labels = date_format("%Y-%m"))
          g1 <- g1 + xlab("")
          g1 <- g1 + ylab("")
          g1 <- g1 + ggtitle(chartTitle)
          print(g1)
        })
        output$plot1_MMT <- rp1_MMT
        
        rp2_MMT <- renderPlot({
          par(mar = c(5, 4, 3, 3))
          seasonData <- data.frame(dataset_MMT, month = month.abb[month(dataset_MMT[, 1])])
          seasonData$month <- factor(seasonData$month, levels = month.abb)
          g2 <- ggplot(data = seasonData, aes(x = month, y = value))
          g2 <- g2 + geom_boxplot(lwd = 1)
          g2 <- g2 + theme(plot.title = element_text(size = 15))
          g2 <- g2 + theme(axis.title.x = element_text(size = 15))
          g2 <- g2 + theme(axis.title.y = element_text(size = 15))
          g2 <- g2 + theme(axis.text.x = element_text(size = 15, angle = 0, hjust = 0.5, vjust = 0.5))
          g2 <- g2 + theme(axis.text.y = element_text(size = 15))
          g2 <- g2 + xlab("")
          g2 <- g2 + ylab("")
          g2 <- g2 + ggtitle(chartTitle)
          print(g2)
        })
        output$plot2_MMT <- rp2_MMT        
        
        rp3_MMT <- renderPlot({
          par(mar = c(5, 4, 3, 3))
          seasonData <- data.frame(dataset_MMT, year = year(dataset_MMT[, 1]))
          seasonData$year <- factor(seasonData$year)
          g3 <- ggplot(data = seasonData, aes(x = year, y = value))
          g3 <- g3 + geom_boxplot(lwd = 1)
          g3 <- g3 + theme(plot.title = element_text(size = 15))
          g3 <- g3 + theme(axis.title.x = element_text(size = 15))
          g3 <- g3 + theme(axis.title.y = element_text(size = 15))
          g3 <- g3 + theme(axis.text.x = element_text(size = 13, angle = 90, hjust = 0.5, vjust = 0.5))
          g3 <- g3 + theme(axis.text.y = element_text(size = 15))
          g3 <- g3 + xlab("")
          g3 <- g3 + ylab("")
          g3 <- g3 + ggtitle(chartTitle)
          print(g3)
        })
        output$plot3_MMT <- rp3_MMT        
        
        rp4_MMT <- renderPlot({
          par(mar = c(5, 4, 3, 3))
          seasonData <- data.frame(dataset_MMT, decade = paste0(floor(year(dataset_MMT[, 1])/10)*10,'~'))
          seasonData$decade <- factor(seasonData$decade)
          g4 <- ggplot(data = seasonData, aes(x = decade, y = value))
          g4 <- g4 + geom_boxplot(lwd = 1)
          g4 <- g4 + theme(plot.title = element_text(size = 15))
          g4 <- g4 + theme(axis.title.x = element_text(size = 15))
          g4 <- g4 + theme(axis.title.y = element_text(size = 15))
          g4 <- g4 + theme(axis.text.x = element_text(size = 15, angle = 90, hjust = 0.5, vjust = 0.5))
          g4 <- g4 + theme(axis.text.y = element_text(size = 15))
          g4 <- g4 + xlab("")
          g4 <- g4 + ylab("")
          g4 <- g4 + ggtitle(chartTitle)
          print(g4)
        })
        output$plot4_MMT <- rp4_MMT 
        
        rp5_MMT <- renderPlot({
          decadeData <- data.frame(dataset_MMT, decade = paste0(floor(year(dataset_MMT[, 1])/10)*10,'~'))
          decadeData <- na.omit(decadeData)
          decadeMean <- by(data = decadeData[,2], INDICES = decadeData$decade, FUN = mean)
          decadeSD <- by(data = decadeData[,2], INDICES = decadeData$decade, FUN = sd)
          decadeCI <- by(data = decadeData[,2], 
                         INDICES = decadeData$decade, function(x){qt(0.975,df=length(x)-1)*sd(x)/sqrt(length(x))})
          maxY <- max(decadeMean+decadeSD)
          minY <- min(decadeMean-decadeSD)
          xRange <- seq(1,length(unique(decadeData$decade)))
          par(mar=c(4,3,5,2))
          plot(
            decadeMean,
            type = 'o',
            col='black',
            ylim = c(minY,maxY),
            lwd = 1,  
            xlab = '' ,
            ylab = '' ,
            panel.first = grid(nx = NULL, ny = NULL, lty = 2, equilogs = T) ,
            xaxt = 'n',
            main = paste0(chartTitle,'\nMean with Standard Deviation'),
            cex.axis = 1.2,
            cex.lab = 1.2,
            cex.main = 1.2
          )        
          arrows(xRange, decadeMean + decadeSD, xRange, decadeMean - decadeSD, angle = 90, length = 0.05, col = 'red')
          arrows(xRange, decadeMean - decadeSD, xRange, decadeMean + decadeSD, angle = 90, length = 0.05, col = 'red')
          arrows(xRange, decadeMean + decadeCI, xRange, decadeMean - decadeCI, angle = 90, length = 0.05, col = 'blue')
          arrows(xRange, decadeMean - decadeCI, xRange, decadeMean + decadeCI, angle = 90, length = 0.05, col = 'blue')
          graphics::legend(
            'topleft',
            col=c('black','red','blue'),
            lty=1,
            legend=c('Mean','Mean ± SD',paste0('Mean ± CI(95%). df = n-1','')),
            cex=1.3,bty='n')
          axis(side = 1, at = xRange, labels = unique(decadeData$decade), las=2,cex=1.2)
        })
        output$plot5_MMT <- rp5_MMT 
      }  
    }
  })
  
  output$latestDataDownloadTime_MMT <- renderText({
    if (tabMMT == 1) {
      paste("Data imported time(UTC):" ,
            as.character(latestDataDownloadTime_MMT))
    }
  })
  
  output$remarktext_MMT <- renderUI({
    str <- "<hr>
    <b>Note</b><br>
    <ol>
    <li><a href=\"http://www.data.jma.go.jp/cpdinfo/temp/index.html\" target=\"_blank\">Japan Meteorological Agency</a></li>
    </ol>"
    HTML(str)
  })
  
  output$disclaimer_MMT<- renderUI({
    str <- "<hr>
    <b>Operating Company / Disclaimer</b><br>
    <ol>
    <li><a href=\"http://am-consulting.co.jp\" target=\"_blank\">Asset Management Consulting Corporation -  http://am-consulting.co.jp</a></li>
    <li><a href=\"http://am-consulting.co.jp/disclaimer/\" target=\"_blank\">Disclaimer</a>
    </li>
    </ol>"
    HTML(str)
  })  
  
})