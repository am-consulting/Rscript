library(shiny)
library(ggplot2)
library(scales)
library(lubridate)
library(DT)
library(XML)
library(tseries)
library(dplyr)
library(reshape2)
library(xtable)
library(rvest)
library(RCurl)
library(psych)
library(pastecs)
shinyServer(function(input, output)
{
  tabSST <- 0
  makeReactiveBinding('tabSST')   
  observeEvent(input$searchAction_SST, {
    latestDataDownloadTime_SST <<- as.POSIXlt(Sys.time(), "GMT") 
    
    sourceURL <- "http://www.data.jma.go.jp/gmd/cpd/data/elnino/index/dattab.html"
    url_SST <- c(
      "http://www.data.jma.go.jp/gmd/cpd/data/elnino/index/nino3abs.html",
      "http://www.data.jma.go.jp/gmd/cpd/data/elnino/index/ninowabs.html",
      "http://www.data.jma.go.jp/gmd/cpd/data/elnino/index/iobwabs.html"
    )
    datatitle_SST <<- c(
      "NINO.3 Sea Surface Temperature(Celsius)",
      "NINO.WEST Sea Surface Temperature(Celsius)",
      "IOBW Sea Surface Temperature(Celsius)"
    )
    for (rrr in 1:length(url_SST)) {
      Sys.sleep(1)
      source <- read_html(url_SST[rrr])
      tmp01 <- source %>% html_nodes(xpath = '//pre') %>% html_text %>% iconv(from = "UTF-8")
      buf <- strsplit(tmp01, "\n")
      for (fff in 3:length(buf[[1]])) {
        tmp02 <- buf[[1]][fff]
        tmp02 <- gsub("  ", " ", tmp02)
        tmp02 <- strsplit(tmp02, " ")
        tmp03 <- t(tmp02[[1]][-1])
        if (fff == 3) {
          dataset_SST <- as.data.frame(tmp03)
        } else{
          dataset_SST <- rbind(dataset_SST, as.data.frame(tmp03))
        }
      }
      dataset_SST <- data.frame(apply(dataset_SST, 2, as.numeric), stringsAsFactors = F, check.names = F)
      datasetDF <-
        data.frame(
          Date = seq(as.Date(paste(dataset_SST[1, 1], "-1-1", sep = "")), 
                     by = "month", length = nrow(dataset_SST) * 12), value = as.vector(t(dataset_SST)[-1, ]), 
          check.names = F,stringsAsFactors = F)
      datasetDF[datasetDF == 99.9] <- NA
      colnames(datasetDF)[2] <- datatitle_SST[[rrr]]
      assign(paste0('origData_SST_', rrr), datasetDF, envir = .GlobalEnv)
    }
    
    output$region_SST <- renderUI({    
      selectInput("region_SST", label = "Select", datatitle_SST, selectize = F)    
    })
    
    tabSST <<- 1
  })
  
  observe({
    if (tabSST == 0) {return(NULL)} else{
      if (is.null(input$region_SST)) { # stationname_CO2の読み込みを待つために必要
        return(NULL)} else {  
          if (input$region_SST == datatitle_SST[1]) {
            ccc <- 1
            dataset_SST <- origData_SST_1
          } else if (input$region_SST == datatitle_SST[2]) {
            ccc <- 2
            dataset_SST <- origData_SST_2
          } else{
            ccc <- 3
            dataset_SST <- origData_SST_3
          }
          dataset_SST <- na.omit(dataset_SST)
          
          output$datatitle_SST <- renderText({
            paste0(datatitle_SST[ccc], "\nPeriod:", 
                   format(first(dataset_SST$Date), "%Y-%m"), "-", format(last(dataset_SST$Date), "%Y-%m"))
          })

          mainTitle_SST <-
            paste0(datatitle_SST[ccc],"\nPeriod:",format(first(dataset_SST$Date),"%Y-%m"),"-",format(last(dataset_SST$Date),"%Y-%m"))
          
          rp1_SST  <- renderPlot({
            par(mar = c(5, 4, 3, 3))
            plotData <<- na.omit(dataset_SST)
            g1 <- ggplot()
            g1 <- g1 + geom_line(data = plotData, aes(x = Date, y = plotData[, 2]), size = 0.5, col = "black")
            g1 <- g1 + geom_smooth(data = plotData, aes(x = Date, y = plotData[, 2]), method = lm, lwd = 1, col = "red")
            g1 <- g1 + scale_x_date(labels = date_format("%Y-%m"))
            g1 <- g1 + theme(plot.title = element_text(size = 15))
            g1 <- g1 + theme(axis.title.x = element_text(size = 15))
            g1 <- g1 + theme(axis.title.y = element_text(size = 15))
            g1 <- g1 + theme(axis.text.x = element_text(size = 15, angle = 0))
            g1 <- g1 + theme(axis.text.y = element_text(size = 15, angle = 90))
            g1 <- g1 + xlab("")
            g1 <- g1 + ylab(paste(colnames(plotData)[2]))
            lmResult <- lm(plotData[, 2] ~ plotData$Date)
            g1 <- g1 + ggtitle(
              paste(mainTitle_SST, "\nLiner Model Slope:", 
                    lmResult$coefficients[2], "\nUnit Root Test p-value:", adf.test(plotData[, 2])$p.value))
            print(g1)
          })
          output$plot1_SST <- rp1_SST
          
          rp2_SST  <- renderPlot({
            par(mar = c(5, 4, 3, 3))
            seasonData <- data.frame(dataset_SST, month = month.abb[month(dataset_SST[, 1])])
            seasonData$month <- factor(seasonData$month, levels = month.abb)
            seasonData <- na.omit(seasonData)
            g2 <- ggplot(data = seasonData, aes(x = month, y = seasonData[, 2]))
            g2 <- g2 + geom_boxplot(lwd = 1)
            g2 <- g2 + theme(plot.title = element_text(size = 15))
            g2 <- g2 + theme(axis.title.x = element_text(size = 15))
            g2 <- g2 + theme(axis.title.y = element_text(size = 15))
            g2 <- g2 + theme(axis.text.x = element_text(size = 15, angle = 0, hjust = 0.5, vjust = 0.5))
            g2 <- g2 + theme(axis.text.y = element_text(size = 15))
            g2 <- g2 + ggtitle(paste(mainTitle_SST))
            g2 <- g2 + xlab("")
            g2 <- g2 + ylab("")
            print(g2)
          })
          output$plot2_SST <- rp2_SST
          
          rp3_SST  <- renderPlot({
            par(mar = c(5, 4, 3, 3))
            seasonData <- data.frame(dataset_SST, year = year(dataset_SST[, 1]))
            seasonData$year <- factor(seasonData$year)
            seasonData <- na.omit(seasonData)
            g3 <- ggplot(data = seasonData, aes(x = year, y = seasonData[, 2]))
            g3 <- g3 + geom_boxplot(lwd = 1)
            g3 <- g3 + theme(plot.title = element_text(size = 15))
            g3 <- g3 + theme(axis.title.x = element_text(size = 15))
            g3 <- g3 + theme(axis.title.y = element_text(size = 15))
            g3 <- g3 + theme(axis.text.x = element_text(size = 15, angle = 90, hjust = 0.5, vjust = 0.5))
            g3 <- g3 + theme(axis.text.y = element_text(size = 15))
            g3 <- g3 + ggtitle(paste(mainTitle_SST))
            g3 <- g3 + xlab("")
            g3 <- g3 + ylab("")
            print(g3)
          })
          output$plot3_SST <- rp3_SST
          
          rp4_SST  <- renderPlot({
            par(mar = c(5, 4, 3, 3))
            seasonData <- data.frame(dataset_SST, decade = floor(year(dataset_SST[, 1])/10)*10)
            seasonData$decade <- factor(seasonData$decade)
            seasonData <- na.omit(seasonData)
            g4 <- ggplot(data = seasonData, aes(x = decade, y = seasonData[, 2]))
            g4 <- g4 + geom_boxplot(lwd = 1)
            g4 <- g4 + theme(plot.title = element_text(size = 15))
            g4 <- g4 + theme(axis.title.x = element_text(size = 15))
            g4 <- g4 + theme(axis.title.y = element_text(size = 15))
            g4 <- g4 + theme(axis.text.x = element_text(size = 15, angle = 90, hjust = 0.5, vjust = 0.5))
            g4 <- g4 + theme(axis.text.y = element_text(size = 15))
            g4 <- g4 + ggtitle(paste(mainTitle_SST))
            g4 <- g4 + xlab("")
            g4 <- g4 + ylab("")
            print(g4)
          })
          output$plot4_SST <- rp4_SST

          rp5_SST  <- renderPlot({
            x <- dataset_SST[, 2]
            histChart <- hist(x)
            xfit <- seq(min(x), max(x), length=100) 
            yfit <- dnorm(xfit, mean=mean(x), sd=sd(x)) 
            yfit <- yfit*diff(histChart$mids[1:2])*length(x) 
            par(mar = c(3, 5, 3, 1))          
            histChart <- hist(
              x,
              cex.axis = 1.2,
              cex.lab = 1.2,
              cex.main = 1.2,
              main = colnames(dataset_SST)[2],
              xlab = "",
              col = '#ADD8E6',
              ylim = c(0,max(x,yfit))
            )
            lines(xfit, yfit, col="red", lwd=2)  
          })
          output$plot5_SST <- rp5_SST

          Stat_lm<-na.omit(dataset_SST)
          attach(Stat_lm)
          fit <- lm(`NINO.3 Sea Surface Temperature(Celsius)` ~ `Date`)
          output$summary_SST <- renderPrint({summary(Stat_lm[,2])})
          output$summary_fit_SST <- renderPrint({summary(fit)})
          output$confint_SST <- renderPrint({confint(fit, 'Date', level=0.95)})
          output$psych_SST <- renderPrint({psych::describe(Stat_lm[,2,drop=F])})
          # output$pastecs_SST <- renderPrint({pastecs::stat.desc(Stat_lm[,2])})
          output$adf_SST <- renderPrint({adf.test(`NINO.3 Sea Surface Temperature(Celsius)`)})   

          datatableData_SST <- na.omit(dataset_SST)
          datatableData_SST[, 1] <- format(datatableData_SST[, 1], "%Y-%m")
          rdt_SST <- DT::renderDataTable(
            datatableData_SST,
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
              escape = F, # or escape = 'v1'(特定のcolumn) テーブル内でhtmlマークアップする場合に必須 
              filter = 'bottom'
            )
          )
          output$table1_SST <- rdt_SST
        }
    }
  }) 
  
  output$latestDataDownloadTime_SST <- renderText({
    if (tabSST == 1) {
      paste("Data imported time(UTC):" ,
            as.character(latestDataDownloadTime_SST))
    }
  })
  
  output$remarktext_SST <- renderUI({
    str <- "<hr>
    <b>Note</b><br>
    <ol>
    <li><a href=\"http://www.data.jma.go.jp/gmd/cpd/data/elnino/index/dattab.html\" target=\"_blank\">Japan Meteorological Agency</a></li>
    </ol>"
    HTML(str)
  })
  
  output$disclaimer_SST<- renderUI({
    str <- "<hr>
    <b>Operating Company / Disclaimer</b><br>
    <ol>
    <li><a href=\"http://am-consulting.co.jp\" target=\"_blank\">Asset Management Consulting Corporation -  http://am-consulting.co.jp</a></li>
    <li><a href=\"http://am-consulting.co.jp/disclaimer/\" target=\"_blank\">Disclaimer</a>
    </li>
    </ol>"
    HTML(str)
  })  
  
  output$figure01_SST <- renderUI({
    str_SST <-"<div align=\"center\"><a href=\"http://www.data.jma.go.jp/gmd/cpd/elnino/kanshi_joho/fig/c_b_region_hp.png\">
    <img src=\"http://www.data.jma.go.jp/gmd/cpd/elnino/kanshi_joho/fig/c_b_region_hp.png\" alt=\"\" width=\"50%\"></a>
    <br>Figure 1：Source http://www.data.jma.go.jp/gmd/cpd/data/elnino/index/dattab.html</div>"
    HTML(paste(str_SST))
  }) 
  
})