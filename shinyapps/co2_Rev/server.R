library(shiny)
library(googleVis)
library(ggplot2)
library(scales)
library(lubridate)
library(DT)

shinyServer(function(input, output)
{
  tabCO2 <- 0
  makeReactiveBinding('tabCO2') 
  
  observeEvent(input$searchAction_CO2, {
    latestDataDownloadTime_CO2 <<- as.POSIXlt(Sys.time(), "GMT")
    
    sourceURL_CO2 <- "\nhttp://ds.data.jma.go.jp/ghg/kanshi/info_co2.html"
    url_CO2 <- c(
      "http://ds.data.jma.go.jp/ghg/kanshi/obs/co2_monthave_ryo.csv",
      "http://ds.data.jma.go.jp/ghg/kanshi/obs/co2_monthave_mnm.csv",
      "http://ds.data.jma.go.jp/ghg/kanshi/obs/co2_monthave_yon.csv"
    )
    datatitle_CO2 <<- c( 
      "Monthly average of CO2 concentration(ppm):Ryōri",
      "Monthly average of CO2 concentration(ppm):Minami-Tori-shima",
      "Monthly average of CO2 concentration(ppm):Yonaguni-jima"
    )
    selectList_CO2 <<- c("Ryori", "Minami-Tori-shima", "Yonaguni-jima")
    for (rrr in 1:length(url_CO2)) {
      tmp_CO2 <-
        read.csv(
          url_CO2[rrr],
          header = T,
          skip = 0,
          stringsAsFactor = F,
          check.names = F
        )
      tmp_CO2 <- tmp_CO2[, -(4:5)]                     #列4と列5 , 単位 ppm
      tmp_CO2[, 3] <- as.numeric(tmp_CO2[, 3])         #ppm列を数値化
      tmp_CO2 <- subset(tmp_CO2, tmp_CO2[, 2] != "NA") #コメント(csv最下から4行分)削除
      tmp_CO2 <- data.frame(Date = as.Date(paste(tmp_CO2[, 1], "-", tmp_CO2[, 2], "-1", sep = "")), co2 = tmp_CO2[, 3])
      colnames(tmp_CO2)[2] <- paste(selectList_CO2[rrr], ".co2(ppm)", sep = "")
      assign(paste0('origData_CO2_',rrr), tmp_CO2, envir = .GlobalEnv)
    }
    
    output$stationname_CO2 <- renderUI({
      selectInput(
        "stationname_CO2",
        label = "Select Station",
        selectList_CO2,
        selectize = F
      )
    })   
    
    stations_CO2 <-
      data.frame(
        lat =  c(39.031333,    24.289418,  24.466518),
        long = c(141.821463,  153.983041, 123.010643),
        info = selectList_CO2
      )
    stations_CO2$latlong <- paste(stations_CO2[, 1], stations_CO2[, 2], sep = ":")
    latlong_CO2 <- paste(stations_CO2[1, 1], stations_CO2[2, 1], sep = ":")
    stations_CO2$info <- paste(stations_CO2$info, "-", stations_CO2$latlong)
    stations_CO2 <<- stations_CO2
    
    tabCO2 <<- 1
  })  
  
  observe({
    if (tabCO2 == 0) {
      return(NULL)
    } else{
      if (is.null(input$stationname_CO2)) { # stationname_CO2の読み込みを待つために必要
        return(NULL)
      } else { 
        if (input$stationname_CO2 == selectList_CO2[1]) {
          obj_CO2 <- 1
        } else if (input$stationname_CO2 == selectList_CO2[2]) {
          obj_CO2 <- 2
        } else{
          obj_CO2 <- 3
        }
        mainTitle_CO2 <<- datatitle_CO2[[obj_CO2]]
        dataset_CO2 <<-  get(paste0('origData_CO2_',obj_CO2))
        colnames(dataset_CO2)[2] <- "value"
        co2data <- co2dataNA <-  get(paste0('origData_CO2_',obj_CO2))
        co2data[, 1] <- format(co2data[, 1], "%Y-%m")
        co2data <- na.omit(co2data)
        co2data <<- co2data
        co2dataNA[, 1] <- format(co2dataNA[, 1], "%Y-%m")
        co2dataNA <- co2dataNA[is.na(co2dataNA[, 2]) == T, ]
        co2dataNA[, 2] <- as.character("Not Available")

        output$table1_CO2 <- DT::renderDataTable(
          co2data,
          rownames = F,
          caption = "Table 1: Data Table of CO2 concentration(ppm). All data without NA.",
          options = list(
            autoWidth = F,
            info = T,
            lengthChange = T,
            ordering = T,
            searching = T,
            scrollX = F,
            lengthMenu = list(c(5, 10, -1), c("5", "10", "All")),
            pageLength = 10,
            orderClasses = T,
            order = list(list(0, "desc"))
          )
        )
        
        # if (nrow(co2dataNA) != 0) {
        #   output$table2_CO2 <- DT::renderDataTable(
        #     co2dataNA,
        #     rownames = F,
        #     caption = "Table 2: Data Table of CO2 concentration(ppm). Only Not Available(NA).",
        #     options = list(
        #       autoWidth = F,
        #       info = T,
        #       lengthChange = T,
        #       ordering = T,
        #       searching = T,
        #       scrollX = F,
        #       lengthMenu = list(c(5, 10, -1), c("5", "10", "All")),
        #       pageLength = 10,
        #       orderClasses = T,
        #       order = list(list(0, "desc"))
        #     )
        #   )
        # }
        
        output$plot1_CO2 <- renderPlot({
          par(mar = c(5, 4, 3, 3))
          g1 <- ggplot(data = dataset_CO2,aes(x = Date, y = value))
          g1 <- g1 + geom_line(size = 1,colour = "blue",alpha = 0.5)
          g1 <- g1 + geom_smooth(aes(group = 1),method = loess,color = "red")
          g1 <- g1 + theme(plot.title   = element_text(size = 15))
          g1 <- g1 + theme(axis.title.x = element_text(size = 15))
          g1 <- g1 + theme(axis.title.y = element_text(size = 15))
          g1 <- g1 + theme(axis.text.x  = element_text(size = 15, angle = 0, hjust = 0.5, vjust = 0.5))
          g1 <- g1 + theme(axis.text.y  = element_text(size = 15))
          g1 <- g1 + ggtitle(paste(mainTitle_CO2))
          g1 <- g1 + scale_x_date(labels = date_format("%Y/%m"))
          g1 <- g1 + xlab("")
          g1 <- g1 + ylab("")
          print(g1)
        })
        
        output$plot2_CO2 <- renderPlot({
          par(mar = c(5, 4, 3, 3))
          seasonData_CO2_mm <- data.frame(dataset_CO2, month = month.abb[month(dataset_CO2[, 1])])
          seasonData_CO2_mm$month <- factor(seasonData_CO2_mm$month, levels = month.abb)
          g2 <- ggplot(data = seasonData_CO2_mm, aes(x = month, y = value))
          g2 <- g2 + geom_boxplot(lwd = 1)
          g2 <- g2 + theme(plot.title = element_text(size = 15))
          g2 <- g2 + theme(axis.title.x = element_text(size = 15))
          g2 <- g2 + theme(axis.title.y = element_text(size = 15))
          g2 <- g2 + theme(axis.text.x = element_text(size = 15, angle = 0, hjust = 0.5, vjust = 0.5))
          g2 <- g2 + theme(axis.text.y = element_text(size = 15))
          g2 <- g2 + ggtitle(paste(mainTitle_CO2))
          g2 <- g2 + xlab("")
          g2 <- g2 + ylab("")
          print(g2)
        })
        
        output$plot3_CO2 <- renderPlot({
          par(mar = c(5, 4, 3, 3))
          seasonData_CO2_yy <- data.frame(dataset_CO2, year = year(dataset_CO2[, 1]))
          seasonData_CO2_yy$year <- factor(seasonData_CO2_yy$year)
          g3 <- ggplot(data = seasonData_CO2_yy, aes(x = year, y = value))
          g3 <- g3 + geom_boxplot(lwd = 1)
          g3 <- g3 + theme(plot.title = element_text(size = 15))
          g3 <- g3 + theme(axis.title.x = element_text(size = 15))
          g3 <- g3 + theme(axis.title.y = element_text(size = 15))
          g3 <- g3 + theme(axis.text.x = element_text(size = 15, angle = 90, hjust = 0.5, vjust = 0.5))
          g3 <- g3 + theme(axis.text.y = element_text(size = 15))
          g3 <- g3 + ggtitle(paste(mainTitle_CO2))
          g3 <- g3 + xlab("")
          g3 <- g3 + ylab("")
          print(g3)
        })
        
        output$plot4_CO2 <- renderPlot({
          par(mar = c(5, 4, 3, 3))
          seasonData_CO2_decade <- data.frame(dataset_CO2, decade = paste0(floor(year(dataset_CO2[, 1])/10)*10,'~'))
          seasonData_CO2_decade$decade <- factor(seasonData_CO2_decade$decade)
          g4 <- ggplot(data = seasonData_CO2_decade, aes(x = decade, y = value))
          g4 <- g4 + geom_boxplot(lwd = 1)
          g4 <- g4 + theme(plot.title = element_text(size = 15))
          g4 <- g4 + theme(axis.title.x = element_text(size = 15))
          g4 <- g4 + theme(axis.title.y = element_text(size = 15))
          g4 <- g4 + theme(axis.text.x = element_text(size = 15, angle = 90, hjust = 0.5, vjust = 0.5))
          g4 <- g4 + theme(axis.text.y = element_text(size = 15))
          g4 <- g4 + ggtitle(paste(mainTitle_CO2))
          g4 <- g4 + xlab("")
          g4 <- g4 + ylab("")
          print(g4)
        })
        
        output$gvis_CO2 <- renderGvis({
          commonHeight_CO2 <- 500
          gvisMap(
            stations_CO2[obj_CO2,],
            "latlong",
            "info",
            options = list(
              showTip = T,
              showLine = T,
              enableScrollWheel = T,
              mapType = 'satellite',
              useMapTypeControl = T,
              height = commonHeight_CO2,
              zoomLevel = 3
            )
          )
        })
      }
    }
  })

  output$latestDataDownloadTime_CO2 <- renderText({
    if (tabCO2 == 1) {
      paste("Data imported time(UTC):" ,
            as.character(latestDataDownloadTime_CO2))
    }
  })
  
  output$remarktext_CO2 <- renderUI({
    str <- "<hr>
    <b>Note</b><br>
    <ol>
    <li><a href=\"http://ds.data.jma.go.jp/ghg/kanshi/obs/co2_monthave_ryo.html\" target=\"_blank\">Japan Meteorological Agency</a></li>
    </ol>"
    HTML(str)
  })
  
  output$disclaimer_CO2<- renderUI({
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