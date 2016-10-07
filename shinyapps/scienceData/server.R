library(shiny)
shinyServer(function(input, output)
{
  tabEarthquake <- 0
  makeReactiveBinding('tabEarthquake')
  
  observeEvent(input$searchActionEarthquake, {
    latestDataDownloadTimeEarthquake <<- as.POSIXlt(Sys.time(), "GMT")
    Sys.sleep(1)
    borderM <- 5
    
    data.file <- "all_month.csv"
    dataset <-
      read.csv(
        paste0("http://earthquake.usgs.gov/earthquakes/feed/v1.0/summary/",data.file),
        header = T,
        as.is = T,
        skip = 0,
        stringsAsFactor = F,
        na.strings = c(""),
        check.names = F
      )
    dataset$GMT <- as.POSIXlt(sub("Z", "", sub("T", " ", dataset$time)), "GMT")
    dataset$JST <- as.POSIXct(sub("Z", "", sub("T", " ", dataset$time))) + 3600 * 9 #UTC+9
    dataset$latlong <- paste(dataset$latitude, dataset$longitude, sep = ":")
    dataset$info <- paste(  "Time(UTC)=",  dataset$GMT,
                            ", Time(UTC+9)=",  dataset$JST,
                            ", Depth(km)=",  dataset$depth,
                            ", Mag=",  dataset$mag,
                            ", MagType=",  dataset$magType,
                            ", Place=",  dataset$place  ,
                            sep = ""
    )
    dataset24hour <<-  subset(dataset, (latestDataDownloadTimeEarthquake - 60 * 60 * 24) <= dataset$GMT)
    datasetM <<- subset(dataset, borderM <= dataset$mag)
    datasetJapan <<- dataset[grep("Japan", dataset$place),]
    # for(latestJapan in 5:30){
    #   datasetJapanLatest <<- subset(datasetJapan, (latestDataDownloadTimeEarthquake - 60 * 60 * 24 * latestJapan) <= datasetJapan$GMT)
    #   if(nrow(datasetJapanLatest)!=0){break}
    # }
    latestJapan <- 3
    datasetJapanLatest <<- head(datasetJapan, latestJapan)
    titlegvis24hour <<-
      paste("Earthquakes in a 24-hour period.\nPeriod(UTC): ", latestDataDownloadTimeEarthquake - 60 * 60 * 24, "-", latestDataDownloadTimeEarthquake ,
            ". n=", nrow(dataset24hour))
    titlegvisM <<-
      paste("Earthquakes over Magnitude ", borderM, " in past 30 days. n=", nrow(datasetM))
    titlegvisJapan <<-
      paste("Earthquakes around Japan in past 30 days. n=", nrow(datasetJapan))
    # titlegvisJapanLatest <<-
    #   paste("Earthquakes around Japan in past" ,latestJapan, "days. n=", nrow(datasetJapanLatest))
    titlegvisJapanLatest <<-
      paste("Last",latestJapan,"Earthquakes around Japan.")
    selectList <<-
      c(
        "Earthquakes in a 24-hour period.",
        paste("Earthquakes over Magnitude ", borderM , " in past 30 days."),
        "Earthquakes around Japan in past 30 days.",
        # paste("Earthquakes around Japan in past",latestJapan, "days.")
        paste("Last",latestJapan,"Earthquakes around Japan.")
      )
    
    tabEarthquake <<- 1
    output$maptypeEarthquake <- renderUI({    
      selectInput("maptypeEarthquake", label = "Select map", selectList, selectize = FALSE)
    })
  })
  
  observe({
    if (tabEarthquake == 0) {
      return(NULL)
    } else{
      if(is.null(input$maptypeEarthquake)){return(NULL)}else{
        if (input$maptypeEarthquake ==  selectList[1]) {
          selectedData <- dataset24hour
          maptitle <- titlegvis24hour
        } else if (input$maptypeEarthquake == selectList[2]) {
          selectedData <- datasetM
          maptitle <- titlegvisM
        } else if (input$maptypeEarthquake == selectList[3]) {
          selectedData <- datasetJapan
          maptitle <- titlegvisJapan
        } else if (input$maptypeEarthquake == selectList[4]) {
          selectedData <- datasetJapanLatest
          maptitle <- titlegvisJapanLatest      
        }  
        colnames(selectedData)[1]<-paste0(colnames(selectedData)[1],'(UTC)')
        
        output$titlegvisEarthquake <- renderText({
          maptitle
        })
        
        output$dtTableEarthquake <-
          DT::renderDataTable(
            selectedData[, c(1, 2, 3, 4, 5, 6, 14)],
            rownames = FALSE,
            caption = paste0("Table 1: List of Earthquakes. Unit of depth:kilometers. ",maptitle),
            options = list(
              autoWidth = T,
              info = T,
              lengthChange = T,
              ordering = T,
              searching = T,
              scrollX = T,
              lengthMenu = list(c(10,
                                  15, -1), c("10", "15", "All")),
              p6ageLength = 10,
              orderClasses = TRUE,
              order = list(list(0, "desc"))
            )
          )
        
        output$plot1Earthquake <- renderPlot({
          par(mar = c(5, 4, 5, 3))
          hist(
            selectedData[, 2],
            breaks = "sturges",
            cex.axis = 1,
            cex.lab = 1,
            cex.main = 1.5,
            main = paste(maptitle , "\n" , colnames(selectedData)[2]),
            xlab = "",
            col = "#808000"
          )
        })
        
        output$plot2Earthquake <- renderPlot({
          par(mar = c(5, 4, 5, 3))
          hist(
            selectedData[, 3],
            breaks = "sturges",
            cex.axis = 1,
            cex.lab = 1,
            cex.main = 1.5,
            main = paste(maptitle , "\n" , colnames(selectedData)[3]),
            xlab = "",
            col = "#808000"
          )
        })
        
        output$plot3Earthquake <- renderPlot({
          par(mar = c(5, 4, 5, 3))
          hist(
            selectedData[, 4],
            breaks = "sturges",
            cex.axis = 1,
            cex.lab = 1,
            cex.main = 1.5,
            main = paste(maptitle , "\n" , colnames(selectedData)[4]),
            xlab = "",
            col = "#808000"
          )
        })
        
        output$plot4Earthquake <- renderPlot({
          par(mar = c(5, 4, 5, 3))
          hist(
            selectedData[, 5],
            breaks = "sturges",
            cex.axis = 1,
            cex.lab = 1,
            cex.main = 1.5,
            main = paste(maptitle , "\n" , colnames(selectedData)[5]),
            xlab = "",
            col = "#808000"
          )
        })
        
        output$gvisEarthquake <- renderGvis({
          commonHeight <- 700
          gvisMap(
            selectedData,
            "latlong",
            "info",
            options = list(
              showTip = TRUE,
              showLine = TRUE,
              enableScrollWheel = TRUE,
              mapType = 'terrain',
              useMapTypeControl = TRUE,
              height = commonHeight
            )
          )
        })
      }
    }
  })
  
  output$remarktextEarthquake <- renderUI({
    str <- "<hr>
    <b>Remarks</b><br>
    <ol>
    <li>Caution: Differences in magnitude type are unconsidered in magnitude histgram.</li>
    <li>The histogram cells are right-closed (left open) intervals.</li>
    </ol>"
    HTML(str)
  })
  
  output$latestDataDownloadTimeEarthquake <- renderText({
    if (tabEarthquake == 1) {
      paste("Data imported time(UTC):" ,
            as.character(latestDataDownloadTimeEarthquake))
    }
  })
  
  output$disclaimerEarthquake <- renderUI({
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
  # SeaIceIndex  
  tabSeaIceIndex <- 0
  makeReactiveBinding('tabSeaIceIndex') 
  
  observeEvent(input$searchAction_SeaIceIndex, {
    latestDataDownloadTime_SeaIceIndex <<- as.POSIXlt(Sys.time(), "GMT")
    datasetSeaIceIndex <- list()
    datatitleSeaIceIndex <<- c(
      "Sea Ice Index Data:North",
      "Sea Ice Index Data:South",
      "Sea Ice Index Data:Total(North+South)"
    )
    dataUrlSeaIceIndex <- c(
      "ftp://sidads.colorado.edu/DATASETS/NOAA/G02135/north/daily/data/NH_seaice_extent_final_v2.csv",
      "ftp://sidads.colorado.edu/DATASETS/NOAA/G02135/north/daily/data/NH_seaice_extent_nrt_v2.csv",
      "ftp://sidads.colorado.edu/DATASETS/NOAA/G02135/south/daily/data/SH_seaice_extent_final_v2.csv",
      "ftp://sidads.colorado.edu/DATASETS/NOAA/G02135/south/daily/data/SH_seaice_extent_nrt_v2.csv"
    )
    output$regionSeaIceIndex <- renderUI({
      selectInput("regionSeaIceIndex",
                  label = "Select",
                  datatitleSeaIceIndex,
                  selectize = F)
    })
    chkNH <- 0
    chkSH <- 0
    for (ddd in 1:length(dataUrlSeaIceIndex)) {
      Sys.sleep(1)
      buf_SeaIceIndex <- read.csv(dataUrlSeaIceIndex[ddd], header = T, skip = 0, stringsAsFactor = F, na.strings = c(""),check.names = F)
      tmp_SeaIceIndex <- apply(buf_SeaIceIndex[-1, -6], 2, as.numeric)
      if (regexpr("NH_seaice", dataUrlSeaIceIndex[ddd]) != -1) {
        if (chkNH == 0) {
          ppp <- 1
          datasetSeaIceIndex[[ppp]] <- tmp_SeaIceIndex
          chkNH <- 1
        } else{
          datasetSeaIceIndex[[ppp]] <- rbind(datasetSeaIceIndex[[ppp]], tmp_SeaIceIndex)
        }
      } else{
        if (chkSH == 0) {
          ppp <- 2
          datasetSeaIceIndex[[ppp]] <- tmp_SeaIceIndex
          chkSH <- 1
        } else{
          datasetSeaIceIndex[[ppp]] <- rbind(datasetSeaIceIndex[[ppp]], tmp_SeaIceIndex)
        }
      }
    }
    head(datasetSeaIceIndex[[1]])
    head(datasetSeaIceIndex[[2]])
    for (ppp in 1:2) {
      Date <- as.Date(
        paste0(datasetSeaIceIndex[[ppp]][, 1], "-", datasetSeaIceIndex[[ppp]][, 2], "-", datasetSeaIceIndex[[ppp]][, 3]))
      if (ppp == 1) {
        colnames(datasetSeaIceIndex[[ppp]]) <- paste("NH_", colnames(datasetSeaIceIndex[[ppp]]), sep = "")
      } else{
        colnames(datasetSeaIceIndex[[ppp]]) <- paste("SH_", colnames(datasetSeaIceIndex[[ppp]]), sep = "")
      }
      datasetSeaIceIndex[[ppp]] <- data.frame(datasetSeaIceIndex[[ppp]], Date, check.names = F, stringsAsFactors = F)
    }
    head(datasetSeaIceIndex[[1]])
    head(datasetSeaIceIndex[[2]])
    datasetSeaIceIndex[[3]] <- merge(datasetSeaIceIndex[[1]], datasetSeaIceIndex[[2]],by = 'Date')
    datasetSeaIceIndex[[3]]$TotalExtent <- as.numeric(datasetSeaIceIndex[[3]][, 5]) + as.numeric(datasetSeaIceIndex[[3]][, 10])
    head(datasetSeaIceIndex[[3]])
    assign('datasetSeaIceIndex01',datasetSeaIceIndex[[1]],envir = .GlobalEnv)
    assign('datasetSeaIceIndex02',datasetSeaIceIndex[[2]],envir = .GlobalEnv)
    assign('datasetSeaIceIndex03',datasetSeaIceIndex[[3]],envir = .GlobalEnv)
    buf_SeaIceIndex <<- buf_SeaIceIndex
    tabSeaIceIndex <<- 1
  })
  
  observe({
    if (tabSeaIceIndex == 0) {
      return(NULL)
    } else{
      if (is.null(input$regionSeaIceIndex)) { # regionSeaIceIndexの読み込みを待つために必要。挟まないとエラーが出る。
        return(NULL)
      } else{
        if (input$regionSeaIceIndex == datatitleSeaIceIndex[1]) {
          ccc <- 1
          plotData_SeaIceIndex <- get('datasetSeaIceIndex01')
        } else if (input$regionSeaIceIndex == datatitleSeaIceIndex[2]) {
          ccc <- 2
          plotData_SeaIceIndex <- get('datasetSeaIceIndex02')
        } else{
          ccc <- 3
          plotData_SeaIceIndex <- get('datasetSeaIceIndex03')
        }
        title_SeaIceIndex <-
          paste0(datatitleSeaIceIndex[ccc], "\nPeriod:", first(plotData_SeaIceIndex$Date), "-",last(plotData_SeaIceIndex$Date))
        output$datatitleSeaIceIndex <- renderText({title_SeaIceIndex})
        if (ccc <= 2) {
          valueColumn <- 4  # NH_Extent , SH_Extent
        } else{
          valueColumn <- 12 # TotalExtent
        }
        mainTitle_SeaIceIndex <- title_SeaIceIndex
        rp1  <- renderPlot({
          par(mar = c(5, 4, 3, 3))
          g <- ggplot()
          g <- g + geom_line(
            data = plotData_SeaIceIndex,
            aes(x = Date, y = plotData_SeaIceIndex[, valueColumn]),
            size = 0.5,
            col = "black")
          g <- g + geom_smooth(
            data = plotData_SeaIceIndex,
            aes(x = Date, y = plotData_SeaIceIndex[, valueColumn]),
            method = lm,
            lwd = 1,
            col = "red")
          g <- g + scale_x_date(labels = date_format("%Y-%m-%d"))
          g <- g + theme(plot.title = element_text(size = 15))
          g <- g + theme(axis.title.x = element_text(size = 15))
          g <- g + theme(axis.title.y = element_text(size = 15))
          g <- g + theme(axis.text.x = element_text(size = 15, angle = 0))
          g <- g + theme(axis.text.y = element_text(size = 15, angle = 90))
          g <- g + xlab("")
          g <- g + ylab(paste(colnames(buf_SeaIceIndex)[4], "(", unlist(buf_SeaIceIndex[1,][4]), ")"))
          lmResult <- lm(plotData_SeaIceIndex[, valueColumn] ~ plotData_SeaIceIndex$Date)
          g <- g + ggtitle(
            paste(mainTitle_SeaIceIndex, "\nLiner Model Slope:",
                  lmResult$coefficients[2], "\nUnit Root Test p-value:",
                  adf.test(plotData_SeaIceIndex[, valueColumn])$p.value)
          )
          print(g)
        })
        output$plot1_SeaIceIndex <- rp1
        
        rp2  <- renderPlot({
          par(mar = c(5, 4, 3, 3))
          seasonData_SeaIceIndex <- data.frame(plotData_SeaIceIndex, month = month.abb[month(plotData_SeaIceIndex$Date)])
          seasonData_SeaIceIndex$month <- factor(seasonData_SeaIceIndex$month, levels = month.abb)
          g1 <- ggplot(data = seasonData_SeaIceIndex, aes(x = month, y = seasonData_SeaIceIndex[, valueColumn]))
          g1 <- g1 + geom_boxplot(lwd = 1)
          g1 <- g1 + theme(plot.title = element_text(size = 15))
          g1 <- g1 + theme(axis.title.x = element_text(size = 15))
          g1 <- g1 + theme(axis.title.y = element_text(size = 15))
          g1 <- g1 + theme(axis.text.x = element_text(size = 15, angle = 0, hjust = 0.5, vjust = 0.5))
          g1 <- g1 + theme(axis.text.y = element_text(size = 15))
          g1 <- g1 + ggtitle(paste(mainTitle_SeaIceIndex))
          g1 <- g1 + xlab("")
          g1 <- g1 + ylab(paste(colnames(buf_SeaIceIndex)[4], "(", unlist(buf_SeaIceIndex[1,][4]), ")"))
          print(g1)
        })
        output$plot2_SeaIceIndex <- rp2
        
        rp3  <- renderPlot({
          par(mar = c(3, 5, 5, 3))
          acf(plotData_SeaIceIndex[, valueColumn],
              lag = 1000,
              panel.first = grid(nx = NULL, ny = NULL, lty = 2, equilogs = F),
              main = mainTitle_SeaIceIndex)
        })
        output$plot3_SeaIceIndex <- rp3
        
        rp4  <- renderPlot({
          plotData_SeaIceIndex$yearMonth <-
            as.Date(paste0(year(plotData_SeaIceIndex$Date), "-", month(plotData_SeaIceIndex$Date), "-1"))
          functionList <- c("mean", "median", "max", "min", "sd")
          par(mfrow = c(2, 3), ps = 18, mar = c(5, 5, 8, 3), cex.main = 1.0, cex.lab = 1.0, cex.axis = 1.0)
          for (iii in 1:length(functionList)) {
            tmp_SeaIceIndex <-
              aggregate(
                x   = list(value = plotData_SeaIceIndex[, valueColumn]),
                by  = list(Date = plotData_SeaIceIndex$yearMonth),
                FUN = get(functionList[iii])
              )
            histTitle <-
              paste0(mainTitle_SeaIceIndex, "\n", 
                     format(first(tmp_SeaIceIndex[, 1]), "%Y/%m"), "-", format(last(tmp_SeaIceIndex[, 1]), "%Y/%m"))
            hist(
              tmp_SeaIceIndex[, 2],
              col = "grey",
              breaks = "scott",
              main = paste0(histTitle, "\naggregated by Year & Month\n", functionList[iii]),
              xlab = unlist(buf_SeaIceIndex[1,][4]),
              freq = T
            )
          }
        }, height = 650)
        output$plot4_SeaIceIndex <- rp4    
        
        datatableData_SeaIceIndex0 <- datasetSeaIceIndex03[, c(1, 5, 6, 10, 11, 12)]
        datatableData_SeaIceIndex <- tail(datatableData_SeaIceIndex0, 100)
        colnames(datatableData_SeaIceIndex)[-1] <- paste0(colnames(datatableData_SeaIceIndex)[-1] , "(", unlist(buf_SeaIceIndex[1,][4]), ")")
        rdt <- DT::renderDataTable(
          datatableData_SeaIceIndex,
          rownames = FALSE,
          caption = "Table :NH,SH and Total. Last 100 dates.",
          options = list(
            autoWidth = F,
            info = T,
            lengthChange = T,
            ordering = T,
            searching = T,
            scrollX = F,
            lengthMenu = list(c(5, 10, -1), c("5", "10", "All")),
            pageLength = 5,
            orderClasses = TRUE,
            order = list(list(0, "desc"))
          )
        )
        output$table1_SeaIceIndex <- rdt
        
        datatableData_SeaIceIndex0 <- datatableData_SeaIceIndex0[, c(1, 2, 4, 6)]
        colnames(datatableData_SeaIceIndex0)[-1] <- paste0(colnames(datatableData_SeaIceIndex0)[-1],'-',"(", unlist(buf_SeaIceIndex[1,][4]), ")")
        output$title01_SeaIceIndex <-   renderText({paste0('Fundamental Statistic:', 
                                                           head(datatableData_SeaIceIndex0[,1],1),'~',tail(datatableData_SeaIceIndex0[,1],1))})      
        output$summary01_SeaIceIndex <- renderPrint({summary(datatableData_SeaIceIndex0[,-1])})
        output$psych01_SeaIceIndex <-   renderPrint({psych::describe(datatableData_SeaIceIndex0[,-1])})
        output$pastecs01_SeaIceIndex <- renderPrint({pastecs::stat.desc(datatableData_SeaIceIndex0[,-1])})
      }
    }
  })
  
  output$latestDataDownloadTime_SeaIceIndex <- renderText({
    if (tabSeaIceIndex == 1) {
      paste("Data imported time(UTC):" ,
            as.character(latestDataDownloadTime_SeaIceIndex))
    }
  })
  
  output$remarktext_SeaIceIndex <- renderUI({
    str <- "
    <hr>
    <b>Note</b><br>
    <ol>
    <li><a href=\"https://nsidc.org/data/seaice_index/archives.html\" target=\"_blank\">Sea Ice Index Archives</a></li>
    <li><a href=\"http://nsidc.org/data/docs/noaa/g02135_seaice_index/#jul-2016\" target=\"_blank\">July 2016: Sea Ice Index Updated to Version 2</a></li>
    </ol>"
    HTML(str)
  })
  
  output$disclaimer_SeaIceIndex<- renderUI({
    str <- "
    <hr>
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
  # SeaIceIndex  
  # Sunspot  
  tabSunspot <- 0
  makeReactiveBinding('tabSunspot') 
  
  observeEvent(input$searchAction_Sunspot, {
    latestDataDownloadTime_Sunspot <<- as.POSIXlt(Sys.time(), "GMT")
    
    script <-
      getURL(
        "https://raw.githubusercontent.com/am-consulting/Rscript/master/Rscript_SunspotNumber.r",
        ssl.verifypeer = F
      )
    eval(parse(text = script))
    
    datatitle_Sunspot <<- c(
      "Monthly mean total sunspot number",
      "North monthly mean sunspot number",
      "South monthly mean sunspot number")
    
    output$region_Sunspot <- renderUI({
      selectInput("region_Sunspot" , label = "Select" , datatitle_Sunspot , selectize = F)    
    })    
    
    tabSunspot <<- 1
  }) 
  
  observe({
    if (tabSunspot == 0) {
      return(NULL)
    } else{
      if (is.null(input$region_Sunspot)) { # region_Sunspotの読み込みを待つために必要
        return(NULL)
      } else {  
        if (input$region_Sunspot == datatitle_Sunspot[1]) {
          ccc <- 1
          dataset_Sunspot<-origData_sunspot01[, c(1, 3)]
        } else if (input$region_Sunspot == datatitle_Sunspot[2]) {
          ccc <- 2
          dataset_Sunspot<-origData_sunspot02[, c(1, 4)]
        } else{
          ccc <- 3
          dataset_Sunspot<-origData_sunspot02[, c(1, 5)]
        }
        
        output$datatitle_Sunspot <- renderText({
          paste(
            datatitle_Sunspot[ccc],
            "\nPeriod:",
            format(dataset_Sunspot[1, 1], "%Y-%m"),
            "-",
            format(dataset_Sunspot[nrow(dataset_Sunspot), 1], "%Y-%m"),
            sep = ""
          )
        })
        
        mainTitle_Sunspot <-
          paste0(datatitle_Sunspot[ccc], 
                 "\nPeriod:", 
                 format(dataset_Sunspot[1, 1], "%Y-%m"), "-", format(dataset_Sunspot[nrow(dataset_Sunspot), 1], "%Y-%m"))
        
        rp1_Sunspot  <- renderPlot({
          par(mar = c(5, 4, 3, 3))
          g <- ggplot(data = dataset_Sunspot, aes(x = dataset_Sunspot[, 1], y = dataset_Sunspot[, 2]))
          g <- g + geom_line(size = 0.5, col = "black")
          g <- g + geom_smooth(method = loess, lwd = 1, col = "red")
          g <- g + scale_x_date(labels = date_format("%Y-%m"))
          g <- g + theme(plot.title   = element_text(size = 15))
          g <- g + theme(axis.title.x = element_text(size = 15))
          g <- g + theme(axis.title.y = element_text(size = 15))
          g <- g + theme(axis.text.x  = element_text(size = 15, angle = 0))
          g <- g + theme(axis.text.y  = element_text(size = 15, angle = 90))
          g <- g + xlab("")
          g <- g + ylab(colnames(dataset_Sunspot)[2])
          g <- g + ggtitle(paste(mainTitle_Sunspot))
          print(g)
        })
        output$plot1_Sunspot <- rp1_Sunspot
        
        rp2_Sunspot  <- renderPlot({
          par(mar = c(5, 4, 3, 3))
          seasonData_Sunspot <- data.frame(dataset_Sunspot, month = month.abb[month(dataset_Sunspot[, 1])])
          seasonData_Sunspot$month <- factor(seasonData_Sunspot$month, levels = month.abb)
          #          seasonData_Sunspot <<- seasonData_Sunspot
          g1 <- ggplot(data = seasonData_Sunspot, aes(x = month, y = seasonData_Sunspot[, 2]))
          g1 <- g1 + geom_boxplot(lwd = 1)
          g1 <- g1 + theme(plot.title   = element_text(size = 15))
          g1 <- g1 + theme(axis.title.x = element_text(size = 15))
          g1 <- g1 + theme(axis.title.y = element_text(size = 15))
          g1 <- g1 + theme(axis.text.x  = element_text(size = 15, angle = 90, hjust = 0.5, vjust = 0.5))
          g1 <- g1 + theme(axis.text.y  = element_text(size = 15, angle = 90))
          g1 <- g1 + ggtitle(paste(mainTitle_Sunspot))
          g1 <- g1 + xlab("")
          g1 <- g1 + ylab(colnames(dataset_Sunspot)[2])
          print(g1)
        })
        output$plot2_Sunspot <- rp2_Sunspot
        
        rp3_Sunspot  <- renderPlot({
          par(mar = c(3, 5, 5, 3))
          result_Sunspot <-
            acf(dataset_Sunspot[, 2], lag = 240, panel.first = grid(nx = NULL, ny = NULL, lty = 2, equilogs = F), main = mainTitle_Sunspot)
          resultDataframe_Sunspot <<- data.frame(lag = result_Sunspot$lag, acf = result_Sunspot$acf)
        })
        output$plot3_Sunspot <- rp3_Sunspot
        
        rp4_Sunspot  <- renderPlot({
          hist(
            dataset_Sunspot[, 2],
            col = "grey",
            breaks = "scott",
            main = paste0(mainTitle_Sunspot,' : right-closed left open intervals'),
            xlab = colnames(dataset_Sunspot)[2],
            freq = T
          )
        })
        output$plot4_Sunspot <- rp4_Sunspot
        
        datatableData_Sunspot <- dataset_Sunspot[, c(1, 2)]
        datatableData_Sunspot[, 1] <- format(dataset_Sunspot[, 1], "%Y-%m")
        rdt_Sunspot <- DT::renderDataTable(
          datatableData_Sunspot,
          rownames = F,
          caption = "Table 1: Monthly mean total sunspot number.",
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
            order = list(list(0, "desc"))
          )
        )
        output$table1_Sunspot <- rdt_Sunspot
        
        rdtNA_Sunspot <- DT::renderDataTable(
          resultDataframe_Sunspot,
          rownames = F,
          caption = "Table 2: Result of Autocorrelation.",
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
            order = list(list(1, "desc"))
          )
        )
        output$table2_Sunspot <- rdtNA_Sunspot
        
        stat01_Sunspot <- origData_sunspot01[,c(1,3)]
        output$summary01_Sunspot <- renderPrint({summary(stat01_Sunspot)})
        output$psych01_Sunspot <- renderPrint({psych::describe(stat01_Sunspot[,2,drop=F])})
        output$pastecs01_Sunspot <- renderPrint({pastecs::stat.desc(stat01_Sunspot[,2,drop=F])})        
        
      }
    }
  })
  
  output$latestDataDownloadTime_Sunspot <- renderText({
    if (tabSunspot == 1) {
      paste("Data imported time(UTC):" ,
            as.character(latestDataDownloadTime_Sunspot))
    }
  })
  
  output$remarktext_Sunspot <- renderUI({
    str <- "<hr>
    <b>Note</b><br>
    <ol>
    <li><a href=\"http://www.sidc.be/silso/home\" target=\"_blank\">Sunspot Index and Long-term Solar Observations</a></li>
    </ol>"
    HTML(str)
  })
  
  output$disclaimer_Sunspot<- renderUI({
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
  # Sunspot  
  # Carbon Dioxide Concentration  
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
  # Carbon Dioxide Concentration  
  # Monthly Mean Temperature Anomaly
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

  # Monthly Mean Temperature Anomaly
  # Sea Surface Temperature  
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
              ylim = c(0,max(x,yfit)*1.01)
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
    str_SST <- "<div align=\"center\"><a href=\"http://www.data.jma.go.jp/gmd/cpd/elnino/kanshi_joho/fig/c_b_region_hp.png\"><img src=\"http://www.data.jma.go.jp/gmd/cpd/elnino/kanshi_joho/fig/c_b_region_hp.png\" alt=\"\" width=\"50%\"></a><br>Figure 1 Source http://www.data.jma.go.jp/gmd/cpd/data/elnino/index/dattab.html</div>"
    HTML(str_SST)
  })
  # Sea Surface Temperature  
})