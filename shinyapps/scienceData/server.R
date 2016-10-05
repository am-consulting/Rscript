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
})