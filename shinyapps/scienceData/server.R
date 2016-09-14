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
})