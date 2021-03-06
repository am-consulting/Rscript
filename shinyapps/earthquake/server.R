library(shiny)
library(googleVis)
library(DT)
shinyServer(function(input, output, session)
{
  reactiveData <- reactive({
    if (input$maptype ==  selectList[1]) {
      selectedData <- dataset24hour
      maptitle <- titlegvis24hour
    } else if (input$maptype == selectList[2]) {
      selectedData <- datasetM
      maptitle <- titlegvisM
    } else{
      selectedData <- datasetJapan
      maptitle <- titlegvisJapan
    }
    selectedData <<- selectedData
    maptitle <<- maptitle
    
    output$titlegvis <- renderText({
      maptitle
    })
    
    output$datachecktime <- renderText({
      paste("Data downloaded time(UTC):" ,
            as.character(downloadTime))
    })
    
    output$dtTable <-
      DT::renderDataTable(
        selectedData[, c(1, 2, 3, 4, 5, 6, 14)],
        rownames = FALSE,
        caption = "Table 1: List of Earthquakes. Unit of depth:kilometers",
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
    
    output$plot1 <- renderPlot({
      par(mar = c(5, 4, 3, 3))
      hist(
        selectedData[, 2],
        breaks = "sturges",
        cex.axis = 1,
        cex.lab = 1,
        cex.main = 1,
        main = paste(maptitle , "\n" , colnames(selectedData)[2]),
        xlab = "",
        col = "#DCDCDC"
      )
    })
    
    output$plot2 <- renderPlot({
      par(mar = c(5, 4, 3, 3))
      hist(
        selectedData[, 3],
        breaks = "sturges",
        cex.axis = 1,
        cex.lab = 1,
        cex.main = 1,
        main = paste(maptitle , "\n" , colnames(selectedData)[3]),
        xlab = "",
        col = "#DCDCDC"
      )
    })
    
    output$plot3 <- renderPlot({
      par(mar = c(5, 4, 3, 3))
      hist(
        selectedData[, 4],
        breaks = "sturges",
        cex.axis = 1,
        cex.lab = 1,
        cex.main = 1,
        main = paste(maptitle , "\n" , colnames(selectedData)[4]),
        xlab = "",
        col = "#DCDCDC"
      )
    })
    
    output$plot4 <- renderPlot({
      par(mar = c(5, 4, 3, 3))
      hist(
        selectedData[, 5],
        breaks = "sturges",
        cex.axis = 1,
        cex.lab = 1,
        cex.main = 1,
        main = paste(maptitle , "\n" , colnames(selectedData)[5]),
        xlab = "",
        col = "#DCDCDC"
      )
    })
  })
  
  output$gvis <- renderGvis({
    reactiveData()
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
  
  output$remarktext <- renderUI({
    str <- "<hr>
    <b>Remarks</b><br>
    <ol>
    <li><a href=\"http://www.saecanet.com\" target=\"_blank\">SaECaNet</a></li>
    <li>Other apps <a href=\"http://webapps.saecanet.com\" target=\"_blank\">SaECaNet - Web Applications</a></li>
    <li><a href=\"http://am-consulting.co.jp\" target=\"_blank\">Asset Management Consulting Corporation / アセット･マネジメント･コンサルティング株式会社</a></li>
    <li><a href=\"http://www.saecanet.com/subfolder/disclaimer.html\" target=\"_blank\">Disclaimer</a></li>
    <li><a href=\"http://earthquake.usgs.gov/earthquakes/feed/v1.0/csv.php\" target=\"_blank\">Raw Data Source</a></li>
    </ol>"
    HTML(str)
  })
  
  output$history <- renderUI({
    str <- "<hr>
    <b>History</b><br>
    <ol>
    <li>2016-06-08:ver.1.0.0</li>
    <li>2016-06-17:ver.1.0.1</li>
    <li>2016-07-06:ver.1.0.2</li>
    </ol>"
    HTML(str)
  })
  
  output$gitcode <- renderUI({
    str <- "<hr>
    <b>Code</b><br>
    <ol>
    <li><a href=\"https://github.com/am-consulting/Rscript/tree/master/shinyapps/earthquake\" target=\"_blank\">Move to GitHub</a></li>
    </ol>"
    HTML(str)
  })
  
  output$linkList <- renderUI({
    str <- linkList
    HTML(str)
  })

})