library(shiny)
library(googleVis)
library(ggplot2)
library(scales)
library(lubridate)
library(DT)

shinyServer(function(input, output, session)
{
  reactiveData <- reactive({
    if (input$stationname == selectList[1]) {
      obj <- 1
    } else if (input$stationname == selectList[2]) {
      obj <- 2
    } else{
      obj <- 3
    }
    obj <<- obj
    mainTitle <<- datatitle[[obj]]
    dataset <<- origData[[obj]]
    colnames(dataset)[2] <- "value"
    co2data <- co2dataNA <-  origData[[obj]]
    co2data[, 1] <- format(co2data[, 1], "%Y-%m")
    co2data <- na.omit(co2data)
    co2data <<- co2data
    co2dataNA[, 1] <- format(co2dataNA[, 1], "%Y-%m")
    co2dataNA <- co2dataNA[is.na(co2dataNA[, 2]) == T, ]
    co2dataNA[, 2] <- as.character("Not Available")
    co2dataNA <<- co2dataNA
    
    output$datachecktime <- renderText({
      paste("Data downloaded time(UTC):" ,
            as.character(downloadTime))
    })
    
    output$table1 <- DT::renderDataTable(
      co2data,
      rownames = FALSE,
      caption = "Table 1: Data Table of CO2 concentration(ppm). All data without NA.",
      options = list(
        autoWidth = T,
        info = T,
        lengthChange = T,
        ordering = T,
        searching = T,
        scrollX = T,
        lengthMenu = list(c(5,
                            10, -1), c("5", "10", "All")),
        pageLength = 10,
        orderClasses = TRUE,
        order = list(list(0, "desc"))
      )
    )
    
    if (nrow(co2dataNA) != 0) {
      output$table2 <- DT::renderDataTable(
        co2dataNA,
        rownames = FALSE,
        caption = "Table 2: Data Table of CO2 concentration(ppm). Only Not Available(NA).",
        options = list(
          autoWidth = T,
          info = T,
          lengthChange = T,
          ordering = T,
          searching = T,
          scrollX = T,
          lengthMenu = list(c(5,
                              10, -1), c("5", "10", "All")),
          pageLength = 10,
          orderClasses = TRUE,
          order = list(list(0, "desc"))
        )
      )
    }
    
    output$plot1 <- renderPlot({
      par(mar = c(5, 4, 3, 3))
      g <- ggplot()
      g <-
        g + geom_line(
          data = dataset,
          aes(x = Date, y = value),
          size = 1,
          colour = "blue",
          alpha = 0.5
        )
      g <-
        g + geom_smooth(
          data = dataset,
          aes(x = Date, y = value, group = 1),
          method = loess,
          color = "red"
        )
      g <- g + theme(plot.title = element_text(size = 15))
      g <- g + theme(axis.title.x = element_text(size = 15))
      g <- g + theme(axis.title.y = element_text(size = 15))
      g <-
        g + theme(axis.text.x = element_text(
          size = 15,
          angle = 0,
          hjust = 0.5,
          vjust = 0.5
        ))
      g <- g + theme(axis.text.y = element_text(size = 15))
      g <- g + ggtitle(paste(mainTitle))
      g <- g + scale_x_date(labels = date_format("%Y/%m"))
      g <- g + xlab("")
      g <- g + ylab("")
      print(g)
    })
    
    output$plot2 <- renderPlot({
      par(mar = c(5, 4, 3, 3))
      seasonData <-
        data.frame(dataset, month = month.abb[month(dataset[, 1])])
      seasonData$month <-
        factor(seasonData$month, levels = month.abb)
      g1 <- ggplot(data = seasonData, aes(x = month, y = value))
      g1 <- g1 + geom_boxplot(lwd = 1)
      g1 <- g1 + theme(plot.title = element_text(size = 15))
      g1 <- g1 + theme(axis.title.x = element_text(size = 15))
      g1 <- g1 + theme(axis.title.y = element_text(size = 15))
      g1 <-
        g1 + theme(axis.text.x = element_text(
          size = 15,
          angle = 0,
          hjust = 0.5,
          vjust = 0.5
        ))
      g1 <- g1 + theme(axis.text.y = element_text(size = 15))
      g1 <- g1 + ggtitle(paste(mainTitle))
      g1 <- g1 + xlab("")
      g1 <- g1 + ylab("")
      print(g1)
    })
  })
  
  output$gvis <- renderGvis({
    reactiveData()
    commonHeight <- 500
    gvisMap(
      stations[obj,],
      "latlong",
      "info",
      options = list(
        showTip = TRUE,
        showLine = TRUE,
        enableScrollWheel = TRUE,
        mapType = 'satellite',
        useMapTypeControl = TRUE,
        height = commonHeight,
        zoomLevel = 3
      )
    )
  })
  
  output$remarktext <- renderUI({
    str <- "<hr>
    <b>Remarks</b><br>
    <ol>
    <li>Other apps <a href=\"http://www.saecanet.com\" target=\"_blank\">SaECaNet</a></li>
    </ol>"
    HTML(str)
  })
  
  output$history <- renderUI({
    str <- "<hr>
    <b>History</b><br>
    <ol>
    <li>2016-06-08:ver.1.0.0</li>
    </ol>"
    HTML(str)
  })
  
})