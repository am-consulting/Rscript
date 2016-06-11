library(shiny)
library(ggplot2)
library(scales)
library(lubridate)
library(DT)
shinyServer(function(input, output, session)
{
  reactiveData <- reactive({
    if (input$region == datatitle[1]) {
      iii <- 1
    } else{
      iii <- 2
    }
    
    output$datatitle <- renderText({
      paste(datatitle[iii])
    })
    
    mainTitle <<- datatitle[iii]
    start <- as.numeric(gsub("年", "", origData[[iii]][1, 1]))
    tmpValue <- as.numeric(as.vector(t(origData[[iii]][, -1])))
    tmpDate <-
      seq(as.Date(paste(start, "/1/1", sep = "")),
          by = "month",
          length.out = length(tmpValue))
    dataset <- tdData <-
      tdDataNA <- data.frame(Date = tmpDate, value = tmpValue)
    colnames(dataset)[2] <- "value"
    tdData[, 1] <- format(tdData[, 1], "%Y-%m")
    tdData <- na.omit(tdData)
    tdDataNA[, 1] <- format(tdDataNA[, 1], "%Y-%m")
    tdDataNA <- tdDataNA[is.na(tdDataNA[, 2]) == T, ]
    tdDataNA[, 2] <- as.character("Not Available")
    chartTitle <-
      paste(format(dataset[1, 1], "%Y-%m"),
            "-",
            format(dataset[nrow(dataset), 1], "%Y-%m"),
            "\nSource:JMA")
    
    rdt <- DT::renderDataTable(
      tdData,
      rownames = FALSE,
      caption = "Table 1: without NA.",
      options = list(
        autoWidth = T,
        info = T,
        lengthChange = T,
        ordering = T,
        searching = T,
        scrollX = T,
        lengthMenu = list(c(5, 10, -1), c("5", "10", "All")),
        pageLength = 5,
        orderClasses = TRUE,
        order = list(list(0, "desc"))
      )
    )
    output$table1 <- rdt
    
    if (nrow(tdDataNA) != 0) {
      rdtNA <- DT::renderDataTable(
        tdDataNA,
        rownames = FALSE,
        caption = "Table 2: Only NA.",
        options = list(
          autoWidth = T,
          info = T,
          lengthChange = T,
          ordering = T,
          searching = T,
          scrollX = T,
          lengthMenu = list(c(5, 10, -1), c("5", "10", "All")),
          pageLength = 5,
          orderClasses = TRUE,
          order = list(list(0, "desc"))
        )
      )
      output$table1NA <- rdtNA
    }
    
    rp1  <- renderPlot({
      par(mar = c(5, 4, 3, 3))
      g <- ggplot()
      g <-
        g + geom_line(
          data = dataset,
          aes(x = Date, y = value),
          size = 0.5,
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
      g <- g + scale_x_date(labels = date_format("%Y-%m"))
      g <- g + xlab("")
      g <- g + ylab("")
      g <- g + ggtitle(chartTitle)
      print(g)
    })
    output$plot1 <- rp1
    
    rp2 <- renderPlot({
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
      g1 <- g1 + xlab("")
      g1 <- g1 + ylab("")
      g1 <- g1 + ggtitle(chartTitle)
      print(g1)
    })
    output$plot2 <- rp2
  })
  
  output$datachecktime <- renderText({
    reactiveData()
    paste("Data downloaded time(UTC):" ,
          as.character(downloadTime))
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