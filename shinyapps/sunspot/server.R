library(shiny)
library(ggplot2)
library(scales)
library(lubridate)
shinyServer(function(input, output, session)
{
  reactiveData <- reactive({
    if (input$region == datatitle[1]) {
      ccc <<-
        1
      lll <-
        1
      rrr <-
        3
    } else if (input$region == datatitle[2]) {
      ccc <<- 2
      lll <- 2
      rrr <- 4
    } else{
      ccc <<- 3
      lll <- 2
      rrr <- 5
    }
    dataset <<- origData[[lll]][, c(1, rrr)]
    output$datatitle <- renderText({
      paste(
        datatitle[ccc],
        "\nPeriod:",
        format(dataset[1, 1], "%Y-%m"),
        "-",
        format(dataset[nrow(dataset), 1], "%Y-%m"),
        sep = ""
      )
    })
    mainTitle <-
      paste(
        datatitle[ccc],
        "\nPeriod:",
        format(dataset[1, 1], "%Y-%m"),
        "-",
        format(dataset[nrow(dataset), 1], "%Y-%m"),
        sep = ""
      )
    
    rp1  <- renderPlot({
      par(mar = c(5, 4, 3, 3))
      g <- ggplot()
      g <-
        g + geom_line(
          data = dataset,
          aes(x = dataset[, 1], y = dataset[, 2]),
          size = 0.5,
          col = "black"
        )
      g <-
        g + geom_smooth(
          data = dataset,
          aes(x = dataset[, 1], y = dataset[, 2]),
          method = loess,
          lwd = 1,
          col = "red"
        )
      g <- g + scale_x_date(labels = date_format("%Y-%m"))
      g <- g + theme(plot.title = element_text(size = 15))
      g <- g + theme(axis.title.x = element_text(size = 15))
      g <- g + theme(axis.title.y = element_text(size = 15))
      g <-
        g + theme(axis.text.x = element_text(size = 15, angle = 0))
      g <-
        g + theme(axis.text.y = element_text(size = 15, angle = 90))
      g <- g + xlab("")
      g <- g + ylab(colnames(dataset)[2])
      g <- g + ggtitle(paste(mainTitle))
      print(g)
    })
    output$plot1 <- rp1
    
    rp2  <- renderPlot({
      par(mar = c(5, 4, 3, 3))
      seasonData <-
        data.frame(dataset, month = month.abb[month(dataset[, 1])])
      seasonData$month <-
        factor(seasonData$month, levels = month.abb)
      seasonData <<- seasonData
      g1 <-
        ggplot(data = seasonData, aes(x = month, y = seasonData[, 2]))
      g1 <- g1 + geom_boxplot(lwd = 1)
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
      g1 <-
        g1 + theme(axis.text.y = element_text(size = 15, angle = 90))
      g1 <- g1 + ggtitle(paste(mainTitle))
      g1 <- g1 + xlab("")
      g1 <- g1 + ylab(colnames(dataset)[2])
      print(g1)
    })
    output$plot2 <- rp2
    
    rp3  <- renderPlot({
      par(mar = c(3, 5, 5, 3))
      result <-
        acf(
          dataset[, 2],
          lag = 240,
          panel.first = grid(
            nx = NULL,
            ny = NULL,
            lty = 2,
            equilogs = F
          ),
          main = mainTitle
        )
      resultDataframe <<-
        data.frame(lag = result$lag, acf = result$acf)
    })
    output$plot3 <- rp3
    
    rp4  <- renderPlot({
      hist(
        dataset[, 2],
        col = "grey",
        breaks = "scott",
        main = mainTitle,
        xlab = colnames(dataset)[2],
        freq = T
      )
    })
    output$plot4 <- rp4
    
    datatableData <- dataset[, c(1, 2)]
    datatableData[, 1] <- format(dataset[, 1], "%Y-%m")
    rdt <- DT::renderDataTable(
      datatableData,
      rownames = FALSE,
      caption = "Table 1: Monthly mean total sunspot number.",
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
    rdtNA <- DT::renderDataTable(
      resultDataframe,
      rownames = FALSE,
      caption = "Table 2: Result of Autocorrelation.",
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
        order = list(list(1, "desc"))
      )
    )
    output$table2 <- rdtNA
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
    <li>2016-06-09:ver.1.0.0</li>
    </ol>"
    HTML(str)
  })
  
  output$datachecktime <- renderText({
    reactiveData()
    paste("Data downloaded time(UTC):" ,
          as.character(downloadTime))
  })
})