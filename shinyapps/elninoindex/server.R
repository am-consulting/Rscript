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

shinyServer(function(input, output, session)
{
  reactiveData <- reactive({
    if (input$region == datatitle[1]) {
      ccc <<- 1
    } else if (input$region == datatitle[2]) {
      ccc <<- 2
    } else{
      ccc <<- 3
    }
    dataset <<- origData[[ccc]]
    output$datatitle <- renderText({
      paste(
        datatitle[ccc],
        "\nPeriod:",
        format(first(dataset$Date), "%Y-%m"),
        "-",
        format(last(dataset$Date), "%Y-%m"),
        sep = ""
      )
    })
    
    mainTitle <-
      paste(
        datatitle[ccc],
        "\nPeriod:",
        format(first(dataset$Date), "%Y-%m"),
        "-",
        format(last(dataset$Date), "%Y-%m"),
        sep = ""
      )
    
    rp1  <- renderPlot({
      par(mar = c(5, 4, 3, 3))
      plotData <<- na.omit(dataset)
      g <- ggplot()
      g <-
        g + geom_line(
          data = plotData,
          aes(x = Date, y = plotData[, 2]),
          size = 0.5,
          col = "black"
        )
      g <-
        g + geom_smooth(
          data = plotData,
          aes(x = Date, y = plotData[, 2]),
          method = lm,
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
      g <- g + ylab(paste(colnames(plotData)[2]))
      lmResult <- lm(plotData[, 2] ~ plotData$Date)
      g <- g + ggtitle(
        paste(
          mainTitle,
          "\nLiner Model Slope:",
          lmResult$coefficients[2],
          "\nUnit Root Test p-value:",
          adf.test(plotData[, 2])$p.value
        )
      )
      print(g)
    })
    output$plot1 <- rp1
    
    rp2  <- renderPlot({
      par(mar = c(5, 4, 3, 3))
      seasonData <-
        data.frame(dataset, month = month.abb[month(dataset[, 1])])
      seasonData$month <-
        factor(seasonData$month, levels = month.abb)
      seasonData <- na.omit(seasonData)
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
    output$plot2 <- rp2
    
    datatableData <- na.omit(dataset)
    datatableData[, 1] <- format(datatableData[, 1], "%Y-%m")
    rdt <- DT::renderDataTable(
      datatableData,
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
    
    output$figure01 <- renderUI({
      str <-
        "<div align=\"center\"><a href=\"http://www.data.jma.go.jp/gmd/cpd/elnino/kanshi_joho/fig/c_b_region_hp.png\">
      <img src=\"http://www.data.jma.go.jp/gmd/cpd/elnino/kanshi_joho/fig/c_b_region_hp.png\" alt=\"\" width=\"50%\"></a><br>Figure 1：Source http://www.data.jma.go.jp/gmd/cpd/data/elnino/index/dattab.html</div>"
      HTML(paste(str))
    })
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
    <li>2016-06-17:ver.1.0.1</li>
    </ol>"
    HTML(str)
  })
  
  output$gitcode <- renderUI({
    str <- "<hr>
    <b>Code</b><br>
    <ol>
    <li><a href=\"https://github.com/am-consulting/Rscript/tree/master/shinyapps/elninoindex\" target=\"_blank\">Move to GitHub</a></li>
    </ol>"
    HTML(str)
  })
  
})