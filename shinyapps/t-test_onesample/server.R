library(shiny)
library(dplyr)
library(nortest)
library(zoo)
options(shiny.maxRequestSize = 0.5 * 1024 ^ 2)
options(download.file.method="libcurl")
shinyServer(function(input, output , session) {
  uploadFile <- reactive({
    inFile <- input$file
    if (is.null(inFile)) {
      return(NULL)
    } else{
      origData <<- read.csv(
        inFile$datapath,
        header = input$header,
        sep = input$sep,
        quote = input$quote,
        check.names = input$chkname,
        stringsAsFactors = F,
        fileEncoding = "cp932"
      )
    }
  })
  
  output$targetData <- renderUI({
    uploadFile()
    if (is.null(input$file))
      return(NULL)
    selectInput("targetData",
                "Select as Sample",
                colnames(origData),
                selected = colnames(origData)[1])
  })
  
  output$selectrow <- renderUI({
    uploadFile()
    if (is.null(input$file))
      return(NULL)
    sliderInput(
      "selectedRow",
      label = "Select Rows",
      min = 1,
      max = nrow(origData),
      value =  c(1, nrow(origData)),
      step = 1
    )
  })
  
  trueMean <- reactive({
    if (is.null(input$targetData))
      return(NULL)
    targetcol <-
      which(colnames(origData) == input$targetData)
    dataset <- origData[,targetcol,drop=F]
    dataset <-
      data.frame(ID = seq(1, nrow(dataset)),
                 dataset,
                 check.names = F)
    dataset <-
      dataset %>% filter(input$selectedRow[1] <= dataset$ID,
                         dataset$ID <= input$selectedRow[2])
    dataset <<- dataset
    output$mu <- renderUI({
      textInput("truemean",
                label = "True Mean",
                value = mean(dataset[, 2]))
    })
    altList <- c("two.sided", "less", "greater")
    output$alternative <- renderUI({
      selectInput(
        "alternative",
        label = "Alternative",
        choices = altList ,
        selected = altList[1]
      )
    })
  })
  
  resultOutput <- reactive({
    if (is.null(input$alternative))
      return(NULL)
    summaryDF <- data.frame()
    x <- dataset[, 2]
    iii <- 1
    #unbiased variance
    summaryDF[1, iii] <- var(x)
    #unbiased deviation
    summaryDF[2, iii] <- sd(x)
    #sample variance
    summaryDF[3, iii] <-
      var(x) * (length(x) - 1) / length(x)
    #sample standard deviation
    summaryDF[4, iii] <- sqrt(summaryDF[3, iii])
    summaryDF[5, iii] <- mean(x)
    summaryDF[6, iii] <- median(x)
    summaryDF[7, iii] <- max(x)
    summaryDF[8, iii] <- min(x)
    summaryDF[9, iii] <- ad.test(x)$p.value
    summaryDF[9, iii + 1] <- paste(ad.test(x)$method, ": p-value")
    summaryItem <-
      c(
        "Unbiased Variance",
        "Unbiased Deviation",
        "Sample Variance",
        "Sample Standard Deviation",
        "Mean",
        "Median",
        "Max",
        "Min",
        "Normality"
      )
    summaryDF <-
      data.frame(ID = seq(1, nrow(summaryDF)), Item = summaryItem, summaryDF)
    colnames(summaryDF) <- c("ID", "Item", "Value", "Remark")

    output$scatter <- renderPlot({
      par(mar = c(4, 4, 2, 1))
      collist <- c("red")
      plot(
        x,
        x,
        type = "p",
        col = "black",
        xlab = colnames(dataset)[2] ,
        ylab = colnames(dataset)[2] ,
        panel.first = grid(
          nx = NULL,
          ny = NULL,
          lty = 2,
          equilogs = T
        ) ,
        main = "Scatter Plot",
        cex.axis = 1,
        cex.lab = 1,
        cex.main = 1,
        asp = 1
      )
    })
    
    output$hist <- renderPlot({
      par(mar = c(5, 4, 3, 3))
      hist(
        x,
        breaks = "sturges",
        cex.axis = 1,
        cex.lab = 1,
        cex.main = 1,
        main = colnames(dataset)[2],
        xlab = "",
        col = "#DCDCDC"
      )
    })
    
    output$qq <- renderPlot({
      qqnorm(x,
             panel.first = grid(
               nx = NULL,
               ny = NULL,
               lty = 2,
               equilogs = T
             ))
      qqline(x, col = "red")
    })
    
    if (input$truemean == "") {
      muvalue <- 0
    } else{
      muvalue <- as.numeric(input$truemean)
    }
    resultttest <-
      t.test(x, mu = muvalue, alternative = input$alternative) # "two.sided", "less", "greater"
    attributes(resultttest)
    ttestDF <-
      data.frame(
        Item = c(
          "t",
          "df",
          "p-value",
          "CI=95%.L",
          "CI=95%.U",
          "mean of x",
          "null value"
        ),
        Value = c(
          resultttest$statistic,
          resultttest$parameter,
          resultttest$p.value,
          resultttest$conf.int[1],
          resultttest$conf.int[2],
          resultttest$estimate,
          resultttest$null.value
        )
      )
    ttestDF <- data.frame(ID = seq(1, nrow(ttestDF)), ttestDF)
    
    output$ttestDF <- DT::renderDataTable(
      ttestDF,
      rownames = F,
      caption = paste(
        "Table 2: ",
        resultttest$method,
        ":",
        resultttest$alternative
      ),
      options = list(
        autoWidth = T,
        info = F,
        lengthChange = F,
        ordering = F,
        searching = F,
        scrollX = T,
        paging = F,
        orderClasses = TRUE,
        order = list(list(0, "asc"))
      )
    )
    
    output$summaryDF <- DT::renderDataTable(
      summaryDF,
      rownames = F,
      caption = paste("Table 1: Statistical Summary"),
      options = list(
        autoWidth = T,
        info = F,
        lengthChange = F,
        ordering = F,
        searching = F,
        scrollX = T,
        paging = F,
        orderClasses = TRUE,
        order = list(list(0, "asc"))
      )
    )

  output$DF <- DT::renderDataTable(
    dataset,
    rownames = F,
    caption = paste("Table 3: Tested Dataset"),
    options = list(
      autoWidth = T,
      info = T,
      lengthChange = T,
      ordering = T,
      searching = F,
      scrollX = T,
      lengthMenu = list(c(10, -1, 1), c("10", "All", "1")),
      orderClasses = TRUE,
      order = list(list(0, "asc"))
    )
  )
  })
  
  output$completiontime <- renderText({
    if (is.null(input$targetData))
      return(NULL)
    trueMean()
    resultOutput()
    paste("Completion(UTC):" ,
          as.character(as.POSIXlt(Sys.time(), "GMT")))
  })
  
  output$remarktext <- renderUI({
    str <- "<hr>
    <b>Remarks</b><br>
    <ol>
    <li><a href=\"http://www.saecanet.com\" target=\"_blank\">SaECaNet</a></li>
    <li>Other apps <a href=\"http://webapps.saecanet.com\" target=\"_blank\">SaECaNet - Web Applications</a></li>
    <li><a href=\"http://am-consulting.co.jp\" target=\"_blank\">Asset Management Consulting Corporation / アセット･マネジメント･コンサルティング株式会社</a></li>
    <li><a href=\"http://www.saecanet.com/subfolder/disclaimer.html\" target=\"_blank\">Disclaimer</a></li>
    </ol>"
    HTML(str)
  })
  
  output$history <- renderUI({
    str <- "<hr>
    <b>History</b><br>
    <ol>
    <li>2016-06-15:ver.1.0.0</li>
    <li>2016-06-17:ver.1.0.1</li>
    <li>2016-07-12:ver.1.0.2</li>
    </ol>"
    HTML(str)
  })

  output$gitcode <- renderUI({
    str <- "<hr>
    <b>Code</b><br>
    <ol>
    <li><a href=\"https://github.com/am-consulting/Rscript/tree/master/shinyapps/t-test_onesample\" target=\"_blank\">Move to GitHub</a></li>
    </ol>"
    HTML(str)
  })

  output$linkList <- renderUI({
    str <- linkList
    HTML(str)
  })
  
})