library(shiny)
library(tseries)
library(urca)
library(vars)
library(dplyr)
library(forecast)
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
  
  output$datax <- renderUI({
    uploadFile()
    if (is.null(input$file))
      return(NULL)
    selectInput("dataX",
                "Select as Data x",
                colnames(origData),
                selected = colnames(origData)[1])
  })
  
  output$datay <- renderUI({
    uploadFile()
    if (is.null(input$file))
      return(NULL)
    selectInput("dataY",
                "Select as Data y",
                colnames(origData),
                selected = colnames(origData)[2])
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
  
  output$selectlag <- renderUI({
    uploadFile()
    if (is.null(input$file))
      return(NULL)
    sliderInput("lag",
                "Lag",
                min = 0,
                max = 20,
                value = 0)
  })
  
  resultOutput <- reactive({
    if (is.null(input$dataX) | is.null(input$dataY))
      return(NULL)
    colx <-        which(colnames(origData) == input$dataX)
    coly <-        which(colnames(origData) == input$dataY)
    dataset <-
      data.frame(ID = seq(1, nrow(origData)),
                 origData[, c(colx, coly)],
                 check.names = F)
    dataset <-
      dataset %>% filter(input$selectedRow[1] <= dataset[, 1],
                         dataset[, 1] <= input$selectedRow[2])
    if (input$lag != 0) {
      tmp <-
        data.frame(diff(as.matrix(dataset), lag = input$lag), check.names = F)
      dataset <-
        data.frame(ID = seq(1, nrow(tmp)),
                   (tmp / head(dataset, nrow(tmp)))[, -1] * 100,
                   check.names = F)
    }
    
    bufDatasetColnames<-colnames(dataset)
    colnames(dataset)[2:3]<-c("x","y")
    summaryDF <- data.frame()
    for (iii in 1:2) {
      #unbiased variance
      summaryDF[1, iii] <- var(dataset[, iii + 1])
      #unbiased deviation
      summaryDF[2, iii] <- sd(dataset[, iii + 1])
      #sample variance
      summaryDF[3, iii] <-
        var(dataset[, iii + 1]) * (length(dataset[, iii + 1]) - 1) / length(dataset[, iii + 1])
      #sample standard deviation
      summaryDF[4, iii] <- sqrt(summaryDF[3, iii])
      summaryDF[5, iii] <- mean(dataset[, iii + 1])
      summaryDF[6, iii] <- median(dataset[, iii + 1])
      summaryDF[7, iii] <- max(dataset[, iii + 1])
      summaryDF[8, iii] <- min(dataset[, iii + 1])
      adfResult <-    adf.test(dataset[, iii + 1])
      summaryDF[9, iii] <- adfResult$p.value
      summaryDF[10, iii] <- adfResult$parameter
      summaryDF[11, iii] <- paste(dataset[1,1],"-",dataset[nrow(dataset),1])
    }
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
        "ADF Test p-value",
        "ADF Test Lag order",
        "Data ID"
      )
    colnames(summaryDF) <- bufDatasetColnames[-1]
    summaryDF <-
      data.frame(ID = seq(1, nrow(summaryDF)), Item = summaryItem, summaryDF)
    #without unit root
    aicResult <<- VARselect(dataset[, -1])$selection[1]
    varResult <- VAR(dataset[, -1], p = aicResult)
    #with unit root
    coTest <-
      ca.jo(
        dataset[, -1],
        type = "eigen",
        ecdet = "none",
        spec = "longrun",
        K = max(aicResult, 2)
      )
    coTest@cval
    coTest@teststat
    vecmr <- length(coTest@teststat) - 1#3系列以上に拡張する場合は要注意
    vecmResult <-
      vec2var(coTest, r = vecmr)#2系列としているためr=1となるが今後3系列以上に拡張する場合は要注意
    
    output$tsplot <- renderPlot({tsplot()})

    output$Download1 <- downloadHandler(
      filename = function() {
        paste("SaECaNet-",
              format(as.POSIXlt(Sys.time(), "GMT"), "%Y-%m-%d-%H-%M-%S"),
              ".png",
              sep = "")
      },
    content = function(file) {
        png(file, width = 1000, height = 600)
        tsplot()
        dev.off()
      }
    )

    tsplot<-function(){
      par(mar = c(3, 4, 2, 4))
      plot(
        dataset[, 1],
        dataset[, 2],
        type = "l",
        col = "blue",
        xlab = "" ,
        ylab = "" ,
        panel.first = grid(
          nx = NULL,
          ny = NULL,
          lty = 2,
          equilogs = T
        ) ,
        main = paste(bufDatasetColnames[2], "×", bufDatasetColnames[3]),
        cex.axis = 1,
        cex.lab = 1,
        cex.main = 1
      )
      par(new = T)
      plot(
        dataset[, 1],
        dataset[, 3],
        type = "l",
        col = "red",
        xlab = "" ,
        ylab = "" ,
        panel.first = grid(
          nx = NULL,
          ny = NULL,
          lty = 2,
          equilogs = T
        ) ,
        xaxt = "n",
        yaxt = "n",
        cex.axis = 1,
        cex.lab = 1,
        cex.main = 1
      )
      axis(4, cex.axis = 1, cex.lab = 1)
      legend(
        "topleft",
        col = c("blue", "red"),
        lty = 1,
        legend = bufDatasetColnames[2:3],
        cex = 1
      )
      mtext(
        bufDatasetColnames[2],
        side = 2,
        line = 3.2,
        cex = 1
      )
      mtext(
        bufDatasetColnames[3],
        side = 4,
        line = 3.2,
        cex = 1
      )
    }

    output$ccf <- renderPlot({ccfplot()})

    output$Download2 <- downloadHandler(
      filename = function() {
        paste("SaECaNet-",
              format(as.POSIXlt(Sys.time(), "GMT"), "%Y-%m-%d-%H-%M-%S"),
              ".png",
              sep = "")
      },
    content = function(file) {
        png(file, width = 1000, height = 600)
        ccfplot()
        dev.off()
      }
    )
    
    ccfplot<-function(){    
      #Cross Correlation
      par(mar = c(3, 4, 4, 1))
      ccf(
        dataset[, 2],
        dataset[, 3],
        lag = 36,
        panel.first = grid(
          nx = NULL,
          ny = NULL,
          lty = 2,
          equilogs = F
        ),
        main = paste(
          "Cross-Correlation Function\n",
          bufDatasetColnames[2],
          "×",
          bufDatasetColnames[3]
        )
      )
    }

    output$scatter <- renderPlot({scatter()})

    output$Download3 <- downloadHandler(
      filename = function() {
        paste("SaECaNet-",
              format(as.POSIXlt(Sys.time(), "GMT"), "%Y-%m-%d-%H-%M-%S"),
              ".png",
              sep = "")
      },
    content = function(file) {
        png(file, width = 1000, height = 600)
        scatter()
        dev.off()
      }
    )

    x <- dataset[, 2]
    y <- dataset[, 3]
    fit  <- lm(y ~ x)
    scatter<-function(){
      par(mar = c(4, 4, 2, 1))
      collist <- c("red")
      plot(
        x,
        y,
        type = "p",
        col = "black",
        xlab = bufDatasetColnames[2] ,
        ylab = bufDatasetColnames[3] ,
        panel.first = grid(
          nx = NULL,
          ny = NULL,
          lty = 2,
          equilogs = T
        ) ,
        main = "Scatter Plot with Correlation",
        cex.axis = 1,
        cex.lab = 1,
        cex.main = 1
      )
      abline(fit, col = "red")
      legend(
        "topleft",
        col = collist,
        lty = 1,
        legend = c("y=a*x+Intercept"),
        cex = 1
      )
    }
    
    varResultOP <-
      vars::irf(
        varResult,
        impulse = "x",
        response = "y",
        n.ahead = 10
      )

    vecmResultOP <-
      vars::irf(
        vecmResult,
        impulse = "x",
        response = "y",
        n.ahead = 10
      )

    varplot<-function(){
      plot(varResultOP,
           main=paste("Orthogonal Impulse Response from ",bufDatasetColnames[2]," to ",bufDatasetColnames[3],"\nwithout unit root:VAR{vars}→irf{vars}",sep=""))
    }
    
    vecmplot<-function(){
      plot(vecmResultOP,
           main=paste("Orthogonal Impulse Response from ",bufDatasetColnames[2]," to ",bufDatasetColnames[3],"\nwith unit root:ca.jo{urca}→vec2var{vars}→irf{vars}",sep=""))
    }
    
    output$varresult <- renderPlot({varplot()})

    output$vecmresult <- renderPlot({vecmplot()})

    output$Download4 <- downloadHandler(
      filename = function() {
        paste("SaECaNet-",
              format(as.POSIXlt(Sys.time(), "GMT"), "%Y-%m-%d-%H-%M-%S"),
              ".png",
              sep = "")
      },
    content = function(file) {
        png(file, width = 1000, height = 600)
        varplot()
        dev.off()
      }
    )
    
    output$Download5 <- downloadHandler(
      filename = function() {
        paste("SaECaNet-",
              format(as.POSIXlt(Sys.time(), "GMT"), "%Y-%m-%d-%H-%M-%S"),
              ".png",
              sep = "")
      },
    content = function(file) {
        png(file, width = 1000, height = 600)
        vecmplot()
        dev.off()
      }
    )

    funPlot <- function(iii) {
      assign(paste("acfPlot", iii, sep = ""), renderPlot({
        par(mar = c(3, 4, 3, 1))
        acf(
          dataset[, iii],
          lag = 180,
          panel.first = grid(
            nx = NULL,
            ny = NULL,
            lty = 2,
            equilogs = F
          ),
          main = paste("ACF:", bufDatasetColnames[iii])
        )
      })
      , .GlobalEnv)
      
      assign(paste("arimaPlot", iii, sep = ""),
             renderPlot({
               par(mar = c(3, 4, 3, 1))
               ci <- c(50, 90)
               forecastN <- 30
               result.arima <-
                 auto.arima(dataset[, iii],
                            ic = "aic",
                            trace = F,
                            stepwise = T)
               result.forecast <-
                 forecast::forecast(result.arima, level = ci, h = forecastN)
               plot(
                 result.forecast,
                 main = paste("ARIMA:", bufDatasetColnames[iii], "\nCI=", ci[1], "-", ci[2]),
                 ylab = "",
                 panel.first = grid(
                   nx = NULL,
                   ny = NULL,
                   lty = 2,
                   equilogs = T
                 )
               )
             })
             ,
             .GlobalEnv)
      
      assign(paste("histPlot", iii, sep = "") ,
             renderPlot({
               par(mar = c(3, 4, 3, 1))
               hist(
                 dataset[, iii],
                 main = paste("Histgram:", bufDatasetColnames[iii]),
                 xlab = "",
                 col = "#E0FFFF",
                 breaks = "Scott"
               )
             })
             ,
             .GlobalEnv)
    }
    
    iii <- 2
    funPlot(iii)
    output$acfx <- get(paste("acfPlot", iii, sep = ""))
    output$arimax <- get(paste("arimaPlot", iii, sep = ""))
    output$histx <- get(paste("histPlot", iii, sep = ""))
    iii <- 3
    funPlot(iii)
    output$acfy <- get(paste("acfPlot", iii, sep = ""))
    output$arimay <- get(paste("arimaPlot", iii, sep = ""))
    output$histy <- get(paste("histPlot", iii, sep = ""))
    
    output$summaryDF <- DT::renderDataTable(
      summaryDF,
      rownames = F,
      caption = "Table 1: Summary of two Time Series Data.",
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
    
    coefx <- fit$coefficients[2]
    coefIntercept <- fit$coefficients[1]
    Prx <- summary(fit)$coef[, "Pr(>|t|)"][2]
    PrIntercept <- summary(fit)$coef[, "Pr(>|t|)"][1]
    adj.r.squared <- summary(fit)$adj.r.squared
    f <- summary(fit)$fstatistic
    pvalue <- pf(f[1], f[2], f[3], lower.tail = F)
    fitDF <-
      data.frame(
        ID = seq(1, 7),
        Item = c(
          "x",
          "Intercept",
          "x.pr",
          "Intercept.pr",
          "Adjusted r^2",
          "p value",
          "Data ID"
        ),
        Value = c(
          coefx,
          coefIntercept,
          Prx,
          PrIntercept,
          adj.r.squared,
          pvalue,
          paste(dataset[1,1],"-",dataset[nrow(dataset),1])
        )
      )
    output$fitDF <- DT::renderDataTable(
      fitDF,
      rownames = F,
      caption = paste(
        "Table 2: Linear Regression Model. x:",
        bufDatasetColnames[2],
        ",y:",
        bufDatasetColnames[3]
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
    
    adfDF <- data.frame(ID = c(1:3), tail(summaryDF[, -1], 3))
    output$adfDF <- DT::renderDataTable(
      adfDF,
      rownames = F,
      caption = "Table 3: Augmented Dickey-Fuller Test of two Time Series Data.",
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
    
    fitResiduals <- adf.test(fit$residuals)
    attributes(fitResiduals)
    t1 <- fitResiduals$method
    t2 <- fitResiduals$alternative
    fitResidualsDF <-
      data.frame(
        ID = c(1:4),
        Item = c("p-value", "Lag order", "Dickey-Fuller","Data ID"),
        Value = c(
          fitResiduals$p.value,
          fitResiduals$parameter,
          fitResiduals$statistic,
          paste(dataset[1,1],"-",dataset[nrow(dataset),1])
        )
      )
    output$fitResidualsDF <- DT::renderDataTable(
      fitResidualsDF,
      rownames = F,
      caption = paste("Table 4: ADF test for Residuals of OLS. Method:", t1, " , H1:", t2),
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
    
    dwtestResult <- dwtest(dataset[, 3] ~ dataset[, 2])
    DW <- dwtestResult$statistic
    dwpvalue <- dwtestResult$p.value
    dwH1 <- dwtestResult$alternative
    dwDF <-
      data.frame(
        ID = c(1:3),
        Item = c("DW", "p-value","Data ID"),
        Value = c(DW, dwpvalue,
          paste(dataset[1,1],"-",dataset[nrow(dataset),1]))
      )
    output$dwDF <- DT::renderDataTable(
      dwDF,
      rownames = F,
      caption = paste("Table 5: Durbin-Watson Test. H1:", dwH1),
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
    
    potest <- po.test(dataset[, -1])
    poresult <- potest$statistic[1]
    polag <- potest$parameter
    popvalue <- potest$p.value
    potestname <- potest$method
    potestDF <-
      data.frame(
        ID = c(1:4),
        Item = c(
          "Phillips-Ouliaris demeaned",
          "Truncation lag parameter",
          "p-value",
          "Data ID"
        ),
        Value = c(poresult, polag, popvalue,
          paste(dataset[1,1],"-",dataset[nrow(dataset),1]))
      )
    output$potestDF <- DT::renderDataTable(
      potestDF,
      rownames = F,
      caption = paste("Table 6:", potestname),
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
    
    varselectResult <-
      data.frame(VARselect(dataset[, -1])$selection[, drop = F])
    varselectResult <-
      data.frame(
        ID = seq(1, nrow(varselectResult)),
        Item = rownames(varselectResult),
        Lag = varselectResult[, 1],
        check.names = F
      )
    output$varselectResult <-
      DT::renderDataTable(
        varselectResult,
        rownames = F,
        caption = paste("Table 7: Lag Selection by VARselect {vars}"),
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
    
    varDF <-
      data.frame(varResultOP$Lower, varResultOP$Upper, varResultOP$irf)
    colnames(varDF) <-
      paste(varResultOP$response, c(":Lower", ":Upper", ":irf"))
    varDF <-
      data.frame(ID = seq(1, nrow(varDF)), varDF, check.names = F)
    output$varDF <- DT::renderDataTable(
      varDF,
      rownames = F,
      caption = paste("Table 8: Impulse response by VAR {vars}."),
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
    
    vecmDF <-
      data.frame(vecmResultOP$Lower, vecmResultOP$Upper, vecmResultOP$irf)
    colnames(vecmDF) <-
      paste(vecmResultOP$response, c(":Lower", ":Upper", ":irf"))
    vecmDF <-
      data.frame(ID = seq(1, nrow(vecmDF)), vecmDF, check.names = F)
    output$vecmDF <- DT::renderDataTable(
      vecmDF,
      rownames = F,
      caption = paste("Table 9: Impulse response by ca.jo {urca} and vec2var {vars}."),
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
    
    datasetForDT<-dataset
    colnames(datasetForDT)<-bufDatasetColnames
    output$dt <- DT::renderDataTable(
      datasetForDT,
      rownames = FALSE,
      caption = "Table 10: Time Series Data.",
      options = list(
        autoWidth = T,
        info = T,
        lengthChange = T,
        ordering = T,
        searching = T,
        scrollX = T,
        lengthMenu = list(c(20, 10, -1), c("20", "10", "All")),
        pageLength = 20,
        orderClasses = TRUE,
        order = list(list(0, "asc"))
      )
    )
  })
  
  output$completiontime <- renderText({
    if (is.null(input$dataX) & is.null(input$dataY))
      return(NULL)
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
    <li>2016-06-14:ver.1.0.0</li>
    <li>2016-06-15:ver.1.0.1</li>
    <li>2016-06-17:ver.1.0.2</li>
    <li>2016-07-09:ver.1.0.3</li>
    </ol>"
    HTML(str)
  })

  output$gitcode <- renderUI({
    str <- "<hr>
    <b>Code</b><br>
    <ol>
    <li><a href=\"https://github.com/am-consulting/Rscript/tree/master/shinyapps/timeseriesdatastatisticalanalysis\" target=\"_blank\">Move to GitHub</a></li>
    </ol>"
    HTML(str)
  })

  output$linkList <- renderUI({
    str <- linkList
    HTML(str)
  })
    
})