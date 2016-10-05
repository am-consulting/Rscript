library(shiny)
library(ggplot2)
library(scales)
library(lubridate)
library(RCurl)
library(DT)
shinyServer(function(input, output)
{
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
 
})