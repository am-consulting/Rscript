library(shiny)
library(XML)
library(Nippon)
library(lubridate)
library(xtable)
shinyServer(function(input, output)
{
  tab1weekEQ <- 0
  makeReactiveBinding('tab1weekEQ')

  funDataTable_1weekEQ <- function(obj, ind = '', orderC = 0, orderD = 'asc', width0 = '10%', width1 = '10%') {
    DT::renderDataTable(
      obj,
      options = list(
        paging = T, autoWidth = F, info = T, lengthChange = T, ordering = T, searching = T,
        scrollX = F, lengthMenu = list(c(10,-1, 1), c(10, 'All', 1)), orderClasses = T,
        order = list(list(orderC, orderD)), search = list(regex = T, caseInsensitive = T),
        columnDefs = list(list(width = width0, targets = 0),list(width = width1, targets = 1))
      ),
      rownames = F,   class = 'display compact', filter = 'bottom', caption = ind
    )
  }

  observeEvent(input$searchAction_1weekEQ, {
    sourceURL_1weekEQ <- "http://www.jma.go.jp/jp/quake/quake_local_index.html"
    tmp_1weekEQ <- readHTMLTable(doc=sourceURL_1weekEQ, header=T, trim=T, stringsAsFactors=F, as.data.frame=T, which=4)
    getTime_1weekEQ <<- Sys.time()
    title_1weekEQ <<- paste0('地震情報(最近一週間の各地の震度に関する情報,震度1以上). 出所:気象庁. データ取得日時:', getTime_1weekEQ)
    colnames(tmp_1weekEQ) <- iconv(colnames(tmp_1weekEQ),"utf-8")
    buf_1weekEQ <- data.frame()
    for(rrr in 1:nrow(tmp_1weekEQ)){
      for(ccc in 1:ncol(tmp_1weekEQ)){
        buf_1weekEQ[rrr,ccc] <- zen2han(tmp_1weekEQ[rrr,ccc])
      }
    }
    colnames(buf_1weekEQ) <- colnames(tmp_1weekEQ)
    buf_1weekEQ[,4] <- as.numeric(gsub('M','',buf_1weekEQ[,4]))
    assign('dataSet_1weekEQ', buf_1weekEQ, envir = .GlobalEnv)

    epicenter_1weekEQ <- unique(dataSet_1weekEQ[,3])
    earthquakeTable_1weekEQ <- data.frame()
    for(eee in 1:length(epicenter_1weekEQ)){
      buf0 <- subset(dataSet_1weekEQ,dataSet_1weekEQ[,3]==epicenter_1weekEQ[eee])
      earthquakeTable_1weekEQ[eee,1] <- epicenter_1weekEQ[eee]
      earthquakeTable_1weekEQ[eee,2] <- nrow(buf0)
      earthquakeTable_1weekEQ[eee,3] <- max(buf0[,4])
      earthquakeTable_1weekEQ[eee,4] <- min(buf0[,4])
      earthquakeTable_1weekEQ[eee,5] <- tail(buf0[,2],1)
      earthquakeTable_1weekEQ[eee,6] <- head(buf0[,2],1)
    }
    colnames(earthquakeTable_1weekEQ) <- c('震央地名', '検知総回数', '最大マグニチュード', '最小マグニチュード', '初検知', '直近検知')
    assign('earthquakeTable_1weekEQ',earthquakeTable_1weekEQ, envir = .GlobalEnv)

    tab1weekEQ <<- 1
  })

  observe({
    if (tab1weekEQ == 0) {return(NULL)} else{
      rdt_1week01EQ <- funDataTable_1weekEQ(obj = dataSet_1weekEQ,
                                            ind = title_1weekEQ, orderD = 'desc', width0 = '30%', width1 = '20%')
      output$table1_1week01EQ <- rdt_1week01EQ
      rdt_1week02EQ <- funDataTable_1weekEQ(obj = earthquakeTable_1weekEQ,
                                            ind = title_1weekEQ, orderC = 1,  orderD = 'desc', width0 = '16%', width1 = '14%')
      output$table2_1week02EQ <- rdt_1week02EQ

    }
  })

  output$latestDataDownloadTime_1weekEQ <- renderText({
    if (tab1weekEQ == 1) {paste("Data imported time(UTC):" , as.character(getTime_1weekEQ))}
  })

  output$remarktext_1weekEQ <- renderUI({
    str <- "<hr>
    <b>Note</b><br>
    <ol>
    <li><a href=\"http://www.jma.go.jp/jp/quake/quake_local_index.html\" target=\"_blank\">Japan Meteorological Agency</a></li>
    </ol>"
    HTML(str)
  })

  output$disclaimer_1weekEQ<- renderUI({
    str <- "<hr>
    <b>Operating Company / Disclaimer</b><br>
    <ol>
    <li><a href=\"http://am-consulting.co.jp\" target=\"_blank\">Asset Management Consulting Corporation -  http://am-consulting.co.jp</a></li>
    <li><a href=\"http://am-consulting.co.jp/disclaimer/\" target=\"_blank\">Disclaimer</a>
    </li>
    </ol>"
    HTML(str)
  })

})