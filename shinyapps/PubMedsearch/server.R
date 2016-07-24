library(shiny)
library(DT)
library(RISmed)
options(download.file.method = "libcurl")
shinyServer(function(input, output, session)
{
  observeEvent(input$searchAction, {
    queryWord <- input$searchWord
    if (queryWord == "") {
      return(NULL)
    } else{
      retmax <- input$returnMax
      startDate <- input$dateRange[1]
      endDate <- input$dateRange[2]
      Sys.sleep(1)
      summaryData <- EUtilsSummary(
        queryWord,
        type = "esearch",
        db = "pubmed",
        datetype = 'pdat',
        mindate = format(startDate, "%Y/%m/%d"),
        maxdate = format(endDate, "%Y/%m/%d"),
        retmax = retmax
      )
      if (QueryCount(summaryData) == 0) {
        pubmedDataSet <- data.frame()
      } else{
        articleData <- EUtilsGet(summaryData)
        pubmedDataSet <-
          data.frame(
            "Date" = as.Date(
              paste(
                YearPubmed(articleData),
                "-",
                MonthPubmed(articleData),
                "-",
                DayPubmed(articleData),
                sep = ""
              )
            ),
            "Title" = ArticleTitle(articleData),
            "Abstract" = AbstractText(articleData),
            "ID" = QueryId(summaryData),
            check.names = F
          )
      }
      
      output$resultTable <- DT::renderDataTable(
        pubmedDataSet,
        rownames = F,
        caption = paste("Table "),
        options = list(
          autoWidth = T,
          info = T,
          lengthChange = T,
          ordering = T,
          searching = T,
          scrollX = T,
          lengthMenu = list(c(3, 5, 10, -1, 1), c(3, 5, 10, "All", 1)),
          orderClasses = T,
          order = list(list(0, "desc")),
          searchHighlight = T,
          dom= '<"top"lifp>rt<"bottom"><"clear">'
        )
      )
      
      output$searchStatusFinish <- renderText({
        paste("Searching Finish(UTC):" ,
              as.character(as.POSIXlt(Sys.time(), "GMT")))
      })
    }
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
    <li>2016-06-29:ver.1.0.0</li>
    <li>2016-07-24:ver.1.0.1</li>
    </ol>"
    HTML(str)
  })
  
  output$gitcode <- renderUI({
    str <- "<hr>
    <b>Code</b><br>
    <ol>
    <li><a href=\"https://github.com/am-consulting/Rscript/tree/master/shinyapps/PubMedsearch\" target=\"_blank\">Move to GitHub</a></li>
    </ol>"
    HTML(str)
  })
 
  output$linkList <- renderUI({
    str <- linkList
    HTML(str)
  })
       
})