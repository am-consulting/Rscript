library(shiny)
shinyServer(function(input, output, session)
{
  options(scipen = 999)
  imputData <- function() {
    Reynolds <<-
      input$slider.Re * eval(parse(text = input$power.Re))
  }
  
  outputResult <- function() {
    frictionFunction(Reynolds)
    Result <- lamnda
    output$Reynolds <- renderPrint({
      Reynolds
    })
    output$Result <- renderPrint({
      Result
    })
  }
  
  frictionFunction <- function(Reynolds) {
    lamnda <<- 64 / Reynolds
  }
  
  outputDT1 <- function() {
    ReRange <<-
      unique(c(
        seq(10 ^ 1, 10 ^ 2, by = 10 ^ 1 / 4),
        seq(10 ^ 2, 10 ^ 3, by = 10 ^ 2 / 4),
        seq(10 ^ 3, 10 ^ 4, by = 10 ^ 3 / 4)
      ))
    frictionFactor <<- vector()
    cnt <- 1
    for (buf in 1:length(ReRange)) {
      Reynolds <<- ReRange[buf]
      frictionFunction(Reynolds)
      frictionFactor[cnt] <<- lamnda
      cnt <- cnt + 1
    }
    sciRe <- format(ReRange, scientific = T)
    dataset <- data.frame(ReRange , sciRe,  frictionFactor)
    datatableData <- dataset
    colnames(datatableData) <-
      c("Re", "Re(SN)", "λ")
    rdt1 <- DT::renderDataTable(
      datatableData,
      rownames = FALSE,
      caption = "Table 1:Reynolds number and Friction Factor.",
      options = list(
        autoWidth = T,
        info = T,
        lengthChange = T,
        ordering = T,
        searching = T,
        scrollX = T,
        lengthMenu = list(c(5, 10, -1), c("5", "10", "All")),
        pageLength = 10,
        orderClasses = TRUE,
        order = list(list(0, "asc"))
      )
    )
    output$dtTable <- rdt1
  }
  
  reactiveData <- reactive({
    imputData()
    outputDT1()
    imputData()
    outputResult()
  })
  
  output$remarktext <- renderUI({
    str <- "<hr>
    <b>Remarks</b><br>
    <ol>
    <li>Loss coefficient for straight pipe：λ</li>
    <li>Roughness height：ε</li>
    <li>Pipe diameter：d</li>
    <li>Reynolds number：Re</li>
    <li><a href=\"http://equations.am-consulting.co.jp/2014/04/08/1097/\" target=\"_blank\">配管摩擦損失係数:ムーディー線図/Moody chart</a></li>
    <li><a href=\"http://equations.am-consulting.co.jp/2014/04/08/1095/\" target=\"_blank\">配管摩擦損失係数:層流/Laminar</a></li>
    <li><a href=\"http://equations.am-consulting.co.jp/2014/04/08/1093/\" target=\"_blank\">配管摩擦損失係数:滑面配管/Smooth pipes</a></li>
    <li><a href=\"http://equations.am-consulting.co.jp/2014/04/08/1089/\" target=\"_blank\">配管摩擦損失係数:粗面配管/Rough pipes</a></li>
    <li><a href=\"http://equations.am-consulting.co.jp/2014/04/08/1082/\" target=\"_blank\">配管摩擦損失係数:コールブルック・ホワイトの式/Colebrook-White Equation</a></li>
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
    <li>2016-06-09:ver.1.0.0</li>
    <li>2016-06-17:ver.1.0.1</li>
    <li>2016-07-06:ver.1.0.2</li>
    </ol>"
    HTML(str)
  })

  output$gitcode <- renderUI({
    str <- "<hr>
    <b>Code</b><br>
    <ol>
    <li><a href=\"https://github.com/am-consulting/Rscript/tree/master/shinyapps/frictionfactor_laminar\" target=\"_blank\">Move to GitHub</a></li>
    </ol>"
    HTML(str)
  })

  output$linkList <- renderUI({
    str <- linkList
    HTML(str)
  })

  output$datachecktime <- renderText({
    reactiveData()
    completionTime <- as.POSIXlt(Sys.time(), "GMT")
    paste("Completion(UTC) :" , as.character(completionTime))
  })
})