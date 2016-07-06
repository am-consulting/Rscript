library(shiny)
shinyServer(function(input, output, session)
{
  options(scipen = 999)
  imputData <- function() {
    ed <<-  input$slider.ed * eval(parse(text = input$power.ed))
  }
  
  outputResult <- function() {
    Resultlamnda <<- lamnda
    ResultRe01 <<- Reynolds01
    ResultRe02 <<- Reynolds02
    
    output$ed <- renderPrint({
      ed
    })
    output$Resultlamnda <- renderPrint({
      Resultlamnda
    })
    output$ResultRe01 <- renderPrint({
      ResultRe01
    })
    output$ResultRe02 <- renderPrint({
      ResultRe02
    })
  }
  
  frictionFunction <- function(ed) {
    lamnda <<- (1 / (1.14 - 2 * log10(ed))) ^ 2
    Reynolds01 <<- 200 / ((lamnda ^ 0.5) * ed)
    Reynolds02 <<- 3500 / ed
  }
  
  outputDT <- function() {
    edRange <<-
      unique(c(
        seq(10 ^ -6, 10 ^ -5, by = 10 ^ -6 / 4),
        seq(10 ^ -5, 10 ^ -4, by = 10 ^ -5 / 4),
        seq(10 ^ -4, 10 ^ -3, by = 10 ^ -4 / 4),
        seq(10 ^ -3, 10 ^ -2, by = 10 ^ -3 / 4),
        seq(10 ^ -2, 10 ^ -1, by = 10 ^ -2 / 4)
      ))
    frictionFactor <<- vector()
    Reynoldsnumber01 <<- vector()
    Reynoldsnumber02 <<- vector()
    cnt <- 1
    for (buf in 1:length(edRange)) {
      ed <<- edRange[buf]
      frictionFunction(ed)
      frictionFactor[cnt] <<- lamnda
      Reynoldsnumber01[cnt] <<- Reynolds01
      Reynoldsnumber02[cnt] <<- Reynolds02
      cnt <- cnt + 1
    }
    sciRe <- format(edRange, scientific = T)
    dataset <-
      data.frame(edRange ,
                 sciRe,
                 frictionFactor,
                 Reynoldsnumber01,
                 Reynoldsnumber02)
    datatableData <- dataset
    colnames(datatableData) <-
      c("ε/d",
        "ε/d(SN)",
        "λ",
        "Re(1)",
        "Re(2)")
    rdt <- DT::renderDataTable(
      datatableData,
      rownames = FALSE,
      caption = "Table 1: Friction Factor and Reynolds number.",
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
    output$dtTable <- rdt
  }
  
  reactiveData <- reactive({
    imputData()
    outputDT()
    imputData()
    frictionFunction(ed)
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
    <li><a href=\"https://github.com/am-consulting/Rscript/tree/master/shinyapps/frictionfactor_Roughpipe\" target=\"_blank\">Move to GitHub</a></li>
    </ol>"
    HTML(str)
  })

  output$linkList <- renderUI({
    str <- linkList
    HTML(str)
  })

  output$datachecktime <- renderText({
    reactiveData()
    completionTime <- Sys.time()
    paste("Completion :" , as.character(completionTime))
  })
})