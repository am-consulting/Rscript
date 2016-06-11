library(shiny)
shinyServer(function(input, output, session)
{
  options(scipen = 999)
  imputData <- function() {
    ed <<-  input$slider.ed * eval(parse(text = input$power.ed))
    Reynolds <<-
      input$slider.Re * eval(parse(text = input$power.Re))
  }
  
  outputResult <- function() {
    Result <<- uniroot(frictionFunction, c(0.005, 0.1))$root
    output$ed <- renderPrint({
      ed
    })
    output$Reynolds <- renderPrint({
      Reynolds
    })
    output$Result <- renderPrint({
      Result
    })
  }
  
  frictionFunction <- function(lamnda) {
    -2 * log10(ed / 3.71 + 2.51 / (Reynolds * sqrt(lamnda))) - 1 / sqrt(lamnda)
  }
  
  outputDT1 <- function() {
    ReRange <<-
      unique(c(
        seq(10 ^ 3, 10 ^ 4, by = 10 ^ 3 / 4),
        seq(10 ^ 4, 10 ^ 5, by = 10 ^ 4 / 4),
        seq(10 ^ 5, 10 ^ 6, by = 10 ^ 5 / 4),
        seq(10 ^ 6, 10 ^ 7, by = 10 ^ 6 / 4),
        seq(10 ^ 7, 10 ^ 8, by = 10 ^ 7 / 4)
      ))
    frictionFactor <<- vector()
    cnt <- 1
    for (buf in 1:length(ReRange)) {
      Reynolds <<- ReRange[buf]
      frictionFactor[cnt] <<-
        uniroot(frictionFunction, c(0.005, 0.1))$root
      cnt <- cnt + 1
    }
    sciRe <- format(ReRange, scientific = T)
    dataset <- data.frame(ReRange , sciRe,  frictionFactor)
    datatableData <- dataset
    colnames(datatableData) <-
      c("Re", "Re(SN)", paste("λ : ε/d=", ed))
    rdt1 <- DT::renderDataTable(
      datatableData,
      rownames = FALSE,
      caption = "Table 1: Fixed ε/d.",
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
    output$table1 <- rdt1
  }
  
  outputDT2 <- function() {
    edRange <<-
      unique(c(
        seq(10 ^ -6, 10 ^ -5, by = 10 ^ -6 / 4),
        seq(10 ^ -5, 10 ^ -4, by = 10 ^ -5 / 4),
        seq(10 ^ -4, 10 ^ -3, by = 10 ^ -4 / 4)
      ))
    frictionFactor <<- vector()
    cnt <- 1
    for (buf in 1:length(edRange)) {
      ed <<- edRange[buf]
      frictionFactor[cnt] <<-
        uniroot(frictionFunction, c(0.005, 0.1))$root
      cnt <- cnt + 1
    }
    sciRe <- format(edRange, scientific = T)
    dataset <- data.frame(edRange , sciRe,  frictionFactor)
    datatableData <- dataset
    colnames(datatableData) <-
      c("ε/d",
        "ε/d(SN)",
        paste("λ : Re=", Reynolds))
    rdt2 <- DT::renderDataTable(
      datatableData,
      rownames = FALSE,
      caption = "Table 2: Fixed Re.",
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
    output$table2 <- rdt2
  }
  
  reactiveData <- reactive({
    imputData()
    outputDT1()
    imputData()
    outputDT2()
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
    completionTime <- Sys.time()
    paste("Completion :" , as.character(completionTime))
  })
})