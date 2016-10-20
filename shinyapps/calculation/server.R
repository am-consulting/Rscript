library(shiny)
shinyServer(function(input, output, session)
{
  options(scipen = 999)
# colebrook
  frictionFunction_01 <- function(lamnda_01) {
    -2 * log10(ed / 3.71 + 2.51 / (Reynolds * sqrt(lamnda_01))) - 1 / sqrt(lamnda_01)
  }

  getLamnda_01 <- function(ed, Reynolds){
    ed <<- ed
    Reynolds <<- Reynolds
    uniroot(frictionFunction_01, c(0.005, 0.1))$root
  }

  imputData_01 <- function() {
    assign('orig_ed_01',       input$slider.ed_01 * eval(parse(text = input$power.ed_01)), envir = .GlobalEnv)
    assign('orig_Reynolds_01', input$slider.Re_01 * eval(parse(text = input$power.Re_01)), envir = .GlobalEnv)
    output$orig_ed_01 <- renderPrint({orig_ed_01})
    output$orig_Reynolds_01 <- renderPrint({orig_Reynolds_01})
  }

  outputResult_01 <- function() {
    Result_01 <- getLamnda_01(ed = orig_ed_01, Reynolds = orig_Reynolds_01)
    output$Result_01 <- renderPrint({Result_01})
  }

  outputDT1_01 <- function() {
    ReRange_01 <- unique(c(
        seq(10 ^ 3, 10 ^ 4, by = 10 ^ 3 / 4),
        seq(10 ^ 4, 10 ^ 5, by = 10 ^ 4 / 4),
        seq(10 ^ 5, 10 ^ 6, by = 10 ^ 5 / 4),
        seq(10 ^ 6, 10 ^ 7, by = 10 ^ 6 / 4),
        seq(10 ^ 7, 10 ^ 8, by = 10 ^ 7 / 4)
      ))
    frictionFactor_01 <- vector()
    cnt <- 1
    for (buf in 1:length(ReRange_01)) {
      Reynolds_01 <- ReRange_01[buf]
      frictionFactor_01[cnt] <- getLamnda_01(ed = orig_ed_01, Reynolds = Reynolds_01)
      cnt <- cnt + 1
    }
    sciRe_01 <- format(ReRange_01, scientific = T)
    dataset_01 <- data.frame(ReRange_01 , sciRe_01, frictionFactor_01)
    datatableData_01 <- dataset_01
    colnames(datatableData_01) <- c("Re", "Re(SN)", paste("λ : ε/d=", orig_ed_01))
    rdt1_01 <- DT::renderDataTable(
      datatableData_01,
      rownames = F,
      caption = "Table 1: Fixed ε/d.",
      options = list(
        autoWidth = F,
        info = T,
        lengthChange = T,
        ordering = T,
        searching = T,
        scrollX = F,
        lengthMenu = list(c(5, 10, -1), c("5", "10", "All")),
        pageLength = 10,
        orderClasses = T,
        order = list(list(0, "asc")),
        search = list(regex = T, caseInsensitive = T)
      ),
      filter = 'bottom'
    )
    output$table1_01 <- rdt1_01
  }

  outputDT2_01 <- function() {
    edRange_01 <- unique(c(
        seq(10 ^ -6, 10 ^ -5, by = 10 ^ -6 / 4),
        seq(10 ^ -5, 10 ^ -4, by = 10 ^ -5 / 4),
        seq(10 ^ -4, 10 ^ -3, by = 10 ^ -4 / 4)
      ))
    frictionFactor_01 <- vector()
    cnt <- 1
    for (buf in 1:length(edRange_01)) {
      ed_01 <- edRange_01[buf]
      frictionFactor_01[cnt] <- getLamnda_01(ed = ed_01, Reynolds = orig_Reynolds_01)
      cnt <- cnt + 1
    }
    sciRe_01 <- format(edRange_01, scientific = T)
    dataset_01 <- data.frame(edRange_01 , sciRe_01,  frictionFactor_01)
    datatableData_01 <- dataset_01
    colnames(datatableData_01) <- c("ε/d", "ε/d(SN)", paste("λ : Re=", orig_Reynolds_01))
    rdt2_01 <- DT::renderDataTable(
      datatableData_01,
      rownames = F,
      caption = "Table 2: Fixed Re.",
      options = list(
        autoWidth = F,
        info = T,
        lengthChange = T,
        ordering = T,
        searching = T,
        scrollX = F,
        lengthMenu = list(c(5, 10, -1), c("5", "10", "All")),
        pageLength = 10,
        orderClasses = T,
        order = list(list(0, "asc")),
        search = list(regex = T, caseInsensitive = T)
      ),
      filter = 'bottom'
    )
    output$table2_01 <- rdt2_01
  }

  reactiveData_01 <- reactive({
    imputData_01()
    outputDT1_01()
    outputDT2_01()
    outputResult_01()
  })

  output$remarktext_01 <- renderUI({
    str <- "<hr>
<b>Remarks</b><br>
<ol>
<li>Loss coefficient for straight pipe：λ</li>
<li>Roughness height：ε</li>
<li>Pipe diameter：d</li>
<li>Reynolds number：Re</li>
<li>Moody chart:
<a href=\"http://equations.am-consulting.co.jp/2014/04/08/1097/\" target=\"_blank\">http://equations.am-consulting.co.jp/2014/04/08/1097/</a></li>
<li>Laminar:
<a href=\"http://equations.am-consulting.co.jp/2014/04/08/1095/\" target=\"_blank\">http://equations.am-consulting.co.jp/2014/04/08/1095/</a></li>
<li>Smooth pipes:
<a href=\"http://equations.am-consulting.co.jp/2014/04/08/1093/\" target=\"_blank\">http://equations.am-consulting.co.jp/2014/04/08/1093/</a></li>
<li>Rough pipes:
<a href=\"http://equations.am-consulting.co.jp/2014/04/08/1089/\" target=\"_blank\">http://equations.am-consulting.co.jp/2014/04/08/1089/</a></li>
<li>Colebrook-White Equation:
<a href=\"http://equations.am-consulting.co.jp/2014/04/08/1082/\" target=\"_blank\">http://equations.am-consulting.co.jp/2014/04/08/1082/</a></li>
<li><a href=\"http://am-consulting.co.jp\" target=\"_blank\">Asset Management Consulting Corporation</a></li>
<li><a href=\"http://www.saecanet.com/subfolder/disclaimer.html\" target=\"_blank\">Disclaimer</a></li>
</ol>"
    HTML(str)
  })

  output$datachecktime_01 <- renderText({
    reactiveData_01()
    completionTime_01 <- as.POSIXlt(Sys.time(), "GMT")
    paste("Completion(UTC) :" , as.character(completionTime_01))
  })
# colebrook

# rough
  imputData_02 <- function() {
    assign('orig_ed_02', input$slider.ed_02 * eval(parse(text = input$power.ed_02)), envir = .GlobalEnv)
  }

  frictionFunction_02 <- function(ed) {
    lamnda_02     <<- (1 / (1.14 - 2 * log10(ed))) ^ 2
    Reynolds01_02 <<- 200 / ((lamnda_02 ^ 0.5) * ed)
    Reynolds02_02 <<- 3500 / ed
  }

  outputResult_02 <- function() {
    orig_lamnda_02         <- lamnda_02
    orig_Reynolds01_02     <- Reynolds01_02
    orig_Reynolds02_02     <- Reynolds02_02
    output$orig_ed_02      <- renderPrint({orig_ed_02})
    output$Resultlamnda_02 <- renderPrint({orig_lamnda_02})
    output$ResultRe01_02   <- renderPrint({orig_Reynolds01_02})
    output$ResultRe02_02   <- renderPrint({orig_Reynolds02_02})
  }

  outputDT_02 <- function() {
    edRange_02 <- unique(c(
      seq(10 ^ -6, 10 ^ -5, by = 10 ^ -6 / 4),
      seq(10 ^ -5, 10 ^ -4, by = 10 ^ -5 / 4),
      seq(10 ^ -4, 10 ^ -3, by = 10 ^ -4 / 4),
      seq(10 ^ -3, 10 ^ -2, by = 10 ^ -3 / 4),
      seq(10 ^ -2, 10 ^ -1, by = 10 ^ -2 / 4)
    ))
    frictionFactor_02 <- vector()
    Reynoldsnumber01_02 <- vector()
    Reynoldsnumber02_02 <- vector()
    cnt <- 1
    for (buf in 1:length(edRange_02)) {
      frictionFunction_02(ed =  edRange_02[buf])
      frictionFactor_02[cnt] <- lamnda_02
      Reynoldsnumber01_02[cnt] <- Reynolds01_02
      Reynoldsnumber02_02[cnt] <- Reynolds02_02
      cnt <- cnt + 1
    }
    sciRe_02 <- format(edRange_02, scientific = T)
    datatableData_02 <-
      data.frame(edRange_02 ,
                 sciRe_02,
                 frictionFactor_02,
                 Reynoldsnumber01_02,
                 Reynoldsnumber02_02)
    colnames(datatableData_02) <- c("ε/d", "ε/d(SN)", "λ", "Re(1)", "Re(2)")
    rdt_02 <- DT::renderDataTable(
      datatableData_02,
      rownames = F,
      caption = "Table: Friction Factor and Reynolds number.",
      options = list(
        autoWidth = F,
        info = T,
        lengthChange = T,
        ordering = T,
        searching = T,
        scrollX = F,
        lengthMenu = list(c(5, 10, -1), c("5", "10", "All")),
        pageLength = 10,
        orderClasses = T,
        order = list(list(0, "asc")),
        search = list(regex = T, caseInsensitive = T)
      ),
      filter = 'bottom'
    )
    output$dtTable_02 <- rdt_02
  }

  reactiveData_02 <- reactive({
    imputData_02()
    frictionFunction_02(ed =  orig_ed_02)
    outputResult_02()
    outputDT_02()
  })

  output$remarktext_02 <- renderUI({
    str <- "<hr>
    <b>Remarks</b><br>
    <ol>
    <li>Loss coefficient for straight pipe：λ</li>
    <li>Roughness height：ε</li>
    <li>Pipe diameter：d</li>
    <li>Reynolds number：Re</li>
    <li>Moody chart:
    <a href=\"http://equations.am-consulting.co.jp/2014/04/08/1097/\" target=\"_blank\">http://equations.am-consulting.co.jp/2014/04/08/1097/</a></li>
    <li>Laminar:
    <a href=\"http://equations.am-consulting.co.jp/2014/04/08/1095/\" target=\"_blank\">http://equations.am-consulting.co.jp/2014/04/08/1095/</a></li>
    <li>Smooth pipes:
    <a href=\"http://equations.am-consulting.co.jp/2014/04/08/1093/\" target=\"_blank\">http://equations.am-consulting.co.jp/2014/04/08/1093/</a></li>
    <li>Rough pipes:
    <a href=\"http://equations.am-consulting.co.jp/2014/04/08/1089/\" target=\"_blank\">http://equations.am-consulting.co.jp/2014/04/08/1089/</a></li>
    <li>Colebrook-White Equation:
    <a href=\"http://equations.am-consulting.co.jp/2014/04/08/1082/\" target=\"_blank\">http://equations.am-consulting.co.jp/2014/04/08/1082/</a></li>
    <li><a href=\"http://am-consulting.co.jp\" target=\"_blank\">Asset Management Consulting Corporation</a></li>
    <li><a href=\"http://www.saecanet.com/subfolder/disclaimer.html\" target=\"_blank\">Disclaimer</a></li>
    </ol>"
    HTML(str)
  })

  output$datachecktime_02 <- renderText({
    reactiveData_02()
    completionTime_02 <- as.POSIXlt(Sys.time(), "GMT")
    paste("Completion(UTC) :" , as.character(completionTime_02))
  })
# rough

# smooth
  imputData_03 <- function() {
    assign('orig_Reynolds_03', input$slider.Re_03 * eval(parse(text = input$power.Re_03)), envir = .GlobalEnv)
  }

  frictionFunction_03 <- function(lamnda_03) {
    2 * log10(Reynolds * sqrt(lamnda_03)) - 0.8 - 1 / sqrt(lamnda_03)
  }

  getLamnda_03 <- function(Reynolds){
    Reynolds <<- Reynolds
    uniroot(frictionFunction_03, c(0.005, 0.1))$root
  }

  outputResult_03 <- function() {
    Result_03 <- getLamnda_03(Reynolds = orig_Reynolds_03)
    output$Reynolds_03 <- renderPrint({orig_Reynolds_03})
    output$Result_03 <- renderPrint({Result_03})
  }

  outputDT1_03 <- function() {
    ReRange_03 <-
      unique(c(
        seq(10 ^ 3, 10 ^ 4, by = 10 ^ 3 / 4),
        seq(10 ^ 4, 10 ^ 5, by = 10 ^ 4 / 4),
        seq(10 ^ 5, 10 ^ 6, by = 10 ^ 5 / 4),
        seq(10 ^ 6, 10 ^ 7, by = 10 ^ 6 / 4),
        seq(10 ^ 7, 10 ^ 8, by = 10 ^ 7 / 4)
      ))
    frictionFactor_03 <- vector()
    cnt <- 1
    for (buf in 1:length(ReRange_03)) {
      frictionFactor_03[cnt] <- getLamnda_03(Reynolds = ReRange_03[buf])
      cnt <- cnt + 1
    }
    sciRe_03 <- format(ReRange_03, scientific = T)
    datatableData_03 <- data.frame(ReRange_03 , sciRe_03,  frictionFactor_03)
    colnames(datatableData_03) <- c("Re", "Re(SN)", "λ")
    rdt1_03 <- DT::renderDataTable(
      datatableData_03,
      rownames = F,
      caption = "Table : Reynolds number and Friction Factor.",
      options = list(
        autoWidth = F,
        info = T,
        lengthChange = T,
        ordering = T,
        searching = T,
        scrollX = F,
        lengthMenu = list(c(5, 10, -1), c("5", "10", "All")),
        pageLength = 10,
        orderClasses = T,
        order = list(list(0, "asc")),
        search = list(regex = T, caseInsensitive = T)
      ),
      filter = 'bottom'
    )
    output$dtTable_03 <- rdt1_03
  }

  reactiveData_03 <- reactive({
    imputData_03()
    outputResult_03()
    outputDT1_03()
  })

  output$remarktext_03 <- renderUI({
    str <- "<hr>
    <b>Remarks</b><br>
    <ol>
    <li>Loss coefficient for straight pipe：λ</li>
    <li>Roughness height：ε</li>
    <li>Pipe diameter：d</li>
    <li>Reynolds number：Re</li>
    <li>Moody chart:
    <a href=\"http://equations.am-consulting.co.jp/2014/04/08/1097/\" target=\"_blank\">http://equations.am-consulting.co.jp/2014/04/08/1097/</a></li>
    <li>Laminar:
    <a href=\"http://equations.am-consulting.co.jp/2014/04/08/1095/\" target=\"_blank\">http://equations.am-consulting.co.jp/2014/04/08/1095/</a></li>
    <li>Smooth pipes:
    <a href=\"http://equations.am-consulting.co.jp/2014/04/08/1093/\" target=\"_blank\">http://equations.am-consulting.co.jp/2014/04/08/1093/</a></li>
    <li>Rough pipes:
    <a href=\"http://equations.am-consulting.co.jp/2014/04/08/1089/\" target=\"_blank\">http://equations.am-consulting.co.jp/2014/04/08/1089/</a></li>
    <li>Colebrook-White Equation:
    <a href=\"http://equations.am-consulting.co.jp/2014/04/08/1082/\" target=\"_blank\">http://equations.am-consulting.co.jp/2014/04/08/1082/</a></li>
    <li><a href=\"http://am-consulting.co.jp\" target=\"_blank\">Asset Management Consulting Corporation</a></li>
    <li><a href=\"http://www.saecanet.com/subfolder/disclaimer.html\" target=\"_blank\">Disclaimer</a></li>
    </ol>"
    HTML(str)
  })

  output$datachecktime_03 <- renderText({
    reactiveData_03()
    completionTime_03 <- as.POSIXlt(Sys.time(), "GMT")
    paste("Completion(UTC) :" , as.character(completionTime_03))
  })
# smooth

# laminar
  frictionFunction_04 <- function(Reynolds) {
    lamnda_04 <<- 64 / Reynolds
  }

  imputData_04 <- function() {
    assign('Orig_Reynolds_04', input$slider.Re_04 * eval(parse(text = input$power.Re_04)), envir = .GlobalEnv)
  }

  outputResult_04 <- function() {
    frictionFunction_04(Reynolds =  Orig_Reynolds_04)
    output$Reynolds_04 <- renderPrint({Orig_Reynolds_04})
    output$Result_04   <- renderPrint({lamnda_04})
  }

  outputDT1_04 <- function() {
    ReRange_04 <-
      unique(c(
        seq(10 ^ 1, 10 ^ 2, by = 10 ^ 1 / 4),
        seq(10 ^ 2, 10 ^ 3, by = 10 ^ 2 / 4),
        seq(10 ^ 3, 10 ^ 4, by = 10 ^ 3 / 4)
      ))
    frictionFactor_04 <- vector()
    cnt <- 1
    for (buf in 1:length(ReRange_04)) {
      frictionFunction_04(Reynolds = ReRange_04[buf])
      frictionFactor_04[cnt] <- lamnda_04
      cnt <- cnt + 1
    }
    sciRe_04 <- format(ReRange_04, scientific = T)
    datatableData_04 <- data.frame(ReRange_04 , sciRe_04,  frictionFactor_04)
    colnames(datatableData_04) <- c("Re", "Re(SN)", "λ")
    rdt1_04 <- DT::renderDataTable(
      datatableData_04,
      rownames = F,
      caption = "Table 1:Reynolds number and Friction Factor.",
      options = list(
        autoWidth = F,
        info = T,
        lengthChange = T,
        ordering = T,
        searching = T,
        scrollX = F,
        lengthMenu = list(c(5, 10, -1), c("5", "10", "All")),
        pageLength = 10,
        orderClasses = T,
        order = list(list(0, "asc")),
        search = list(regex = T, caseInsensitive = T)
      ),
      filter = 'bottom'
    )
    output$dtTable_04 <- rdt1_04
  }

  reactiveData_04 <- reactive({
    imputData_04()
    outputDT1_04()
    outputResult_04()
  })

  output$remarktext_04 <- renderUI({
    str <- "<hr>
    <b>Remarks</b><br>
    <ol>
    <li>Loss coefficient for straight pipe：λ</li>
    <li>Roughness height：ε</li>
    <li>Pipe diameter：d</li>
    <li>Reynolds number：Re</li>
    <li>Moody chart:
    <a href=\"http://equations.am-consulting.co.jp/2014/04/08/1097/\" target=\"_blank\">http://equations.am-consulting.co.jp/2014/04/08/1097/</a></li>
    <li>Laminar:
    <a href=\"http://equations.am-consulting.co.jp/2014/04/08/1095/\" target=\"_blank\">http://equations.am-consulting.co.jp/2014/04/08/1095/</a></li>
    <li>Smooth pipes:
    <a href=\"http://equations.am-consulting.co.jp/2014/04/08/1093/\" target=\"_blank\">http://equations.am-consulting.co.jp/2014/04/08/1093/</a></li>
    <li>Rough pipes:
    <a href=\"http://equations.am-consulting.co.jp/2014/04/08/1089/\" target=\"_blank\">http://equations.am-consulting.co.jp/2014/04/08/1089/</a></li>
    <li>Colebrook-White Equation:
    <a href=\"http://equations.am-consulting.co.jp/2014/04/08/1082/\" target=\"_blank\">http://equations.am-consulting.co.jp/2014/04/08/1082/</a></li>
    <li><a href=\"http://am-consulting.co.jp\" target=\"_blank\">Asset Management Consulting Corporation</a></li>
    <li><a href=\"http://www.saecanet.com/subfolder/disclaimer.html\" target=\"_blank\">Disclaimer</a></li>
    </ol>"
    HTML(str)
  })

  output$datachecktime_04 <- renderText({
    reactiveData_04()
    completionTime_04 <- as.POSIXlt(Sys.time(), "GMT")
    paste("Completion(UTC) :" , as.character(completionTime_04))
  })
# laminar
})