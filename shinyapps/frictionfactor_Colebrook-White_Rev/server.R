library(shiny)
shinyServer(function(input, output, session)
{
  options(scipen = 999)

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
})