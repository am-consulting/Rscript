library(shiny)
shinyServer(function(input, output)
{
  options(scipen = 999)
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
})