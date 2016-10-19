library(shiny)
shinyServer(function(input, output)
{
  options(scipen = 999)

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
})