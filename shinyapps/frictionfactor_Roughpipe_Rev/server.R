library(shiny)
shinyServer(function(input, output)
{
  options(scipen = 999)
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
})