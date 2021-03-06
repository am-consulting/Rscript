# License:GPL(version 2 or later)
# Data Source:Statistics Bureau, Ministry of Internal Affairs and Communications
options(download.file.method = "libcurl")
# file <-
#   "http://www.e-stat.go.jp/SG1/estat/Csvdl.do?sinfid=000011288589" # 2010年基準
file <-
 "http://www.e-stat.go.jp/SG1/estat/Csvdl.do?sinfid=000031431706" # 2015年基準
# file <- targetURL
buf <-
  read.csv(
    file,
    header = F,
    skip = 0,
    stringsAsFactor = F,
    check.names = F,
    na.strings = c(""),
    fileEncoding = "cp932"
  )
colnames(buf) <- buf[2,] # English
colnames(buf) <- buf[1,] # Japanese
for (rrr in 1:nrow(buf)) {
  tmp <- as.numeric(substr(buf[rrr, 1], 1, 6))
  if (!is.na(tmp)) {
    break
  }
}
startDate <-
  as.Date(paste(substr(tmp, 1, 4), "-", substr(tmp, 5, 6), "-1", sep = ""))
origData <- buf[-(1:(rrr - 1)), 2:ncol(buf)]
origData <- apply(origData, 2, function(x) {
  as.numeric(x)
})
tmpDF <- data.frame(Date = seq(startDate, length.out = nrow(origData), by = "months"), origData, check.names = F)
colnames(tmpDF)[-1] <- paste0('消費者物価指数前年同月比(%):',colnames(tmpDF)[-1])
assign('consumerPriceIndexofJapan',tmpDF)
# csv出力パート
scriptFile <- 'R-writeCSVtoFolder.r'
script <-
  RCurl::getURL(
    paste0("https://raw.githubusercontent.com/am-consulting/am-consulting.github.io/master/",
           scriptFile),
    ssl.verifypeer = F)
eval(parse(text = script))
fun_writeCSVtoFolder(objData = consumerPriceIndexofJapan,dataType = 1,csvFileName = '消費者物価指数前年同月比')
# csv出力パート
