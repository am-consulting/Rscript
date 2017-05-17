# License:GPL(version 2 or later)
# Data Source:International Monetary Fund (IMF)
# Reference http://stackoverflow.com/questions/24165623/read-excel-file-into-r-with-xlconnect-package-from-url
# Reference http://stackoverflow.com/questions/1844829/how-can-i-read-and-parse-the-contents-of-a-webpage-in-r
library(RCurl)
#library(XLConnect) need this package when import a excel file without perl. require Java.
library(gdata)
library(XML)
obj <- 1
perl <- gdata:::findPerl("perl")
# IMF Excel filename
fileNames <- "External_Data.xls"
if (1 < obj) {
  IMFhtmlpage <-
    getURL("http://www.imf.org/external/np/res/commod/index.aspx")
  IMFhtmlpage <-
    readLines(tmp <- textConnection(IMFhtmlpage))
  close(tmp)
  matchLines <-
    which(regexpr("data[[:digit:]]*?.xls", IMFhtmlpage) != -1)
  for (iii in 1:length(matchLines)) {
    tmp <-
      regexpr("data[[:digit:]]*?.xls", IMFhtmlpage[matchLines[iii]])
    assign(paste("xlsFile", iii, sep = ""),
           regmatches(IMFhtmlpage[matchLines[iii]], tmp),
           .GlobalEnv)
    fileNames <- c(fileNames, get(paste("xlsFile", iii, sep = "")))
  }
}
# IMF Excel filename
IMFURL <-
  c(
    paste(
      "http://www.imf.org/external/np/res/commod/",
      fileNames[1],
      sep = ""
    ),
    paste(
      "http://www.imf.org/external/np/res/commod/data/",
      fileNames[2],
      sep = ""
    ),
    paste(
      "http://www.imf.org/external/np/res/commod/data/",
      fileNames[3],
      sep = ""
    ),
    paste(
      "http://www.imf.org/external/np/res/commod/data/",
      fileNames[4],
      sep = ""
    )
  )
# without perl
funWithoutPERL <- function(dataURL) {
  xlsFile <-
    paste("temporary", as.numeric(Sys.time()), ".xls", sep = "")
  #importIMFdata <- function(dataURL) {
  f = CFILE(xlsFile, mode = "wb")
  curlPerform(url = dataURL,
              writedata = f@ref,
              ssl.verifypeer = FALSE)
  close(f)
  buf <<-
    readWorksheetFromFile(
      file = xlsFile,
      sheet = 1 ,
      check.names = F  ,
      header = F
    )
  #}
}
# without perl
# with perl
funWithPERL <- function(dataURL) {
  buf <<-
    read.xls(
      dataURL,
      perl = perl,
      check.names = F,
      header = F,
      stringsAsFactors = FALSE
    )
}
# with perl
for (iii in 1:obj) {
  dataURL <- IMFURL[iii]
  #funWithoutPERL(dataURL)
  funWithPERL(dataURL)
  assign(paste("origData", iii, sep = ""), buf, .GlobalEnv)
}
origData <- origData1[-(1:4),]
origData <-
  data.frame(origData[, 1], apply(origData[, -1], 2, as.numeric), check.names = F)
origData <-
  origData[, which(colSums(is.na(origData)) != nrow(origData))]
bufnames01 <- origData1[1, which(!is.na(origData1[1,]))]
bufnames02 <- origData1[3, which(!is.na(origData1[3,]))]
colnames(origData) <-
  paste(unlist(bufnames02), ". Type:", unlist(bufnames01))
origData[, 1] <-
  as.Date(paste(substr(origData[, 1], 1, 4), "-", substr(origData[, 1], 6, 7), "-1", sep =
                  ""))
colnames(origData)[1] <- "Date"
origData <- origData
# csv出力パート
scriptFile <- 'R-writeCSVtoFolder.r'
script <-
  RCurl::getURL(
    paste0("https://raw.githubusercontent.com/am-consulting/am-consulting.github.io/master/",
           scriptFile),
    ssl.verifypeer = F)
eval(parse(text = script))
fun_writeCSVtoFolder(objData = origData,dataType = 1,csvFileName = 'IMFPrimaryCommodityPrices')
# csv出力パート
