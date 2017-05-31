# License:GPL(version 2 or later)
# Data Source:Cabinet Office, Government Of Japan
# csv List part
library(RCurl);library(XLConnect);library(Nippon)
username <- Sys.info()['user']
pathOutput <- paste0("C:/Users/", username, "/Desktop/R_Data_Write/")
setwd(pathOutput)
sourceURL <- 'http://www.mlit.go.jp/sogoseisaku/jouhouka/sosei_jouhouka_tk4_000002.html'
htmlMarkup <- getURL(sourceURL,.encoding = 'utf-8')
pattern <- "(<a\\shref=\".*?\\.xls\">)+?.+?</a>"
buf <- unlist(regmatches(htmlMarkup, gregexpr(pattern, htmlMarkup, fixed = F)))
pattern <- '[0-9].+?\\.xls'
xlsList <- unlist(regmatches(buf, gregexpr(pattern, buf, fixed = F)))
titleList <- gsub('　','',gsub('<.*?a.*?>','',buf))
titleList <- unlist(lapply(titleList,zen2han))
url0 <- 'http://www.mlit.go.jp/common/'
for(fff in 1:length(xlsList)){
  if(length(grep('住宅・建築物',titleList[fff]))!=0){
    download.file(paste0(url0, xlsList[fff]), xlsList[fff], mode = "wb")
    assign(paste0('origData',formatC(fff,width=2,flag="0")),
           readWorksheetFromFile(paste0(pathOutput, xlsList[fff]), sheet = 1, check.names = F, header = F),
           envir = .GlobalEnv)
  }
}
# 原数値パート
fff <- 1
buf01 <- get(paste0('origData',formatC(fff,width=2,flag="0")))
title01 <- buf01[1,2]
buf01 <- buf01[,1:3]
buf01 <- buf01[!is.na(buf01[,1]),]
buf01 <- buf01[!is.na(buf01[,2]),]
buf01[,1] <- gsub('\\s','',sapply(buf01[,1],zen2han))
buf01 <- buf01[grep('S[0-9]+年',buf01[,1],ignore.case = T)[1]:nrow(buf01),]
yyyy <-
  as.numeric(gsub('S([0-9]+)年.+','\\1',buf01[1,1],ignore.case = T)) + 1925
mm <-
  as.numeric(gsub('S.+年([0-9]+)月','\\1',buf01[1,1],ignore.case = T))
buf01[,1] <-
  seq(as.Date(paste0(yyyy,'-',mm,'-1')),by = '+1 month',length.out = nrow(buf01))
colnames(buf01) <-
  c('Date','新設住宅着工戸数(戸)','新設住宅着工戸数前年比(%)')
buf01[,-1] <-
  apply(buf01[,-1],2,function(x)as.numeric(gsub(',','',x)))
row.names(buf01) <- NULL
NSAData <- buf01
# 原数値パート
# 季節調整パート
fff <- 2
buf02 <- get(paste0('origData',formatC(fff,width=2,flag="0")))
title02 <- buf02[1,2]
buf02 <- buf02[,1:3]
buf02 <- buf02[!is.na(buf02[,1]),]
buf02 <- buf02[!is.na(buf02[,2]),]
buf02[,1] <- gsub('\\s','',sapply(buf02[,1],zen2han))
buf02 <- buf02[grep('[0-9]+年',buf02[,1],ignore.case = T)[1]:nrow(buf02),]
yyyy <-
  as.numeric(gsub('([0-9]+)年.+','\\1',buf02[1,1],ignore.case = T)) + 1925
mm <-
  as.numeric(gsub('.+年([0-9]+)月','\\1',buf02[1,1],ignore.case = T))
buf02[,1] <-
  seq(as.Date(paste0(yyyy,'-',mm,'-1')),by = '+1 month',length.out = nrow(buf02))
colnames(buf02) <-
  c('Date','新設住宅着工戸数(戸,季節調整値)','新設住宅着工戸数季節調整値前月比(%)')
buf02[,-1] <-
  apply(buf02[,-1],2,function(x)as.numeric(gsub(',','',x)))
row.names(buf02) <- NULL
SAData <- buf02
# 季節調整パート
# csv出力パート
scriptFile <- 'R-writeCSVtoFolder.r'
script <-
  RCurl::getURL(
    paste0("https://raw.githubusercontent.com/am-consulting/am-consulting.github.io/master/",
           scriptFile),
    ssl.verifypeer = F)
eval(parse(text = script))
fun_writeCSVtoFolder(objData = NSAData,dataType = 1,csvFileName = '新設住宅着工戸数_戸_原数値')
fun_writeCSVtoFolder(objData = SAData,dataType = 1,csvFileName = '新設住宅着工戸数_戸_季節調整値')
# csv出力パート
