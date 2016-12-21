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
           readWorksheetFromFile(paste0(pathOutput, xlsList[fff]), sheet = 1, check.names = F, header = F),envir = .GlobalEnv)
  }
}
# 原数値パート
fff <- 1
buf01 <- get(paste0('origData',formatC(fff,width=2,flag="0")))
buf01 <- buf01[,1:3]
title01 <- buf01[1,2]
buf01[buf01=='　'] <- NA
colnames(buf01) <- buf01[7,]
buf01 <- buf01[-c(1:7),]
buf01 <- na.omit(buf01)
buf01[,1] <- unlist(lapply(buf01[,1],zen2han))
bufrrr <- tail(grep('H',buf01[,1]),1)
yyyy <- as.numeric(gsub('H','',substring(buf01[bufrrr,1],1,3))) + 1988
if(nrow(buf01) != bufrrr){mm <- as.numeric(tail(buf01[,1],1))}else{mm <- as.numeric(gsub('月','',substring(buf01[bufrrr,1],5)))}
rrrS <- which(buf01[,1]=='S41年 1月')
buf01 <- buf01[rrrS:nrow(buf01),]
buf01[,1] <- rev(seq(as.Date(paste0(yyyy,'-',mm,'-1')),by = '-1 month',length.out = nrow(buf01)))
colnames(buf01) <- c('Date','新設住宅着工戸数(戸)','新設住宅着工戸数前年比(%)')
buf01[,-1] <- apply(buf01[,-1],2,function(x)as.numeric(gsub(',','',x)))
NSAData <- buf01
# 原数値パート
# 季節調整パート
fff <- 2
buf02 <- get(paste0('origData',formatC(fff,width=2,flag="0")))
buf02 <- buf02[,1:3]
title02 <- buf02[1,2]
buf02[buf02=='　'] <- NA
colnames(buf02) <- buf02[3,]
buf02 <- buf02[-c(1:5),]
buf02 <- na.omit(buf02)
buf02 <- tail(buf02,nrow(buf01))
buf02[,1] <- buf01[,1]
colnames(buf02) <- c('Date','新設住宅着工戸数(戸,季節調整値)','新設住宅着工戸数季節調整値前月比(%)')
buf02[,-1] <- apply(buf02[,-1],2,function(x)as.numeric(gsub(',','',x)))
SAData <- buf02
# 季節調整パート
