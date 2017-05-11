# License:GPL(version 2 or later)
# Data Source:Ministry of Finance Japan
library(Nippon)
# csv出力パート
scriptFile <- 'R-writeCSVtoFolder.r'
script <-
  RCurl::getURL(
    paste0("https://raw.githubusercontent.com/am-consulting/am-consulting.github.io/master/",
           scriptFile),
    ssl.verifypeer = F)
eval(parse(text = script))
# csv出力パート
sourceURL <- c(
  "http://www.mof.go.jp/international_policy/reference/balance_of_payments/bp_trend/bpnet/sbp/s-1/6s-1-4.csv",
  "http://www.mof.go.jp/international_policy/reference/balance_of_payments/bp_trend/bpnet/sbp/s-2/6s-2-4.csv",
  "http://www.mof.go.jp/international_policy/reference/balance_of_payments/bp_trend/bpnet/sbp/s-3/6s-3-4.csv",
  "http://www.mof.go.jp/international_policy/reference/balance_of_payments/bp_trend/bpnet/sbp/s-4/6s-4-4.csv")
username <- Sys.info()['user']
pathOutput <- paste0("C:/Users/", username, "/Desktop/R_Data_Write/")
setwd(pathOutput)
sheetUnit <- sheetTitle <- vector()
for (ddd in 1:length(sourceURL)) {
  Sys.sleep(1)
  tmp0 <-
    read.csv(
      sourceURL[ddd],
      header = F,
      skip = 0,
      stringsAsFactor = F,
      check.names = F,
      fileEncoding = "cp932")
  setwd(pathOutput)
  download.file(url = sourceURL[ddd],destfile = paste0('origData',ddd,'.csv'),mode = 'wb')
  objRow <- !is.na(as.numeric(gsub(',','',tmp0[,5])))
  buf1 <- tmp0[objRow,]
  sheetUnit[ddd] <-
    gsub('.+単位.?(.+)\\)','\\1',zen2han(tmp0[5,1]))
  sheetTitle[ddd] <-
    gsub('.+\\.','',zen2han(tmp0[2,1]))
  if(ddd==1){
    keyWord1 <- '^経常収支.+\\)$'
    namesRow <- c(1,3,5,6)}
  if(ddd==2){
    keyWord1 <- '^サービス収支$'
    namesRow <- c(1,3,4)}
  if(ddd==3){
    keyWord1 <- '^第一次所得収支$'
    namesRow <- c(1,3,5,6)}
  if(ddd==4){
    keyWord1 <- '^金融収支.?$'
    namesRow <- c(1,3,4)}
  objRow1 <- which(apply(tmp0,1,function(x)(length(grep(keyWord1,x,ignore.case = T))))!=0)
  objCol1 <- which(apply(tmp0,2,function(x)(length(grep(keyWord1,x,ignore.case = T))))!=0)
  buf2 <-
    tmp0[objRow1:tail(which(objRow),1),objCol1:ncol(tmp0)]
  buf3 <-
    buf2[,apply(buf2,2,function(x)sum(is.na(x)))!=nrow(buf2)]
  row.names(buf3) <- NULL
  for(objRow in seq(length(namesRow))){
    tmp <- ''
    for(ccc in seq(ncol(buf3))){
      if(ccc!=1 & objRow!=1){
        if(buf3[namesRow[objRow-1],ccc-1]!=buf3[namesRow[objRow-1],ccc]){tmp <- ''}
      }
      if(buf3[namesRow[objRow],ccc]!=''){tmp <- gsub('\\(.+\\)|\\s','',zen2han(buf3[namesRow[objRow],ccc]))}
      buf3[namesRow[objRow],ccc] <- tmp
    }
  }
  gsubPattern <- '\\s|:{2,5}|:$|:[a-z]+'
  if(length(namesRow)==4){
  colnames(buf3) <-
    gsub(gsubPattern,'',
         sapply(paste0(buf3[namesRow[1],],':',
                       buf3[namesRow[2],],':',
                       buf3[namesRow[3],],':',
                       buf3[namesRow[4],]),zen2han),ignore.case = T)
  }
  if(length(namesRow)==3){
    colnames(buf3) <-
      gsub(gsubPattern,'',
           sapply(paste0(buf3[namesRow[1],],':',
                         buf3[namesRow[2],],':',
                         buf3[namesRow[3],]),zen2han),ignore.case = T)
  }
  buf4 <-
    buf3[head(which(!is.na(as.numeric(gsub(',','',buf3[,1])))),1):nrow(buf3),]
  row.names(buf4) <- NULL
  Date <-
    seq(as.Date(paste0(buf1[1,3],'-',gsub('月','',buf1[1,2]),'-1')),by='month',length.out=nrow(buf1))
  buf4 <- apply(buf4,2,function(x)as.numeric(gsub(',','',x)))
  buf5 <-
    data.frame(Date,buf4,check.names = F,stringsAsFactors = F,row.names = NULL)
  fun_writeCSVtoFolder(objData = buf5,dataType = 1,
                       csvFileName = paste0(sheetTitle[ddd],'_',sheetUnit[ddd]))
  assign(paste0("origData",ddd),buf5)
}
