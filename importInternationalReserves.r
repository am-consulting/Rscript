# License:GPL(version 2 or later)
# Data Source:Ministry of Finance Japan
options(download.file.method = "libcurl")
# csv出力パート
scriptFile <- 'R-writeCSVtoFolder.r'
script <-
  RCurl::getURL(
    paste0("https://raw.githubusercontent.com/am-consulting/am-consulting.github.io/master/",
           scriptFile),
    ssl.verifypeer = F)
eval(parse(text = script))
# csv出力パート
library(quantmod);library(Nippon)
EXJPUSData <- getSymbols("EXJPUS", src = "FRED", auto.assign = FALSE)
sourceURL <-
  "http://www.mof.go.jp/international_policy/reference/official_reserve_assets/historical.csv"
tmp0 <-
  read.csv(
    sourceURL,
    header = F,
    skip = 0,
    stringsAsFactor = F,
    check.names = F,
    fileEncoding = "cp932"
  )
sheetUnit <- zen2han(tmp0[5,1])
buf1 <- tmp0[grep('月',tmp0[,2]),]
keyWord <- 'Ｉ．外貨準備及びその他外貨資産'
objRow1 <- which(apply(tmp0,1,function(x)(length(grep(keyWord,x))))!=0)
objCol1 <- which(apply(tmp0,2,function(x)(length(grep(keyWord,x))))!=0)
keyWord <- 'II．短期の外貨建債務等'
objCol2 <- which(apply(tmp0,2,function(x)(length(grep(keyWord,x))))!=0)
buf2 <-
  tmp0[objRow1:(tail(grep('月',tmp0[,2]),1)),objCol1:(objCol2-1)]
buf2 <-
  buf2[,apply(buf2,2,function(x)sum(is.na(x)))!=nrow(buf2)]
row.names(buf2) <- NULL
buf3 <- buf2[-c(1,2,4,6,8,10,12),]
row.names(buf3) <- NULL
for(objRow in 1:4){
  tmp <- ''
  for(ccc in seq(ncol(buf3))){
    if(ccc!=1 & objRow!=1){
      if(buf3[objRow-1,ccc-1]!=buf3[objRow-1,ccc]){tmp <- ''}
    }
    if(buf3[objRow,ccc]!=''){tmp <- gsub('.+\\.(.+)','\\1',zen2han(buf3[objRow,ccc]))}
    buf3[objRow,ccc] <- tmp
  }
}
colnames(buf3) <-
  gsub('\\s|:{2,5}|:$','',
       sapply(paste0(buf3[1,],':',buf3[2,],':',buf3[3,],':',buf3[4,],':',buf3[5,],':'),zen2han))
buf4 <- buf3[-c(1:5),]
buf4 <- apply(buf4,2,as.numeric)
# ドル建て(百万ドル)
buf5 <-
  data.frame(
    Date = seq(as.Date(paste0(buf1[1,3],'-',match(buf1[1,4],month.abb),'-1')),by = 'month',length.out = nrow(buf1)),
    buf4,stringsAsFactors = F,check.names = F,row.names = NULL)
buf6 <- buf5[,c(1,grep('外貨準備',colnames(buf5)))]
origData1 <- buf6[,-15]
# ドル建て(百万ドル)
#外貨準備に占める割合(%)
origData2 <-
  data.frame(Date = origData1[,1],
             round(origData1[,-1]/origData1[,2]*100,2),
             check.names = F,stringsAsFactors = F,row.names = NULL)
#外貨準備に占める割合(%)
#円建て(兆円)
exRate <-
  data.frame(subset(EXJPUSData,
                    head(origData1[,1],1) <= index(EXJPUSData) & tail(origData1[,1],1) >= index(EXJPUSData)))
buf <- origData1[,-1]*10^-6
bufcolnames <- colnames(buf)
origData3 <-
  data.frame(Date = origData1[,1],
             apply(buf,2,function(x){x*exRate[,1,drop = F]}),row.names=NULL,stringsAsFactors = F,check.names = F)
colnames(origData3)[-1] <- bufcolnames
#円建て(兆円)
#金:(重量[百万トロイオンス])
origData4 <- buf6[,c(1,15)]
#金:(重量[百万トロイオンス])
#ドル円為替レート
origData5 <-
  data.frame(Date = row.names(exRate),exRate,row.names = NULL,stringsAsFactors = F,check.names = F)
colnames(origData5)[2] <- "USD/JPY"
#ドル円為替レート
# csv出力パート
fun_writeCSVtoFolder(objData = origData1,dataType = 1,csvFileName = 'InternationalReserves_1MillionDollars')
fun_writeCSVtoFolder(objData = origData2,dataType = 1,csvFileName = 'InternationalReserves_CompositionRatio')
fun_writeCSVtoFolder(objData = origData3,dataType = 1,csvFileName = 'InternationalReserves_1trillionYen')
fun_writeCSVtoFolder(objData = origData4,dataType = 1,csvFileName = 'InternationalReservesGoldWeight_1MillionTroyOunce')
fun_writeCSVtoFolder(objData = origData5,dataType = 1,csvFileName = 'USDJPYExchangeRate')
# csv出力パート
