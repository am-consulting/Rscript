# License:GPL(version 2 or later)
# Data Source:Statistics Bureau, Ministry of Internal Affairs and Communications
# library(gdata);library(excel.link);
# csv出力パート
scriptFile <- 'R-writeCSVtoFolder.r'
script <-
  RCurl::getURL(
    paste0("https://raw.githubusercontent.com/am-consulting/am-consulting.github.io/master/",
           scriptFile),
    ssl.verifypeer = F)
eval(parse(text = script))
# csv出力パート
library(XLConnect);library(Nippon)
options(download.file.method = "libcurl")
# perl <- gdata:::findPerl("perl")
username <- Sys.info()['user']
pathOutput <- paste0("C:/Users/", username, "/Desktop/R_Data_Write/")
setwd(pathOutput)
fileName <- 'lt01-a10.xls'
dataURL <- c("http://www.stat.go.jp/data/roudou/longtime/zuhyou/",'季節調整値','原数値')
download.file(paste0(dataURL[1],fileName), fileName , mode = 'wb')

for(sheet in 2:3){
buf <-
  readWorksheetFromFile(paste0(pathOutput, fileName),
                        sheet = iconv(dataURL[sheet],'shift_jis','utf8'),
                        check.names = F, header = F)
for (rrr in 1:nrow(buf)) {
  tmp <- as.numeric(substr(buf[rrr, 1], 1, 4))
  if (!is.na(tmp)) {
    break
  }
}
year <- tmp
rrr <- rrr - 6
bufdataSet <- buf[rrr:nrow(buf), 5:ncol(buf)]
for (ccc in 1:ncol(bufdataSet)) {
  if (!is.na(bufdataSet[1, ccc]) & bufdataSet[1, ccc] != "") {
    tmpcolname <- bufdataSet[1, ccc]
  }
  bufdataSet[3, ccc] <-
    paste(tmpcolname, "-",  bufdataSet[3, ccc], sep = "")
}
colnames(bufdataSet) <- bufdataSet[3, ]
bufdataSet <- bufdataSet[-(1:5), ]
startDate <- as.Date(paste(year, "-1-1", sep = ""))
origData <-
  data.frame(Date = as.Date(seq(startDate, length.out = nrow(bufdataSet), by = "months")),
             bufdataSet, check.names = F,stringsAsFactors = F)
origData[, -1] <-
  apply(origData[, -1], 2, function(x) {
    as.numeric(gsub("\\(|\\)|<|>", "", x)) #caution
  })
colnames(origData)[-1] <- paste0(colnames(origData)[-1],':',dataURL[sheet])
bufC <- grep('率',colnames(origData))
colnames(origData)[-bufC] <-
  gsub('-',paste0(zen2han(buf[4,5]),'-'),colnames(origData)[-bufC])
if(sheet==2){
  assign('laborforcesurvey', na.omit(origData))
}else{
  assign('laborforcesurveyNSA', na.omit(origData))
}
# csv出力パート
fun_writeCSVtoFolder(objData = na.omit(origData),dataType = 1,
                     csvFileName = paste0('労働力調査_',dataURL[sheet]))
# csv出力パート
}
