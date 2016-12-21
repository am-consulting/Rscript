# License:GPL(version 2 or later)
# Data Source:Statistics Bureau, Ministry of Internal Affairs and Communications
# library(gdata);library(excel.link);
library(XLConnect);library(Nippon)
options(download.file.method = "libcurl")
# perl <- gdata:::findPerl("perl")
username <- Sys.info()['user']
pathOutput <- paste0("C:/Users/", username, "/Desktop/R_Data_Write/")
setwd(pathOutput)
fileName <- 'lt01-a10.xls'
dataURL <- c("http://www.stat.go.jp/data/roudou/longtime/zuhyou/",'季節調整値')#1) # sheet1:季節調整値
download.file(paste0(dataURL[1],fileName), fileName , mode = 'wb')
buf <-
  readWorksheetFromFile(paste0(pathOutput, fileName), sheet = iconv(dataURL[2],'shift_jis','utf8'), check.names = F, header = F)
# for (iii in seq(1, length(dataURL), by = 2)) {
#   buf <-
#     read.xls(
#       dataURL[iii],
#       perl = perl,
#       check.names = F,
#       header = F,
#       stringsAsFactors = F,
#       sheet = iconv(dataURL[iii + 1],'shift_jis','utf8'),
#       fileEncoding = "utf-8"
#     )
#   gc()
#   gc()
# }
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
  data.frame(Date = as.Date(seq(startDate, length.out = nrow(bufdataSet), by = "months")), bufdataSet, check.names = F)
origData[, -1] <-
  apply(origData[, -1], 2, function(x) {
    as.numeric(gsub("\\(|\\)|<|>", "", x)) #caution
  })
colnames(origData)[-1] <- paste0(colnames(origData)[-1],':',dataURL[2])
bufC <- grep('率',colnames(origData))
colnames(origData)[-bufC] <-
  gsub('-',paste0(zen2han(buf[4,5]),'-'),colnames(origData)[-bufC])
assign('laborforcesurvey', na.omit(origData), envir = .GlobalEnv)
