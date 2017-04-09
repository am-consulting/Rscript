# License:GPL(version 2 or later)
# Data Source:Japan Exchange Group, Inc.
library(gdata);library(Nippon)
options(download.file.method='libcurl')

perl <- gdata:::findPerl('perl')
dataURL <-
  c('http://www.jpx.co.jp/markets/statistics-equities/misc/tvdivq00000023wp-att/historical-genbutsu.xls')
  Sys.sleep(1) #avoid to overload
  buf <-
    read.xls(
      dataURL[1],
      perl = perl,
      check.names = F,
      header = F,
      stringsAsFactors = F,
      sheet = 1,
      fileEncoding = 'utf-8',
      na.strings = c('')
    )
  buf0 <- buf
  for(rrr in 5:9){
    buf0[rrr,] <-
      gsub('([^\\s]+)\\s.+','\\1',gsub('([^\n]+)\n.+','\\1',buf0[rrr,]))
  }
  tmp <- NA
  for(ccc in seq(ncol(buf0))){
    if(!is.na(buf0[5,ccc])){tmp <- buf0[5,ccc]}
    buf0[5,ccc] <- tmp
  }
  tmp <- NA
  for(ccc in seq(ncol(buf0))){
    if(!is.na(buf0[6,ccc])){tmp <- buf0[6,ccc]}
    if(length(grep('内国株式',buf0[5,ccc]))!=0){buf0[6,ccc] <- tmp}
  }
  tmp <- NA
  for(ccc in seq(ncol(buf0))){
    if(!is.na(buf0[7,ccc])){tmp <- buf0[7,ccc]}
    buf0[7,ccc] <- tmp
  }
  colnames(buf0) <-
    sapply(gsub(':na','',paste0(buf0[5,],':',buf0[6,],':',buf0[7,],':',buf0[8,],':',buf0[9,]),ignore.case = T),zen2han)
  colnames(buf0) <-
    gsub('立会','立会日数',colnames(buf0))
  buf1 <- buf0[-c(1:9),]
  Date <-
    as.Date(gsub('([0-9]+)/([0-9]+)','\\1-\\2-1',buf1[,1]))
  buf2 <-
    data.frame(Date,apply(buf1[,-1],2,function(x)as.numeric(gsub(',','',x))),
               check.names = F,row.names = NULL,stringsAsFactors = F)
  buf3 <-
    buf2[,apply(buf2,2,function(x)sum(is.na(x)))!=nrow(buf2)]
  assign(paste0('monthlyTradingVolumeValue'), buf3, envir = .GlobalEnv)
