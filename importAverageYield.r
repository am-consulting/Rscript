# License:GPL(version 2 or later)
# Data Source:Japan Exchange Group, Inc.
library(gdata);library(Nippon)
options(download.file.method='libcurl')

perl <- gdata:::findPerl('perl')
dataURL <-
  c('http://www.jpx.co.jp/markets/statistics-equities/misc/tvdivq0000001w9e-att/historical-y.xls')
  Sys.sleep(1) #avoid to overload
  buf0 <-
    read.xls(
      dataURL[1],
      perl = perl,
      check.names = F,
      header = F,
      stringsAsFactors = F,
      sheet = 1,
      fileEncoding = 'utf-8')
  buf1 <- buf0
  buf1[4,] <- gsub('[a-zA-Z0-9\\.]+|\n|\\s','',buf1[4,])
  buf1[5,] <- gsub('[a-zA-Z0-9\\.]+|\n|\\s','',buf1[5,])
  buf2 <- buf1[,!is.na(buf1[1,])]
  tmp <- NA
  for(ccc in seq(ncol(buf2))){
    if(buf2[4,ccc]!=''){tmp <- buf2[4,ccc]}
    buf2[4,ccc] <- tmp
  }
  colnames(buf2) <- sapply(paste0(buf2[4,],':',buf2[5,],buf2[3,ncol(buf2)]),zen2han)
  buf3 <- buf2[!is.na(as.numeric(buf2[,2])),]
  colnames(buf3)[1] <- 'Date'
  buf3[, 1] <- as.Date(paste0(buf3[,1],'/1'))
  buf3[,-1] <- apply(buf3[,-1,drop=F],2,as.numeric)
  row.names(buf3) <- NULL
  assign(paste0('averageYield'), buf3, envir = .GlobalEnv)
