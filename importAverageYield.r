# License:GPL(version 2 or later)
# Data Source:Japan Exchange Group, Inc.
library(gdata)
options(download.file.method='libcurl')

perl <- gdata:::findPerl('perl')
dataURL <-
  c('http://www.jpx.co.jp/markets/statistics-equities/misc/tvdivq0000001w9e-att/historical-y.xls')
  Sys.sleep(1) #avoid to overload
  objcolumn <- c(1:7)
  buf0 <-
    read.xls(
      dataURL[1],
      perl = perl,
      check.names = F,
      header = F,
      stringsAsFactors = F,
      sheet = 1,
      fileEncoding = 'utf-8'
    )
  buf1 <- buf0[,objcolumn]
  colnames(buf1) <- paste0(buf1[1,1],'-',gsub('\n','',unlist(buf1[5, ])))
  buf2 <- buf1[-c(1:5),]
  colnames(buf2)[1]<-'Date'
  buf2[, 1] <- as.Date(paste0(buf2[,1],'/1'))
  buf2[,-1] <- apply(buf2[,-1,drop=F],2,as.numeric)
  assign(paste0('averageYield'), buf2, envir = .GlobalEnv)