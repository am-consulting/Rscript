# License:GPL(version 2 or later)
# Data Source:Japan Exchange Group, Inc.
library(gdata);library(Nippon)
options(download.file.method='libcurl')

perl <- gdata:::findPerl('perl')
dataURL <-
  c('http://www.jpx.co.jp/markets/statistics-equities/misc/tvdivq0000001w3y-att/historical-jika.xls')
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
  buf1 <- buf0[,!is.na(buf0[1,])]
  obj.col <- which(gsub('[a-zA-Z0-9\n]+|\\s','',buf1[3,])=='')
  buf1[3,-obj.col] <- gsub('([^a-zA-Z0-9\n]+).+','\\1',buf1[3,-obj.col])
  colnames(buf1) <-  paste0(gsub('[a-zA-Z0-9\n]+|\\s','',buf1[1,1]),':',buf1[3,])
  buf2 <- buf1[!is.na(as.numeric(gsub(',','',buf1[,2]))),]
  buf2[,-1] <- apply(buf2[,-1],2,function(x)as.numeric(gsub(',','',x))*10^-6) # 兆円
  colnames(buf2)[-1] <- sapply(paste0(colnames(buf2)[-1],'(兆円)'),zen2han)
  buf2[,1]  <- as.Date(buf2[,1])
  colnames(buf2)[1] <- 'Date'
  row.names(buf2) <- NULL
  assign(paste0('marketCapitalization'), buf2, envir = .GlobalEnv)
