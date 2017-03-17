# License:GPL(version 2 or later)
# Data Source:Japan Exchange Group, Inc.
library(gdata)
options(download.file.method='libcurl')

perl <- gdata:::findPerl('perl')
dataURL <-
  c('http://www.jpx.co.jp/markets/statistics-equities/misc/tvdivq00000023wp-att/historical-genbutsu.xls')
  Sys.sleep(1) #avoid to overload
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
  buf1 <- buf0[,1:(c(grep('二部',buf0[6,]))-1)]
  buf2 <- buf1[,c(1,grep('平均',buf1))]
  buf  <- buf2[-c(1:9),]
  buf[,1] <- as.Date(paste0(buf[,1],'/1'))
  colnames(buf)<-c('Date','一部･1日平均売買高(億株)','一部･1日平均売買代金高(兆円)')
  buf[,2] <- sapply(buf[,2],function(x)as.numeric(gsub(',','',x))*10^-5)
  buf[,3] <- sapply(buf[,3],function(x)as.numeric(gsub(',','',x))*10^-6)
  assign(paste0('monthlyTradingVolumeValue'), buf, envir = .GlobalEnv)
