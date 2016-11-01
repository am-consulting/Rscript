# License:GPL(version 2 or later)
# Data Source:Japan Exchange Group, Inc.
library(gdata)
options(download.file.method='libcurl')

perl <- gdata:::findPerl('perl')
dataURL <-
  c('http://www.jpx.co.jp/markets/statistics-equities/misc/tvdivq0000001w3y-att/historical-jika.xls')
  Sys.sleep(1) #avoid to overload
  objcolumn <- c(2:8)
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
  buf1 <- buf0[, c(1, objcolumn)]
  colnames(buf1) <-  paste0(buf1[1,1],'-',gsub('\n','',unlist(buf1[3, ])))
  buf2 <- buf1[-c(1:3), ]
  buf2[,-1] <- apply(buf2[,-1],2,function(x)as.numeric(gsub(',','',x))*10^-6) # 兆円
  buf2[,1]  <- as.Date(buf2[,1])
  colnames(buf2)[1] <- 'Date'
  assign(paste0('marketCapitalization'), buf2, envir = .GlobalEnv)