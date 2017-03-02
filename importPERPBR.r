# License:GPL(version 2 or later)
# Data Source:Japan Exchange Group, Inc.
library(gdata);library(lubridate)
options(download.file.method='libcurl')

perl <- gdata:::findPerl('perl')
dataURL <-
  c('http://www.jpx.co.jp/markets/statistics-equities/misc/tvdivq00000015r6-att/j_longrange-perpbr.xls')
  Sys.sleep(1) #avoid to overload

  objsheet <- 3; title <- '連結総合(加重)'
  objcolumn <- c(1:18)
  buf0 <-
    read.xls(
      dataURL[1],
      perl = perl,
      check.names = F,
      header = F,
      stringsAsFactors = F,
      sheet = objsheet,
      fileEncoding = 'utf-8'
    )
  buf1 <- buf0[,objcolumn]
  colnames(buf1) <- gsub('\n','',unlist(buf1[2, ]))
  buf2 <- buf1[,1:(grep('二部',buf1[1,])-1)]; title <- paste0(title,':市場第一部')
  buf3 <- buf2[-c(1:2),]
  buf3[,1] <- as.Date(paste0(buf3[,1],'-',buf3[,3],'-1')) %m+% months(1) - 1
  buf4 <- buf3[,c(1,5,6)]
  buf4[,-1] <- apply(buf4[,-1,drop=F],2,function(x)as.numeric(gsub(',','',x)))
  colnames(buf4)[-1] <- paste0(title,':',colnames(buf4)[-1])
  colnames(buf4)[1] <- 'Date'
  assign(paste0('PERPBR'), buf4, envir = .GlobalEnv)
