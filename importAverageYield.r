# License:GPL(version 2 or later)
# Data Source:Japan Exchange Group, Inc.
library(gdata)
options(download.file.method='libcurl')

perl <- gdata:::findPerl('perl')
dataURL <-
  c('http://www.jpx.co.jp/markets/statistics-equities/misc/tvdivq0000001w9e-att/historical-y.xls')
for (iii in 1:length(dataURL)) { 
  Sys.sleep(1) #avoid to overload    
  switch (iii,
          objsheet <- 1)
  switch (iii,
          objcolumn <- c(1:7))
  buf0 <-
    read.xls(
      dataURL[iii],
      perl = perl,
      check.names = F,
      header = F,
      stringsAsFactors = F,
      sheet = objsheet,
      fileEncoding = 'utf-8'
    )
  buf <- buf0[,objcolumn]
  colnames(buf) <- gsub('\n','',unlist(buf[5, ]))
  buf <- buf[-c(1:5),]
  colnames(buf)[1]<-'Date'
  buf[,1] <- as.Date(paste0(buf[,1],'/1'))
  buf[,-1]<-apply(buf[,-1,drop=F],2,as.numeric)
  assign(paste0('AverageYield'), buf, envir = .GlobalEnv)
  gc();gc()
}