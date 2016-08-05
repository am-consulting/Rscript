# License:GPL(version 2 or later)
# Data Source:Japan Exchange Group, Inc.
library(gdata)
options(download.file.method='libcurl')

perl <- gdata:::findPerl('perl')
dataURL <-
  c('http://www.jpx.co.jp/markets/statistics-equities/misc/tvdivq00000023wp-att/historical-genbutsu.xls')
for (iii in 1:length(dataURL)) { 
  Sys.sleep(1) #avoid to overload    
  switch (iii,
          objsheet <- 1)
  switch (iii,
          objcolumn <- c(2:8))
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
  buf <- buf0[-c(1:9),c(1,4)]
  buf[,1] <- as.Date(paste0(buf[,1],'/1'))
  colnames(buf)<-c('Date','一部･1日平均売買高(億株)')
  buf[,-1]<-apply(buf[,-1,drop=F],2,function(x)as.numeric(gsub(',','',x))*10^-5)
  assign(paste0('MonthlyTradingVolumeValue'), buf, envir = .GlobalEnv)
  gc();gc()
}