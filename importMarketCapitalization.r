# License:GPL(version 2 or later)
# Data Source:Japan Exchange Group, Inc.
library(gdata)
options(download.file.method='libcurl')

perl <- gdata:::findPerl('perl')
dataURL <-
  c('http://www.jpx.co.jp/markets/statistics-equities/misc/tvdivq0000001w3y-att/historical-jika.xls')
for (iii in 1:length(dataURL)) { 
  Sys.sleep(1) #avoid to overload    
  switch (iii,
          objsheet <- 1)
  switch (iii,
          objcolumn <- c(2:8))
  buf <-
    read.xls(
      dataURL[iii],
      perl = perl,
      check.names = F,
      header = F,
      stringsAsFactors = F,
      sheet = objsheet,
      fileEncoding = 'utf-8'
    )
  buf <- buf[, c(1, objcolumn)]
  colnames(buf) <-  gsub('\n','',unlist(buf[3, ]))
  buf <- buf[-c(1:3), ]
  buf[,-1]<-apply(buf[,-1],2,function(x)as.numeric(gsub(',','',x))*10^-6)
  buf[,1]<-as.Date(buf[,1])
  assign(paste0('MarketCapitalization'), buf, envir = .GlobalEnv)
  gc();gc()
}