# License:GPL(version 2 or later)
# Data Source:Japan Exchange Group, Inc.
library(gdata)
options(download.file.method='libcurl')

perl <- gdata:::findPerl('perl')
dataURL <-
  c('http://www.jpx.co.jp/markets/statistics-equities/misc/tvdivq00000015r6-att/j_longrange-perpbr.xls')
for (iii in 1:length(dataURL)) { 
  Sys.sleep(1) #avoid to overload    
  switch (iii,
          objsheet <- 3) # 連結総合(加重)
  switch (iii,
          objcolumn <- c(1:18))
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
  colnames(buf) <- gsub('\n','',unlist(buf[2, ]))
  buf <- buf[-c(1:2),]
  buf[,1] <- as.Date(paste0(buf[,1],'-',buf[,3],'-',buf[,2]))
  buf <- buf[,c(1,4:8)] # 市場第一部
  buf[,-1] <- apply(buf[,-1,drop=F],2,function(x)as.numeric(gsub(',','',x)))
  assign(paste0('PERPBR'), buf, envir = .GlobalEnv)
  gc();gc()
  
  
}