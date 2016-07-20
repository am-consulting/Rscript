# License:GPL(version 2 or later)
# Data Source:U.S. Energy Information Administration , OANDA Corporation - https://www.oanda.com/
library(quantmod)
library(gdata)
options(download.file.method="libcurl")
commodityList <-
  c(
    "XAU/USD"
    )
commodityName <<- data.frame(
  Symbols = 
    c(
      "XAU"
      ),
  Commodity =
    c(
      "Gold(oz.)"
    ),stringsAsFactors = F
)
for (iii in 1:length(commodityList)) {
  if(commodityList[iii]=="XAU/USD"){srcFrom<-"oanda"}else{srcFrom<-"FRED"}
  buf <-
    getSymbols(commodityList[iii], src = srcFrom, auto.assign = F, from = Sys.Date()-365*5, to = Sys.Date())
  if (iii == 1) {
    origData <- buf
  } else{
    origData <- merge(origData, buf, all = T)
  }
  gc();gc()
}
origData <-
  data.frame(
    Date = index(origData),
    origData,
    row.names = NULL,
    check.names = F
  )
colnames(origData)[-1]<-commodityName[,2]

buf <- getSymbols("DCOILWTICO", src = "FRED", auto.assign = F)
currencyData <-
  data.frame(
    Date = as.Date(index(buf)),
    buf,
    check.names = F,
    row.names = NULL
  )
origData <- merge(origData, currencyData, by = "Date", all = T)

perl <- gdata:::findPerl("perl")
dataURL <-
  c("http://ir.eia.gov/wpsr/psw01.xls") # "http://ir.eia.gov/wpsr/psw11.xls"
for (iii in 1:length(dataURL)) { 
  Sys.sleep(1) #avoid to overload    
  switch (iii,
          objsheet <- 2,
          objsheet <- 2)
  switch (iii,
          objcolumn <- 3,
          objcolumn <- c(2:3))
  buf <-
    read.xls(
      dataURL[iii],
      perl = perl,
      check.names = F,
      header = F,
      stringsAsFactors = F,
      sheet = objsheet
    )

  buf <- buf[, c(1, objcolumn)]
  colnames(buf) <-  unlist(buf[3, ])
  buf <- buf[-c(1:3), ]
  if (iii == 1) {
    EIAData <- buf
  } else{
    EIAData <- merge(EIAData, buf, by = "Date", all = T)
  }
  gc();gc()
}
EIAData[, 1] <-
  as.Date(paste(
    substr(EIAData[, 1], 9, 12),
    "-",
    match(substr(EIAData[, 1], 1, 3), month.abb),
    "-",
    substr(EIAData[, 1], 5, 6),
    sep = ""
  ))
EIAData <- EIAData[order(EIAData[, 1]), ]
origData <- merge(origData, EIAData, by = "Date", all = T)
origData[,-1]<-apply(origData[,-1],2,as.numeric)
origData<<-origData