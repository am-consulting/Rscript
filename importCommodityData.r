# License:GPL(version 2 or later)
# Data Source:Federal Reserve Bank of St. Louis
library(quantmod)
library(gdata)
currencyList <-
  c("DCOILWTICO",
    "GOLDAMGBD228NLBM",
    "DCOILBRENTEU",
    "DTWEXB")
commodityName <<- data.frame(
  Symbols = c("DCOILWTICO", "GOLDAMGBD228NLBM", "DCOILBRENTEU", "DTWEXB"),
  Currency =
    c(
      "Crude Oil Prices: West Texas Intermediate (WTI) - Cushing, Oklahoma",
      "Gold Fixing Price 10:30 A.M. (London time) in London Bullion Market, based in U.S. Dollars",
      "Crude Oil Prices: Brent - Europe",
      "Trade Weighted U.S. Dollar Index: Broad"
    ),stringsAsFactors = F
)
for (iii in 1:length(currencyList)) {
  buf <-
    getSymbols(currencyList[iii], src = "FRED", auto.assign = FALSE)
  if (iii == 1) {
    origData <- buf
  } else{
    origData <- merge(origData, buf, all = T)
  }
}
origData <-
  data.frame(
    Date = index(origData),
    origData,
    row.names = NULL,
    check.names = F
  )
colnames(origData)[-1]<-commodityName[,2]
perl <- gdata:::findPerl("perl")
dataURL <-
  c("http://ir.eia.gov/wpsr/psw01.xls",
    "http://ir.eia.gov/wpsr/psw09.xls")
for (iii in 1:length(dataURL)) {
  switch (iii,
          objsheet <- 2,
          objsheet <- 7)
  switch (iii,
          objcolumn <- 3,
          objcolumn <- 6)
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