# License:GPL(version 2 or later)
# Data Source:Federal Reserve Bank of St. Louis
library(quantmod)
currencyList <-
  c(
    "DEXJPUS",
    "DEXSFUS",
    "DEXUSAL",
    "DEXUSEU",
    "DEXUSUK",
    "DEXCAUS",
    "DEXSZUS",
    "DEXUSNZ",
    "DTWEXB"
  )
currencyName <<-data.frame(
  Symbols=c("US","JP","SF","AL","EU","UK","CA","SZ","NZ","DTWEXB"),
  Currency=
  c(
    "U.S. Dollar",
    "Japanese Yen",
    "South African Rand",
    "Australian Dollar",
    "Euro",
    "British Pound",
    "Canadian Dollars",
    "Swiss Francs",
    "New Zealand Dollar",
    "Trade Weighted U.S. Dollar Index: Broad"
  )
)  
#ttt<-proc.time()
for (iii in 1:length(currencyList)) {
  Sys.sleep(2) #avoid to overload
  buf <-
    getSymbols(currencyList[iii], src = "FRED", auto.assign = FALSE)
  if (length(grep("DEXUS", currencyList[iii])) != 0) {
    buf <- 1 / buf
    colnames(buf) <-
      paste("DEX", substr(currencyList[iii], 6, 7), "US", sep = "")
  }
  if (iii == 1) {
    origData <- buf
  } else{
    origData <- merge(origData, buf, all = T)
  }
  gc();gc()
}
colnames(origData) <-
  gsub("US", "/US", gsub("DEX", "", colnames(origData)))
origData <-
  data.frame(
    Date = index(origData),
    origData,
    row.names = NULL,
    check.names = F
  )
origData <<- origData
#proc.time()-ttt