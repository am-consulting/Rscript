# License:GPL(version 2 or later)
# Reference https://cran.r-project.org/web/packages/quantmod/quantmod.pdf
library(quantmod)
currencyList <-
  c(
    "USD/JPY",
    "USD/EUR",
    "USD/GBP",
    "USD/CAD",
    "USD/AUD",
    "USD/CHF",
    "USD/NZD",
    "USD/ZAR"
  )
currencyName <<-data.frame(
  Symbols=c(
    "USD",
    "JPY",
    "EUR",
    "GBP",
    "CAD",
    "AUD",
    "CHF",
    "NZD",
    "ZAR"
    ),
  Currency=c(
    "U.S. Dollar",
    "Japanese Yen",
    "Euro",
    "British Pound",
    "Canadian Dollars",
    "Australian Dollar",
    "Swiss Francs",
    "New Zealand Dollar",
    "South African Rand"
  )
)  
for (iii in 1:length(currencyList)) {
  buf <-
    getSymbols(currencyList[iii], src = "oanda", auto.assign = F, from = Sys.Date()-365*5, to = Sys.Date())
  if (iii == 1) {
    origData <- buf
  } else{
    origData <- merge(origData, buf, all = T)
  }
  gc();gc()
}
colnames(origData) <-
  gsub("USD.", "USD/", colnames(origData))
origData <-
  data.frame(
    Date = index(origData),
    origData,
    row.names = NULL,
    check.names = F
  )
origData <<- origData