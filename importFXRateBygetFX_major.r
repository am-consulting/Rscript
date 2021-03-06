# License:GPL(version 2 or later)
# Reference https://cran.r-project.org/web/packages/quantmod/quantmod.pdf
library(quantmod)
options(download.file.method="libcurl")
currencyList <-
  c(
    "USD/JPY",
    "USD/EUR",
    "USD/GBP"
  )
currencyName <<-data.frame(
  Symbols=c(
    "USD",
    "JPY",
    "EUR",
    "GBP"
    ),
  Currency=c(
    "U.S. Dollar",
    "Japanese Yen",
    "Euro",
    "British Pound"
  )
)  
for (iii in 1:length(currencyList)) {
  Sys.sleep(2) #avoid to overload  
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
origData_FXoanda <<- origData