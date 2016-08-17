library(quantmod)
library(RCurl)
dgs <- c(1, 5, 10, 30)
for (ddd in 1:length(dgs)) {
  buf0 <-
    getSymbols(paste0('DGS', dgs[ddd]), src = "FRED", auto.assign = F)
  buf <-
    data.frame(
      Date = as.Date(index(buf0)),
      buf0,
      check.names = F,
      row.names = NULL
    )
  colnames(buf)[2] <-
    paste0(dgs[ddd], '-Year Treasury Constant Maturity Rate')
  if (ddd == 1) {
    DGSData <- buf
  } else{
    DGSData <- merge(DGSData, buf, all = T)
  }
}
script <-
  getURL(
    "https://raw.githubusercontent.com/am-consulting/Rscript/master/Rscript_JGBInterestRate.r",
    ssl.verifypeer = FALSE
  )
eval(parse(text = script))
colnames(jgbData)[-1] <- paste0('JGB-', colnames(jgbData)[-1])
bondInterestRate <- merge(DGSData, jgbData, all = T)
getSymbols('DEXJPUS', src = "FRED", auto.assign = T)
DEXJPUS <-
  data.frame(
    Date = as.Date(index(DEXJPUS)),
    DEXJPUS,
    check.names = F,
    row.names = NULL
  )
bondInterestRateWithUSDJPY <-
  merge(DEXJPUS, bondInterestRate, all = T)
colnames(bondInterestRateWithUSDJPY)[2] <- 'USD/JPY'