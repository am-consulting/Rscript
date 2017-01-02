library(lubridate)
SD <- Sys.Date()
# year <- c((year(SD) - 3):year(SD))
year <- c((lastYear - 3):lastYear)
username <- Sys.info()['user']
pathCFTC <- paste('C:/Users/', username, '/Desktop/CFTC/', sep = '')
setwd(pathCFTC)
url <- 'http://www.cftc.gov/files/dea/history/'
cftcAll <- NULL
for (iii in 1:length(year)) {
  filename <- paste('deacot', year[iii], '.zip', sep = '')
  download.file(paste(url, 'deacot', year[iii], '.zip', sep = ''), filename, mode = 'wb')
  tmp <- read.table(unzip(filename),
                    sep = ',',
                    header = T,
                    as.is = T)
  if (colnames(tmp)[126] == 'Contract_Units') {
    colnames(tmp)[126] <- 'Contract.Units' #2015/06/21 2015年分対策用
  }
  cftcAll <- rbind(cftcAll, tmp)
}
ccc01 <- grep('Market.and.Exchange.Names', colnames(cftcAll))
ccc02 <- grep('As.of.Date.in.Form.YYYY.MM.DD', colnames(cftcAll))
ccc03 <- grep('Noncommercial.Positions.Long..All.', colnames(cftcAll))
ccc04 <- grep('Noncommercial.Positions.Short..All.', colnames(cftcAll))
ccc05 <- grep('Contract.Units', colnames(cftcAll))
cftcAll$Noncommercial.Positions.Net <- cftcAll[, ccc03] - cftcAll[, ccc04]
cftc <- cftcAll[, c(ccc01, ccc02, ccc03, ccc04, ccc05, ncol(cftcAll))]
colnames(cftc) <- c('Names', 'Date', 'Long', 'Short', 'Unit', 'Net')
cftc[, 2] <- as.Date(cftc[, 2])
currency <- c(
  'JAPANESE YEN - CHICAGO MERCANTILE EXCHANGE',
  'EURO FX - CHICAGO MERCANTILE EXCHANGE',
  'AUSTRALIAN DOLLAR - CHICAGO MERCANTILE EXCHANGE',
  'BRITISH POUND STERLING - CHICAGO MERCANTILE EXCHANGE',
  'SWISS FRANC - CHICAGO MERCANTILE EXCHANGE',
  'NEW ZEALAND DOLLAR - CHICAGO MERCANTILE EXCHANGE',
  'CANADIAN DOLLAR - CHICAGO MERCANTILE EXCHANGE',
  'RUSSIAN RUBLE - CHICAGO MERCANTILE EXCHANGE',
  'MEXICAN PESO - CHICAGO MERCANTILE EXCHANGE',
  'BRAZILIAN REAL - CHICAGO MERCANTILE EXCHANGE',
  'GOLD - COMMODITY EXCHANGE INC.'
)
unit <- c(
  '(CONTRACTS OF JPY 12,500,000)',
  '(CONTRACTS OF EUR 125,000)',
  '(CONTRACTS OF AUD 100,000)',
  '(CONTRACTS OF GBP 62,500)',
  '(CONTRACTS OF CHF 125,000)',
  '(CONTRACTS OF NZD 100,000)',
  '(CONTRACTS OF CAD 100,000)',
  '(CONTRACTS OF RUB 2,500,000)',
  '(CONTRACTS OF MXN 500,000)',
  '(CONTRACTS OF BRL 100,000)',
  '(CONTRACTS OF 100 TROY OUNCES)'
)
dataset <- list()
for (ccc in 1:length(currency)) {
  dataset[[ccc]] <- cftc[grep(currency[ccc], cftc[, 1]), ]
  dataset[[ccc]] <- dataset[[ccc]][grep(unit[ccc], dataset[[ccc]][, 5]), ]
  dataset[[ccc]] <- dataset[[ccc]][, c(2,3,4,6,1,5)]
  itemName <- substr(currency[ccc], 1, regexpr('\\s-', currency[ccc]) - 1)
  colnames(dataset[[ccc]])[2:4] <- paste(colnames(dataset[[ccc]])[2:4], '-', itemName, sep = '')
  rrr <- order(dataset[[ccc]][, 1], decreasing = FALSE)
  dataset[[ccc]] <- dataset[[ccc]][rrr, ]
  print(tail(dataset[[ccc]]))
}
