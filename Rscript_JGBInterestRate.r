# License:GPL(version 2 or later)
# Interest rate data source:Ministry of Finance Japan( http://www.mof.go.jp/english/index.htm )
jgb <- function() {
  jgbDataSource <<- "Ministry of Finance Japan"
  jgbBaseURL <- "http://www.mof.go.jp/english/jgbs/reference/interest_rate/"
  jgbFiles <- c("historical/jgbcme_all.csv", "jgbcme.csv")
  Sys.sleep(1)
  historicalData <- read.csv(paste(jgbBaseURL, jgbFiles[1], sep = ""), 
    header = T, skip = 1, stringsAsFactor = F, check.names = F, na.strings = c("-"))
  Sys.sleep(1)
  currentData <- read.csv(paste(jgbBaseURL, jgbFiles[2], sep = ""), header = T, 
    skip = 1, stringsAsFactor = F, check.names = F, na.strings = c("-"))
  buf <- colnames(historicalData) == colnames(currentData)
  if (length(buf[buf == F]) == 0) {
    tmp <- rbind(historicalData, currentData)
    tmp[, 1] <- as.Date(tmp[, 1])
    tmp <- data.frame(tmp[1], apply(tmp[-1], 2, function(x) {
      as.numeric(x)
    }), check.names = F)
    jgbData <<- tmp
  } else {
    cat("Some sort of change(s) in source data.")
  }
}