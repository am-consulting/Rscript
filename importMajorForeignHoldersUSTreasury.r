# License:GPL(version 2 or later)
# Data Source:U.S. Department of the Treasury
# Reference https://www.treasury.gov/resource-center/data-chart-center/tic/Pages/ticsec2.aspx
options(download.file.method = "libcurl")
nSkipRow <- 14
targetWord <- "Notes:"
url <- c(
  "http://www.treasury.gov/resource-center/data-chart-center/tic/Documents/slt3d_globl.csv"
)
Sys.sleep(1)
buf <-
  read.csv(
    url,
    header = F,
    skip = nSkipRow,
    stringsAsFactor = F,
    na.strings = c("-", "－", "n.a.")
  )
origData <- buf
ntmp <- which(origData[, 1] == targetWord)
origData <- head(origData, ntmp - 1)
origData <- origData[, c(3, 4, 1)]
origData[, 1] <- as.Date(paste(origData[, 1], "-1", sep = ""))
origData[, 2] <- as.numeric(gsub(",", "", origData[, 2]))
origData <- na.omit(origData)
colnames(origData) <-
  c("Date", "Total Holdings(Millions of dollars)", "Country･Region")
origData<<-origData