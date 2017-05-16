# License:GPL(version 2 or later)
# Data Source:U.S. Department of the Treasury
# Reference https://www.treasury.gov/resource-center/data-chart-center/tic/Pages/ticsec2.aspx
options(download.file.method = "libcurl")
nSkipRow <- 14
targetWord <- "Notes:"
url <- c(
  "https://www.treasury.gov/resource-center/data-chart-center/tic/Documents/slt3d_globl.csv"
)
Sys.sleep(1)
buf <-
  read.csv(
    url[1],
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
bufDate<-unique(origData[,1])
for(ccc in 1:length(bufDate)) {
  bufData0 <- subset(origData, bufDate[ccc] == origData[, 1])
  bufData1 <- t(bufData0[, 2])
  colnames(bufData1) <- bufData0[, 3]
  bufData1 <- data.frame(Date = bufDate[ccc], bufData1, check.names = F)
  if (ccc == 1) {
    allData <- bufData1
  } else{
    allData <- merge(allData, bufData1, all =  T)
  }
}
# allData<<-allData
# csv出力パート
scriptFile <- 'R-writeCSVtoFolder.r'
script <-
  RCurl::getURL(
    paste0("https://raw.githubusercontent.com/am-consulting/am-consulting.github.io/master/",
           scriptFile),
    ssl.verifypeer = F)
eval(parse(text = script))
fun_writeCSVtoFolder(objData = allData,dataType = 1,csvFileName = 'MajorForeignHoldersUSTreasury')
# csv出力パート
