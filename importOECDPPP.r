sourceURL <-
  "https://stats.oecd.org/sdmx-json/data/DP_LIVE/.PPP.TOT../OECD?contentType=csv&detail=code&separator=comma&csv-lang=en"
tmp <-
  read.csv(
    sourceURL,
    header = T,
    skip = 0,
    stringsAsFactor = F,
    check.names = F
  )
colnames(tmp)[1] <- "LOCATION"
currency <- unique(tmp[, 1])
dataSet <- list()
for (c in 1:length(currency)) {
  dataSet[[c]] <- subset(tmp, tmp[, 1] == currency[c])
  colnames(dataSet[[c]])[7] <- unique(dataSet[[c]][, 1])
  if (c == 1) {
    origData <-
      dataSet[[c]][, 6:7]
  } else{
    origData <- merge(origData, dataSet[[c]][, 6:7], all = T)
  }
}
origData[, -1] <- apply(origData[, -1], 2, as.numeric)
colnames(origData)[-1]<-paste(colnames(origData)[-1]," to one USA",sep="")
origData <<- origData