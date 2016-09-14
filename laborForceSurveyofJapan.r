# License:GPL(version 2 or later)
# Data Source:Statistics Bureau, Ministry of Internal Affairs and Communications
library(gdata)
options(download.file.method = "libcurl")
perl <- gdata:::findPerl("perl")
dataURL <-
  c("http://www.stat.go.jp/data/roudou/longtime/zuhyou/lt01-a10.xls",1)
for (iii in seq(1, length(dataURL), by = 2)) {
  buf <-
    read.xls(
      dataURL[iii],
      perl = perl,
      check.names = F,
      header = F,
      stringsAsFactors = F,
      sheet = dataURL[iii + 1],
      fileEncoding = "utf-8"
    )
  gc()
  gc()
}
for (rrr in 1:nrow(buf)) {
  tmp <- as.numeric(substr(buf[rrr, 1], 1, 4))
  if (!is.na(tmp)) {
    break
  }
}
year <- tmp
# rrr <- rrr - 4 #caution English
rrr <- rrr - 5 #caution Japanese
bufdataSet <- buf[rrr:nrow(buf), 5:ncol(buf)] #caution
for (ccc in 1:ncol(bufdataSet)) {
  if (bufdataSet[1, ccc] != "") {
    tmpcolname <- bufdataSet[1, ccc]
  }
  bufdataSet[3, ccc] <-
    paste(tmpcolname, "-",  bufdataSet[3, ccc], sep = "")
}
colnames(bufdataSet) <- bufdataSet[3, ]
# bufdataSet <- bufdataSet[-(1:3), ] # English
bufdataSet <- bufdataSet[-(1:4), ] # Japanese
startDate <- as.Date(paste(year, "-1-1", sep = ""))
origData <-
  data.frame(Date = as.Date(seq(
    startDate, length.out = nrow(bufdataSet), by = "months"
  )), bufdataSet, check.names = F)
origData[, -1] <-
  apply(origData[, -1], 2, function(x) {
    as.numeric(gsub("\\(|\\)|<|>", "", x)) #caution
  })
origData <<- origData