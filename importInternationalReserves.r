# License:GPL(version 2 or later)
# Data Source:Ministry of Finance Japan
options(download.file.method = "libcurl")
library(quantmod)
sourceURL <-
  "http://www.mof.go.jp/international_policy/reference/official_reserve_assets/historical.csv"
tmp <-
  read.csv(
    sourceURL,
    header = F,
    skip = 0,
    stringsAsFactor = F,
    check.names = F,
    fileEncoding = "cp932"
  )
rrr <- 1
dataTitle <- vector()
while (tmp[rrr, 1] != "") {
  dataTitle[rrr] <- tmp[rrr, 1]
  rrr <- rrr + 1
}
while (tmp[rrr, 1] == "") {
  rrr <- rrr + 1
}
startDate <-
  as.Date(paste(tmp[rrr, 3], "-", match(tmp[rrr, 4], month.abb), "-1", sep =
                  ""))
date <- seq(startDate, by = "month", length.out = nrow(tmp) - rrr + 1)
EXJPUSData <- getSymbols("EXJPUS", src = "FRED", auto.assign = FALSE)
#ドル建て
origData1 <-
  data.frame(Date = date, tmp[rrr:nrow(tmp), 5:19], check.names = F)
origData1[, -1] <- apply(origData1[, -1], 2, as.numeric)
colnames(origData1)[-1] <- c(
  "Official reserve assets/外貨準備",
  "Foreign currency reserves/外貨",
  "Foreign currency reserves-Securities/外貨-証券",
  "Foreign currency reserves-Securities-of which: issuer headquartered in Japan/外貨-証券-証券うち本邦発行体分",
  "Foreign currency reserves-Deposits with/外貨-預金",
  "Foreign currency reserves-Deposits with-Foreign central banks and BIS/外貨-預金-外国中央銀行及びBISへの預金",
  "Foreign currency reserves-Deposits with-Banks headquartered in Japan/外貨-預金-本邦金融機関への預金",
  "Foreign currency reserves-Deposits with-Banks headquartered in Japan-of which: located abroad/外貨-預金-本邦金融機関への預金うち海外拠点分",
  "Foreign currency reserves-Deposits with-Banks headquartered outside Japan/外貨-預金-外国金融機関への預金",
  "Foreign currency reserves-Deposits with-Banks headquartered outside Japan-of which: located in Japan/外貨-預金-外国金融機関への預金うち本邦内拠点分",
  "IMF reserve position/IMFリザーブポジション",
  "SDRs/SDR",
  "Gold/金",
  "Gold-(volume [in million fine troy ounces])/金重量[百万トロイオンス]",
  "other reserve assets/その他外貨準備"
)
#ドル建て
#外貨準備に占める割合
origData2 <-
  data.frame(Date = origData1[, 1],
             round(origData1[, -c(1, 15)] / origData1[, 2] * 100, 1),
             check.names = F)
#外貨準備に占める割合
#円建て
exRate <-
  data.frame(subset(
    EXJPUSData,
    head(origData1[, 1], 1) <= index(EXJPUSData) &
      tail(origData1[, 1], 1) >= index(EXJPUSData)
  ))
origData5 <-
  data.frame(Date = origData1[, 1], exRate[, 1, drop = F], row.names = NULL)
colnames(origData5)[2] <- "USD/JPY"
buf <- origData1[, -c(1, 15)] * 10 ^ -6
bufcolnames <- colnames(buf)
origData3 <-
  data.frame(Date = origData1[, 1], apply(buf, 2, function(x) {
    x * exRate[, 1, drop = F]
  }), row.names = NULL)
colnames(origData3)[-1] <- bufcolnames
#円建て
origData4<<-origData1[,c(1,15)]
origData1<<-origData1[,-15]
origData2<<-origData2
origData3<<-origData3
origData5<<-origData5
dataTitle<<-dataTitle