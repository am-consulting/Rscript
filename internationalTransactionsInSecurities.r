# License:GPL(version 2 or later)
# Data Source:Ministry of Finance Japan
url <-
  "http://www.mof.go.jp/international_policy/reference/itn_transactions_in_securities/week.csv"
buf <-
  read.csv(
    url,
    header = F,
    skip = 0,
    stringsAsFactor = F,
    na.strings = c(""),
    check.names = F,
    fileEncoding = "cp932"
  )
for (rrr in 1:nrow(buf)) {
  tmp <- buf[rrr, ]
  tmp <- grep("Acquisition", tmp)
  if (length(tmp) != 0) {
    break
  }
}
tmp <- na.omit(buf[-(1:rrr), ])
origData0 <-
  data.frame(tmp[, 1], apply(tmp[, -1], 2, function(x) {
    as.numeric(gsub(",", "", x))
  }), check.names = F)
namesJ <<- c(
  "期間",
  "対外-株式･投資ファンド持分-取得",
  "対外-株式･投資ファンド持分-処分",
  "対外-株式･投資ファンド持分-ネット",
  "対外-中長期債-取得",
  "対外-中長期債-処分",
  "対外-中長期債-ネット",
  "対外-小計-ネット",
  "対外-短期債-取得",
  "対外-短期債-処分",
  "対外-短期債-ネット",
  "対外-合計-ネット",
  "対内-株式･投資ファンド持分-取得",
  "対内-株式･投資ファンド持分-処分",
  "対内-株式･投資ファンド持分-ネット",
  "対内-中長期債-取得",
  "対内-中長期債-処分",
  "対内-中長期債-ネット",
  "対内-小計-ネット",
  "対内-短期債-取得",
  "対内-短期債-処分",
  "対内-短期債-ネット",
  "対内-合計-ネット"
)
namesE <<- c(
  "Period",
  "Assets-Equity and investment fund shares-Acquisition",
  "Assets-Equity and investment fund shares-Disposition",
  "Assets-Equity and investment fund shares-Net",
  "Assets-Long-term debt securities-Acquisition",
  "Assets-Long-term debt securities-Disposition",
  "Assets-Long-term debt securities-Net",
  "Assets-Subtotal-Net",
  "Assets-Short-term debt securities-Acquisition",
  "Assets-Short-term debt securities-Disposition",
  "Assets-Short-term debt securities-Net",
  "Assets-Total-Net",
  "Liabilities-Equity and investment fund shares-Acquisition",
  "Liabilities-Equity and investment fund shares-Disposition",
  "Liabilities-Equity and investment fund shares-Net",
  "Liabilities-Long-term debt securities-Acquisition",
  "Liabilities-Long-term debt securities-Disposition",
  "Liabilities-Long-term debt securities-Net",
  "Liabilities-Subtotal-Net",
  "Liabilities-Short-term debt securities-Acquisition",
  "Liabilities-Short-term debt securities-Disposition",
  "Liabilities-Short-term debt securities-Net",
  "Liabilities-Total-Net"
)
colnames(origData0) <- namesE
origData <<- data.frame(ID=seq(1,nrow(origData0)),origData0,check.names = F)
titleJ<<-"対外及び対内証券売買契約等の状況(週次・指定報告機関ベース)"
titleE<<-"International Transactions in Securities(Weekly; based on reports from designated major investors)"