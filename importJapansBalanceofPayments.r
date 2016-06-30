options(download.file.method="libcurl")
datacolnames1 <- c(
  "Current account/経常収支",
  "Goods & services/貿易･サービス収支",
  "Goods/貿易収支",
  "Goods-Exports/貿易収支-輸出",
  "Goods-Imports/貿易収支-輸入",
  "Services/サービス収支",
  "Primary income/第一次所得収支",
  "Secondary income/第二次所得収支",
  "Capital account/資本移転等収支",
  "Financial account/金融収支",
  "Financial account-Direct investment/金融収支-直接投資",
  "Financial account-Portfolio investment/金融収支-証券投資",
  "Financial account-Financial derivatives(other than reserves)/金融収支-金融派生商品",
  "Financial account-Other investment/金融収支-その他投資",
  "Financial account-Reserve assets/金融収支-外貨準備",
  "Net errors & omissions/誤差脱漏"
)
datacolnames2 <- c(
  "Services/サービス収支",
  "Services-Transport/サービス収支-輸送",
  "Services-Travel/サービス収支-旅行",
  "Services-Other services/サービス収支-その他サービス",
  "Services-Other services-Manufacturing services on physical inputs owned by others/サービス収支-その他サービス-委託加工サービス",
  "Services-Other services-Maintenance and repair services n.i.e./サービス収支-その他サービス-維持修理サービス",
  "Services-Other services-Construction/サービス収支-その他サービス-建設",
  "Services-Other services-Insurance and pension services/サービス収支-その他サービス-保険・年金サービス",
  "Services-Other services-Financial services/サービス収支-その他サービス-金融サービス",
  "Services-Other services-Charges for the use of intellectual property n.i.e./サービス収支-その他サービス-知的財産権等使用料",
  "Services-Other services-Telecommunications, computer, and information services/サービス収支-その他サービス-通信・コンピュータ・情報サービス",
  "Services-Other services-Other business services/サービス収支-その他サービス-その他業務サービス",
  "Services-Other services-Personal, cultural, and recreational services/サービス収支-その他サービス-個人・文化・娯楽サービス",
  "Services-Other services-Government goods and services, n.i.e./サービス収支-その他サービス-公的サービス等"
)
datacolnames3 <- c(
  "Primary income/第一次所得収支",
  "Primary income-Compensation of employees/第一次所得収支-雇用者報酬",
  "Primary income-Investment income/第一次所得収支-投資収益",
  "Primary income-Investment income-Direct Investment/第一次所得収支-投資収益-直接投資収益",
  "Primary income-Investment income-Direct Investment-Dividends and withdrawals from income of quasi-corporations/第一次所得収支-投資収益-直接投資収益-配当金・配分済支店収益",
  "Primary income-Investment income-Direct Investment-Reinvested earnings/第一次所得収支-投資収益-直接投資収益-再投資収益",
  "Primary income-Investment income-Direct Investment-Interest/第一次所得収支-投資収益-直接投資収益-利子所得等",
  "Primary income-Investment income-Portfolio Investment/第一次所得収支-投資収益-証券投資収益",
  "Primary income-Investment income-Portfolio Investment-Investment income on equity and investment fund shares/第一次所得収支-投資収益-証券投資収益-配当金",
  "Primary income-Investment income-Portfolio Investment-Interest/第一次所得収支-投資収益-証券投資収益-債券利子",
  "Primary income-Investment income-Other investment/第一次所得収支-投資収益-その他投資収益",
  "Primary income-Other primary income/第一次所得収支-その他第一次所得"
)
datacolnames4 <- c(
  "Financial account/金融収支",
  "Financial account-Direct investment/金融収支-直接投資",
  "Financial account-Direct investment-Equity other than reinvestment of earnings/金融収支-直接投資-株式資本",
  "Financial account-Direct investment-Reinvestment of earnings/金融収支-直接投資-収益の再投資",
  "Financial account-Direct investment-Debt instruments/金融収支-直接投資-負債性資本",
  "Financial account-Portfolio investment/金融収支-証券投資",
  "Financial account-Portfolio investment-Equity securities other than investment fund shares/金融収支-証券投資-株式",
  "Financial account-Portfolio investment-Investment fund shares or unit/金融収支-証券投資-投資ファンド持分",
  "Financial account-Portfolio investment-Long-term debt securities/金融収支-証券投資-中長期債",
  "Financial account-Portfolio investment-Short-term debt securities/金融収支-証券投資-短期債",
  "Financial account-Financial derivatives (other than reserves)/金融収支-金融派生商品",
  "Financial account-Other investment/金融収支-その他投資",
  "Financial account-Reserve assets/金融収支-外貨準備"
)
sourceURL <- c(
  "http://www.mof.go.jp/international_policy/reference/balance_of_payments/bp_trend/bpnet/sbp/s-1/6s-1-4.csv",
  "http://www.mof.go.jp/international_policy/reference/balance_of_payments/bp_trend/bpnet/sbp/s-2/6s-2-4.csv",
  "http://www.mof.go.jp/international_policy/reference/balance_of_payments/bp_trend/bpnet/sbp/s-3/6s-3-4.csv",
  "http://www.mof.go.jp/international_policy/reference/balance_of_payments/bp_trend/bpnet/sbp/s-4/6s-4-4.csv"
)
for (ddd in 1:length(sourceURL)) {
  tmp <-
    read.csv(
      sourceURL[ddd],
      header = F,
      skip = 0,
      stringsAsFactor = F,
      check.names = F,
      fileEncoding = "cp932"
    )
  nonNumeric <- c(1, 2, 3, 4)
  tmp <-
    data.frame(Date = tmp[, nonNumeric],
               apply(tmp[, -nonNumeric], 2, function(x) {
                 as.numeric(gsub(",", "", x))
               }),
               check.names = F)
  rrr <- nrow(tmp)
  while (tmp[rrr, 2] != "") {
    rrr <- rrr - 1
  }
  startDate <-
    as.Date(paste(tmp[rrr + 1, 3], "-", match(tmp[rrr + 1, 4], month.abb), "-1", sep =
                    ""))
  date <- seq(startDate, by = "month", length.out = nrow(tmp) - rrr)
  colName <- get(paste("datacolnames", ddd, sep = ""))
  tmp <- tmp[(rrr + 1):nrow(tmp), 5:ncol(tmp)]
  colnames(tmp) <- colName
  assign(paste("origData", ddd, sep = ""),
         data.frame(Date = date, tmp, check.names =
                      F),
         envir = .GlobalEnv)
}