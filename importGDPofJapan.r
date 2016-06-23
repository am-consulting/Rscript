# License:GPL(version 2 or later)
# Data Source:Cabinet Office, Government Of Japan
options(download.file.method="libcurl")
url <- c(
  "http://www.esri.cao.go.jp/jp/sna/data/data_list/sokuhou/files/2016/qe161/__icsFiles/afieldfile/2016/05/17/gaku-mk1611.csv",
  "http://www.esri.cao.go.jp/jp/sna/data/data_list/sokuhou/files/2016/qe161/__icsFiles/afieldfile/2016/05/17/gaku-jk1611.csv",
  "http://www.esri.cao.go.jp/jp/sna/data/data_list/sokuhou/files/2016/qe161/__icsFiles/afieldfile/2016/05/17/nritu-mk1611.csv",
  "http://www.esri.cao.go.jp/jp/sna/data/data_list/sokuhou/files/2016/qe161/__icsFiles/afieldfile/2016/05/17/nritu-jk1611.csv"
)
titleJ <<- c("名目季節調整系列",
             "実質季節調整系列",
             "名目季節調整系列(年率)",
             "実質季節調整系列(年率)
             ")
titleE <<- c(
  "Nominal Gross Domestic Product (seasonally adjusted series)",
  "Real Gross Domestic Product (seasonally adjusted series)",
  "Annualized rate of Changes from the previous quarter (Nominal: seasonally adjusted series)",
  "Annualized rate of Changes from the previous quarter (Real: seasonally adjusted series)
  "
)
colnameDataJ <- c(
  "国内総生産(支出側)",
  "民間最終消費支出",
  "家計最終消費支出",
  "家計最終消費支出-除く持ち家の帰属家賃",
  "民間住宅",
  "民間企業設備",
  "民間在庫品増加",
  "政府最終消費支出",
  "公的固定資本形成",
  "公的在庫品増加",
  "財貨･サービス-純輸出",
  "財貨･サービス-輸出",
  "財貨･サービス-輸入"
)
colnameDataE <- c(
  "GDP(Expenditure Approach)",
  "PrivateConsumption",
  "Consumption ofHouseholds",
  "ExcludingImputed Rent",
  "PrivateResidentialInvestment",
  "Private Non-Resi.Investment",
  "Changein PrivateInventories",
  "GovernmentConsumption",
  "PublicInvestment",
  "Changein PublicInventories",
  "Goods & Services-Net Exports",
  "Goods & Services-Exports",
  "Goods & Services-Imports"
)
for (uuu in 1:length(url)) {
  tmp <-
    read.csv(
      url[uuu],
      header = F,
      skip = 0,
      stringsAsFactor = F,
      na.strings = c("", "***"),
      check.names = F,
      fileEncoding = "cp932"
    )
  for (rrr in 1:nrow(tmp)) {
    if (is.na(tmp[rrr, 1]) == F) {
      if (is.na(as.numeric(substring(tmp[rrr, 1], 1, 4))) == F) {
        break
      }
    }
  }
  for (ccc in 1:ncol(tmp)) {
    if (is.na(tmp[4, ccc]) == F) {
      if ((tmp[4, ccc] == "輸入") == T) {
        break
      }
    }
  }
  bufNumeric <- tmp[-c(1:(rrr - 1)), 2:ccc]
  for (aaa in nrow(bufNumeric):1) {
    if (is.na(bufNumeric[aaa, 1]) == F) {
      break
    }
  }
  bufNumeric <- bufNumeric[1:aaa,]
  bufstartDate <- nchar(tmp[rrr, 1])
  bufDate <-
    seq(as.Date(paste(
      substring(tmp[rrr, 1], 1, 4),
      "-",
      substring(tmp[rrr, 1], bufstartDate - 1, bufstartDate - 1),
      "-1",
      sep = ""
    )),
    by = "3 month",
    length.out = nrow(bufNumeric))
  buf <- data.frame(bufDate, bufNumeric)
  colnames(buf)[1] <- "Date"
  buf[, -1] <- apply(buf[, -1], 2, function(x)
    gsub(",", "", x))
  buf[, -1] <- apply(buf[, -1], 2, function(x)
    as.numeric(x))
  if (length(grep("ritu", url[uuu])) == 0) {
    buf[, -1] <-
      apply(buf[, -1], 2, function(x)
        x * (10 ^ -3))
  }
  colnames(buf)[-1] <- colnameDataE
  assign(paste("gdp", uuu, sep = ""), buf, envir = .GlobalEnv)
}
origData<<-gdp1 #representative