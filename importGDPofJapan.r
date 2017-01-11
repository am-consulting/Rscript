# License:GPL(version 2 or later)
# Data Source:Cabinet Office, Government Of Japan
library(RCurl);library(Nippon)
sourceURL <- 'http://www.esri.cao.go.jp/jp/sna/sokuhou/sokuhou_top.html'
htmlMarkup <- getURL(sourceURL,.encoding = 'utf-8')
pattern <- "(<a\\shref=.+?\">)+?統計表一覧"
pageList <- unlist(regmatches(htmlMarkup, gregexpr(pattern, htmlMarkup, fixed = F)))
pattern <- '/.+?html'
pageList <- unlist(regmatches(pageList, gregexpr(pattern, pageList, fixed = F)))
targetURL <- paste0('http://www.esri.cao.go.jp/jp/sna',pageList)
htmlMarkup <- getURL(targetURL,.encoding = 'utf-8')
pattern <- "(<a.+?(.csv\")+?.+?>)+?.+?</a>"
csvList <- unlist(regmatches(htmlMarkup, gregexpr(pattern, htmlMarkup, fixed = F)))
pattern <- "<img.*?/>.*?</a>"
csvList <- gsub(pattern,'',csvList)
pattern <- "target=\"_blank\""
csvList <- gsub(pattern,'',csvList)
pattern <- "<a.+?>"
csvList0 <- unlist(regmatches(csvList, gregexpr(pattern, csvList, fixed = F)))
csvList1 <- gsub(pattern,'',csvList)
pattern <- "/.*?\\.csv"
csvList0 <- paste0('http://www.esri.cao.go.jp',unlist(regmatches(csvList0, gregexpr(pattern, csvList0, fixed = F))))
GDPList <- data.frame(csvList0,csvList1,check.names = F,stringsAsFactors = F)
GDPList0 <- rbind(
  GDPList[grep('名目季節調整系列',GDPList[,2])[1],],
  GDPList[grep('実質季節調整系列',GDPList[,2])[1],],
  GDPList[grep('年率換算の名目季節調整系列',GDPList[,2])[1],],
  GDPList[grep('年率換算の実質季節調整系列',GDPList[,2])[1],],
  GDPList[grep('四半期デフレーター季節調整系列',GDPList[,2])[1],],
  GDPList[grep('名目原系列',GDPList[,2])[1],],
  GDPList[grep('実質原系列',GDPList[,2])[1],],
  GDPList[grep('名目暦年',GDPList[,2])[1],],
  GDPList[grep('実質暦年',GDPList[,2])[1],],
  GDPList[grep('名目原系列\\(前年同期比\\)',GDPList[,2])[1],],
  GDPList[grep('名目季節調整系列\\(前期比\\)',GDPList[,2])[1],],
  GDPList[grep('名目暦年\\(前年比\\)',GDPList[,2])[1],],
  GDPList[grep('実質原系列\\(前年同期比\\)',GDPList[,2])[1],],
  GDPList[grep('実質季節調整系列\\(前期比\\)',GDPList[,2])[1],],
  GDPList[grep('実質暦年\\(前年比\\)',GDPList[,2])[1],])
url <- GDPList0[,1]
titleJ <- GDPList0[,2]
appendJ <- substring(GDPList0[,2],1,sapply(GDPList0[,2],function(x)regexpr('csv',x,ignore.case = T))-2)
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
  firstDateRow <-
    which(!is.na(as.numeric(substring(tmp[,1],1,4))))[1]
  buf <-
    gsub('\\s','',tmp[firstDateRow,1])
  firstYear <-
    as.numeric(substring(buf,1,4))
  firstMonth <-
    as.numeric(substring(buf,6,regexpr('-',buf)-1))
  if(length(grep('暦年|前年比', tmp[1,1]))==0){
    DateColumn <-
      seq(as.Date(paste0(firstYear,'-',firstMonth+2,'-1')), by = "3 month", length.out = nrow(tmp)-firstDateRow +1)
  }else{
    DateColumn <-
      seq(as.Date(paste0(firstYear,'-',firstMonth,'-1')), by = "12 month", length.out = nrow(tmp)-firstDateRow +1)
  }
  sheetTitle <- zen2han(tmp[1,1])
  valueColumn <-
    tmp[-c(1:(firstDateRow - 1)), -1]
  dataSetWithoutColnames <-
    data.frame(DateColumn,valueColumn,stringsAsFactors = F)
  endColumn <-
    grep('参考',tmp[1,])-2
  dataSetWithoutColnames <- dataSetWithoutColnames[,1:(endColumn-1)]
  tmp0 <- tmp[,1:(endColumn-1)]
  buf <- NA
  for(ccc in 1:ncol(tmp0)){
    if(!is.na(tmp0[3,ccc])){buf <- tmp0[3,ccc]}
    tmp0[3,ccc] <- buf
  }
  buf <- NA
  for(ccc in 1:ncol(tmp0)){
    if(!is.na(tmp0[4,ccc])){buf <- tmp0[4,ccc]}
    if(length(grep('民間最終消費支出', tmp0[3,ccc]))!=0){tmp0[4,ccc] <- buf}
  }
  dataSetColnames <-
    paste0(sheetTitle,':',sapply(gsub(':na','',paste0(tmp0[3,],':',tmp0[4,],':',tmp0[5,]),ignore.case = T),zen2han))
  colnames(dataSetWithoutColnames) <- dataSetColnames
  colnames(dataSetWithoutColnames)[1] <- 'Date'
  dataSetWithoutColnames[,-1] <-
    apply(dataSetWithoutColnames[,-1],2,function(x)as.numeric(gsub(',','',x)))
  dataSet <-
    dataSetWithoutColnames[-which((ncol(dataSetWithoutColnames)-1) <=
                                  apply(dataSetWithoutColnames,1,function(x)sum(is.na(x)))),]
  assign(paste("gdp", uuu, sep = ""), dataSet, envir = .GlobalEnv)
}
