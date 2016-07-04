# License:GPL(version 2 or later)
# Data Source:Ministry of Finance Japan
options(download.file.method = "libcurl")
FUN.0 <- function(x) {
  as.numeric(x) / (10 ^ 9)
} #オリジナルの単位は千円、兆円に変換
FUN.1 <- function(x) {
  as.numeric(x) / (10 ^ 5)
} #オリジナルの単位は千円、億円に変換
FUN.2 <- function(x) {
  all(x != 0)
}
file.name <- c("d41ma", "d51ma", "d61ma")#51輸出(export),61輸入(import)
base.url <- "http://www.customs.go.jp/toukei/suii/html/data/"
for (iii in 1:length(file.name)) {
  Sys.sleep(1)
  url <- paste(base.url, file.name[iii], ".csv", sep = "")
  tmp0 <-
    read.csv(
      url,
      header = F,
      skip = 0,
      stringsAsFactor = F,
      check.names = F,
      fileEncoding = "cp932"
    )
  if (grepl("d41ma.csv", url) == T) {
    origData <- tmp0
    colnames(origData) <- c("Date", origData[3, 2], origData[3, 3])
    colnames(origData) <- gsub("-", ".", colnames(origData))
    origData <- origData[-c(1:4),]
    origData[, 1] <- gsub("/", "-", origData[, 1])
    origData[, 1] <- paste(origData[, 1], "-01", sep = "")
    origData <- cbind(origData[1], apply(origData[-1], 2, FUN.0))
  } else if (grepl("d51ma.csv", url) == T ||
             grepl("d61ma.csv", url) == T) {
    buf <- tmp0
    buf[3,] <- gsub("．|１|２|３|４|５|６|７|８|９", "", buf[3,])
    buf[3,] <- gsub("([.-])|[[:punct:]]", "", buf[3,])
    buf[4,] <- gsub("([.-])|[[:punct:]]", "", buf[4,])
    tmp.name <- ""
    for (ccc in 2:length(buf)) {
      if (buf[3, ccc] != buf[4, ccc]) {
        if (buf[3, ccc] == "" && buf[4, ccc] != "") {
          tmp.name <- c(tmp.name, buf[4, ccc])
        } else if (buf[3, ccc] != "" && buf[4, ccc] == "") {
          tmp.name <- c(tmp.name, buf[3, ccc])
        } else{
          tmp.name <-
            c(tmp.name, paste(buf[3, ccc], ".", buf[4, ccc], sep = ""))
        }
      } else{
        tmp.name <- c(tmp.name, buf[3, ccc])
      }
    }
    tmp.name <- tmp.name[tmp.name != ""]
    tmp.logical <- (buf[6,] == "金額")
    tmp.value <- buf[, tmp.logical == TRUE]
    tmp.value <- tmp.value[-c(1:7),]
    tmp.date <- gsub("/", "-", buf[, 1])
    tmp.date <- paste(tmp.date, "-01", sep = "")
    tmp.date <- tmp.date[-c(1:7)]
    buf <- cbind(tmp.date, tmp.value)
    colnames(buf) <- c("Date", tmp.name)
    origData <- buf
    origData <- cbind(origData[1], apply(origData[-1], 2, FUN.1))
  }
  origData[, 1] <- as.Date(origData[, 1])
  # 全系列ともゼロ値またはNAの行を削除(オリジナルデータは最新年の12月分まで枠が生成されている
  rowNotZero <- apply(origData[, -1], 1, FUN.2)
  origData <- origData[rowNotZero,]
  # origData <- na.omit(origData) 一部にNAを含む行がomitされるため注意
  # 全系列ともゼロ値またはNAの行を削除(オリジナルデータは最新年の12月分まで枠が生成されている
  assign(paste("origData", iii, sep = ""), origData,envir = .GlobalEnv)
}
assign(paste("origData",iii+1,sep=""),data.frame(Date=origData1[,1],Net=origData1[,2]-origData1[,3]),envir = .GlobalEnv)
