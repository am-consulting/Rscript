# License:GPL(version 2 or later)
# Data source : http://www.sidc.be/silso/home
options(download.file.method = "libcurl")
sunSpot <- function() {
  origData <<- list()
  datacolnames <- list()
  url <-
    c(
      "http://www.sidc.be/silso/INFO/snmtotcsv.php",
      "http://www.sidc.be/silso/INFO/snmhemcsv.php"
    )
  datacolnames[[1]] <-
    c(
      "Date",
      "Date in fraction of year.",
      "Monthly mean total sunspot number.",
      "Monthly mean standard deviation of the input sunspot numbers.",
      "Number of observations used to compute the monthly mean total sunspot number.",
      "Definitive/provisional marker. '1' indicates that the value is definitive. '0' indicates that the value is still provisional."
    )
  datacolnames[[2]] <-
    c(
      "Date",
      "Date in fraction of year for the middle of the corresponding month (day 15)",
      "Monthly mean total sunspot number.",
      "North monthly mean sunspot number.",
      "South monthly mean sunspot number.",
      "Monthly mean standard deviation of total sunspot number data",
      "Monthly mean standard deviation of North sunspot number data",
      "Monthly mean standard deviation of South sunspot number data" ,
      "Number of observations for the monthly mean total sunspot number.",
      "Number of observations for the monthly mean north sunspot number (not used yet)",
      "Number of observations for the monthly mean south sunspot number (not used yet)",
      "Definitive/provisional marker. '1' indicates that the value is definitive. '0' indicates that the value is still provisional."
    )
  for (iii in 1:length(url)) {
    Sys.sleep(1)
    buf <-
      read.csv(
        url[iii],
        header = F,
        skip = 0,
        stringsAsFactor = F,
        na.strings = c(""),
        check.names = F,
        sep = ";"
      )
    buf <-
      data.frame(as.Date(paste(buf[, 1], "-", buf[, 2], "-1", sep = "")), buf[, 3:ncol(buf)])
    colnames(buf) <- datacolnames[[iii]]
    origData[[iii]] <<- buf
  }
}