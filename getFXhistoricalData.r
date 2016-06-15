# License:GPL(version 2 or later)
# Data Source:Mizuho Bank, Ltd.
url <- "http://www.mizuhobank.co.jp/rate/market/csv/quote.csv"
buf <-
  read.csv(
    url,
    header = F,
    skip = 1,
    stringsAsFactor = F,
    na.strings = c("*****", "NA"),
    check.names = F
  )
bufcolnames <- paste(buf[1, ], ":", buf[2, ], sep = "")
bufcolnames[1] <- "Date"
buf <- buf[-(1:2), ]
buf[, 1] <- as.Date(buf[, 1])
buf <-
  data.frame(buf[, 1], apply(buf[, -1], 2, as.numeric), check.names = F)
colnames(buf) <- bufcolnames
origData <<- buf