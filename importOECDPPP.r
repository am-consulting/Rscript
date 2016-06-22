# License:GPL(version 2 or later)
# Reference https://data.oecd.org/conversion/purchasing-power-parities-ppp.htm#indicator-chart
# Reference Organisation for Economic Co-operation and Development
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
origData <<- origData
abbr <- c(
  "EA19",
  "EU28",
  "ARG",
  "AUS",
  "AUT",
  "BEL",
  "BRA",
  "CAN",
  "CHE",
  "CHL",
  "CHN",
  "COL",
  "CRI",
  "CZE",
  "DEU",
  "DEW",
  "DNK",
  "ESP",
  "EST",
  "FIN",
  "FRA",
  "GBR",
  "GRC",
  "HUN",
  "IDN",
  "IND",
  "IRL",
  "ISL",
  "ISR",
  "ITA",
  "JPN",
  "KOR",
  "LTU",
  "LUX",
  "LVA",
  "MEX",
  "NLD",
  "NOR",
  "NZL",
  "POL",
  "PRT",
  "RUS",
  "SAU",
  "SVK",
  "SVN",
  "SWE",
  "TUR",
  "USA",
  "ZAF"
)
country <- c(
  "Euro area(19 countries)",
  "European Union(28 countries)",
  "Argentina",
  "Australia",
  "Austria",
  "Belgium",
  "Brazil",
  "Canada",
  "Switzerland",
  "Chile",
  "China(People's Republic of)",
  "Colombia",
  "Costa Rica",
  "Czech Republic",
  "Germany",
  "Former Federal Republic of Germany",
  "Denmark",
  "Spain",
  "Estonia",
  "Finland",
  "France",
  "United Kingdom",
  "Greece",
  "Hungary",
  "Indonesia",
  "India",
  "Ireland",
  "Iceland",
  "Israel",
  "Italy",
  "Japan",
  "Korea",
  "Lithuania",
  "Luxembourg",
  "Latvia",
  "Mexico",
  "Netherlands",
  "Norway",
  "New Zealand",
  "Poland",
  "Portugal",
  "Russia",
  "Saudi Arabia",
  "Slovak Republic",
  "Slovenia",
  "Sweden",
  "Turkey",
  "United States",
  "South Africa"
)
locationList<<-data.frame(abbr,country)
dataTitle<<-"Purchasing power parities (PPP)Total, National currency units/US dollar"