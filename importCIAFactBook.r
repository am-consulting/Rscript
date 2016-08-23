allData <- ''
dataset <- list()
sdataset <- list()
username <- Sys.info()['user']
pathOutput <-
  paste('C:/Users/', username, '/Desktop/ciaFactbook/', sep = '')
setwd(pathOutput)
csvFileList <- list.files()
for (lll in 1:length(csvFileList)) {
  dataset[[lll]] <-
    read.table(
      file = paste(pathOutput, csvFileList[lll], sep = ''),
      sep = ',',
      header = T,
      as.is = T,
      skip = 0,
      check.names = F,
      stringsAsFactors = F
    )
  colnames(dataset[[lll]])[3] <-
    paste(gsub('.csv', '', csvFileList[lll]),
          '-',
          colnames(dataset[[lll]])[3],
          sep = '')
  sdataset[[lll]] <- dataset[[lll]][, c(2, 3)]
  print(head(sdataset[[lll]]))
  print(tail(sdataset[[lll]]))
  if (lll == 1) {
    allData <-
      sdataset[[lll]]
  } else{
    allData <- merge(allData, sdataset[[lll]], all = T, by = 'Country')
  }
}
print(head(allData))