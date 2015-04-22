if(!require(readxl)){
  install.packages('readxl')
  require(readxl)
}

source('helpers.R')

newfile <- 'Workbook1.xlsx'
datafile <- 'kontotransactionlist.csv'

# check if there is new data
if( file.exists(newfile) ) {
  newdata <- read_excel(newfile)
  newdata <- sanetizedata(newdata)
  newdata <- newdata[order(newdata$Date),]
  
  olddata <- read.delim( datafile )
  olddata <- sanetizedata(olddata)
  olddata <- olddata[order(olddata$Date),]
  data <- merge(olddata, newdata, all=TRUE)
  data <- sanetizedata(data)
  data <- data[order(data$Date),]
  write.table(data, datafile, row.names=FALSE, sep='\t')
}
