# Please do the following steps to update the data file:
# * open kontostats.xlx with excel
# * remove rows above header
# * save to visakarte.xlsx
# * open file in LibreOffice
# * change Date format to ISO
# * save as visakarte.csv
# * using tab delimiter

# script to
# * read balance from visakarte.csv
# * plot some stats

require(gdata)
newfile <- 'kontotransactionlist.xls'
datafile <- 'kontotransactionlist.csv'

# HELPER FUNCTIONS
readnumbers <- function(xi)
{
  # remove whitespaces and change komma to period
  sapply( xi, function(xii){as.numeric(gsub(' ', '', gsub(',', '.', xii ) ))} )
}

sanetizedata <- function(x)
{
  x <- x[,colSums (is.na(x)) == 0]
  if( sum(colnames(x)=='Transaktionsdatum') > 0) {
    x$Date <- as.POSIXct( x$Transaktionsdatum )
    
    x$Balance <- readnumbers(x$Saldo)
    x$Amount <- readnumbers(x$Belopp)
  } else {
    x$Date <- as.POSIXct( x$Date )
  }
  
  return (subset(x, select=c('Text','Date','Balance','Amount')))
}

regression_line <- function(dataset, col, linetype)
{
  model <- lm( Balance ~ Date, dataset )
  slope <- signif(coefficients(model)[2], 3)
  abline( model, lty=linetype, col=ifelse(slope>0,col,'red'), lwd=2 )
  secondsPerDay <- 60*60*24
  label <- paste( '(', slope * secondsPerDay, ' SEK/d)', sep='' )
  
  return(label)
}

# check if there is new data
if( file.exists(newfile) ) {
  newdata <- read.xls(newfile)
  newdata <- sanetizedata(newdata)
  newdata <- newdata[order(newdata$Date),]
  
  olddata <- read.delim( datafile )
  olddata <- sanetizedata(olddata)
  olddata <- olddata[order(olddata$Date),]
  data <- merge(olddata, newdata, all=TRUE)
  data <- data[order(data$Date),]
  write.table(data, datafile, row.names=FALSE, sep='\t')
}

data <- read.delim( datafile )

# MAIN PROCESSING
data <- sanetizedata(data)
data <- data[order(data$Date),]

plot( data$Date, data$Balance,
      type='h', lend=2, lwd=3,
      xlab='Date', ylab='Balance in SEK' )

label.all <- regression_line( data,
                              'black',
                              2 )
label.last100 <- regression_line( data[difftime( Sys.time(), data$Date, units=c("days") ) < 100,],
                                  'gray',
                                  4 )

title('Kontoverlauf')
legend('topleft', 
       legend=paste(c('Gesamt','Letzte 100d'),
                    c(label.all, label.last100)), 
       lty=c(2,4), col=c('black','gray') )

