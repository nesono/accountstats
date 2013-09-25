# Please do the following steps to update the data file:
# * open kontostats.xlx with excel
# * remove rows above header
# * save file
# * open file in LibreOffice
# * change Date format to ISO
# * save as visakarte.csv
# * using tab delimiter

# script to
# * read balance from visakarte.csv
# * plot some stats

data <- read.delim( 'visakarte.csv' )

# BEGIN HELPER FUNCTIONS
readnumbers <- function(xi)
{
  # remove whitespaces and change komma to period
  sapply( xi, function(xii){as.numeric(gsub(' ', '', gsub(',', '.', xii ) ))} )
}

sanetizedata <- function(x)
{
  # remove columns with NA values
  x <- x[,colSums (is.na(x)) == 0]
  # convert date to be real date
  x$Date <- as.POSIXct( x$Transaction.date )
  
  # convert balance to real numbers
  x$Balance <- readnumbers(x$Balance)
  x$Amount <- readnumbers(x$Amount)
  
  return (x)
}
# END HELPER FUNCTIONS

# stanetize data
data <- sanetizedata(data)

# order by transaction date
data <- data[order(data$Transaction.date),]

plot( data$Date, data$Balance, 
      type='h', lend=2, lwd=3,
      xlab='Date', ylab='Balance' )
abline( lm( Balance ~ Date, data ) )
abline( lm( Balance ~ Date, data[difftime( Sys.time(), data$Date, units=c("days") ) < 60,] ), col='red' )