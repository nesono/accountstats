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