source('helpers.R')

datafile <- 'kontotransactionlist.csv'

data <- read.delim( datafile )
data <- sanetizedata(data)

regression_line <- function(dataset, col, linetype)
{
  model <- lm( Balance ~ Date, dataset )
  slope <- signif(coefficients(model)[2], 3)
  abline( model, lty=linetype, col=ifelse(slope>0,col,'#F2A398'), lwd=1 )
  secondsPerDay <- 60*60*24
  label <- paste( '(', slope * secondsPerDay, ' SEK/d)', sep='' )
  
  return(label)
}

balance_label <- function(balance, color)
{
  mtext(format(balance, big.mark=","), side=4, 
        at=balance, col=color, line=.5,
        las=1, cex=1 )
}

if( FALSE )
{
data <- data[grep('D ', data$Text, invert=TRUE),]
data$Balance <- cumsum(data$Amount)
}


# lay out plot
par(mar=c(0, 4, 4, 5)+0.1)
layout(matrix(c(1,2), 2, 1), heights=c(3,1))

# draw balance plot frame
plot( data$Date, data$Balance,
      type='n',
      xlab='', xaxt='n',
      ylab='Balance in SEK' )
grid()

# plot balance
lines( data$Date, data$Balance,
      lend=2, lwd=1,
      col='green3' )

label.all <- regression_line( data,
                              'black',
                              5 )

N <- 100
data.lastN <- data[difftime( Sys.time(), data$Date, units=c("days") ) < N,]
label.lastN <- regression_line( data.lastN,
                                  'gray',
                                  4 )

date.nDaysAgo <- Sys.time() - 3600 * 24 * N
abline(v=date.nDaysAgo, col='#9BD0E3', lty='dotted', lwd=1)

min.lastN <- round(min(data.lastN$Balance))
abline(h=min.lastN, col='#9BD0E3', lty=3, lwd=1)

balance.now <- rev(data$Balance[order(data$Date)])[1]
balance_label(balance.now, 'green3')

balance.max <- max(data$Balance)
balance_label(balance.max, 'gray')

balance.min <- min(data$Balance)
balance_label(balance.min, '#F2A398')

date.range <- difftime(max(data$Date),min(data$Date), units=c('d'))

title('Kontoverlauf')
legend('topleft', 
       legend=paste(
         c('Gesamt','Letzte')
         , c(label.all, N)
         , c('',label.lastN))
       , lty=c(2,4)
       , col=c(
         ifelse(length(grep("\\(-",label.all))==1,'#F2A398','#9BD0E3')
         , ifelse(length(grep("\\(-",label.lastN))==1,'#F2A398','#9BD0E3')),
       , lwd=2 )

par(mar=c(5, 4, -0.1, 5)+0.1)
# plot revenue
plot( data$Date, abs(data$Amount),
      type='n',
      xlab='Date', ylab='Delta', bty='n', yaxt='n' )
grid()
points( data$Date, abs(data$Amount),
      col=ifelse(data$Amount>0, '#A0E191', '#F2A398'),
      type='h', lend=2, lwd=2)

# get amount by txt (find biggest names)
# data.bytxt <- split(data, by=data$Text)
# amount.bytxt <- sapply(data.bytxt, function(x){sum(x$Amount)})

