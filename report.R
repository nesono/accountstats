if(!require(ggplot2)){
  install.packages('ggplot2')
  require(ggplot2)
}
if(!require(gridExtra)){
  install.packages('gridExtra')
  require(gridExtra)
}
library(scales)
source('helpers.R')

datafile <- 'kontotransactionlist.csv'

data <- read.delim( datafile )
data <- sanetizedata(data)


g.balance <- ggplot( data=data, aes(x=Date, y=Balance), color="green")
g.balance <- g.balance + ylab("Balance SEK") + xlab("Transaction Date")
g.balance <- g.balance + geom_line(colour = 'green3') + geom_smooth(colour='black') + scale_y_continuous(labels=comma)

col.sign <- c('red3','green3')
col.bar <- col.sign[(sign(data$Amount)+1)/2+1]
g.volume <- ggplot( data=data, aes(Date, abs(Amount))) + ylab("Volume")
g.volume <- g.volume + ylab("Volume SEK") + xlab("Transaction Date")
g.volume <- g.volume + geom_bar(colour=col.bar, stat = "identity") + scale_y_continuous(labels=comma)

grid.arrange(g.balance, g.volume, nrow=2)
