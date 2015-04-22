if(!require(ggplot2)){
  install.packages('ggplot2')
  require(ggplot2)
}

source('helpers.R')

datafile <- 'kontotransactionlist.csv'

data <- read.delim( datafile )
data <- sanetizedata(data)

# regression_line <- function(dataset, col, linetype)
# {
#   model <- lm( Balance ~ Date, dataset )
#   slope <- signif(coefficients(model)[2], 3)
#   abline( model, lty=linetype, col=ifelse(slope>0,col,'#F2A398'), lwd=1 )
#   secondsPerDay <- 60*60*24
#   label <- paste( '(', slope * secondsPerDay, ' SEK/d)', sep='' )
#   
#   return(label)
# }
# 
# balance_label <- function(balance, color)
# {
#   mtext(format(balance, big.mark=","), side=4, 
#         at=balance, col=color, line=.5,
#         las=1, cex=1 )
# }


# gg <- ggplo

