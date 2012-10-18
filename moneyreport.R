dat <- read.csv( '2012-10-17 gehalt.csv', header=T )
dat$Buchungstag <- as.POSIXlt(as.Date(dat$Buchungstag))
dat <- subset( dat, select=c(Betrag, Buchungstag,Kategorie))

yearly.report <- function( data ) {
  
  pdat <- subset( data )
  
  credit <- subset( pdat, Betrag >= 0 )
  debit  <- subset( pdat, Betrag < 0 )
  
  cat.credit <- by( credit, as.factor(credit$Kategorie), function(x){sum(x$Betrag)} )
  cat.credit <- na.omit(as.matrix(cat.credit))
  cat.debit  <- by( debit, as.factor(debit$Kategorie), function(x){sum(x$Betrag)} )
  cat.debit  <- na.omit(as.matrix(cat.debit))
  
  orig.par <- par(mfrow=c(1,2))
  on.exit(par(orig.par))
  
  year <- pdat$Buchungstag$year[1]+1900
  
  par(las=2)
  par(mar=c(8,3,2.5,1))
  barplot( as.numeric(cat.credit), names.arg=dimnames(cat.credit)[[1]], 
           main=paste(year,'credit'), col='black' )
  barplot( -as.numeric(cat.debit), names.arg=dimnames(cat.debit)[[1]], 
           main=paste(year,'debit'), col='red' )
}

monthly.report <- function( data ) {
  
  pdat <- subset( data )
  
  credit <- subset( pdat, Betrag >= 0 )
  debit  <- subset( pdat, Betrag < 0 )
  
  cat.credit <- by( credit, as.factor(credit$Kategorie), function(x){sum(x$Betrag)} )
  cat.credit <- na.omit(as.matrix(cat.credit))
  cat.debit  <- by( debit, as.factor(debit$Kategorie), function(x){sum(x$Betrag)} )
  cat.debit  <- na.omit(as.matrix(cat.debit))
  
  orig.par <- par(mfrow=c(1,2))
  on.exit(par(orig.par))
  
  month <- pdat$Buchungstag$mon[1]+1
  
  par(las=2)
  par(mar=c(8,3,2.5,1))
  if( length(cat.credit) > 0 )
    barplot( as.numeric(cat.credit), names.arg=dimnames(cat.credit)[[1]], 
             main=paste(year,month,'credit'), col='black' )
  if( length(cat.debit) > 0 )
    barplot( -as.numeric(cat.debit), names.arg=dimnames(cat.debit)[[1]], 
             main=paste(year,month,'debit'), col='red' )
}

orig.par <- par(mfrow=c(1,6))

ydat <- split( dat, as.factor(dat$Buchungstag$year+1900) )
for( year in names(ydat) )
{
  tydat <- ydat[[year]]
  yearly.report(tydat)
  
  mdat <- split( tydat, as.factor(tydat$Buchungstag$mon+1))
  
  for( month in names(mdat)) {
    tmdat <- mdat[[month]]
    
    monthly.report(tmdat)
  }
}

#acc.barplot( dat, 'overall' )

par(orig.par)