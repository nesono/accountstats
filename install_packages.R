# script to install necessary R packages
packages <- c('gdata','knitr','stringr')

checkinstall <- function( x )
{
  tmp <- data.frame(installed.packages())
  if( any(x ==  tmp$Package) ) {
    cat('already installed\n')
  } else {
    install.packages(x)
  }
}

lapply( packages, checkinstall )