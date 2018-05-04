###############################################################################################
##### Calculating percentage change
###############################################################################################

#' @name fpctcng
#' @aliases fpctcng
#' @title Calculating rate of return of a vector
#' @description Calculating the rate of return of a vector for further analysis, including calculating beta of companies, plotting to see the trend of the stock for technical analysis.
#' @usage fpctcng(x,n)
#' @param x :a numeric vector
#' @param n : number of lag periods
#' @examples fpctcng(mtcars[,1],1)


fpctcng <- function(x, n){

  ratereturn <- c(as.numeric("NA"),(lag(as.ts(x), k = n)/as.ts(x))-1)

  return(ratereturn)
}
