###############################################################################################
##### Calculating percentage change
###############################################################################################

#' @name cv.pctcng
#' @aliases cv.pctcng
#' @title Calculating rate of return of a vector
#' @description Calculating the percentage change of a time series vector for further analysis, including calculating beta of companies, plotting to see the trend of the stock for technical analysis.
#' @usage cv.pctcng(x,n)
#' @param x :a numeric vector
#' @param n : number of lag periods
#' @examples cv.pctcng(mtcars[,1],1)


cv.pctcng <- function(x, n){

  ratereturn <- c(as.numeric("NA"),(lag(as.ts(x), k = n)/as.ts(x))-1)

  return(ratereturn)

}
