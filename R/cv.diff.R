###############################################################################################
##### Calculating difference in a time series vector
###############################################################################################

#' @name cv.diff
#' @aliases cv.diff
#' @title Calculating the difference of a time series
#' @description Calculate the difference of a time series, with a specific lag period. The difference is used to show the
#' change in value over set period.
#' @usage cv.diff(x,n)
#' @param x : a numeric vector
#' @param n : number of lag periods
#' @examples cv.diff(mtcars[,2],1)


cv.diff <- function(x, n){

  d <- c(as.numeric("NA"), (lag(as.ts(x), k = n)-as.ts(x)))

  return(d)

}
