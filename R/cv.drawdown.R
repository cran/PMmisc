###############################################################################################
##### Calculate largest draw down of a series of returns
###############################################################################################

#' @name cv.drawdown
#' @aliases cv.drawdown
#' @title Largest draw down of returns
#' @description Calculate largest draw down of a series of returns. This function calculates the maximum
#' decrease in percentage over time, which can be used to test portfolio returns.
#' @usage cv.drawdown(x)
#' @param x : a numeric vector of returns
#' @examples
#' # rnorm() is used to simulate portfolio returns
#' returns <- rnorm(100)
#' cv.drawdown(returns)


cv.drawdown<-function(x){

  min(sapply(1:length(x),

  function(i){

               min(cumprod(1+x[i:length(x)]))

               }))-1

}

