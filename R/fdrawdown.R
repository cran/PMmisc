###############################################################################################
##### Calculate largest draw down of a series of returns
###############################################################################################

#' @name fdrawdown
#' @aliases fdrawdown
#' @title Largest draw down of returns
#' @description Calculate largest draw down of a series of returns. This function calculates the maximum
#' decrease in percentage over time, which can be used to test portfolio returns.
#' @usage fdrawdown(x)
#' @param x : a numeric vector of returns
#' @examples fdrawdown(rnorm(100))


fdrawdown<-function(x){

  min(sapply(1:length(x),
             function(i){
               min(cumprod(1+x[i:length(x)]))
               }))-1

}

