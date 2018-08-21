###############################################################################################
##### Normal density function
###############################################################################################

#' @name tr.nd
#' @aliases tr.nd
#' @title Normal density function
#' @description Calculate normal density function value at x with a mean of mu and standard deviation of sig.
#' @usage tr.nd(x,mu,sig)
#' @param x :x value
#' @param mu :mean value
#' @param sig :standard deviation
#' @examples  tr.nd(seq(-3, 3, 0.1), 0, 1)


tr.nd<-function(x,mu,sig){

  exp(-((x-mu)^2)/sig)

  }
