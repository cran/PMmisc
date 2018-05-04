###############################################################################################
##### Normal density function
###############################################################################################

#' @name fnd
#' @aliases fnd
#' @title Normal density function
#' @description Calculate normal density function value at x with a mean of mu and standard deviation of sig.
#' @usage fnd(x,mu,sig)
#' @param x :x value
#' @param mu :mean value
#' @param sig :standard deviation
#' @examples  fnd(seq(-3, 3, 0.1), 0, 1)


fnd<-function(x,mu,sig){

  exp(-((x-mu)^2)/sig)

  }
