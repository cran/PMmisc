###############################################################################################
##### Logistic function
###############################################################################################

#' @name flogtb
#' @aliases flogtb
#' @title Logistic function
#' @description Generate logistic series, with set top and bottom value and acceleration.
#' @usage flogtb(x,top,bot,a,b)
#' @param x :a vector
#' @param top :higher level y asymptote
#' @param bot :lower level y asymptote
#' @param a :a number to control top acceleration of the curve
#' @param b :a number to control bottom acceleration of the curve
#' @examples flogtb(seq(-3, 3, 0.1), 1, 0.4, -3, 3)

flogtb<-function(x, top, bot,a, b){

  (top +bot* exp(a + b * x)) /(1 + exp(a + b * x))

}
