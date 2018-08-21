###############################################################################################
##### Logistic function
###############################################################################################

#' @name tr.logtb
#' @aliases tr.logtb
#' @title Logistic function
#' @description Generate logistic series, with set top and bottom value and acceleration.
#' @usage tr.logtb(x,top,bot,a,b)
#' @param x :a vector
#' @param top :higher level y asymptote
#' @param bot :lower level y asymptote
#' @param a :a number to control top acceleration of the curve
#' @param b :a number to control bottom acceleration of the curve
#' @examples tr.logtb(seq(-3, 3, 0.1), 1, 0.4, -3, 3)

tr.logtb<-function(x, top, bot, a, b){

  (top + bot* exp(a + b * x)) /(1 + exp(a + b * x))

}
