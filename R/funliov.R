###############################################################################################
#####Normal loss integral
###############################################################################################

#' @name funliov
#' @aliases funliov
#' @title Unite normal loss integral
#' @description Compute the value of the unit normal loss integral, with set discontinuity and volatility.
#' @usage funliov(x,disc,vol)
#' @param x :a xtor
#' @param disc :discontinuity
#' @param vol :volatility
#' @examples  funliov(-3:10, 1, 3)


funliov<-function(x,disc,vol){

  (.398942 * exp(-0.5 *((x-disc)/vol)*((x-disc)/vol))-(-((x-disc)/vol))*(1-pnorm(-((x-disc)/vol))))

}
