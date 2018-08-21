###############################################################################################
#####Normal loss integral
###############################################################################################

#' @name tr.unli
#' @aliases tr.unli
#' @title Unit normal loss integral
#' @description Compute the value of the unit normal loss integral, with discontinuity and dispersion
#' @usage tr.unli(x,disc,disp)
#' @param x :a vector
#' @param disc :discontinuity
#' @param disp :dispersion
#' @examples  tr.unli(-3:10, 1, 3)


tr.unli<-function(x,disc,disp){

  (.398942 * exp(-0.5 *((x-disc)/disp)*((x-disc)/disp))-(-((x-disc)/disp))*(1-pnorm(-((x-disc)/disp))))

}
