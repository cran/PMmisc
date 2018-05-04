###############################################################################################
##### Create a lead variable
###############################################################################################

#' @name flead
#' @aliases flead
#' @title Create a lead variable
#' @description Create a lead variable, with a choice of lead periods. The lead variable can be used
#' to test lead effects between variables.
#' @usage flead(x,n)
#' @param x :a vector
#' @param n :number of lead periods
#' @examples flead(mtcars[,2],3)

flead <- function(x,n){

  b <- c(x, as.numeric(rep("NA",n)))

  b <- b[n+1:length(b)]

  return(b)

}
