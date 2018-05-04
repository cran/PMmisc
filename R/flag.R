###############################################################################################
##### Create a lag variable
###############################################################################################

#' @name flag
#' @aliases flag
#' @title Create a lag variable
#' @description Create a lag variable, with a choice of lag periods. The lag variable can be used
#' to test lag effects between variables.
#' @usage flag(x,n)
#' @param x :a vector
#' @param n :number of lag periods
#' @examples flag(mtcars[,2],3)

flag <- function(x,n){

  b <- c(as.numeric(rep("NA",n)), (lag(as.ts(x), k = n)))

  b <- b[1:length(b)-1]

  return(b)

}

