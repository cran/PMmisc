###############################################################################################
##### Create a lead variable
###############################################################################################

#' @name cv.lead
#' @aliases cv.lead
#' @title Create a lead variable
#' @description Create a lead variable, with a choice of lead periods. The lead variable can be used
#' to test lead effects between variables.
#' @usage cv.lead(x,n)
#' @param x :a vector
#' @param n :number of lead periods
#' @examples cv.lead(mtcars[,2],3)
#' data.frame(mtcars,cv.lead(mtcars[,3], 3))

cv.lead <- function(x,n){

  b <- c(x, as.numeric(rep("NA",n)))

  b <- b[n+1:length(x)]

  return(b)

}
