###############################################################################################
##### Create a lag variable
###############################################################################################

#' @name cv.lag
#' @aliases cv.lag
#' @title Create a lag variable
#' @description Create a lag variable, with a choice of lag periods. The lag variable can be used
#' to test lag effects between variables.
#' @usage cv.lag(x,n)
#' @param x :a vector
#' @param n :number of lag periods
#' @examples cv.lag(mtcars[,2],3)
#' data.frame(mtcars,cv.lag(mtcars[,3], 1))

cv.lag <- function(x,n){

  len <- length(x)

  b <- c(as.numeric(rep("NA",n)), (lag(as.ts(x), k = n)))

  b <- b[1:len]


  return(b)

}

