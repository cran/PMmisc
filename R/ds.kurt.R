###############################################################################################
##### kurtosis
###############################################################################################

#' @name ds.kurt
#' @aliases ds.kurt
#' @title Calculating kurtosis for numeric data.
#' @description Kurtosis
#' @usage ds.kurt(x)
#' @param x :a numeric variable
#' @examples ds.kurt(mtcars[,2])


ds.kurt <- function(x){

  x <- na.omit(x)
  me <- mean(x,na.rm = T)
  med <- median(x, na.rm = T)
  std <- sd(x,na.rm = T)
  kurtosis <- mean((x - me)^4)/(mean((x-me)^2)^2)

  return(kurtosis)

}
