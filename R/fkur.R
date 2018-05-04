###############################################################################################
##### kurtosis
###############################################################################################

#' @name fkur
#' @aliases fkur
#' @title Calculating kurtosis for numeric data.
#' @description Kurtosis
#' @usage fkur(x)
#' @param x :a numeric variable
#' @examples fkur(mtcars[,2])


fkur <- function(x){
  x <- na.omit(x)
  me <- mean(x,na.rm = T)
  med <- median(x, na.rm = T)
  std <- sd(x,na.rm = T)
  kurtosis <- mean((x - me)^4)/(mean((x-me)^2)^2)
  return(kurtosis)
}
