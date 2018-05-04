###############################################################################################
##### Spearman Rank Correlation
###############################################################################################

#' @name fspearcor
#' @aliases fspearcor
#' @title Spearman Rank Correlation
#' @description Calculate Spearman Rank Correlation, which is the nonparametric version of the Pearson product-moment correlation.
#' @usage fspearcor(x,y)
#' @param x :a numeric varibale
#' @param y :a numeric varibale
#' @examples  fspearcor(mtcars[,1], mtcars[,3])


fspearcor <- function(x,y){
  # compute Spearman Rank Correlation for 2 vectors
  # nonlinear, non-parametric correlation
  cordf <- na.omit(data.frame(x, y))
  cordf$xr <- rank(cordf$x)
  cordf$yr <- rank(cordf$y)
  n <- dim(cordf)[1]
  r <- 1 - 6*sum((cordf$xr - cordf$yr)^2) / (n*(n^2 - 1))

  return(r)
}
