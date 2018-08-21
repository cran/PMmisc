###############################################################################################
##### Spearman Rank Correlation
###############################################################################################

#' @name cor.spearman
#' @aliases cor.spearman
#' @title Spearman rank correlation
#' @description Calculate Spearman Rank Correlation, which is the nonparametric version of the Pearson product-moment correlation.
#' @usage cor.spearman(x,y)
#' @param x :a numeric variable
#' @param y :a numeric variable
#' @examples  cor.spearman(mtcars[,1], mtcars[,3])


cor.spearman <- function(x,y){
  # compute Spearman Rank Correlation for 2 vectors
  # nonlinear, non-parametric correlation
  cordf <- na.omit(data.frame(x, y))
  cordf$xr <- rank(cordf$x)
  cordf$yr <- rank(cordf$y)
  n <- dim(cordf)[1]
  r <- 1 - 6*sum((cordf$xr - cordf$yr)^2) / (n*(n^2 - 1))

  return(r)
}
