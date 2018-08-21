###############################################################################################
##### Correlation Matrix
###############################################################################################

#' @name ds.corm
#' @aliases ds.corm
#' @title Correlation matrix
#' @description Calculating the correlation matrix of a data frame and return in a data frame object
#' @usage ds.corm(x,n)
#' @param x :a data frame
#' @param n :number of decimal points
#' @examples ds.corm(mtcars,3)


ds.corm <- function(x,n){

  typeofvar <- sapply(x,class)
  ha <- names(typeofvar[typeofvar == "numeric" | typeofvar == "integer" | typeofvar == "double"])
  x <- x[,c(ha)]

  cormatirx <-data.frame(round(cor(x, use = "na.or.complete"), n))
  return(cormatirx)

}
