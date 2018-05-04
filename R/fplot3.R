###############################################################################################
##### scatter plot with text overlay
###############################################################################################

#' @name fplot3
#' @aliases fplot3
#' @title Scatter plot with text overlay
#' @description Generate a scatter plot with text overlay. This plot is to better show the effect of
#' the text variable in the domain of x and y variable.
#' @usage fplot3(x,y,txt)
#' @param x : a numeric vector
#' @param y : a numeric vector
#' @param txt : a vector used as labels
#' @examples fplot3(mtcars[,1], mtcars[,3], row.names(mtcars))


fplot3 <- function(x, y, txt){

  plot(x, y, type = "n"); text(x,y,txt, cex = .5)

}
