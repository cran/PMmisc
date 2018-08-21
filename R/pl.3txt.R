###############################################################################################
##### scatter plot with text overlay
###############################################################################################

#' @name pl.3txt
#' @aliases pl.3txt
#' @title Scatter plot with text overlay
#' @description Generate a scatter plot with text overlay. This plot is to better show the effect of
#' the text variable in the domain of x and y variable.
#' @usage pl.3txt(x,y,txt)
#' @param x : a numeric vector
#' @param y : a numeric vector
#' @param txt : a vector used as labels
#' @examples pl.3txt(mtcars[,1], mtcars[,3], row.names(mtcars))


pl.3txt <- function(x, y, txt){

  plot(x, y, type = "n"); text(x,y,txt, cex = .5)

}
