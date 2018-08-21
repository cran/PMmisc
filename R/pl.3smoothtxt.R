###############################################################################################
##### scatter plot with text overlay
###############################################################################################

#' @name pl.3smoothtxt
#' @aliases pl.3smoothtxt
#' @title Scatter smooth plot with text overlay
#' @description Generate a scatter plot with text overlay, with a smooth curve fitted by loess.
#' @usage pl.3smoothtxt(x,y,txt,ce)
#' @param x : a numeric vector
#' @param y : a numeric vector
#' @param txt : a vector used as labels
#' @param ce : text size, which default is set as 0.5
#' @examples pl.3smoothtxt(mtcars[,1], mtcars[,3], row.names(mtcars))


pl.3smoothtxt <- function(x, y, txt, ce=.5){

  plotdf <- na.omit(data.frame(x,y))

  scatter.smooth(x, y, type = "n"); text(x,y,txt, cex = ce)

}
