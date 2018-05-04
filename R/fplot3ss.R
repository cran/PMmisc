###############################################################################################
##### scatter plot with text overlay
###############################################################################################

#' @name fplot3ss
#' @aliases fplot3ss
#' @title Scatter smooth plot with text overlay
#' @description Generate a scatter plot with text overlay, with a smooth curve fitted by loess.
#' @usage fplot3ss(x,y,txt,ce)
#' @param x : a numeric vector
#' @param y : a numeric vector
#' @param txt : a vector used as labels
#' @param ce : text size, which default is set as 0.5
#' @examples fplot3ss(mtcars[,1], mtcars[,3], row.names(mtcars))


fplot3ss <- function(x, y, txt,ce=.5){

  plotdf <- na.omit(data.frame(x,y))
  scatter.smooth(x, y, type = "n"); text(x,y,txt, cex = ce)

}
