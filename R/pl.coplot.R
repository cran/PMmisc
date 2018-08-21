###############################################################################################
##### Conditioning plot
###############################################################################################

#' @name pl.coplot
#' @aliases pl.coplot
#' @title Scatter plot of x and y divided by z
#' @description Generate 4 scatter plots of x and y divided by variable z, with a fitted line using a robust linear regression method.
#' @usage pl.coplot(x,y,z,varN)
#' @param x :x-axis value
#' @param y :y-axis value
#' @param z :classification variable used to condition plots based on ascending values of z
#' @param varN :variable name of z
#' @examples pl.coplot(mtcars[,1], mtcars[,3], mtcars[,4], "hp")


pl.coplot <- function(x, y, z, varN){

  plotdf <- na.omit(data.frame(x,y,z))
  plotdf$r <- rank(plotdf$z)
  plotdf$cat <- as.numeric(cut(plotdf$r, 4))
  xlim1 <- min(x, na.rm=T); xlim2 <- max(x,na.rm=T)
  ylim1 <- min(y, na.rm=T); ylim2 <- max(y,na.rm=T)

  par(mfcol=c(2,2))

  for(i in 1:4){

    pdf <- plotdf[plotdf$cat == i,]
    plot(pdf$x, pdf$y, xlim = c(xlim1, xlim2), ylim = c(ylim1, ylim2)); title(varN)
    abline(robust::lmRob(y ~ x, data = pdf))
  }

}
