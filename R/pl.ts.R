##################################################################################################
### Ploting Function
##################################################################################################


#' @name pl.ts
#' @aliases pl.ts
#' @title Plot time series plots  for a data frame
#' @description Plotting time series plots for a data frame, with titles and label numbers.
#' @param x :a data frame
#' @param l : the beginning label number in the title (default set to 1)
#' @examples pl.ts(mtcars,1)

pl.ts <- function(x,l = 1){

  x <- x
  var <- names(x)
  n <- length(var)

  for(i in 1:n){
    ts.plot(x[,i], main = paste("Fig.", paste(i+l-1, paste("Timeseries plot of",var[i]))), xlab = var[i])
  }

}
