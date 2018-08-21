##################################################################################################
### Ploting Function
##################################################################################################


#' @name pl.hist
#' @aliases pl.hist
#' @title Plot histograms  for a data frame
#' @description Plotting histograms for a data frame, with titles and label numbers.
#' @param x :a data frame
#' @param l : the beginning label number in the title (default set to 1)
#' @examples pl.hist(mtcars,1)

pl.hist <- function(x,l = 1){



  x <- x
  var <- names(x)
  n <- length(var)

    for(i in 1:n){
      hist(x[,i], main = paste("Fig.", paste(i+l-1, paste("Histogram of",var[i]))), xlab = var[i])
    }

}
