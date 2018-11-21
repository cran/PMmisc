##################################################################################################
### Ploting Function
##################################################################################################


## dependent is the column name of the dependent variable
#' @name pl.s
#' @aliases pl.s
#' @title Plot scatter plots for a data frame
#' @description Plotting scatter plots for a data frame, with titles and label numbers.
#' @usage pl.s(x,dependent,l)
#' @param x :a data frame, which includes the dependent variable
#' @param dependent :the dependent variable for scatter pl.s
#' @param l : the beginning label number in the title (default set to 1)
#' @examples pl.s(mtcars,"mpg",1)

pl.s <- function(x,dependent,l = 1){

  x <- na.omit(x)
  yl <- dependent
  dependent <- x[,dependent]

  x <- x[,names(x) != yl]
  var <- names(x)
  n <- length(var)

  for(i in 1:n){
    plot(x[,i], dependent, main = paste("Fig.", paste(i+1-1, paste("Scatter plot of",var[i],"v.",yl))), xlab = var[i], ylab = yl)
  }

}
