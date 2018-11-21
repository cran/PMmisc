##################################################################################################
### Ploting Function
##################################################################################################


## dependent is the column name of the dependent variable
#' @name pl.sm
#' @aliases pl.sm
#' @title Plot scatter smooth plots for a data frame
#' @description Plotting scatter smooth plots for a data frame, with titles and label numbers.
#' @usage pl.sm(x,dependent,l)
#' @param x :a data frame, which includes the dependent variable
#' @param dependent :the dependent variable for scatter smooth plots
#' @param l : the beginning label number in the title (default set to 1)
#' @examples pl.sm(mtcars,"mpg",1)

pl.sm <- function(x,dependent,l = 1){

  x <- na.omit(x)
  yl <- dependent
  dependent <- x[,dependent]

  x <- x[,names(x) != yl]
  var <- names(x)
  n <- length(var)

  for(i in 1:n){
    scatter.smooth(x[,i], dependent, main = paste("Fig.", paste(i+1-1, paste("Scatter plot of",var[i],"v.",yl))), xlab = var[i], ylab = yl)
  }

}
