##################################################################################################
### Ploting Function
##################################################################################################

## function to create histogram and scatter plots for data.frame:
## a = 0 for both
## a = 1 for histogram
## a = 2 for scatter plots
## dependent is the column name of the dependent variable
#' @name pl.hs
#' @aliases pl.hs
#' @title Plot histograms and scatter plots for a data frame
#' @description Plotting histograms or scatter plots of your choice for a data frame. Also the function will name the graphs and number them.
#' The purpose of the function is to save time when plotting graphs for a regression analysis or other usage.
#' The function can plot, name and number the graphs at one step.
#' @usage pl.hs(x,a,dependent,l)
#' @param x :a data frame
#' @param a :the type of graph you want; a = 1 for histograms; a = 2 for scatter plots; a = 0 for both
#' @param dependent :the dependent variable for scatterplots
#' @param l : the beginning label number in the title (default set to 1)
#' @examples pl.hs(mtcars,0,"mpg",1)

pl.hs <- function(x,a,dependent,l = 1){

  yl <- dependent
  dependent <- x[,dependent]

  x <- x[,names(x) != yl]
  var <- names(x)
  n <- length(var)

  if (a == 1){
    hist(dependent, main = paste("Fig.", paste(l, paste("Histogram of",yl))), xlab = yl)
    for(i in 1:n){
      hist(x[,i], main = paste("Fig.", paste(i+l, paste("Histogram of",var[i]))), xlab = var[i])
    }
  }else if(a == 2){
    for(i in 1:n){
      plot(x[,i], dependent, main = paste("Fig.", paste(i, paste("Scatter Plot of",var[i],"v.",yl))), xlab = var[i], ylab = yl)
    }
  }else{
    hist(dependent, main = paste("Fig.", paste(l, paste("Histogram of",yl))), xlab = yl)
    for(i in 1:n){
      hist(x[,i], main = paste("Fig.", paste(i+l, paste("Histogram of",var[i],"v.",yl))), xlab = var[i])}
    for(i in 1:n){
      plot(x[,i], dependent, main = paste("Fig.", paste(i+n+l, paste("Scatter Plot of",var[i],"v.",yl))), xlab = var[i], ylab = yl)
    }
  }
}
