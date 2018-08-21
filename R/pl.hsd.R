

## dependent is the column name of the dependent variable
#' @name pl.hsd
#' @aliases pl.hsd
#' @title Plot histogram with density line for a data frame
#' @description Plotting histogram with density for a data frame, with titles and label numbers.
#' @usage pl.hsd(dataframe,l)
#' @param dataframe :a data frame
#' @param l : the beginning label number in the title (default set to 1)
#' @examples pl.hsd(mtcars,1)



pl.hsd <- function(dataframe, l){

  num <- dim(dataframe)[2]

  for(i in 1:num){

    xlabel <- names(dataframe)[i]
    title <- paste("Fig.", as.character(l + i - 1),"Histogram of", xlabel)
    hist(dataframe[,i], main = title, xlab = xlabel, probability = T); lines(density(dataframe[,i]))

  }

}
