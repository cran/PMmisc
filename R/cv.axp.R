###############################################################################################
##### axp simulator
###############################################################################################

#' @name cv.axp
#' @aliases cv.axp
#' @title Create logarithm with a random base
#' @description Create a new variable with the base of a random number and power of the selected variable
#' @usage cv.axp(dataframe, var, n, range)
#' @param dataframe :a data frame
#' @param var :the variable selected
#' @param n :number of new variables created
#' @param range :the range of base
#' @examples cv.axp(mtcars,"wt",5,c(1, 2))

cv.axp <- function(dataframe, var, n, range){

  endcol <- dim(dataframe)[2]
  namestring <- names(dataframe)

  for (i in 1:n) {


    ax <- runif(1, range[1], range[2])

    dataframe[, endcol + i] <- ax ^ dataframe[, var]

    namestring <- c(namestring, paste0("x", as.character(round(ax,2)), "to", var))

  }

  names(dataframe) <- namestring
  return(dataframe)

}
