###############################################################################################
##### log simulator
###############################################################################################

#' @name cv.logs
#' @aliases cv.logs
#' @title Create logarithm with a random base
#' @description Create a new variable that is the logarithm of the selected variable with the base of a random number
#' @usage cv.logs(dataframe, var, n, range)
#' @param dataframe :a data frame
#' @param var :the variable selected
#' @param n :number of new variables created
#' @param range :the range of base
#' @examples cv.logs(mtcars,"wt",5,c(1, 2))

cv.logs <- function(dataframe, var, n, range){

  endcol <- dim(dataframe)[2]
  namestring <- names(dataframe)

  for (i in 1:n) {


    logbase <- runif(1, range[1], range[2])

    dataframe[, endcol + i] <- log(dataframe[, var],base = logbase)

    namestring <- c(namestring, paste0("log", as.character(round(logbase, 2)), var))

  }

  names(dataframe) <- namestring
  return(dataframe)

}
