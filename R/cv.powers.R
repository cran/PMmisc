###############################################################################################
##### power simulator
###############################################################################################

#' @name cv.powers
#' @aliases cv.powers
#' @title Create nth power variable
#' @description Create a new variable that is the nth power of the selected variable
#' @usage cv.powers(dataframe, var, n, range)
#' @param dataframe :a data frame
#' @param var :the variable selected
#' @param n :number of new variables created
#' @param range :the range of power
#' @examples cv.powers(mtcars,"wt",5,c(1, 2))

cv.powers <- function(dataframe, var, n, range){

  endcol <- dim(dataframe)[2]
  namestring <- names(dataframe)

  for (i in 1:n) {


    nth <- runif(1, range[1], range[2])

    dataframe[, endcol + i] <- dataframe[, var] ^ nth

    namestring <- c(namestring, paste0(var,"to",as.character(round(nth,2))))

  }

  names(dataframe) <- namestring
  return(dataframe)

}
