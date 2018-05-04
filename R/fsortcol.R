###############################################################################################
##### sort a data frame by a column
###############################################################################################

#' @name fsortcol
#' @aliases fsortcol
#' @title Sort a data frame by a column
#' @description Sort a data frame by a column of choice. The column of choice is specified by the number of the column.
#' @usage fsortcol(x,n,dec)
#' @param x :a data frame
#' @param n :number column to sort
#' @param dec :the order of sorting, default set to TRUE
#' @examples fsortcol(mtcars,1,dec = TRUE)

fsortcol <- function(x, n, dec = T){

  if(dec == FALSE){

    x[order(x[,1]), ]

  }else{

    x[order(x[,1], decreasing = T), ]

  }

}

