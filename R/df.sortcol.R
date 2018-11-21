###############################################################################################
##### sort a data frame by a column
###############################################################################################

#' @name df.sortcol
#' @aliases df.sortcol
#' @title Sort a data frame by a column
#' @description Sort a data frame by a column of choice. The column of choice is specified by the number of the column.
#' @usage df.sortcol(x,n,desc)
#' @param x :a data frame
#' @param n :number column to sort
#' @param desc :the order of sorting, default set to TRUE; for ascending order set to FALSE
#' @examples df.sortcol(mtcars,2,desc = TRUE)

df.sortcol <- function(x, n, desc = T){

  if(desc == FALSE){

    x[order(x[,n]), ]

  }else{

    x[order(x[,n], decreasing = T), ]

  }

}

