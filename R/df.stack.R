###############################################################################################
##### Stacking data frame by one classifier
###############################################################################################

#' @name df.stack
#' @aliases df.stack
#' @title Stack data frame by one classifier
#' @description Stack data frame by one classifier. This function takes the first column as a ordering variable.
#' Then it take the variables names and repeat as the second column. The last column will be data under each variable name. This
#' function is created to enable easier investigation with apply functions.
#' @usage df.stack(df,name)
#' @param df : a data frame used to stack
#' @param name : new variable names of the data frame
#' @examples df <- data.frame(matrix(nrow=100,ncol=100))
#'for(i in 1:100){
#'  df[,i] <- rep(runif(1,1,100),100)
#'}
#'dim(df)
#' hdf <- df.stack(df,c("date","tkr","price"))

df.stack <- function(df,name){

  nRow <- dim(df)[1]
  nCol <- dim(df)[2]

  stackdf <- data.frame(matrix(nrow = 1, ncol = 2))
  nn <-names(stackdf)

  tempdf  <- data.frame(matrix(nrow = 1, ncol = 2))
  colN <- names(df)

  for (i in 2:nCol) {

    tempdf <- data.frame(rep(colN[i], nRow), df[,i])
    names(tempdf) <- nn
    stackdf <- rbind(stackdf, tempdf)

  }

  firstcol <- rep(df[,1],nCol-1)
  stackdf <- stackdf[-1,]
  finaldf <- cbind(firstcol, stackdf)
  names(finaldf) <- name

  return(finaldf)
}
