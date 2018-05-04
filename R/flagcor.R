###############################################################################################
##### Lag/Lead Correlation
###############################################################################################

#' @name flagcor
#' @aliases flagcor
#' @title Lag/Lead Correlation
#' @description Calculating correlation of two vectors with lag and lead periods. The correlations are
#' used to determine the lag or lead effect between two variables. The corelattion function uses "na.or.complete"
#' method and calculate the pearson's correlation.
#' @usage flagcor(x,y,lag,lead)
#' @param x :the moving  vector
#' @param y :the fixed vector
#' @param lag :number of lag periods
#' @param lead :number of lead periods
#' @examples flagcor(mtcars[,1],mtcars[,2],3,3)

flagcor <- function(x,y,lag,lead){

  # output dataframe
a <- data.frame(matrix(nrow = 1, ncol = lag+lead+1))
names(a) <- c(paste0("lag",1:lag),"0",paste0("lead",1:lead))


for(i in 1:lag){

  b <- na.omit(cbind(flag(x,i),y))
  a[i]<-cor(b[,1],b[,2],use = "na.or.complete")

}

a[lag+1] <- cor(x,y,use = "na.or.complete")

for (i in 1:lead) {

  b <- na.omit(cbind(flead(x,i),y))
  a[i+lag+1] <- cor(b[,1],b[,2],use = "na.or.complete")


}

return(a)

}
