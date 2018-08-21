###############################################################################################
##### Lag/Lead Correlation
###############################################################################################

#' @name cor.lag
#' @aliases cor.lag
#' @title Lag/Lead Correlation
#' @description Calculating correlation of two vectors with lag and lead periods. The correlations are
#' used to determine the lag or lead effect between two variables. The correlation function uses "na.or.complete"
#' method and calculate the Pearson's correlation.
#' @usage cor.lag(x,y,lag,lead)
#' @param x :the moving  vector
#' @param y :the fixed vector
#' @param lag :number of lag periods
#' @param lead :number of lead periods
#' @examples cor.lag(mtcars[,1],mtcars[,2],3,3)

cor.lag <- function(x,y,lag,lead){

  # output dataframe
a <- data.frame(matrix(nrow = 1, ncol = lag+lead+1))

names(a) <- c(paste0("lag",1:lag),"0",paste0("lead",1:lead))


for(i in 1:lag){

  b <- na.omit(cbind(cv.lag(x,i),y))
  a[i]<-cor(b[,1],b[,2],use = "na.or.complete")

}

a[lag+1] <- cor(x,y,use = "na.or.complete")

for (i in 1:lead) {

  b <- na.omit(cbind(cv.lead(x,i),y))
  a[i+lag+1] <- cor(b[,1],b[,2],use = "na.or.complete")


}

return(a)

}
