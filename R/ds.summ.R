###############################################################################################
##### Descriptive statistics
###############################################################################################

#' @name ds.summ
#' @aliases ds.summ
#' @title Descriptive statistics of a data frame
#' @description Calculating the descriptive statistics of a data frame and exporting in a data frame.
#' The report data frame contains: number of observations, maximum value, minimum value, mean, median,
#' mode, variance, standard deviation, skewness and kurtosis.
#' @usage ds.summ(x,n)
#' @param x :a data frame
#' @param n :number of decimal points rounded
#' @examples ds.summ(mtcars,3)

ds.summ <- function(x,n){


  des <- as.data.frame(matrix(nrow = dim(x)[2], ncol = 11))
  names(des) <- c("name","obs","max","min","mean","median","mode","var","std","skew","kurt")
  des[,1] <- names(x)
  des[,2] <- round(apply(x,2, length), n)
  des[,3] <- round(apply(x,2, max, na.rm = T), n)
  des[,4] <- round(apply(x,2, min, na.rm = T), n)
  des[,5] <- round(apply(x,2,mean, na.rm = T), n)
  des[,6] <- round(apply(x,2,median, na.rm = T), n)
  des[,7] <- round(apply(x,2,ds.mode), n)
  des[,8] <- round(apply(x,2,var, na.rm = T), n)
  des[,9] <- round(apply(x,2,sd, na.rm = T), n)
  des[,10] <- round(apply(x,2,ds.skew), n)
  des[,11] <- round(apply(x,2,ds.kurt), n)

  return(des)
}
