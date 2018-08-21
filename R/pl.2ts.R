

## dependent is the column name of the dependent variable
#' @name pl.2ts
#' @aliases pl.2ts
#' @title Time series plot for two variables
#' @description Plotting two time series in one plot, with title.
#' @usage pl.2ts(ts1,ts2,title)
#' @param ts1 :time series variable one
#' @param ts2 :time series variable two
#' @param title :title for the plot
#' @examples DAX <- EuStockMarkets[,1]
#' FTSE <- EuStockMarkets[,4]
#' pl.2ts(DAX,FTSE, "Times Series Plot of DAX and FTSE")



pl.2ts <- function(ts1, ts2,title){

  ts.plot(ts1, main = title, type = "l", lty = 1, ylab = "");lines(ts2, type = "l",lty = 3)

}
