
#' @name pl.tss
#' @aliases pl.tss
#' @title Time series plot with multiple variables
#' @description This function will return a time series plot with up to 6 variables, each with different line type.
#' @usage pl.tss(dataframe,ylb,title)
#' @param dataframe :a data frame
#' @param ylb :y-axis label
#' @param title :plot title
#' @examples pl.tss(EuStockMarkets,"Price","Daily Closing Prices of Major European Stock Indices")

pl.tss <- function(dataframe,ylb,title){

  if (dim(dataframe)[2] <= 6) {

    ts.plot(dataframe[,1], lty = 1, ylab = ylb,main = title)

    for (i in 2:dim(dataframe)[2]) {


      lines(dataframe[,i],lty = i)

    }

  }else{

    print("The data frame has more than 6 variables and has exceeded the limt of line types")

  }

}

