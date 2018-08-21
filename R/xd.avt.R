###############################################################################################
##### Download data from Alpha Vantage
###############################################################################################

#' @name xd.avt
#' @aliases xd.avt
#' @title Download data from Alpha Vantage
#' @description This function will return time series data of financial securities from Alpha Vantage.
#'  Before using this function, a onetime registration is required to obtain a APIKEY, which is unique
#'  for users. Also, the key is valid for lifetime.
#' @usage xd.avt(ticker,type,size="full",apikey,interval)
#' @param ticker :the ticker for desired financial security. e.g. AAPL, MSFT
#' @param type :data type; usable values: "TIME_SERIES_INTRADAY", "TIME_SERIES_DAILY", "TIME_SERIES_DAILY_ADJUSTED",
#' "TIME_SERIES_WEEKLY", "TIME_SERIES_WEEKLY_ADJUSTED", "TIME_SERIES_MONTHLY", "TIME_SERIES_MONTHLY_ADJUSTED"
#' @param size :the size of data downloaded; use "compact" for the latest 100 observations and "full" for at most the last 20
#' years as claimed by the website. However, most data only go back to Jan. 2000.
#' @param apikey :a string that is obtained by a onetime registration
#' @param interval :required for intraday data; usable values: "1min", "5min", "15min", "30min", "60min"
#' @examples
#' # All parameters are required to be strings
#' # The example is to download Ford Motor Company's daily adjusted price.
#' # The apikey here is just for demonstration purposes.
#' T <- xd.avt("T","TIME_SERIES_DAILY","full","QB45BDBGP0O7W8TB")

xd.avt <- function(ticker, type, size = "full", apikey, interval){

  if(type == "TIME_SERIES_INTRADAY"){

    url <- paste0("https://www.alphavantage.co/query?function=",type,"&symbol=",ticker,"&interval=",interval,"&outputsize=",size,"&apikey=",apikey,"&datatype=csv")

  }else{

    url <- paste0("https://www.alphavantage.co/query?function=",type,"&symbol=",ticker,"&outputsize=",size,"&apikey=",apikey,"&datatype=csv")

  }
  return(read.csv(url))
}


