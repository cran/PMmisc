###############################################################################################
##### Download data from Federal Reserve Bank of St. Louis
###############################################################################################

#' @name xd.fred
#' @aliases xd.fred
#' @title Download data from Federal Reserve Bank of St. Louis
#' @description This function returns a data from the Federal Reserve Bank of St. Louis database
#' @usage xd.fred(tkr, start_date, end_date)
#' @param tkr :data tickers used by the database
#' @param start_date :starting date of the data(default is set as 1900-01-01)
#' @param end_date :ending date of the data(default is set as 2018-01-01)
#' @examples
#' cpi <- xd.fred("CPIAUCSL") # CPI data
#' head(cpi)
#' tail(cpi)
#'
#' #Frequently used tickers:
#' #CPIAUCSL: Consumer Price Index for All Urban Consumers: All Items
#' #A191RL1Q225SBEA: Real Gross Domestic Product
#' #DGS10: 10-Year Treasury Constant Maturity Rate
#' #UNRATE: Civilian Unemployment Rate

xd.fred <- function(tkr, start_date = "1900-01-01", end_date = "2018-01-01"){

  url <- paste0("https://fred.stlouisfed.org/graph/fredgraph.csv?chart_type=line&recession_bars=on&log_scales=&bgcolor=%23e1e9f0&graph_bgcolor=%23ffffff&fo=Open+Sans&ts=12&tts=12&txtcolor=%23444444&show_legend=yes&show_axis_titles=yes&drp=0&cosd=",start_date,"&coed=",end_date,"&height=450&stacking=&range=&mode=fred&id=",tkr,"&transformation=lin&nd=1947-01-01&ost=-99999&oet=99999&lsv=&lev=&mma=0&fml=a&fgst=lin&fgsnd=2009-06-01&fq=Monthly&fam=avg&vintage_date=&revision_date=&line_color=%234572a7&line_style=solid&lw=2&scale=left&mark_type=none&mw=2&width=1168")


  return(read.csv(url))

}
