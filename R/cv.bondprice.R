##################################################################################################
### Plain Vanilla Bond
##################################################################################################

#' @name cv.bondprice
#' @aliases cv.bondprice
#' @title Calculate the plain vanilla bond price
#' @description Calculate the plain vanilla bond price
#' @usage cv.bondprice(par,c,yield,n,m)
#' @param par :the face value of the bond
#' @param c :the annual coupon rate of the bond
#' @param yield :the annual yield to maturity of a bond
#' @param n :number of years
#' @param m :compounding period in a year
#' @examples cv.bondprice(1000,0.0248,0.0248,10,2)


cv.bondprice <- function(par,c,yield,n,m){

  pmt <- c*par/m

  p <- pmt*((1-((1+yield/m)^(-m*n)))/(yield/2))+(par/((1+yield/m)^(m*n)))

  return(p)

}
