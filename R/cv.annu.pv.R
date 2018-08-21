##################################################################################################
### Deferred Annuity Present Value
##################################################################################################

#' @name cv.annu.pv
#' @aliases cv.annu.pv
#' @title Calculate present value of annuity
#' @description Calculate present value of an ordinary annuity or an annuity due.
#' @usage cv.annu.pv(pmt,i,n,k)
#' @param pmt :the equal amount of payment of each period
#' @param i :interest rate according to the period
#' @param n :number of periods
#' @param k :number of periods deferred until first payment
#' @examples cv.annu.pv(100,0.0248,10,4)


cv.annu.pv <- function(pmt,i,n,k){

  pv <- pmt*((1-((1+i)^-n))/i)*((1+i)^-k)
  round(pv,2)

  return(pv)
}
