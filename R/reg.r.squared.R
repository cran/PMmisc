###############################################################################################
##### r.squred for reg.linreg()
###############################################################################################

#' @name reg.r.squared
#' @aliases reg.r.squared
#' @title R-squared for lm.fit
#' @description Calculate R-squared for the outcome of lm.fit(). This function is built for reg.linreg for higher efficiency only.
#' It can't be used for calculating R-squared in general operation.
#' @usage reg.r.squared(SSR,SSTO)
#' @param SSR :regression sum of squares or explained of squares
#' @param SSTO :total sum of squares
#' @examples X <- as.matrix(cbind(1,EuStockMarkets[,1:2])) # create the design matrix
#' Y <- as.data.frame(EuStockMarkets)$FTSE
#' fit <- lm.fit(x = X, y = Y)
#' me <- mean(Y)
#' SSR <- sum((fit$fitted.values - me)^2)
#' SSTO <- sum((Y - me)^2)
#' reg.r.squared(SSR,SSTO)


reg.r.squared <- function(SSR,SSTO){


  return(SSR/SSTO)

}

