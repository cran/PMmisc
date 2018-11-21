###############################################################################################
##### adj.r.squred for reg.linreg()
###############################################################################################

#' @name reg.adj.r.squared
#' @aliases reg.adj.r.squared
#' @title Adjusted R-squared for lm.fit
#' @description Calculate Adjusted R-squared for the outcome of lm.fit. This function is built for reg.linreg() for higher efficiency only.
#' It can't be used for calculating Adjusted R-squared in general operation.
#' @usage reg.adj.r.squared(r,n,p)
#' @param r :R-squared for regression
#' @param n :number of observations aka. sample size
#' @param p :number of explanatory variables in the model
#' @examples X <- as.matrix(cbind(1,EuStockMarkets[,1:2])) # create the design matrix
#' Y <- as.data.frame(EuStockMarkets)$FTSE
#' fit <- lm.fit(x = X, y = Y)
#' SSR <- sum((fit$fitted.values - mean(Y))^2)
#' SSTO <- sum((Y - mean(Y))^2)
#' r <- reg.r.squared(SSR,SSTO)
#' n <- dim(X)[1]; p <- dim(X)[2]
#' reg.adj.r.squared(r,n,p)



reg.adj.r.squared <- function(r,n,p){


  return(1-((1 - r) * (n - 1) / (n - p - 1)))

}
