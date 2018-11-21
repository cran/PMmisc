##################################################################################################
### AIC for lm.fit()
##################################################################################################


#' @name reg.aic
#' @aliases reg.aic
#' @title AIC for lm.fit
#' @description Calculate AIC for the outcome of AIC. This function is built for reg.linreg for higher efficiency only.
#' It can't be used for calculating AIC in general operation.
#' @usage reg.aic(fit,w)
#' @param fit :the outcome of lm.fit
#' @param w :wright
#' @examples X <- as.matrix(cbind(1,EuStockMarkets[,1:2])) # create the design matrix
#' Y <- as.data.frame(EuStockMarkets)$FTSE
#' fit <- lm.fit(x = X, y = Y)
#' w <- rep(1,length(Y))
#' reg.aic(fit,w)

reg.aic <- function(fit, w){

  return(2 * (fit$rank + 1) - 2* (0.5 * (sum(log(w)) - length(w) * (log(2 * pi) + 1 - log(length(w)) + log(sum(w * fit$residuals^2))))))

}
