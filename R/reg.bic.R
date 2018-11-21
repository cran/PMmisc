##################################################################################################
### BIC for lm.fit()
##################################################################################################


#' @name reg.bic
#' @aliases reg.bic
#' @title BIC for lm.fit
#' @description Calculate BIC for the outcome of lm.fit This function is built for reg.linreg() for higher efficiency only.
#' It can't be used for calculating BIC in general operation.
#' @usage reg.bic(fit,w)
#' @param fit :the outcome of lm.fit
#' @param w :wright
#' @examples X <- as.matrix(cbind(1,EuStockMarkets[,1:2])) # create the design matrix
#' Y <- as.data.frame(EuStockMarkets)$FTSE
#' fit <- lm.fit(x = X, y = Y)
#' w <- rep(1,length(Y))
#' reg.bic(fit,w)

reg.bic <- function(fit,w){

  return(log(length(w)) * (fit$rank + 1) - 2* (0.5 * (sum(log(w)) - length(w) * (log(2 * pi) + 1 - log(length(w)) + log(sum(w * fit$residuals^2))))))

}
