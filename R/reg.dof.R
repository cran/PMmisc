###############################################################################################
##### df for reg.linreg()
###############################################################################################

#' @name reg.dof
#' @aliases reg.dof
#' @title Degree of freedom for lim.fit
#' @description Calculate degree of freedom for the outcome of lm.fit(). This function is built for reg.linreg for higher efficiency only.
#' It can't be used for calculating degree of freedom in general operation.
#' @usage reg.dof(fit)
#' @param fit :outcome of lm.f
#' @examples X <- as.matrix(cbind(1,EuStockMarkets[,1:2])) # create the design matrix
#' Y <- as.data.frame(EuStockMarkets)$FTSE
#' fit <- lm.fit(x = X, y = Y)
#' reg.dof(fit)



reg.dof <- function(fit){


  return(length(fit$fitted.values) - fit$rank)

}
