###############################################################################################
##### standard error for reg.linreg()
###############################################################################################

#' @name reg.std.err
#' @aliases reg.std.err
#' @title Standard error for lim.fit
#' @description Calculate standard error for the outcome of lm.fit(). This function is built for reg.linreg for higher efficiency only.
#' It can't be used for calculating standard error in general operation.
#' @usage reg.std.err(SSE,dof)
#' @param SSE :error sum of squared aka. residual sum of squared
#' @param dof :degree of freedom
#' @examples X <- as.matrix(cbind(1,EuStockMarkets[,1:2])) # create the design matrix
#' Y <- as.data.frame(EuStockMarkets)$FTSE
#' fit <- lm.fit(x = X, y = Y)
#' SSE <- sum((Y - fit$fitted.values)^2)
#' dof <- reg.dof(fit)
#' reg.std.err(SSE,dof)



reg.std.err <- function(SSE,dof){


  return(sqrt(SSE/(dof)))

}
