###########################################################################################################################
# regression model fitter and partial evaluator
###########################################################################################################################

#' @name reg.linreg
#' @aliases reg.linreg
#' @title Linear regression processor
#' @description This function will take a data frame and the dependent variable and fit all possible combinations of models.
#' The result will be a data frame of models and test statistics for all the models possible. The test statistics are current set and contain all
#' the following: R-squared, Adjusted R-squared, Degree of freedom, Residual standard error, AIC, BIC, Durbin-Watson statistic.
#' @usage reg.linreg(dataframe,dependent)
#' @param dataframe :a data frame, which includes the dependent variable
#' @param dependent :dependent variable
#' @examples reg.linreg(mtcars,"mpg")

reg.linreg <- function(dataframe, dependent){

  dff <- dataframe
  Y <- dataframe[,dependent]
  dataframe <- dataframe[,names(dataframe) != dependent]

  n <- dim(dataframe)[1]
  p <- dim(dataframe)[2]
  w<-rep(1,n)
  count <- 1
  me <- mean(Y)

  # model <- as.data.frame(matrix(nrow = 2^p-1, ncol = 1))
  rs <- numeric(2^p-1)
  ars <- numeric(2^p-1)
  std.err <- numeric(2^p-1)
  dof <- numeric(2^p-1)
  aic <- numeric(2^p-1)
  bic <- numeric(2^p-1)
  dw <- numeric(2^p-1)

  for (i in 1 : dim(dataframe)[2]) {

    comb <- combn(p,i)

    for (j in 1:dim(comb)[2]) {

      # create design matrix
      X <- as.matrix(cbind(1, dataframe[,comb[,j]]))

      #model[,count] <- comb[,j]

      # fitting model
      fit <- lm.fit(x = X, y = Y)

      # test statistics
      # r_squared
      SSE <- sum((Y - fit$fitted.values)^2)
      SSR <- sum((fit$fitted.values - me)^2)
      SSTO <- sum((Y - me)^2)

      rs[count] <- reg.r.squared(SSR,SSTO)

      # adjusted r squared
      ars[count] <- reg.adj.r.squared(rs,n,p)

      # degree of freedom
      dof[count] <- reg.dof(fit)

      # standard error
      std.err[count] <- reg.std.err(SSE,dof[count])

      # AIC
      aic[count] <- reg.aic(fit,w)

      # BIC
      bic[count] <- reg.bic(fit,w)

      # Durbin-Watcon
      dw[count] <- reg.dw(fit)


      #print(rs)
      #print(ars)
      #print(aic)
      #print(bic)
      #print(count)
      count <- count +1
    }


    result <- data.frame(reg.model(dff,dependent), ars, dof, std.err,aic, bic,dw)

  }

  return(result)
}
