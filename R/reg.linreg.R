###########################################################################################################################
# regression model fitter and partial evaluator
###########################################################################################################################

# dependent <- "wt"

# dataframe <- mtcars

#' @name reg.linreg
#' @aliases reg.linreg
#' @title Linear regression processor
#' @description This function will take a data frame and the dependent variable, and fit all possible combinations of models.
#' The result will be a data frame of models and test statistics for all the models possible.
#' @usage reg.linreg(dataframe,dependent)
#' @param dataframe :a data frame, which includes the dependent variable
#' @param dependent :dependent variable
#' @examples reg.linreg(mtcars,"mpg")

reg.linreg <- function(dataframe, dependent){

  models <- reg.model(dataframe, dependent)

  regnum <- dim(models)[1]

  # results of regression w/ partial evaluation, diagnostic statistics to be added
  reg_result <- as.data.frame(matrix(nrow = regnum, ncol = 3))
  reg_result[,1] <- models[,2]
  names(reg_result) <- c("model", "r-squared", "adj-r-squared")

  for(i in 1:regnum){

    formula <- as.formula(models[i,2])

    Fit <- lm(formula, dataframe, na.action = na.omit)

# test statistics
    reg_result[i,2] <- summary(Fit)[8]
    reg_result[i,3] <- summary(Fit)[9]

  }
  return(reg_result)
}
