###############################################################################################
##### DW Test
###############################################################################################

#' @name reg.dw
#' @aliases reg.dw
#' @title Durbin-Watson Test
#' @description Performs the Durbin-Watson Test for a regression model
#' @usage reg.dw(fit)
#' @param fit :a lm object
#' @examples fit <- lm(mpg~wt, mtcars, na.action = na.omit)
#' reg.dw(fit)


reg.dw <- function(fit){

  # hide warning message created by cv.lag()
  options(warn=-1)

  down <- sum(fit$residuals^2)
  up <- sum((fit$residuals - cv.lag(fit$residuals, 1))^2, na.rm = TRUE)

  return(up/down)

}
