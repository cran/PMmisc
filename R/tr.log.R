###############################################################################################
##### Sigmoid function
###############################################################################################

#' @name tr.log
#' @aliases tr.log
#' @title Sigmoid function
#' @description Generate sigmoid curve series, which is a specific case of
#' logistic function, with a control of top and bottom acceleration.
#' @usage tr.log(x,top,a,b)
#' @param x :a numeric vector
#' @param top :a numeric value as vertical scaler
#' @param a :a number to control top acceleration of the curve
#' @param b :a number to control bottom acceleration of the curve
#' @examples  sigc <- round(tr.log(seq(-3, 3, 0.1), 1, -3, 3), 3)
#' ts.plot(sigc)

tr.log<-function(x, top, a, b){

  a<- -a

  b<- -b

  top/(1 + exp(a + b * x))

}
