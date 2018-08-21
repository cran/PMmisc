###############################################################################################
##### Mode for numeric variables
###############################################################################################

ds.mode <- function(x){

  x <- na.omit(x)
  uniquex <- unique(x)
  uniquex[which.max(tabulate(match(x, uniquex)))]

}
