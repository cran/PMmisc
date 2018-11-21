##################################################################################################
### Plotting Function
##################################################################################################


#' @name pl.multiplot
#' @aliases pl.multiplot
#' @title Multiple  plot function for ggplot2 objects
#' @description Render multiple ggplt2 plots on one page
#' @usage pl.multiplot(..., plotlist = NULL, cols = 1, layout = NULL)
#' @param ... :ggplot2 objects
#' @param plotlist :a list of ggplot2 objects
#' @param cols :number of columns in the layout
#' @param layout :a matrix that specifies the layout. It has higher priority than 'cols'
#' @references https://stackoverflow.com/questions/24387376/r-error-could-not-find-function-multiplot-using-cookbook-example


pl.multiplot <- function(..., plotlist = NULL, cols = 1, layout = NULL) {

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }

  if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
