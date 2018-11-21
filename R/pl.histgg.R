##################################################################################################
### Ploting Function
##################################################################################################


#' @name pl.histgg
#' @aliases pl.histgg
#' @title Plot histograms for a data frame with ggplot2
#' @description Plotting histograms for a data frame with 4 per image, with titles and label numbers automatically generated.
#' @param x :a data frame
#' @param l : the beginning label number in the title (default set to 1)
#' @note This function uses Freedman-Diaconis rule for histogram bin size calculation. In some occasions feed discrete variables to this function
#' may yield terrible results.
#' @references  Freedman, David; Diaconis, Persi (December 1981). "On the histogram as a density estimator: L2 theory" (PDF). Probability Theory and Related Fields. Heidelberg: Springer Berlin. 57 (4): 453–476. doi:10.1007/BF01025868. ISSN 0178-8051. Retrieved 2009-01-06.
#' @examples pl.histgg(as.data.frame(EuStockMarkets),1)


pl.histgg <- function(x,l = 1){

  columns <- 2
  x <- x
  var <- names(x)
  n <- length(var)

  plots <- list()
      for(i in 1:n){

      plots[[i]] <- ggplot(data = x, aes(x[,i])) +
        geom_histogram(binwidth = bw <- diff(range(x)) / (2 * IQR(x[,i]) / length(x)^(1/3))) +
        ggtitle(paste("Fig.",as.character(i+l-1), "Histogram of", var[i])) +
        theme(plot.title = element_text(hjust = 0.5)) +
        labs(y = "Frequency", x = var[i])


      # hist(x[,i], main = paste("Fig.", paste(i+l-1, paste("Histogram of",var[i]))), xlab = var[i])

      }

  for (i in 1 : ceiling(n/4)) {



    if (4*i <= n) {

      start <- 4 * (i - 1) + 1
      end <- 4 * i

      pl.multiplot(plotlist = plots[start : end], cols = 2)



    }else{

      start <- 4 * (i - 1) + 1
      end <- n

      pl.multiplot(plotlist = plots[start : n], cols = 2)

    }

    # print(i)

  }

  # pl.multiplot(plotlist = plots, cols = columns)


}

