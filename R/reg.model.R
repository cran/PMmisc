###########################################################################################################################
# regression model generator
###########################################################################################################################

#' @name reg.model
#' @aliases reg.model
#' @title Linear model generator
#' @description This function will take a data frame and generate all the combinations of linear model
#' @usage reg.model(dataframe,dependent)
#' @param dataframe :a data frame
#' @param dependent : dependent variable
#' @examples reg.model(mtcars,"mpg")

reg.model <- function(dataframe, dependent){

  # seperate dependent variable and independent variables
  dataframe <- dataframe[ ,names(dataframe) != dependent]


  # number of unique independent varibales
  indnum <- dim(dataframe)[2]
  model_vector <- vector(mode = "logical", length = (2^indnum)-1)

  modelnum <- 1

  indnames <- names(dataframe)

  # i is the number of independent variables
  for(i in 1 : indnum){

    # number of different combinations of independent variables
    comb <- combn(indnum, i)

    for(j in 1 : dim(comb)[2]){

      indname <- indnames[comb[,j]]

      # basic formula format
      form <- paste0(dependent,"~")

      for(h in 1:i){

        form <- paste0(form, indname[h],"+")

      }

      formul <- substring(form, 1, nchar(form)-1)
      #print(formul)

      model_vector[modelnum] <- formul
      modelnum <- modelnum + 1
      #print(modelnum)

    }

  }
  model_data_frame <- data.frame(model_vector, stringsAsFactors = FALSE)

  names(model_data_frame) <- c("Model")

  return(model_data_frame)

}
