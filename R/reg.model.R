###########################################################################################################################
# regression model generator
###########################################################################################################################

# This function is used to generate fomular object to be used in regression functions

# dependent <- "wt"

# dataframe <- mtcars

## dependent is the column name of the dependent variable
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
  depen <- dataframe[ ,dependent]
  dataframe <- dataframe[ ,names(dataframe) != dependent]


  # number of unique independent varibales
  indnum <- dim(dataframe)[2]
  model_data_frame <- as.data.frame(matrix(nrow = (2^indnum)-1, ncol = 2))
  modelnum <- 1

  # i is the number of independent variables
  for(i in 1 : indnum){

    # number of different combinations of independent variables
    comb <- combn(indnum, i)

    for(j in 1 : dim(comb)[2]){

      inddata <- dataframe[comb[,j]]
      regdata <- merge(depen, inddata)
      # basic formula format
      form <- paste0(dependent,"~")

      for(h in 1:i){

        form <- paste0(form, names(regdata)[h+1],"+")

      }

      formul <- substring(form,1,nchar(form)-1)
      # formula <- as.formula(formul)
      #print(formul)

      model_data_frame[modelnum, 1] <- modelnum
      model_data_frame[modelnum, 2] <- formul
      modelnum <- modelnum + 1
      #print(modelnum)

    }

  }
  names(model_data_frame) <- c("Num", "Model")
  model_data_frame <- model_data_frame[1:(2^indnum)-1, ]
  return(model_data_frame)

}

