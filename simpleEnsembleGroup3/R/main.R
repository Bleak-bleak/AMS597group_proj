source("./R/function.R")
source("./R/bagging.R")
source("./R/ensemble.R")
source("./R/kpred.R")

#' Main function of the project
#'
#' The main function is mainly served as user interface, it allows users to
#' input their data and their command of what kinds of regression functions to
#' implement on the data, moreover, it also checks the integrity of the data,
#' it checks if the predictor data (mainly X) are in dataframe form, if not it
#' will prompt the user to input it into one; it checks if the data has missing
#' value; It checks if the regression model that user wanted to implement
#' matches the one our package support ("linear&logistic", "ridge", "lasso",
#' "elastic", "randomforest"), if it matches, it will call the “function”
#' package and perform corresponding operation; It checks if the predictors and
#' outcome has the same size; It will prompt user to change the outcome data
#' into 0 or 1 if data type is binary.  It will prompt the user to choose the
#' most informative predictors if the predictors have greater number than sample
#' size; It will convert categorical data to dummy variables; It will also allow
#' users to choose if they want to perform bagging learning on the data, if yes,
#' the bagging function in the bagging package will be called. Lastly, it also
#' prompts users to choose if they want to perform ensemble learning on the
#' data, if yes, the ensemble.learn function in the ensemble package will be
#' called. The input for the yes and no questions from users are managed by the
#' interactive.check function in the “function” package.
#'
#' @param X Whole predictors matrix
#' @param y Whole response variables
#' @param model The name of model that user prefer
#'
#' @return Predicted values and test statistics of model
#' @examples
#' sipmleEnsembleGroup3(X,y,"ridge")
#' @export
simpleEnsembleGroup3 <- function(X, y, model){
  result.list <- c()

  if(! (is.data.frame(X))){
    stop("error:X must be a data frame, input data again")
  }


  if(anyNA(X) || anyNA(y)){
    stop("error:X or y contain missing values. please drop missing values and input again")
  }

  model.list <- c("linear&logistic", "ridge", "lasso", "elastic", "randomforest")
  if(! (model %in% model.list)){
    stop("error:current version support linear, logistic, ridge, lasso regression, elastic and randomForest.
         the model you unput must be one of these model")
  }

  if(! (nrow(X) == length(y))){
    stop("X and y must match size")
  }

  if(is.factor(y)){
    if(sum(y == 0) + sum(y == 1) != length(y)){
      stop("you need change factor y to level 0 and 1. the current version just support binary data with 2 level")
    }

    criteria <- "binary"
  }else{
    criteria <- "continuous"
  }

  if(nrow(X) < ncol(X)){
    cat("warning: the varible predictor is large than its number(p >> n), would want to pre screening for top K most “informative” predictors ?",sep="\n")
    if(interactive.check()){
      cat("please input the top K most “informative” predictors to be included in the model ", sep = "\n")
      cat("numerb of data predicator is ")
      cat(ncol(X), sep = "/n")
      K.top <- topK.check(ncol(X))

      X <- my.k.pred(X, y, K.top)
    }
  }

  # convert categorical variable into dummy variables
  X <- categorical.toDummy(X)


  model.result <- select.learn(X, X, y, y, model, criteria)

  result.list <- c(result.list, list(original = model.result))

  #bagging or trian
  cat("our package support bagging. would you want bagging learning?" ,sep="\n")
  if(interactive.check()){
    bagging.result = bagging(X, y,model,criteria)
    result.list <- c(result.list, list(bagging = bagging.result))
  }

  #ensemable learning
  cat("our package support ensemble learning. would you want ensemble learning?" ,sep="\n")
  if(interactive.check()){
    comb <- combin.choose()
    ensemble.result <- ensemble.learn(X, y, criteria, comb)
    result.list <- c(result.list, list(ensemble = ensemble.result))
  }

  return(result.list)

}
