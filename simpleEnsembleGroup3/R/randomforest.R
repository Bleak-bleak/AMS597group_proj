#' Random Forest Model
#'
#'As user input a Matrix of X with predictors and y as response variable,
#'this function will train model in the way of random forest.
#'
#' @param X.train The train data to build the model
#' @param X.test The test data to predict outcomes
#' @param y.train The response variable of train data
#' @param y.test The true response variable of test data
#' @param criteria The criteria of the response variable(Bianry or Continuous)
#'
#' @return Predicted values and test statistics of model
#' @examples
#' #Build RandomForest model
#' my.randomForest(X.train, X.test, y.train, y.test, "Continuous")
my.randomForest <- function(X.train, X.test, y.train, y.test, criteria){
  set.seed(123)

  mat <- cbind(X.train, y.train)
  colnames(mat)[ncol(mat)] <- "obs"

  cat("begin turn the model", seq = "\n")

  model_tuned <- tryCatch({
    tuneRF(
      x=X.train, #define predictor variables
      y=y.train, #define response variable
      ntreeTry=500,
      mtryStart=4,
      stepFactor=1.5,
      improve=0.01,
      trace=FALSE,
      plot = F)
  }, error = function(e) {
    # Code block to handle the error
    cat("data size too small, turn function fail to work", sep = "\n")
    message("An error occurred: ", e$message)
    # You can return a default value, or do something else to handle the error
    NA
  })

  #model_tuned <- tuneRF(
  #x=X.train, #define predictor variables
  #y=y.train, #define response variable
  #ntreeTry=500,
  #mtryStart=4,
  #stepFactor=1.5,
  #improve=0.01,
  #trace=FALSE,
  #plot = F
  #)
  cat("finish turn the model", seq = "\n")

  if(anyNA(model_tuned)){
    cat("begin train the randomForest model", seq = "\n")
    model <- randomForest(
      formula = obs ~ .,
      data = mat
    )
    cat("finish train the randomForest model", seq = "\n")

  }else{
    rownames(model_tuned) <- NULL
    best.mtry <- which.min(model_tuned[, 2]) + 1

    cat("begin train the randomForest model", seq = "\n")
    model <- randomForest(
      formula = obs ~ .,
      data = mat,
      mtry = best.mtry
    )
    cat("finish train the randomForest model", seq = "\n")
  }

  pred.obs <- predict(model, newdata=X.test)
  if(criteria == "binary"){
    acc <- sum(pred.obs == y.test) / length(y.test)
    result <- list(acc=acc, y.pred = pred.obs)
  }else{
    msr <- (sum((y.test - pred.obs)^2)) / length(y.test)
    result <- list(msr=msr, y.pred = pred.obs)
  }

  return(result)
}
