#' Linear & Logistic Regression
#'
#'As user input a Matrix of X with predictors and y as response variable,
#'this function will train model in the way of Linear regression
#'or logistic regression depending on the criteria of the result.
#'
#' @param X.train The train data to build the model
#' @param X.test The test data to predict outcomes
#' @param y.train The response variable of train data
#' @param y.test The true response variable of test data
#' @param criteria The criteria of the response variable(Bianry or Continuous)
#'
#' @return Predicted values and test statistics of model
#' @examples
#' #Build linear regression model
#' linear.logistic(X.train, X.test, y.train, y.test, "Continuous")
#' #Build logistic regression model
#' linear.logistic(X.train, X.test, y.train, y.test, "Binary")
linear.logistic <- function(X.train, X.test, y.train, y.test, criteria){
  mat <- cbind(X.train, y.train)
  colnames(mat)[ncol(mat)] <- "obs"

  if(criteria == "binary"){
    logistic.model <- glm(obs ~., data = mat, family = binomial)
    cat("finish train the logistic model", seq = "\n")
    summary(logistic.model)
    pred.obs <- predict(logistic.model, X.test, type = "response")
    pred.class <- as.numeric(pred.obs >= 0.5)
    acc <- sum(pred.class == y.test) /length(y.test)
    result <- list(y.pred = pred.class, acc = acc)

  }else{
    linear.model <- lm(obs ~., data = mat)
    cat("finish train the linear model", seq = "\n")
    summary(linear.model)
    pred.obs <- predict(linear.model, X.test)
    msr <- (sum((y.test - pred.obs)^2)) / length(y.test)
    result <- list(y.pred = pred.obs, msr = msr)
  }

  return(result)
}

