#' Ridge Regression
#'
#'This function will enable users to perform ridge regression. First call
#'cv.glmnet, do cross-validation and select the best lambda.
#'Then train the ridge model based on the best lambda.
#'
#' @param X.train The train data to build the model
#' @param X.test The test data to predict outcomes
#' @param y.train The response variable of train data
#' @param y.test The true response variable of test data
#' @param criteria The criteria of the response variable(Bianry or Continuous)
#'
#' @return Predicted values and test statistics of model
#' @examples
#' #Build ridge model
#' my.ridge(X.train, X.test, y.train, y.test, "Continuous")
my.ridge <- function(X.train, X.test, y.train, y.test, criteria){
  X.train = as.matrix(X.train)
  X.test = as.matrix(X.test)
  y.train = as.matrix(y.train)
  y.test = as.matrix(y.test)

  if(criteria == "binary"){
    threshold <- 0.5
    ridge.model <- cv.glmnet(X.train, y.train, alpha = 0, family = "binomial")
    best.lambda <- ridge.model$lambda.min
    final.model <- glmnet(X.train, y.train, alpha = 0, family = "binomial", lambda = best.lambda)
    cat("finish train the ridge model", seq = "\n")
    summary(final.model)

    pred.obs <- predict(final.model, s = best.lambda, newx= X.test, type = "response")
    pred.class <- as.numeric(pred.obs >= threshold)
    acc <- sum(pred.class == y.test) / length(y.test)
    result <- list(acc = acc, y.pred = pred.class)
  }else{
    ridge.model <- cv.glmnet(X.train, y.train, alpha = 0)
    best.lambda <- ridge.model$lambda.min
    final.model <- glmnet(X.train, y.train, alpha = 0, family = "gaussian", lambda = best.lambda)
    cat("finish train the ridge model", seq = "\n")
    summary(final.model)

    pred.obs <- predict(final.model, s = best.lambda, newx= X.test)
    msr <- (sum((y.test - pred.obs)^2)) / length(y.test)
    result <- list(msr =msr, y.pred = pred.obs)
  }

  return(result)
}
