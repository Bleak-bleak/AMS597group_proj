#' Elastic net Regression
#'
#'This function will enable users to perform elastic regression. Loop through
#'alpha from 0 to 1 and do cross-validation tasks on each alpha value.
#'For each alpha value, do elastic regression based on the best lambda
#'value and compare with each other. Then choose the best elastic model
#'based on the alpha value.
#'
#' @param X.train The train data to build the model
#' @param X.test The test data to predict outcomes
#' @param y.train The response variable of train data
#' @param y.test The true response variable of test data
#' @param criteria The criteria of the response variable(Bianry or Continuous)
#'
#' @return Predicted values and test statistics of model
#' @examples
#' #Elastic net regression model
#' my.elastic(X.train, X.test, y.train, y.test, "Continuous")
my.elastic <- function(X.train, X.test, y.train, y.test, criteria){
  X.train = as.matrix(X.train)
  X.test = as.matrix(X.test)
  y.train = as.matrix(y.train)
  y.test = as.matrix(y.test)

  alpha <- seq(0, 1, by = 0.1)
  set.seed(123)
  family <- ifelse(criteria == "binary", "binomial", "gaussian")

  best.err <- .Machine$integer.max
  for (val in alpha){
    cv.model <- cv.glmnet(X.train, y.train, alpha = val, family = family)
    opt.err <- min(cv.model$cvm)
    if(best.err > opt.err){
      best.err <- opt.err
      best.lambda <- cv.model$lambda.min
      best.alpha <- val
    }
  }

  final.model <- glmnet(X.train, y.train, alpha = best.alpha, lambda = best.lambda)
  cat("finish train the elastic model", seq = "\n")
  summary(final.model)

  if(criteria == "binary"){
    pred.obs <- predict(final.model, newx= X.test, type = "response")
    pred.class <- as.numeric(pred.obs >= 0.5)
    acc <- sum(pred.class == y.test) / length(y.test)
    result <- list(acc=acc, y.pred =pred.obs)
  }else{
    pred.obs <- predict(final.model, newx= X.test)
    msr <- (sum((y.test - pred.obs)^2)) / length(y.test)
    result <- list(msr=msr, y.pred = pred.obs)
  }

  return(result)
}
