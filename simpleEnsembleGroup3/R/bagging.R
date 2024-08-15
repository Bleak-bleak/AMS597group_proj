#Bagging
source("./R/function.R")
#' Bagging
#'
#' This function will promote users to determine the rounds of bootstrap
#' sampling and perform the specific regression model for those rounds.
#' Finally, this function will aggregate the final result by averaging
#' from all the predicted values from previous rounds.
#'
#' @param X Whole predictors matrix
#' @param y Whole response variables
#' @param model The name of model that user prefer
#' @param criteria The criteria of the response variable(Bianry or Continuous)
#'
#' @return Predicted values and test statistics of model
#' @examples
#' # Bagging for a specific model
#' bagging(X,y,"lasso","Continuous")
bagging <- function(X,y,model,criteria){
  cat(unlist(model))
  cat(" model", seq = "\n")
  cat("How many rounds of bagging do you prefer: ",sep="\n")
  i = readline()
  i <- as.integer(i)
  if(i <= 0){
    stop("i must be an integer and larger than 1")
  }

  pv <- c()

  n <- nrow(X)
  select <- round(nrow(X)*0.63 ,digits = 0)
  sub.index.mat <- matrix(sample(n, select * i, replace = TRUE), nrow = i, ncol = select)
  pv <- apply(sub.index.mat, 1, bagging.help, X, y, model, criteria)
  naive <- table(sub.index.mat)
  # Naive score for variable appearance
  naive.score <- naive[order(naive,decreasing = TRUE)]

  if(model == "randomforest"){
    pv <- apply(pv, 2, as.integer)
  }

  if(criteria == "binary"){
    #binary result
    finalpv <- ifelse(apply(pv,1,mean)>0.5,1,0)
    acc <- sum(finalpv == y) / length(y)
    result <- list(y.pred = finalpv, naive.score = naive.score, acc = acc)
  } else {
    #take average of all rounds of bagging
    finalpv <- apply(pv,1,mean) # final predicted value
    msr <- (sum((y - finalpv)^2)) / length(y)
    result <- list(y.pred = finalpv, naive.score = naive.score, msr = msr)
  }

  return(result)
}

bagging.help <- function(row, X, y, model, criteria){
  X.train <- X[row,]
  y.train <- y[row]
  result <- select.learn(X.train,X,y.train,y,model,criteria)$y.pred
}

