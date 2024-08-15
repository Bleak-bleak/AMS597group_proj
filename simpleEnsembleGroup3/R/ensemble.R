source("./R/bagging.R")

#' Ensemble learning
#'
#' This package enables ensemble learning for the data. The function ensemble.
#' learn takes in X and y, and criteria and comb as input, criteria can be
#' binary and continuous, comb are the different kinds of regression models that
#' user wants to perform. For each regression model, it will perform bagging
#' learning on them, the results from different will be combined into a matrix,
#' each column is the result of a different regression model. If the criteria
#' is binary, then the majority voting will be performed by calling the vote.max
#' function, and result will be returned with values predicted and the accuracy.
#' The vote.max function chooses the mode of the binary vector. If the criteria
#' is continuous, then the the mean of matrix of each column will be found and
#' returned as the predicted values, mean square error(MSR) will be calculated
#' and it will return the user the MSR and the predicted value.
#'
#' @param X Whole predictors matrix
#' @param y Whole response variables
#' @param criteria The criteria of the response variable(Bianry or Continuous)
#' @param comb List of names for the model that user prefer
#'
#' @return Predicted values and test statistics of model
#' @examples
#' #Define list of combination of model
#' list_samp <- c("lasso","randomforest")
#' #Ensemble lean
#' ensemble.learn(X,y,"Continuous",list_samp)
ensemble.learn <- function(X, y, criteria, comb){
  pred.list <- c()
  for(index in c(1:length(comb))){
    pred.list <- c(pred.list, bagging(X, y, comb[index], criteria)$y.pred)
  }
  pre.mat <- matrix(pred.list, ncol = length(comb), byrow = F)

  if(criteria == "binary"){
    pre.obs <- apply(pre.mat, 1, vote.max)
    acc <- sum(pre.obs == y) / length(y)
    result <- list(y.pred = pre.obs, acc = acc)

  }else{
    pre.obs <- apply(pre.mat, 1, mean)
    msr <- (sum((y - pre.obs)^2)) / length(y)
    result <- list(y.pred = pre.obs, msr = msr)
  }

  return(result)
}

vote.max <- function(vector){
  freq.table <- table(vector)
  max.freq <- max(freq.table)
  max.elements <- names(freq.table[freq.table == max.freq])

  return(max.elements)
}
