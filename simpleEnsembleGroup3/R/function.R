source("./R/linear_logistic.R")
source("./R/randomforest.R")
source("./R/lasso.R")
source("./R/elastic.R")
source("./R/ridge.R")

interactive.check <- function(){
  user.input <- NA
  while(! user.input %in% c("yes", "no")){
    cat("input: yes/no", sep = "\n")
    user.input <- readline()

  }

  if(user.input == "yes"){
    return(T)
  }

  return(F)
}

topK.check <- function(X.col){
  K.top <- readline()
  K.top <- as.integer(K.top)
  cat("if your input not an integer. we will transfer it to integer", sep = "\n")
  cat("like when you input 14.3, k top is 14", sep = "\n")
  if(K.top == "quit"){
    break
  }

  while(K.top < 0 || X.col <= K.top){
    #checks for valid k
    cat("invalid K. K must >= 1 and smaller than X predictor size", sep = "\n")
    cat("input again", sep = "\n")
    K.top <- readline()
    if(K.top == "quit"){
      break
    }
  }

  return(K.top)
}

# convert categorical variable into dummy variables
categorical.toDummy <- function(data){
  data.types <- sapply(data, class)
  categorical.columns <- names(data.types[data.types %in% c("factor", "character")])
  if(length(categorical.columns) == 0){
    return(data)
  }

  data.continous <- data[, !names(data) %in% categorical.columns]
  data.categorical <- data[, categorical.columns]
  if(length(categorical.columns) == 1){
    data.categorical <- matrix(data.categorical, byrow = F)
    colnames(data.categorical) <- categorical.columns
  }

  data.dummy <- model.matrix(~ . - 1, data.frame(data.categorical))

  return(cbind(data.continous, data.dummy))
}

#data <- data.frame(
#gender = c("Male", "Female", "Male", "Female", "Male"),
#age = c(25, 30, 35, 40, 45),
#income = c(50000, 60000, 70000, 80000, 90000),
#score = c(75, 80, 85, 90, 95)
#)

select.learn <- function(X1, X2, y1, y2, model, criteria){

  if(model == "linear&logistic"){
    result = linear.logistic(X1, X2,y1, y2,criteria)
  }else if(model == "ridge"){
    result = my.ridge(as.matrix(X1), as.matrix(X2), y1, y2, criteria)
  }else if(model == "lasso"){
    result = my.lasso(as.matrix(X1), as.matrix(X2), y1, y2, criteria)
  }else if(model == "elastic"){
    result = my.elastic(as.matrix(X1), as.matrix(X2), y1, y2, criteria)
  }else{
    result = my.randomForest(X1, X2, y1, y2, criteria)
  }

  return(result)
}

combin.choose <- function(){
  model.list <- c("linear&logistic", "ridge", "lasso", "elastic", "randomforest")

  cat("please input the model you want to combine:",sep = "\n")
  cat("please separate each input model with comma",sep = "\n")
  cat("for example, to combine lasso and random forest",sep = "\n")
  cat("input:",sep = "\n")
  cat("lasso, randomForest",sep = "\n")
  cat("option for you to choose:", sep = "\n")
  cat("linear&logistic, ridge, lasso, elastic, randomForest")

  user.input <- readline()
  user.input <- gsub("\\s+", "", user.input)
  com.list <- as.list(strsplit(user.input, ",")[[1]])
  while(sum(com.list %in% model.list) != length(com.list)){
    cat("not satisfied requirement", sep = "\n")
    cat("input again")
    cat("option for you to choose:", sep = "\n")
    cat("linear&logistic, ridge, lasso, elastic, randomForest")
    user.input <- readline()
    user.input <- gsub("\\s+", "", user.input)
    com.list <- as.list(strsplit(user.input, ",")[[1]])
  }

  return(com.list)
}
