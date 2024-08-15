#' K predictors selection
#'
#' Select top predictors based on correlation with response variable
#'
#' @param X Matrix of predictor variables
#' @param Y Response variable that can be either continuous or binary
#' @param k Number of top predictors to select
#'
#' @return Names of the top k predictors
#' @examples
#' my.k.pred(mtcars[, -1], mtcars[, 1], k = 3)
my.k.pred <- function(X, Y, k) {
  Y <- as.numeric(Y) - 1

  # Initialize empty lists to store column names for each variable type
  discrete_vars <- c()
  continuous_vars <- c()
  binary_vars <- c()

  # Loop through each column in the dataframe X
  for (col in names(X)) {
    var <- X[[col]]

    if (length(unique(var)) == 2) {
      # Binary variable
      binary_vars <- c(binary_vars, col)
    } else if (is.numeric(var)) {
      if (all(round(var) == var)) {
        # Discrete variable
        discrete_vars <- c(discrete_vars, col)
      } else {
        # Continuous variable
        continuous_vars <- c(continuous_vars, col)
      }
    }
  }

  # Extract dataframes for each variable type using column names
  discrete_vars <- X[, discrete_vars, drop = FALSE]
  continuous_vars <- X[, continuous_vars, drop = FALSE]
  binary_vars <- X[, binary_vars, drop = FALSE]

  # Check if y is binary or continuous
  is_Y_binary <- length(unique(Y)) == 2

  #initialize p value df to store p values
  p_values_df <- data.frame(Variable = character(), P_Value = numeric())

  if (is_Y_binary) {  # Y is binary
    if (ncol(continuous_vars) > 0) {  # Continuous X
      # Initialize a vector to store p-values for continuous variables
      p_values_continuous <- numeric()
      # Initialize a vector to store the names of continuous variables
      variable_names_continuous <- character()
      # Loop through each variable
      for (col in names(continuous_vars)) {
        # Perform correlation test for the current continuous variable
        p_value <- cor.test(continuous_vars[, col], Y)$p.value
        # Store p-value in the vector
        p_values_continuous <- c(p_values_continuous, p_value)
        # Store variable name
        variable_names_continuous <- c(variable_names_continuous, col)
      }
      # Create a dataframe with variable names and their corresponding p-values
      p_values_continuous_df <- data.frame(Variable = variable_names_continuous, P_Value = p_values_continuous)
      # Append to p_values_df
      p_values_df <- rbind(p_values_df, p_values_continuous_df)
    }
    if (ncol(discrete_vars) > 0) {  # Discrete X
      # Initialize a vector to store p-values for discrete variables
      p_values_discrete <- numeric()
      # Initialize a vector to store the names of discrete variables
      variable_names_discrete <- character()
      # Loop through each variable
      for (col in names(discrete_vars)) {
        # Perform fisher's exact test for the current discrete variable
        p_value <- fisher.test(table(discrete_vars[, col], Y))$p.value
        # Store p-value in the vector
        p_values_discrete <- c(p_values_discrete, p_value)
        # Store variable name
        variable_names_discrete <- c(variable_names_discrete, col)
      }
      # Create a dataframe with variable names and their corresponding p-values
      p_values_discrete_df <- data.frame(Variable = variable_names_discrete, P_Value = p_values_discrete)
      # Append to p_values_df
      p_values_df <- rbind(p_values_df, p_values_discrete_df)
    }
    if (ncol(binary_vars) > 0) {  # Binary X
      # Initialize a vector to store p-values for binary variables
      p_values_binary <- numeric()
      # Initialize a vector to store the names of binary variables
      variable_names_binary <- character()
      # Loop through each variable
      for (col in names(binary_vars)) {
        # Perform fisher's exact test for the current binary variable
        p_value <- chisq.test(table(binary_vars[, col], Y))$p.value
        # Store p-value in the vector
        p_values_binary <- c(p_values_binary, p_value)
        # Store variable name
        variable_names_binary <- c(variable_names_binary, col)
      }
      # Create a dataframe with variable names and their corresponding p-values
      p_values_binary_df <- data.frame(Variable = variable_names_binary, P_Value = p_values_binary)
      # Append to p_values_df
      p_values_df <- rbind(p_values_df, p_values_binary_df)
    }
  } else{  #Y is continuous
    if (ncol(continuous_vars) > 0) {  # Continuous X
      # Initialize a vector to store ANOVA p-values for continuous variables
      p_values_continuous <- numeric()
      # Initialize a vector to store the names of continuous variables
      variable_names_continuous <- character()
      # Loop through each continuous variable
      for (col in names(continuous_vars)) {
        # Perform correlation test for the current continuous variable
        p_value <- cor.test(continuous_vars[, col], Y)$p.value
        # Store p-value in the vector
        p_values_continuous <- c(p_values_continuous, p_value)
        # Store variable name
        variable_names_continuous <- c(variable_names_continuous, col)
      }
      # Create a dataframe with variable names and their corresponding p-values
      p_values_continuous_df <- data.frame(Variable = variable_names_continuous, P_Value = p_values_continuous)
      # Append to p_values_df
      p_values_df <- rbind(p_values_df, p_values_continuous_df)
    }
    if (ncol(discrete_vars) > 0) {  # Discrete X
      # Initialize a vector to store ANOVA p-values for discrete variables
      p_values_discrete <- numeric()
      # Initialize a vector to store the names of discrete variables
      variable_names <- character()
      for (col in names(discrete_vars)) {
        # Perform ANOVA test for the current discrete variable
        anova_result <- aov(Y ~ ., data = data.frame(Y = Y, discrete_vars[col]))
        # Extract p-value from ANOVA summary
        p_value <- summary(anova_result)[[1]]$Pr[[1]]
        # Store p-value in the vector
        p_values_discrete <- c(p_values_discrete, p_value)
        # Store variable name
        variable_names_discrete <- c(variable_names_discrete, col)
      }
      p_values_discrete_df <- data.frame(Variable = variable_names_discrete, P_Value = p_values_discrete)
      # Append to p_values_df
      p_values_df <- rbind(p_values_df, p_values_discrete_df)
    }
    if (ncol(binary_vars) > 0) {  # Binary X
      # Initialize a vector to store p-values for binary variables
      p_values_binary <- numeric()
      # Initialize a vector to store the names of binary variables
      variable_names_binary <- character()
      # Loop through each variable
      for (col in names(binary_vars)) {
        # Perform correlation test for the current continuous variable
        p_value <- cor.test(binary_vars[, col], Y)$p.value
        # Store p-value in the vector
        p_values_binary <- c(p_values_binary, p_value)
        # Store variable name
        variable_names_binary <- c(variable_names_binary, col)
      }
      # Create a dataframe with variable names and their corresponding p-values
      p_values_binary_df <- data.frame(Variable = variable_names_binary, P_Value = p_values_binary)
      # Append to p_values_df
      p_values_df <- rbind(p_values_df, p_values_binary_df)
    }
  }

  #select top predictors based on smallest p-values
  sorted_p_values_df <- p_values_df[order(p_values_df$P_Value), ]
  selected_variable_names <- sorted_p_values_df$Variable[1:k]

  #create a df to store selected predictors and their names
  selected_predictors_df <- data.frame(matrix(ncol = k, nrow = nrow(X)))

  #assign names to df columns
  colnames(selected_predictors_df) <- selected_variable_names

  #fill df with selected predictors
  for (predictor_name in selected_variable_names) {
    selected_predictors_df[[predictor_name]] <- X[[predictor_name]]
  }

  return(selected_predictors_df)
}
