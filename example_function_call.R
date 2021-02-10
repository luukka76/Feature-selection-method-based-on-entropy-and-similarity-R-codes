
# Test the 'feature_selection_sim' function with an example dataset
# -----------------------------------------------------------------------------------------

  source("feature_selection_sim.R") # source the 'similarity based feature selection function.'
  # this works only if the function is in your working directory. If not, add the correct path


# Define an example data frame
  mydata <- data.frame(
                c1 = c(0.46, 0.5, 0.44, 0.76, 0.49, 0.73),
                c2 = c(0.34, 0.34, 0.29, 0.30, 0.25, 0.29),
                c3 = c(0.14, 0.15, 0.14, 0.66, 0.45, 0.63),
                c4 = c(0.03, 0.02, 0.02, 0.21, 0.17, 0.18),
                c5 = c(0.9218, 0.7382, 0.1763, 0.4057, 0.9355, 0.9169),
                class_labels = c(1.0, 1.0, 1.0, 2.0, 2.0, 2.0)
              )

# With default values of the parameters
  output1  <- feature_selection_sim(mydata) # function call
  removed_feature_index     <- output1[[1]] # Index of the removed feature
  data_with_removed_feature <- output1[[2]] # Data with removed feature

# With defined values of the parameters
  # Initialization of the parameters
  p       <- 1
  measure <- 'park'
  output2 <- feature_selection_sim(mydata, measure, p) # function call
  removed_feature_index     <- output2[[1]] # Index of the removed feature
  data_with_removed_feature <- output2[[2]] # Data with removed feature


# -----------------------------------------------------------------------------------------
# Test the 'feature_selection_sim' function with a real dataset (Iris data set)
  
  
  library(datasets) # Load the data set package
  data(iris)        # 'iris' is the name of the dataframe
  
  # Convert categorical class labels to numeric values
  class_variable <- as.factor(iris[,ncol(iris)])
  class_numeric  <- unclass(class_variable)
  
  # Complete data.frame with all variables put together
  mydata  <-cbind(iris[,1:ncol(iris)-1], class_numeric) # combine class variable with others
  
  # Parameters
  p       <- 1
  measure <- 'park'
  output_iris <- feature_selection_sim(mydata, measure, p) # function call
  
  removed_feature_index     <- output_iris[[1]] # Index of the removed feature
  data_with_removed_feature <- output_iris[[2]] # Data with removed feature
