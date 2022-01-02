#' @title Forest Classifier for a method without sliding window
#' @usage classify_forest(descriptors_database, descriptors, no_labelled_threshold, tree_number)
#' @description This function classifies the data segment and finds the labels thanks to a random forest
#' @param descriptors_database the table of database descriptors
#' @param descriptors the vector of descriptors to classify
#' @param no_labelled_threshold the threshold to determine if the movement label is known or not
#' @param tree_number the number of tree in the random forest
#' @return Return a list of labels of the sequence
#' @import randomForest
#' @examples classify_forest(database, descriptors, threshold, n_tree)
#' @export

## Function to classify the data and find the labels thanks to the random forest

classify_forest_without_SW <- function(descriptors_database, descriptors, no_labelled_threshold, tree_number){

  #Random values, we change every time the order
  descriptors_database <- descriptors_database[sample(nrow(descriptors_database)), ]
  #We keep the same order in the label list, as the one in the new database table
  label_movements <- descriptors_database$label_movements

  #We convert the label in factor
  descriptors_database$label_movements <- as.factor(descriptors_database$label_movements)


  #We split the dataset between the training data and the data to predict
  #Data which are training the algorithm
  train <- descriptors_database

  #Data we want to predict on
  test <- descriptors

  #Implementation of the random forest
  forest <- randomForest(label_movements ~ ., train, ntree = tree_number)

  #Predicion on the predict dataset
  preds <- predict(forest, test, type = "prob")

  if(max(preds) < no_labelled_threshold){
    label <- "no_labelled_motion"
    confidence_level <- 0
  } else {
    label <- as.character(label_movements[which.max(preds)])
    confidence_level <- max(preds)
  }
  return_label_confidence_level <- c(label, confidence_level)
  return(return_label_confidence_level)
}
