#' @title classifier
#' @usage classify(normalized_descriptors_database, descriptors, label_movements, no_labelled_threshold, criteria)
#' @description This function classifies the data segment and finds the label according to a criteria without using a sliding window
#' @param normalized_descriptors_database the table of database descriptors
#' @param descriptors the descriptors of the segment
#' @param label_movements the list of movement labels
#' @param no_labelled_threshold the threshold which determines the no labelled movement
#' @param tree_number the number of tree in the random forest for the "forest" criteria
#' @param criteria the criteria to  choose the classifier: "distance", or "forest"
#' @return Return the label corresponding to this movement according to a criteria
#' @examples classify(database, descriptors, labels, no_labelled_threshold, "distance")
#' @export

#Function to classify data according to a criteria

classify_without_SW <- function(normalized_descriptors_database, descriptors, label_movements, no_labelled_threshold, tree_number, criteria){

    if (criteria == "distance"){
      descriptors_distances <- compute_distances(descriptors, normalized_descriptors_database, label_movements, no_labelled_threshold)
      predicted_label <- classify_distance_without_SW(descriptors_distances, label_movements)
    } else if (criteria == "forest"){
      predicted_label <- classify_forest_without_SW(normalized_descriptors_database, descriptors, no_labelled_threshold, tree_number)
    }
  return(predicted_label)
}
