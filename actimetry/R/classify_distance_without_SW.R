#' @title Distance classifier without sliding window
#' @usage classify_distance_without_SW(descriptors_distances, label_movements)
#' @description This function classifies the data segment and finds the label thanks to the distances.
#' @param descriptors_distances the vector of distances
#' @param label_movements the lit of movement labels
#' @return Return the label corresponding to this movement
#' @examples classify_distance_without_SW(descr_dist, labels)
#' @export

## Function to classify a single vector : classification without sliding window

classify_distance_without_SW <- function(descriptors_distances, label_movements){

  if (descriptors_distances[11] == 0){
    label <- "no_labelled_motion"
  } else {
    min_for_label <- min(descriptors_distances[- 11])
    vec <- descriptors_distances[- 11]
    for (j in 1:length(label_movements)){        #I couldn't use the which.min and I didn't succeed to find an other way than a loop
      if (vec[j] == min_for_label){
        label <- label_movements[j]  #We keep the label that is
      }
    }
  }
  return(label)
}
