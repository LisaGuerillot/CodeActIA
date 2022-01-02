#' @title Distance between descriptors vectors of the window and the database computation
#' @usage compute_distances(descr, database_descr, label, distance_threshold)
#' @description This function computes the distances between the window descriptors vector and all the database descriptors vector, in order to obtain for the window, a distance from each label.
#' @param normalized_descriptors the window descriptors vector
#' @param normalized_descriptors_database a label descriptors vector
#' @param label_movements the liste of movements labels
#' @param distance_threshold the no motion distance threshold
#' @return Return a vector of distances between the window descriptors vector and each label
#' @examples compute_distances(x, y, label, dist_thres)
#' @export


## Function to compute distances between the window descriptors vector and the database descriptors vector

compute_distances <- function(normalized_descriptors, normalized_descriptors_database, label_movements, distance_threshold){

  distances <- c()

  for (k in 1:length(label_movements)){
    distance <- euclidean_distance(normalized_descriptors[1,], normalized_descriptors_database[k, -1]) #distance between the descriptors vector of the data set and the database
    distances <- c(distances, distance)
  }
  if (min(distances) > distance_threshold){#If the minimum is higher than the no-movement threshold, so every component of the distances vector is higher than the threshold
    distances <- rep(NA, 10) #If this is a no-movement, we replace the vector values by NA
    no_movement_indicator <- 0 #If this is a no-movement, the indicator is 0
  }else{
    no_movement_indicator <- 1 #If this is a movement, the indicator is 1
  }
  distances <- c(distances, no_movement_indicator)

  return(distances)
}
