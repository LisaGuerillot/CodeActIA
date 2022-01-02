#' @title Euclidean distance between vectors computation
#' @usage euclidean_distance(x, y)
#' @description This is a function which computes the euclidean distance between the two vectors entered as arguments.
#' @param x the first vector
#' @param y the second vector
#' @return Return the euclidean distance between the two vectors
#' @examples euclidean_distance(c(12, 6, 7), c(45, 87, 19))
#' @examples euclidean_distance(c(3, 4), c(1, 2))
#' @export


## Function to compute the distance between two vectors

euclidean_distance <- function(x, y){
  if (length(x) == length(y)){
    sum <- 0
    for (i in 1:length(x)){
      sum <- sum + (x[[i]] - y[[i]])^2
    }
    distance <- sqrt(sum)
  } else {
    print("Impossible distance computation between vector if different sizes !")
  }
  return(distance)
}
