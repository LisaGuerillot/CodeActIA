#' @title Database descriptors computation
#' @usage compute_descriptors_db(x, criteria)
#' @description This function computes the descriptors for each label on the database.
#' @param database the database
#' @param criteria the method of classification
#' @return Return a table with the descriptors for each label
#' @export
#' @import e1071
#' @examples compute_descriptors_db(x, distance)

## Function to compute the descriptor of the database

compute_descriptors_db <- function(database, criteria){

  ids <- unique(database$id)
  max <- length(ids)
  left_mean <- rep(0,max); right_mean <- rep(0,max); left_median <- rep(0,max); right_median <- rep(0,max); left_var <- rep(0,max); right_var <- rep(0,max); left_Q1 <- rep(0,max); right_Q1 <- rep(0,max); left_Q3 <- rep(0,max); right_Q3 <- rep(0,max); left_kurto <- rep(0,max); right_kurto <- rep(0,max); left_skew <- rep(0,max); right_skew <- rep(0,max); label_movements <- rep(0, max)
  descriptors_database <- data.frame(label_movements, left_mean, right_mean, left_median, right_median, left_var, right_var, left_Q1, right_Q1, left_Q3, right_Q3, left_kurto, right_kurto, left_skew, right_skew)

  for (i in 1:length(ids)){
    id <- ids[i]
    descriptors_database[i, "label_movements"] <- database[database$id==id,"label"][1]
    descriptors_database[i, "left_mean"] <- mean(database[database$id==id,"NormLeft"], na.rm = T)
    descriptors_database[i, "right_mean"] <- mean(database[database$id==id, "NormRight"], na.rm = T)
    descriptors_database[i, "left_median"] <- median(database[database$id==id,"NormLeft"], na.rm = T)
    descriptors_database[i, "right_median"] <- median(database[database$id==id,"NormRight"], na.rm = T)
    descriptors_database[i, "left_var"] <- var(database[database$id==id,"NormLeft"], na.rm = T)
    descriptors_database[i, "right_var"] <- var(database[database$id==id,"NormRight"], na.rm = T)
    descriptors_database[i, "left_Q1"] <- database[database$id==id,"NormLeft"][[length(database[database$id==id,"NormLeft"])%/%4]] #integer part
    descriptors_database[i, "right_Q1"] <- database[database$id==id,"NormRight"][[length(database[database$id==id,"NormRight"])%/%4]] #integer part
    descriptors_database[i, "left_Q3"] <- database[database$id==id,"NormLeft"][[(length(database[database$id==id,"NormLeft"])%/%4)*3]] #integer part
    descriptors_database[i, "right_Q3"] <- database[database$id==id,"NormRight"][[(length(database[database$id==id,"NormRight"])%/%4)*3]] #integer part
    descriptors_database[i, "left_kurto"] <- kurtosis(database[database$id==id,"NormLeft"], na.rm = T)
    descriptors_database[i, "right_kurto"] <- kurtosis(database[database$id==id,"NormRight"], na.rm = T)
    descriptors_database[i, "left_skew"] <- skewness(database[database$id==id,"NormLeft"], na.rm = T)
    descriptors_database[i, "right_skew"] <- skewness(database[database$id==id,"NormRight"], na.rm = T)
  }

  if (criteria == "distance"){
    label_movements <- unique(database$label)
    left_mean <- rep(0,10); right_mean <- rep(0,10); left_median <- rep(0,10); right_median <- rep(0,10); left_var <- rep(0,10); right_var <- rep(0,10); left_Q1 <- rep(0,10); right_Q1 <- rep(0,10); left_Q3 <- rep(0,10); right_Q3 <- rep(0,10); left_kurto <- rep(0,10); right_kurto <- rep(0,10); left_skew <- rep(0,10); right_skew <- rep(0,10)
    mean_descriptors_database <- data.frame(label_movements, left_mean, right_mean, left_median, right_median, left_var, right_var, left_Q1, right_Q1, left_Q3, right_Q3, left_kurto, right_kurto, left_skew, right_skew)

    for (i in 1:length(label_movements)){
      lab <- label_movements[i]
      mean_descriptors_database[i, "left_mean"] <- mean(descriptors_database[descriptors_database$label_movements==lab,"left_mean"], na.rm = T)
      mean_descriptors_database[i, "right_mean"] <- mean(descriptors_database[descriptors_database$label_movements==lab, "right_mean"], na.rm = T)
      mean_descriptors_database[i, "left_median"] <- mean(descriptors_database[descriptors_database$label_movements==lab,"left_median"], na.rm = T)
      mean_descriptors_database[i, "right_median"] <- mean(descriptors_database[descriptors_database$label_movements==lab,"right_median"], na.rm = T)
      mean_descriptors_database[i, "left_var"] <- mean(descriptors_database[descriptors_database$label_movements==lab,"left_var"], na.rm = T)
      mean_descriptors_database[i, "right_var"] <- mean(descriptors_database[descriptors_database$label_movements==lab,"right_var"], na.rm = T)
      mean_descriptors_database[i, "left_Q1"] <- mean(descriptors_database[descriptors_database$label_movements==lab,"left_Q1"], na.rm = T)
      mean_descriptors_database[i, "right_Q1"] <- mean(descriptors_database[descriptors_database$label_movements==lab,"right_Q1"], na.rm = T)
      mean_descriptors_database[i, "left_Q3"] <- mean(descriptors_database[descriptors_database$label_movements==lab,"left_Q3"], na.rm = T)
      mean_descriptors_database[i, "right_Q3"] <- mean(descriptors_database[descriptors_database$label_movements==lab,"right_Q3"], na.rm = T)
      mean_descriptors_database[i, "left_kurto"] <- mean(descriptors_database[descriptors_database$label_movements==lab,"left_kurto"], na.rm = T)
      mean_descriptors_database[i, "right_kurto"] <- mean(descriptors_database[descriptors_database$label_movements==lab,"right_kurto"], na.rm = T)
      mean_descriptors_database[i, "left_skew"] <- mean(descriptors_database[descriptors_database$label_movements==lab,"left_skew"], na.rm = T)
      mean_descriptors_database[i, "right_skew"] <- mean(descriptors_database[descriptors_database$label_movements==lab,"right_skew"], na.rm = T)
    }
  }
  return(if (criteria == "distance"){
    mean_descriptors_database} else {
      descriptors_database
    })
}
