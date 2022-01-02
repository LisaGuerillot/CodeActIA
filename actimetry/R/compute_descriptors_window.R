#' @title Window descriptors computation
#' @usage compute_descriptors_window(left_window_data, right_window_data, width_sliding_window, descriptors_database, criteria)
#' @description This function computes the descriptors of the window which slides on the dataset to treat.
#' @param left_window_data the window of left wrist data
#' @param right_window_data the window of right wrist data
#' @param width_sliding_window the size of the window in number of data
#' @param descriptors_database the table of descriptors of the database
#' @param criteria the way of classification
#' @return Return the vector of descriptors of the window
#' @export
#' @import e1071
#' @examples compute_descriptors_window(left_data, right_data, size_wind, descr_database, "distance")


## Function to compute the descriptors of the window

compute_descriptors_window <- function(left_window_data, right_window_data, width_sliding_window, descriptors_database, criteria){

  left_mean <- c(); right_mean <- c(); left_median <- c(); right_median <- c(); left_var <- c(); right_var <- c(); left_Q1 <- c(); right_Q1 <- c(); left_Q3 <- c(); right_Q3 <- c(); left_kurto <- c(); right_kurto <- c(); left_skew <- c(); right_skew <- c()

  if (criteria == "distance"){
    #In order to compute the distance, we need to normalize the descriptors
    normalized_descriptors <- data.frame(left_mean, right_mean, left_median, right_median, left_var, right_var, left_Q1, right_Q1, left_Q3, right_Q3, left_kurto, right_kurto, left_skew, right_skew)

    normalized_descriptors[1, "left_mean"] <- mean(left_window_data,na.rm = T)/max(descriptors_database$left_mean)  ########### Calculation of all the normalized descriptors values for the segment
    normalized_descriptors[1, "right_mean"] <- mean(right_window_data, na.rm = T)/max(descriptors_database$right_mean)
    normalized_descriptors[1, "left_median"] <- median(left_window_data, na.rm = T)/max(descriptors_database$left_median)
    normalized_descriptors[1, "right_median"] <- median(right_window_data, na.rm = T)/max(descriptors_database$right_median)
    normalized_descriptors[1, "left_var"] <- var(left_window_data, na.rm = T)/max(descriptors_database$left_var)
    normalized_descriptors[1, "right_var"] <- var(right_window_data, na.rm = T)/max(descriptors_database$right_var)
    normalized_descriptors[1, "left_Q1"] <- left_window_data[[floor(width_sliding_window/4)]]/max(descriptors_database$left_Q1)
    normalized_descriptors[1, "right_Q1"] <- right_window_data[[floor(width_sliding_window/4)]]/max(descriptors_database$right_Q1)
    normalized_descriptors[1, "left_Q3"] <- left_window_data[[floor(width_sliding_window*(3/4)) + 1]]/max(descriptors_database$left_Q3)
    normalized_descriptors[1, "right_Q3"] <- right_window_data[[floor(width_sliding_window*(3/4)) + 1]]/max(descriptors_database$right_Q3)
    normalized_descriptors[1, "left_kurto"] <- kurtosis(left_window_data, na.rm = T)/max(descriptors_database$left_kurto)
    normalized_descriptors[1, "right_kurto"] <- kurtosis(right_window_data, na.rm = T)/max(descriptors_database$right_kurto)
    normalized_descriptors[1, "left_skew"] <- skewness(left_window_data, na.rm = T)/max(descriptors_database$left_skew)
    normalized_descriptors[1, "right_skew"] <- skewness(right_window_data, na.rm = T)/max(descriptors_database$right_skew)
  } else if(criteria == "forest"){
    #When we use a forest, their is no need to normalize
    segment_descriptors <- data.frame(left_mean, right_mean, left_median, right_median, left_var, right_var, left_Q1, right_Q1, left_Q3, right_Q3, left_kurto, right_kurto, left_skew, right_skew)

    segment_descriptors[1, "left_mean"] <- mean(left_window_data,na.rm = T)
    segment_descriptors[1, "right_mean"] <- mean(right_window_data, na.rm = T)
    segment_descriptors[1, "left_median"] <- median(left_window_data, na.rm = T)
    segment_descriptors[1, "right_median"] <- median(right_window_data, na.rm = T)
    segment_descriptors[1, "left_var"] <- var(left_window_data, na.rm = T)
    segment_descriptors[1, "right_var"] <- var(right_window_data, na.rm = T)
    segment_descriptors[1, "left_Q1"] <- left_window_data[[floor(width_sliding_window/4)]]
    segment_descriptors[1, "right_Q1"] <- right_window_data[[floor(width_sliding_window/4)]]
    segment_descriptors[1, "left_Q3"] <- left_window_data[[floor(width_sliding_window*(3/4)) + 1]]
    segment_descriptors[1, "right_Q3"] <- right_window_data[[floor(width_sliding_window*(3/4)) + 1]]
    segment_descriptors[1, "left_kurto"] <- kurtosis(left_window_data, na.rm = T)
    segment_descriptors[1, "right_kurto"] <- kurtosis(right_window_data, na.rm = T)
    segment_descriptors[1, "left_skew"] <- skewness(left_window_data, na.rm = T)
    segment_descriptors[1, "right_skew"] <- skewness(right_window_data, na.rm = T)

    normalized_descriptors <- segment_descriptors #We chqnge the name in order to be able to return the result if is distance or forest, but the descriptors arre not normalized when this is a forest
  }

  return(normalized_descriptors)
}
