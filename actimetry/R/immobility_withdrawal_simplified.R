#' @title Simplified immobility withdrawal
#' @usage immobility_withdrawal_simplified(resampled_accelerations, resampling_frequency, immobility_threshold, time_threshold)
#' @description This function withdraws all the data which are smaller than the immobility threshold, allowing to keep only the data corresponding to movement. It keeps the data only if the segment is large enough, thanks to a first filter segment size threshold. This is a first filter which allows to segment a first time the dataset.
#' @param resampled_accelerations the table of resampled accelerations data
#' @param immobility_threshold the immobility threshold
#' @param time_threshold the first filter time threshold
#' @param resampling_frequency the resampling frequency
#' @param min_motion_length The minimal length of motion
#' @return Return a table with no more immobility and a column which indicates the first segmentation of the dataset
#' @examples immobility_withdrawal_simplified(x, 2, 7, 1)
#' @export


## Function to retire immobility from the accelerations and to pick up the beginning of each segment after retire immobility

immobility_withdrawal_simplified <- function(resampled_accelerations, resampling_frequency, immobility_threshold, immobility_length_threshold, time_threshold, min_motion_length){

  row_threshold <- time_threshold * resampling_frequency
  immobility_length_threshold <- immobility_length_threshold * resampling_frequency

  df <- resampled_accelerations
  df$immobility <- 0
  df[abs(df$overall_left_acc) + abs(df$overall_right_acc) < immobility_threshold, "immobility"] <- 1

  ## We consider that there is immobility only if there is enough successive immobility
  immo_successive <- diff(which(df$immobility == 1))
  counter <- which(df$immobility == 1)[1]
  indexes_to_change <- c()
  i <- 1
  while (i < length(immo_successive)){
    if (immo_successive[i] == 1){
      l <- i
      nb_immo <- 0
      index <- c()
      while((l< length(immo_successive)) && (immo_successive[l] == 1)){
        index <- c(index, counter + nb_immo)
        nb_immo <- nb_immo + 1
        l <- l + 1
      }
      index <- c(index, counter + nb_immo)
      if (nb_immo < immobility_length_threshold){
        indexes_to_change <- c(indexes_to_change, index)
      }
      counter <- counter + nb_immo + immo_successive[l]
      i <- l + 1
    } else {
      counter <- counter + immo_successive[i]
      i <- i + 1
    }
  }

  for (ind in indexes_to_change){
    df$immobility[ind] <- 0
  }

  #we determine the beginning of movements
  if (df$immobility[1] == 0){
    df$start_mov <- c(-1, diff(df$immobility))
  } else if (df$immobility[1] == 1){
    df$start_mov <- c(0, diff(df$immobility))
  }

  filtered_data <- df[df$immobility == 0, ]

  start_short_move <- c()
  gaps <- diff(which(filtered_data$start_mov == - 1))#return the difference between

  if (gaps[1] < row_threshold){
    start_short_move <- c(start_short_move, 1) #List of the index of beginning of short movements
  }
  ind_tot <- gaps[1] #We count the index where we are

  for (k in 2:length(gaps)){
    if (gaps[k] < row_threshold){
      start_short_move <- c(start_short_move, ind_tot + 1) #We use the total index from the range before + 1, because it corresponds to the beginning of this short movement
    }
    ind_tot <- ind_tot + gaps[k]#the index in the list
  }

  if(tail(start_short_move, 1) == nrow(filtered_data)){ #we check that their is no a problem with the value on the border
    start_short_move <- start_short_move[-length(start_short_move)]
  }

  index_to_remove <- c()
  for (i in start_short_move){
    k <- i + 1
    while(filtered_data$start_mov[k] != - 1){
      index_to_remove <- c(index_to_remove, k)
      k <- k + 1
    }
  }
  filtered_data <- filtered_data[-index_to_remove, ]

  #We check that between each segment of immobility, their is enough data to make a motion.
  ind_beg_seg_filter <- which(filtered_data$start_mov == - 1, arr.ind = TRUE)
  if (1%in%ind_beg_seg_filter){
    ind_end_seg_filter <- ind_beg_seg_filter[-1] - 1
  } else {
    ind_end_seg_filter <- ind_beg_seg_filter - 1
  }
  ind_to_remove <- c()

  for (k in 1 : min(length(ind_beg_seg_filter), length(ind_end_seg_filter))){
    width <- ind_end_seg_filter[k] - ind_beg_seg_filter[k] + 1
    if (width < min_motion_length){ #Filter of time : if the segment of data between two immobility is too small, it's not a motion
      ind_to_remove <- c(ind_to_remove, ind_beg_seg_filter[k] : ind_end_seg_filter[k])
    }
  }
  filtered_data <- filtered_data[-ind_to_remove, ]

  return(filtered_data)
}
