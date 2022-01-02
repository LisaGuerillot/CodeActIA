#' @title Data resampling
#' @usage resample(left_wrist, right_Wrist, resampled_frequency)
#' @description This function resamples the timed data from two dataset at the resampling frequency entered as input.
#' @param left_wrist the first dataset containing timed data of the left wrist
#' @param right_Wrist the second dataset containing timed data of the right wris
#' @param resampling_frequency the resampling frequency used to resample the timed data
#' @return Return a table with the resampled overall accelerations from the two wrist
#' @examples resample(tab1, tab2, 100)
#' @export


## Function to compute the resample of data

resample <- function(left_wrist, right_wrist, resampling_frequency){

  left_wrist$Time <- left_wrist$Time - left_wrist$Time[1]
  right_wrist$Time <- right_wrist$Time - right_wrist$Time[1]
  start_time_right <- right_wrist$Time[1]
  end_time_right <- right_wrist$Time[length(right_wrist$Time)]
  start_time_left <- left_wrist$Time[1]
  end_time_left <- left_wrist$Time[length(left_wrist$Time)]
  start_time_left
  start_time_right

  # To obtain the junction of the two time bases, we need to retain the maximum of the beginning times and the minimum of the ending times.
  final_start_time <- max(start_time_right, start_time_left)
  final_end_time <- min(end_time_right, end_time_left)

  resampling_frequency
  resampling_period <- 1/resampling_frequency

  new_time_axis <- seq(final_start_time, final_end_time, by = resampling_period)
  new_time_axis

  # Resample of the right data set
  resampled_right_accX <- approx(right_wrist$Time, right_wrist$X, xout = new_time_axis, method = "linear", ties = "ordered")$y
  resampled_right_accY <- approx(right_wrist$Time, right_wrist$Y, xout = new_time_axis, method = "linear", ties = "ordered")$y
  resampled_right_accZ <- approx(right_wrist$Time, right_wrist$Z, xout = new_time_axis, method = "linear", ties = "ordered")$y

  # Resample of the left data set
  resampled_left_accX <- approx(left_wrist$Time, left_wrist$X, xout = new_time_axis, method = "linear", ties = "ordered")$y
  resampled_left_accY <- approx(left_wrist$Time, left_wrist$Y, xout = new_time_axis, method = "linear", ties = "ordered")$y
  resampled_left_accZ <- approx(left_wrist$Time, left_wrist$Z, xout = new_time_axis, method = "linear", ties = "ordered")$y

  # Calculation of the right wrist overall acceleration
  overall_right_acc <- sqrt(resampled_right_accX^2 + resampled_right_accY^2 + resampled_right_accZ^2) - 1
  #overall_right_acc
  # Calculation of the left wrist overall acceleration
  overall_left_acc <- sqrt(resampled_left_accX^2 + resampled_left_accY^2 + resampled_left_accZ^2) - 1
  #overall_left_acc

  #Creation of the table of resampled accelerations
  resampled_accelerations <- data.frame(new_time_axis, overall_left_acc, overall_right_acc)

  return(resampled_accelerations)
}


