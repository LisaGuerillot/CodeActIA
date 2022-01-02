#' @title Real motion
#' @usage real_motion(real_time, resampled_accelerations)
#' @description This function creates a table with the acceleration corresponding to the real done motion and attributes to each line, the right label.
#' @param real_time list of the time of beginning and end for each motion
#' @param resampled_accelerations the table of resampled overall accelerations from the two wrist
#' @return Return a table with the time, the left acceleration, the right acceleration and the real label
#' @examples real_motion(real_time, resampled_accelerations)
#' @export

real_motion <- function(real_time, resampled_accelerations){

  #list of time beginning and end of real motion
  beg_real_motion <- c(real_time[seq(1,length(real_time), by = 2)], resampled_accelerations$new_time_axis[length(resampled_accelerations$new_time_axis)])
  end_real_motion <- c(0, real_time[seq(2, length(real_time), by = 2)])

  #Use of the resampled accelerations
  real_motion <- resampled_accelerations

  #Withdrawal of all the lines which don't correspond to a real motion
  index_beg <- c()
  index_end <- c()
  ind_to_remove <- c()
  for (k in 1:length(end_real_motion)){
    index_beg <- c(index_beg, which(resampled_accelerations$new_time_axis == end_real_motion[k]))
    index_end <- c(index_end, which(resampled_accelerations$new_time_axis == beg_real_motion[k]))
  }
  for(i in 1:length(index_beg)){
    ind_to_remove <- c(ind_to_remove, index_beg[i]:index_end[i])
  }
  real_motion <- real_motion[- ind_to_remove,]

  #We add the real label which correspond to the data
  labels_movement <- c("climb_the_stairs", "go_downstairs", "pour_water_right_hand", "pour_water_left_hand", "walk_with_a_dish", "hand_washing", "tooth_washing", "take_off_shoes", "put_on_shoes", "eat_a_compote")
  index_beg <- c()
  index_end <- c()
  beg_real_motion <- real_time[seq(1,length(real_time), by = 2)]
  end_real_motion <- real_time[seq(2, length(real_time), by = 2)]
  for (k in 1:length(end_real_motion)){
    index_beg <- c(index_beg, which(trunc(real_motion$new_time_axis) == beg_real_motion[k])[1])
  }
  index_end <- c(index_beg[-1] - 1, length(real_motion$new_time_axis))

  name_of_lab <- rep(NA, length(real_motion$new_time_axis))
  real_motion <- cbind(real_motion, name_of_lab)
  for (k in 1:length(index_beg)){
    for (ind in index_beg[k]:index_end[k]){
      real_motion$name_of_lab[ind] <- labels_movement[k]
    }
  }

  return(real_motion)
}
