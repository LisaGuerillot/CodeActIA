#' @title Distance classifier with the sliding window method
#' @usage classify_distance_SW(x, label_movements)
#' @description This function classifies the data segment and finds the labels thanks to the distance.
#' @param x the table of treated data, according to the criteria which will be used to classify
#' @param criteria the criteria allowing to classify the data
#' @param label_movements the list of possible labels for movements
#' @return Return a list of labels of the sequence
#' @examples classify_distance_SW(x, labels)
#' @export

## Function to classify the data and find the labels

classify_distance_SW <- function(distances_table, label_movements){

    differences <- diff(distances_table[, 11])

    #Beginning of no movement (+ 1 : because diff return the difference of (next component - component), so 0 - 1 indicates a beginning of movement)
    #End of no movement (no + 1  because diff return the difference of (next component - component), so 1 - 0 indicates an end of movement at component 0)
    index_no_movements_end <- which(differences == 1, arr.ind = TRUE)

    index_movements_beginning <-  which(differences == 1, arr.ind = TRUE) + 1 #We add 1 in order to begin with the beginning of the list of segment
    index_movements_end <-  which(differences == - 1, arr.ind = TRUE) # -1 because if n is the index of a no-movement beginning, the movement is finished at n - 1

    if ((length(index_movements_beginning) != 0) && (length(index_movements_end) != 0)){#We first check if their is a segmentation by the no-movement. If the two lengths are not 0, their are no-movements, which allows to segment the data
      if (distances_table[, 11][1] == 1){
        index_movements_beginning <- c(1, index_movements_beginning)
        index_movements_end <- c(index_movements_end, length(differences))
      } else{
        index_movements_end <- c(index_movements_end, length(differences))
      }

      label_list <- c()

      for (i in 1:length(index_movements_beginning)){
        if((index_movements_end[i] - index_movements_beginning[i]) > 0){
          #We must consider the mean of the distances for each label for a single foretold label
          single_movement_extraction <- distances_table[index_movements_beginning[i] : index_movements_end[i], - 11] # we select the distances values of a single movement, but we suppress the last column because it was only a no-movement indicator and it is no more useful
          column_means <- colMeans(single_movement_extraction, na.rm = TRUE)
          min_for_label <- min(column_means)
          for (j in 1:length(label_movements)){#I couldn't use the which.min and I didn't succeed to find an other way than a loop
            if (column_means[j] == min_for_label){
              label <- label_movements[j]  #We keep the label that is
            }
          }
        } else if ((index_movements_end[i] - index_movements_beginning[i]) == 0){ # if the index of beginning and ending of the movement are the same, we just take the minimal distance to choose the label
          vec <- distances_table[index_movements_end[i], -11]
          min_for_label <- min(vec)
          for (j in 1:length(label_movements)){
            if (vec[j] == min_for_label){
              label <- label_movements[j]  #We keep the label that is
            }
          }
        }
      }
    } else { #Their is no no-movement in the segment because the one of the length of index vector is null. So we just take the label which correspond to the column mean minimum.
      label_list <- c()
      if (!(FALSE%in%is.na(distances_table[, - 11]))){ #If their is only lines of NA, so their is no movement, it's only no-movement
        label <- integer(0)
      } else { #If there is no only NA, it means that there are movements
        if ((class(distances_table[, - 11]) != "numeric") && (nrow(distances_table[, - 11]) > 1)){
          column_means <- colMeans(distances_table[, -11], na.rm = TRUE)
          min_for_label <- min(column_means)
        } else {
          column_means <- distances_table[, - 11]
          min_for_label <- min(column_means)
        }
        for (j in 1:length(label_movements)){        #I couldn't use the which.min and I didn't succeed to find an other way than a loop
          if (column_means[j] == min_for_label){
            label <- label_movements[j]  #We keep the label that is
          }
        }
      }
    }
    label_list <- c(label_list, label)
    return (label_list)
}
