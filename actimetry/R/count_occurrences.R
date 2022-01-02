#' @title Label occurrences computation
#' @usage count_occurrences(label_movements, labels)
#' @description This function counts the occurrences of each movement label of the labels list and creates a table with a column of labels and a column with the occurrences.
#' @param label_movements the list of labels of the database
#' @param labels the table of labels of the window
#' @return Return a table with the occurrences for each label
#' @examples count_occurrences(labels, segment_list)
#' @export

### Function to count occurrences

count_occurrences <- function(label_movements, labels){

  ind_to_remove <- c()
  k  <- 1
  while (k <= length(labels)){
    lab <- labels[k]
    i <- k + 1
    while ((i <= length(labels)) & !(is.na(labels[i])) & (labels[i] == lab)){
      ind_to_remove <- c(ind_to_remove, i)
      i <- i + 1
    }
    k <- i
  }
  labels <- labels[- ind_to_remove]

  label_movements <- c(label_movements, "no_labelled_motion")
  occurrences <- c()
  for (lab in label_movements){
    occ <- sum(labels%in%lab)
    occurrences <- c(occurrences, occ)
  }
  final_res <- data.frame(label_movements, occurrences)
  final_res <- final_res[- length(final_res$label_movements), ]

  return(final_res)
}
