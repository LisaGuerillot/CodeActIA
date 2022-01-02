#' @title Merger of labels for the sliding window approach
#' @usage merger(predicted_labels)
#' @description This function merged the labels thanks to criteria.
#' @param predicted_labels the table of predicted labels
#' @return Return a table of the labels merged. Their is a single label for a motion segment.
#' @examples merger(labels)
#' @export

merger <- function(predicted_labels){

  final_predicted_labels <- predicted_labels

  ind_beg_seg <- which(final_predicted_labels$start_mov == - 1, arr.ind = TRUE)
  if (1%in%ind_beg_seg){
    ind_end_seg <- ind_beg_seg[-1] - 1
  } else {
    ind_end_seg <- ind_beg_seg - 1
  }
  ind_end_seg <- c(ind_end_seg, length(final_predicted_labels$start_mov))

  for (i in 1:length(ind_beg_seg)){
    mouvement_confidence_levels <- predicted_labels$confidence_level[ind_beg_seg[i] : ind_end_seg[i]]
    index <- ind_beg_seg[i] + which.max(mouvement_confidence_levels) - 1
    label <- as.character(predicted_labels$name_lab[index])
    for (k in ind_beg_seg[i]:ind_end_seg[i]){
      if (label == "no_labelled_motion"){
        final_predicted_labels$lab_or_no_lab[k] <- "no_labelled"
        final_predicted_labels$name_lab[k] <- "no_labelled_motion"
      } else {
        final_predicted_labels$lab_or_no_lab[k] <- "labelled"
        final_predicted_labels$name_lab[k] <- label
      }
    }
  }

  return(final_predicted_labels)
}
