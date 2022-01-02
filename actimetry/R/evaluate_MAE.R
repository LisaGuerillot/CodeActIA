#' @title Algorithm performance evaluation
#' @usage evaluate_MAE(x, y, db)
#' @description This function computes the MAE, which is the mean of the absolute error between the expected result and the obtained result.
#' @param predicted_labels the obtained result, a table with the movement labels and the occurrence of each label
#' @param expected_seq the expected result, a table with the movement labels and the occurrence of each label
#' @param database the database
#' @return Return the MAE between the expected result and what it's obtained.
#' @examples evaluate_MAE(x, y, database)
#' @export

## Function to evaluate the result

evaluate_MAE <- function(predicted_labels, expected_seq, database){
  label_list <- c(unique(paste(database$label)))
  sum <- 0
  for (lab in label_list){
    x <- predicted_labels["occurrences"][predicted_labels["label_movements"] == lab]
    y <- expected_seq["occurrences"][expected_seq["label_movements"] == lab]
    sum <- sum + abs(x - y)
  }
  mae <- sum/length(label_list)
  return(mae)
}
