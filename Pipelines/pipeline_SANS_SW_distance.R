# As we have now the segmentation thanks to the immobility withdrawal, 
# we can consider that every segment found after the withdrawal of immobility contains only one movement. 
# No need to use a slidinfg window anymore.

library(actimetry)

# Variables and screen cleaning
graphics.off();cat("\014");rm(list=ls());options(warn=-1);

### INPUTS
resampling_frequency <- 50
immobility_threshold <- 0.5
immobility_length_threshold <- 1
labelled_max_distance <- 2.2
time_threshold <- 1
tree_number <- 1000
min_motion_time <- 2

# Deduced inputs
min_motion_length <- min_motion_time * resampling_frequency

### Data import
setwd("C:/Users/lisag/Documents/Scolaire/EPF/2021-2022/Stage Euromov/Donn�es/Donn�es Alice")
file1 = "AlzCol_test_right_wrist_42977.csv"
file2 = "AlzCol_test_left_wrist_42164.csv"

left_wrist <- read.csv(file = file2, sep = ",", header = FALSE, col.names = c("Time", "X", "Y", "Z"))
right_wrist <- read.csv(file = file1, sep = "," , header = FALSE, col.names = c("Time", "X", "Y", "Z"))

### Resampling of data
resampled_accelerations <- resample(left_wrist, right_wrist, resampling_frequency)

### Immobility filtering
filtered_data <- immobility_withdrawal_simplified(resampled_accelerations, resampling_frequency, 
                                                  immobility_threshold, immobility_length_threshold, 
                                                  time_threshold, min_motion_length)

### Database descriptors computation
# Database import
file3 = "actimetry_database.csv"
database <- read.csv(file = file3, sep = ",", header = FALSE, col.names = c("Time", "Xright", "Yright", 
                                                                            "Zright", "NormRight", "Xleft", 
                                                                            "Yleft", "Zleft", "NormLeft", "label", 
                                                                            "idSubject", "idMove", "id"), skip = 1)
label_movements <- c(unique(paste(database$label)))

# Descriptors computation
descriptors_database <- compute_descriptors_db(database, criteria = "distance")

# Descriptors normalization
left_mean <- rep(0,10); right_mean <- rep(0,10); left_median <- rep(0,10); right_median <- rep(0,10); 
left_var <- rep(0,10); right_var <- rep(0,10); left_Q1 <- rep(0,10); right_Q1 <- rep(0,10); left_Q3 <- rep(0,10);
right_Q3 <- rep(0,10); left_kurto <- rep(0,10); right_kurto <- rep(0,10); left_skew <- rep(0,10); right_skew <- rep(0,10)
normalized_descriptors_database <- data.frame(label_movements, left_mean, right_mean, left_median, right_median, 
                                              left_var, right_var, left_Q1, right_Q1, left_Q3, right_Q3, 
                                              left_kurto, right_kurto, left_skew, right_skew)
for (j in 2: ncol(descriptors_database)){
  normalized_descriptors_database[, j] <- descriptors_database[, j]/max(descriptors_database[, j])
}

### Classification
lab_or_no_lab <- rep(NA, length(filtered_data$new_time_axis))
name_lab <- rep(NA, length(filtered_data$new_time_axis))
predicted_labels <- data.frame(filtered_data$new_time_axis, filtered_data$start_mov, lab_or_no_lab, name_lab)

ind_beg_seg_filter <- which(filtered_data$start_mov == - 1, arr.ind = TRUE)
if (1%in%ind_beg_seg_filter){
  ind_end_seg_filter <- ind_beg_seg_filter[-1] - 1 
} else {
  ind_end_seg_filter <- ind_beg_seg_filter - 1
}

for (k in 1 : min(length(ind_beg_seg_filter), length(ind_end_seg_filter))){
  left_segment_data <- filtered_data[ind_beg_seg_filter[k] : ind_end_seg_filter[k], 2]
  right_segment_data <- filtered_data[ind_beg_seg_filter[k] : ind_end_seg_filter[k], 3]
  width <- ind_end_seg_filter[k] - ind_beg_seg_filter[k] + 1
  if (width > 1){
    descriptors <- compute_descriptors_window(left_segment_data, right_segment_data, 
                                              width, descriptors_database, criteria = "distance")
    if (!(TRUE%in%is.na(descriptors))){
      label <- classify_without_SW(normalized_descriptors_database, descriptors, label_movements, 
                                   labelled_max_distance, tree_number, criteria = "distance")
      for (j in ind_beg_seg_filter[k] : ind_end_seg_filter[k]){
        if (label == "no_labelled_motion"){
          predicted_labels$lab_or_no_lab[j] <- "no_labelled"
          predicted_labels$name_lab[j] <- "no_labelled_motion"
        } else {
          predicted_labels$lab_or_no_lab[j] <- "labelled"
          predicted_labels$name_lab[j] <- label
        }
      }
    }
  }
}
colnames(predicted_labels) <- c("time", "start_mov", "lab_or_no_lab", "name_lab")

### Count occurrences of labels
labels <- predicted_labels[, 4]
obtained_result <- count_occurrences(label_movements, labels)

### Import of the expected result  
global_expect_results <- read.table("AlzCol_test_RES_thirdstep_time_line.txt", header = TRUE, sep = ",")

# We find the different movement labels in the sequence
tot_mov_names <- c(paste(global_expect_results$label))

# Table of occurrences and labels
expect_results <- count_occurrences(label_movements, tot_mov_names)

### Evaluation 
MAE <- evaluate_MAE(obtained_result, expect_results, database)
  
