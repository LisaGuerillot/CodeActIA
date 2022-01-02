library(ggplot2)
library(reshape2)
library(actimetry)
#source("server.R")

main_function<- function(file1, file2, filetime, immobility_threshold, no_labelled_threshold, tree_number){
### INPUTS
resampling_frequency <- 50
#immobility_threshold
immobility_length_threshold <- 1
#no_labelled_threshold
window_second_length <- 1
time_threshold <- 1
#tree_number
min_motion_time <- 2

# Deduced inputs
width_sliding_window <- window_second_length * resampling_frequency
min_motion_length <- min_motion_time * resampling_frequency

### Data import
setwd("C:/Users/lisag/Documents/Scolaire/EPF/2021-2022/Stage Euromov/Activites/shiny App/visualisation_actimetry/data")

left_wrist <- read.csv(file = file2, sep = ",", header = FALSE, col.names = c("Time", "X", "Y", "Z"))
right_wrist <- read.csv(file = file1, sep = "," , header = FALSE, col.names = c("Time", "X", "Y", "Z"))
time_real_motion <- read.csv(file = filetime, sep = "," , header = FALSE, col.names = "Time")
real_time <- time_real_motion$Time

### Resampling of data
resampled_accelerations <- resample(left_wrist, right_wrist, resampling_frequency)

### Real motion to compare
real_motion <- real_motion(real_time, resampled_accelerations)

result <- list()

result$resampling_plot <- show(resampled_accelerations, real_motion, "resampling")

### Immobility filtering
filtered_data <- immobility_withdrawal_simplified(resampled_accelerations, resampling_frequency, immobility_threshold, immobility_length_threshold, time_threshold, min_motion_length)
result$motion_plot <- show(filtered_data, real_motion, "immobility filter")

### Database descriptors computation
# Database import
file3 = "actimetry_database.csv"
database <- read.csv(file = file3, sep = ",", header = FALSE, col.names = c("Time", "Xright", "Yright", "Zright", "NormRight", "Xleft", "Yleft", "Zleft", "NormLeft", "label", "idSubject", "idMove", "id"), skip = 1)
label_movements <- c(unique(paste(database$label)))

# Descriptors computation
descriptors_database <- compute_descriptors_db(database, criteria = "forest")

### Sliding window

lab_or_no_lab <- rep(NA, length(filtered_data$new_time_axis))
name_lab <- rep(NA, length(filtered_data$new_time_axis))
confidence_level <- rep(NA, length(filtered_data$new_time_axis))
predicted_labels <- data.frame(filtered_data$new_time_axis, filtered_data$overall_left_acc, filtered_data$overall_right_acc, filtered_data$start_mov, lab_or_no_lab, name_lab, confidence_level)

ind_beg_seg_filter <- which(filtered_data$start_mov == - 1, arr.ind = TRUE)
if (1%in%ind_beg_seg_filter){
  ind_end_seg_filter <- ind_beg_seg_filter[-1] - 1 
} else {
  ind_end_seg_filter <- ind_beg_seg_filter - 1
}

for (k in 1 : min(length(ind_beg_seg_filter), length(ind_end_seg_filter))){
  left_segment_data <- filtered_data[ind_beg_seg_filter[k] : ind_end_seg_filter[k], 2]
  right_segment_data <- filtered_data[ind_beg_seg_filter[k] : ind_end_seg_filter[k], 3]
  n <- ind_beg_seg_filter[k]
  for (i in seq(1, length(left_segment_data), by=width_sliding_window/2)){
    left_window_data <- left_segment_data[i : (i + width_sliding_window)]
    right_window_data <- right_segment_data[i : (i + width_sliding_window)]
    descriptors <- compute_descriptors_window(left_window_data, right_window_data, width_sliding_window, descriptors_database, criteria = "forest")
    if (!(TRUE%in%is.na(descriptors))){
      label_confidence_level <- classify_forest_without_SW(descriptors_database, descriptors, no_labelled_threshold, tree_number)
      label <- label_confidence_level[1]
      confidence_level <- label_confidence_level[2]
      m <- 1
      while (((n + i + m - 2) <length(filtered_data$new_time_axis)) & (m < width_sliding_window)){
        predicted_labels$name_lab[n + i + m - 2] <- label
        predicted_labels$confidence_level[n + i + m - 2] <- confidence_level
        if (label == "no_labelled_motion"){
          predicted_labels$lab_or_no_lab[n + i + m - 2] <- "no_labelled"
        } else {
          predicted_labels$lab_or_no_lab[n + i + m - 2] <- "labelled"
        }
        m <- m + 1
      }
    }
  }
}
predicted_labels <- predicted_labels[- which(is.na(predicted_labels), arr.ind = TRUE)[,1], ]
colnames(predicted_labels) <- c("time", "left_acceleration", "right_acceleration", "start_mov", "lab_or_no_lab", "name_lab", "confidence_level")

#### Merging  function 
merged_labels <- merger(predicted_labels)
#Labelled and no labelled motion
result$lab_no_lab_plot <- show(merged_labels, real_motion, "no labelled filter")
#Different label motions
result$lab_plot <- show(merged_labels, real_motion, "labels")

# merged_labels_without_nolab <- merged_labels[- which(merged_labels$lab_or_no_lab=="no_labelled"), ]
# result$lab_plot_without_nolab <- show(merged_labels_without_nolab, real_motion, "labels")

### Count occurrences of labels
labels <- merged_labels[, 6] #To have just a list of labels to group and count
obtained_result <- count_occurrences(label_movements, labels)
result$obtained_result <- obtained_result

### Import of the expected result  
global_expect_results <- read.table("AlzCol_test_RES_thirdstep_time_line.txt", header = TRUE, sep = ",")

# We find the different movement labels in the sequence
tot_mov_names <- c(paste(global_expect_results$label))

# Table of occurrences and labels
expect_results <- count_occurrences(label_movements, tot_mov_names)

### Evaluation 
result$MAE <- evaluate_MAE(obtained_result, expect_results, database)
return(result)
}