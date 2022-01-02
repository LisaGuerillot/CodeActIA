
#################################### Overall function without sliding window

global_without_SW <- function(criteria, left_wrist, right_wrist, resampling_frequency, immobility_threshold, immobility_length_threshold, no_labelled_threshold, time_threshold, min_motion_time, tree_number){

  # Deduced inputs
  min_motion_length <- min_motion_time * resampling_frequency

  ### Resampling of data
  resampled_accelerations <- resample(left_wrist, right_wrist, resampling_frequency)

  ### Immobility filtering
  filtered_data <- immobility_withdrawal_simplified(resampled_accelerations, resampling_frequency, immobility_threshold, immobility_length_threshold, time_threshold, min_motion_length)

  ### Database descriptors computation
  # Database import
  file3 = "actimetry_database.csv"
  database <- read.csv(file = file3, sep = ",", header = FALSE, col.names = c("Time", "Xright", "Yright", "Zright", "NormRight", "Xleft", "Yleft", "Zleft", "NormLeft", "label", "idSubject", "idMove", "id"), skip = 1)
  label_movements <- c(unique(paste(database$label)))

  # Descriptors computation
  descriptors_database <- compute_descriptors_db(database, criteria)

  if (criteria == "distance"){
    # Descriptors normalization
    left_mean <- rep(0,10); right_mean <- rep(0,10); left_median <- rep(0,10); right_median <- rep(0,10); left_var <- rep(0,10); right_var <- rep(0,10); left_Q1 <- rep(0,10); right_Q1 <- rep(0,10); left_Q3 <- rep(0,10); right_Q3 <- rep(0,10); left_kurto <- rep(0,10); right_kurto <- rep(0,10); left_skew <- rep(0,10); right_skew <- rep(0,10)
    normalized_descriptors_database <- data.frame(label_movements, left_mean, right_mean, left_median, right_median, left_var, right_var, left_Q1, right_Q1, left_Q3, right_Q3, left_kurto, right_kurto, left_skew, right_skew)
    for (j in 2: ncol(descriptors_database)){
      normalized_descriptors_database[, j] <- descriptors_database[, j]/max(descriptors_database[, j])
    }
    descriptors_database <- normalized_descriptors_database
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
    width <- ind_end_seg_filter[k] - ind_beg_seg_filter[k]
    descriptors <- compute_descriptors_window(left_segment_data, right_segment_data, width, descriptors_database, criteria)
    if (!(TRUE%in%is.na(descriptors))){
      label <- classify_without_SW(descriptors_database, descriptors, label_movements, no_labelled_threshold, tree_number, criteria)
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

  return(MAE)
}

#################################### Overall function with sliding window for forest

global_forest_with_SW <- function(left_wrist, right_wrist, resampling_frequency, immobility_threshold, immobility_length_threshold, no_labelled_threshold, time_threshold, min_motion_time, window_second_length, tree_number){

  # Deduced inputs
  width_sliding_window <- window_second_length * resampling_frequency
  min_motion_length <- min_motion_time * resampling_frequency

  ### Resampling of data
  resampled_accelerations <- resample(left_wrist, right_wrist, resampling_frequency)

  ### Immobility filtering
  filtered_data <- immobility_withdrawal_simplified(resampled_accelerations, resampling_frequency, immobility_threshold, immobility_length_threshold, time_threshold, min_motion_length)

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
  predicted_labels <- data.frame(filtered_data$new_time_axis, filtered_data$start_mov, lab_or_no_lab, name_lab, confidence_level)

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
  colnames(predicted_labels) <- c("time", "start_mov", "lab_or_no_lab", "name_lab", "confidence_level")


  #### Merging  function
  merged_labels <- merger(predicted_labels)

  ### Count occurrences of labels
  labels <- merged_labels[, 4] #To have just a list of labels to group and count
  obtained_result <- count_occurrences(label_movements, labels)

  ### Import of the expected result
  global_expect_results <- read.table("AlzCol_test_RES_thirdstep_time_line.txt", header = TRUE, sep = ",")

  # We find the different movement labels in the sequence
  tot_mov_names <- c(paste(global_expect_results$label))

  # Table of occurrences and labels
  expect_results <- count_occurrences(label_movements, tot_mov_names)

  ### Evaluation
  MAE <- evaluate_MAE(obtained_result, expect_results, database)

  return(MAE)
}

#################################### Overall function with sliding window for distance

global_distance_with_SW <- function(left_wrist, right_wrist, resampling_frequency, labelled_max_distance, immobility_threshold, immobility_length_threshold, time_threshold, min_motion_time, window_second_length){

  # Deduced inputs
  width_sliding_window <- window_second_length * resampling_frequency
  min_motion_length <- min_motion_time * resampling_frequency

  ### Resampling of data
  resampled_accelerations <- resample(left_wrist, right_wrist, resampling_frequency)

  ### Immobility filtering
  filtered_data <- immobility_withdrawal_simplified(resampled_accelerations, resampling_frequency, immobility_threshold, immobility_length_threshold, time_threshold, min_motion_length)

  ### Database descriptors computation
  # Database import
  file3 = "actimetry_database.csv"
  database <- read.csv(file = file3, sep = ",", header = FALSE, col.names = c("Time", "Xright", "Yright", "Zright", "NormRight", "Xleft", "Yleft", "Zleft", "NormLeft", "label", "idSubject", "idMove", "id"), skip = 1)
  label_movements <- c(unique(paste(database$label)))

  # Descriptors computation
  descriptors_database <- compute_descriptors_db(database, criteria = "distance")

  # Descriptors normalization
  left_mean <- rep(0,10); right_mean <- rep(0,10); left_median <- rep(0,10); right_median <- rep(0,10); left_var <- rep(0,10); right_var <- rep(0,10); left_Q1 <- rep(0,10); right_Q1 <- rep(0,10); left_Q3 <- rep(0,10); right_Q3 <- rep(0,10); left_kurto <- rep(0,10); right_kurto <- rep(0,10); left_skew <- rep(0,10); right_skew <- rep(0,10)
  normalized_descriptors_database <- data.frame(label_movements, left_mean, right_mean, left_median, right_median, left_var, right_var, left_Q1, right_Q1, left_Q3, right_Q3, left_kurto, right_kurto, left_skew, right_skew)
  for (j in 2: ncol(descriptors_database)){
    normalized_descriptors_database[, j] <- descriptors_database[, j]/max(descriptors_database[, j])
  }
  ### Sliding window

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
    n <- ind_beg_seg_filter[k]
    distances_table <- c()
    for (i in seq(1, length(left_segment_data), by=width_sliding_window/2)){
      left_window_data <- left_segment_data[i : (i + width_sliding_window)]
      right_window_data <- right_segment_data[i : (i + width_sliding_window)]
      descriptors <- compute_descriptors_window(left_window_data, right_window_data, width_sliding_window, descriptors_database, criteria = "distance")
      if (!(TRUE%in%is.na(descriptors))){
        descriptors_distances <- compute_distances(descriptors, normalized_descriptors_database, label_movements, labelled_max_distance) # distance between 1 vector of window descriptors and all the database descriptors vectors
        distances_table <- rbind(distances_table, descriptors_distances)
      }
    }
    rownames(distances_table) <- c()
    label <- classify_distance_SW(distances_table, label_movements)
    for (j in ind_beg_seg_filter[k] : ind_end_seg_filter[k]){
      if (length(label) == 0){
        predicted_labels$lab_or_no_lab[j] <- "no_labelled"
        predicted_labels$name_lab[j] <- "no_labelled_motion"
      } else {
        predicted_labels$lab_or_no_lab[j] <- "labelled"
        predicted_labels$name_lab[j] <- label
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

  return(MAE)
}


