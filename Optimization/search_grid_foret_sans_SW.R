# Libraries to use

library(actimetry)
library(tidyr)
setwd("C:/Users/lisag/Documents/Scolaire/EPF/2021-2022/Stage Euromov/Activites/package de la fenetre glissante/Variation hyperparamètres")
source("global_functions_to_search_grid.R")

### Data import
file1 = "AlzCol_test_right_wrist_42977.csv"
file2 = "AlzCol_test_left_wrist_42164.csv"

left_wrist <- read.csv(file = file2, sep = ",", header = FALSE, col.names = c("Time", "X", "Y", "Z"))
right_wrist <- read.csv(file = file1, sep = "," , header = FALSE, col.names = c("Time", "X", "Y", "Z"))

# Hyperparameters to vary

#resampling_frequency <- c(50, 100, 150) 
criteria <- "forest"
resampling_frequency <- 50
immobility_threshold <- c(0.2, 0.3, 0.5, 0.6, 0.61, 0.63, 0.65, 0.67, 0.69, 0.7)
immobility_length_threshold <- 1
no_labelled_threshold <- c(0.2, 0.22, 0.24, 0.26, 0.28, 0.3, 0.35, 0.4, 0.5, 0.6, 0.7)
time_threshold <- 1
#tree_number <- c(1, 100, 300, 500, 700, 900, 1000)
tree_number <- 700
min_motion_time <- 2

# Search grid

varied_hyperparameters <- crossing(resampling_frequency, immobility_threshold, immobility_length_threshold, no_labelled_threshold, time_threshold, tree_number, min_motion_time)
search_grid <- c()
for (i in 1:length(varied_hyperparameters$resampling_frequency)){
  res <- global_without_SW(criteria = criteria, left_wrist, right_wrist, resampling_frequency = varied_hyperparameters$resampling_frequency[i], immobility_threshold = varied_hyperparameters$immobility_threshold[i], immobility_length_threshold = varied_hyperparameters$immobility_length_threshold[i], no_labelled_threshold = varied_hyperparameters$no_labelled_threshold[i], time_threshold = varied_hyperparameters$time_threshold[i], min_motion_time = varied_hyperparameters$min_motion_time[i], tree_number = varied_hyperparameters$tree_number[i])
  print(res)
  search_grid <- c(search_grid, res)
}

# Minimal error in the search grid

min(search_grid)

# Optimal hyperparameters : hyperparameters which are those which obtain the minimal error

grid_res <- cbind(varied_hyperparameters, search_grid)
final_res <- grid_res[grid_res$search_grid == min(search_grid), -5]
final_res
