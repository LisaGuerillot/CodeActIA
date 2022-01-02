#' @title Display of graphics
#' @usage show(data, real_data, step)
#' @description This function display the absolute sum of accelerations for each movement : the rhombus represent the motions found with the machine learning and the point represent the real motions.
#' @param data The table of motions found with machine learning
#' @param real_data The table of real motions
#' @param step The type of graph wanted according to the step of the treatment
#' @return Return a graph
#' @import ggplot2
#' @examples show(data, real_data, step)
#' @export

show <- function(data, real_data, step){

  abscissa <- data[, 1]
  ordinate <- abs(data[, 2]) + abs(data[, 3])
  ymax <- max(ordinate)

  if (step == "resampling"){  #### If we want a graph of the resampled data
    df <- data.frame(abscissa, ordinate)
    colnames(df) <- c("Time", "Acceleration")

    plot <- ggplot(df) + geom_point(aes(Time, Acceleration), color = "blue", shape = 20, size = 0.2) +
      xlab("Time (s)") + ylab("Sum of absolute right and left components of wrist acceleration (g)") +
      ylim(0, ymax) + ggtitle("Wrist absolute acceleration as a function of time")

  #}else if (step == "immobility and motion data"){ #### If we want a graph of the resampled data separated according to their nature : motion or immobility
    #df <- data.frame(abscissa, ordinate, data$immobility)
    #colnames(df) <-c("Time", "Acceleration", "Immobility")
    #plot <- ggplot(df) + geom_point(aes(Time, Acceleration, color = as.factor(Immobility)), shape = 20, size = 0.2) +
    #  scale_color_manual(values=c("orange", "blue")) +
    #  xlab("Time (s)") + ylab("Sum of absolute components of wrist acceleration (g)") +
    #  ylim(0, ymax) + ggtitle("Separtion of immobility and motion data")

  } else if (step == "immobility filter"){ #### If we want a graph of only the motion data (immobility deleted)
    df <- data.frame(abscissa, ordinate)
    colnames(df) <- c("Time", "Acceleration")
    plot <- ggplot(df) + geom_point(aes(Time, Acceleration), color = "blue", shape = 20, size = 0.2) +
      xlab("Time (s)") + ylab("Sum of absolute right and left components of wrist acceleration (g)") +
      ylim(0, ymax) + ggtitle("Acceleration without immobility data  as a function of time")

  } else if (step == "no labelled filter"){ ######## If we want a graph of the motion data separated according to their nature : labelled or no labelled
    df <- data.frame(abscissa, ordinate, data$lab_or_no_lab)
    colnames(df) <-c("Time", "Acceleration", "Labelled")
    plot <- ggplot(df) + geom_point(aes(Time, Acceleration, color = as.factor(Labelled)), shape = 20, size = 0.2) +
      scale_color_manual(values=c("orange", "blue")) +
      xlab("Time (s)") + ylab("Sum of absolute components of wrist acceleration (g)") +
      ylim(0, ymax) + ggtitle("Separation between labelled and no labelled data")

  } else if (step == "labels"){  #### If we want a graph of the motion data separated according to the motion their are part of
    df <- data.frame(abscissa, ordinate, data$name_lab)
    colnames(df) <-c("Time", "Acceleration", "Labels")
    abscissa_real <- real_data[, 1]
    ordinate_real <- (-1) * (abs(real_data[, 2]) + abs(real_data[, 3]))
    ymin <- min(ordinate_real)
    df_real <- data.frame(abscissa_real, ordinate_real, real_data$name_of_lab)
    colnames(df_real) <-c("Time", "Acceleration", "Labels")

    plot <- ggplot(df) + geom_point(aes(Time, Acceleration, color = as.factor(Labels)), shape = 23, size = 2) +
      scale_color_manual(values=c("orange", "blue", "black", "yellow", "green", "red", "pink", "purple", "grey", "brown", "magenta")) +
      xlab("Time (s)") +
      ylab("Sum of absolute components of wrist acceleration (g)") +
      ylim(ymin, ymax) +
      geom_point(data = df_real, aes(Time, Acceleration, color = as.factor(Labels)), shape = 20, size = 0.4) +
      ggtitle("Label motions realized by the patient")
  }
  return(plot)
}
