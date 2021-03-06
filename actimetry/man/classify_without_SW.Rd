% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/classify_without_SW.R
\name{classify_without_SW}
\alias{classify_without_SW}
\title{classifier}
\usage{
classify(normalized_descriptors_database, descriptors, label_movements, no_labelled_threshold, criteria)
}
\arguments{
\item{normalized_descriptors_database}{the table of database descriptors}

\item{descriptors}{the descriptors of the segment}

\item{label_movements}{the list of movement labels}

\item{no_labelled_threshold}{the threshold which determines the no labelled movement}

\item{tree_number}{the number of tree in the random forest for the "forest" criteria}

\item{criteria}{the criteria to  choose the classifier: "distance", or "forest"}
}
\value{
Return the label corresponding to this movement according to a criteria
}
\description{
This function classifies the data segment and finds the label according to a criteria without using a sliding window
}
\examples{
classify(database, descriptors, labels, no_labelled_threshold, "distance")
}
