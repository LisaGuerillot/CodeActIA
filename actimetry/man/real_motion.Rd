% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/real_motion.R
\name{real_motion}
\alias{real_motion}
\title{Real motion}
\usage{
real_motion(real_time, resampled_accelerations)
}
\arguments{
\item{real_time}{list of the time of beginning and end for each motion}

\item{resampled_accelerations}{the table of resampled overall accelerations from the two wrist}
}
\value{
Return a table with the time, the left acceleration, the right acceleration and the real label
}
\description{
This function creates a table with the acceleration corresponding to the real done motion and attributes to each line, the right label.
}
\examples{
real_motion(real_time, resampled_accelerations)
}
