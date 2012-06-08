\name{stft}
\alias{stft}
\title{
Computes Short Time Fourier Transforms
}
\description{
Processes a dataset, creating an object contained processed time-frequency analyses. These can then be plotted.
}
\usage{
stft(X, start=0, end=1, length=NULL,  time.format = c("auto"), type = c("mv", "svm", "sum"), mv.indices, date.col, reassign = TRUE,plot.it = FALSE,\dots)
}
\arguments{
  \item{X}{The dataset to be processed.}
  \item{start, end, length, time.format}{A specification for the segment to process, as in \code{\link{get.intervals}}.}
  \item{type}{The type of STFT to compute.}
  \item{mv.indices}{For \code{type = "mv"} or \code{type = "sum"}, the indices to process and the order to process them in.}
  \item{date.col}{logical. Whether the first column should be ignored and treated as a timestamp. If unset, is automatically chosen.}
  \item{reassign}{logical. If TRUE, compute the time-reassigned STFT. For \code{type \%in\% c("mv", "sum")}, this is done with the first coordinate in \code{mv.indices}.}
  \item{plot.it}{logical. Whether to plot the STFT immediately when processing is complete, using the default \code{plot.stft} options.}
  \item{\dots}{Additional optional arguments to control the STFT computation. These are: \cr\cr

  \code{win}: Window size in seconds for STFT computation. Increased window size mean better frequency resolution, but poorer time resolution. Defaults to 10 seconds.\cr

  \code{inc}: Increment between successive time steps for processing. Defaults to \code{win/2}. \cr

  \code{coef}: Number of fourier frequencies to compute. Small values will remove the higher frequencies from the processed object. Defaults to the maximum, \code{win/2}.\cr

  \code{wtype}: String giving the name of a window function, providing coefficients for filtering before processing. "hanning.window" is the default, with "uniform.window" also available.\cr

  \code{freq}: Sampling frequency of data set. If not given, is taken from X itself, or assumed to be 1 if unavailable.\cr

  \code{center}: If TRUE (Default), center the data in each window before processing is done. Useful for avoiding excessively large DC offset coefficients in results.\cr

  \code{calc.null}: If TRUE (Defaults to FALSE), compute a 'null' STFT by resampling the data completely, then doing a STFT.\cr

  \code{pvalues}: If TRUE (Defaults to FALSE) Compute bootstrapped pvalues for each position by resampling within each window and applying a wilcox test.\cr

  \code{time}: Allows the user to set an overriding timestamp vector to be used for processing.\cr

  \code{quiet}: If TRUE, suppress output.
}
}
\details{
This function accepts input in a variety of forms and computes short time fourier transforms to extract frequency structure from the data.
}

\value{
A converted date-time string in the specified format. In the case of "seconds", or "days", a numeric. For POSIX, a \code{\link{POSIXct}} object.
}
\seealso{
\code{\link{convert.time}}, \code{\link{get.intervals}}
}
\examples{

t1 = parse.time("2012-06-21 13:04:01"); print(t1)
parse.time("21/06/12 13:04:01") #gives the same result

parse.time(c("19/07/70", "20/07/70"), format = "days")
#results here will depend on your locale
parse.time(c("19/07/70", "20/07/70"), format = "POSIX", tzone = -4)

#one is the same day, one can only find a match the next day
parse.time("13:05", start = t1) - t1
parse.time("13:00", start = t1) - t1
#asking to wait 1 midnight means both times are considered as 
#times on the same, full day of data
parse.time(c("1 13:05", "1 13:00"), start = t1) - t1
#2012-06-21 is a Thursday, so this is equivalent
parse.time(c("Fri 13:05", "Fri 13:00"), start = t1) - t1
#Longer form days of the week are also understood. Note that 
#the first day does not get matched.
parse.time(c("Thursday 13:05", "Thursday 13:00"), start = t1) - t1

}
\keyword{manip}
