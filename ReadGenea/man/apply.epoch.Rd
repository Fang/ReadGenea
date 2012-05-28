\name{apply.epoch}
\alias{apply.epoch, sd.epoch, mean.epoch}
\title{
Compute epochal summary statistics.
}
\description{
Computes epochal summary statistics for an "AccData" object, matrix, or vector, and collates into a matrix or vector.
}
\usage{
apply.epoch(obj, epoch.size=10, FUN, incl.date = FALSE)
mean.epoch(obj, epoch.size=10, incl.date = FALSE)
sd.epoch(obj, epoch.size=10, incl.date = FALSE)
median.epoch(obj, epoch.size=10, incl.date = FALSE)
mad.epoch(obj, epoch.size=10, incl.date = FALSE)
autocor.epoch(obj, epoch.size=10, lag = 1, incl.date = FALSE)
quantile.epoch(obj, epoch.size = 10, quantiles= c(0.1, 0.25, 0.5, 0.75, 0.9), incl.date = FALSE)
svm(obj)
}
\arguments{
  \item{obj}{The object to compute statistics for. Can be an "AccData" object, a matrix, or a vector.}
  \item{epoch.size}{Numeric giving intervals to consider and aggregate. For "AccData" \code{obj} taken as seconds. Otherwise, considered as rows, or as individual readings.}
  \item{incl.date}{logical. If TRUE, include a column of times or original indices with the results.}
  \item{FUN}{A function to be applied to each epoch.}
  \item{lag}{Autocorrelation lag to compute.}
  \item{quantiles}{Sample quantiles of SVM to compute.}
}
\details{
These functions compute epochal summary statistics for "AccData" objects, matrices and vectors.

\code{apply.epoch} is the general function - according to the size of \code{epoch.size}, it splits up the obj into collections of consecutive rows, each with the same size. These are then successively supplied to \code{FUN} as its first argument. If the result of FUN is a single value, then the results are concatenated into a vector output. Otherwise, an array is formed with each row corresponding to a single epochal group. For AccData, the sampling frequency of the dataset is used to interpret the epoch size in seconds. Otherwise, the raw record indices are used. If incl.date is set, the original timestamp vector of the data, or the original indices, are downsampled and included as the first column of the output.

The remaining functions are wrappers that compute various commonly useful statistics -- in particular, applied to "AccData" objects and arrays, they compute the epochal SVM mean, standard deviation, median, median absolute deviation, and autocorrelation, and sample quantiles respectively. (Arrays are treated as each column representing the x, y, and z components respectively.) Applied to vector inputer, processing will occur without the SVM calculation.

\code{svm} acts identically to 'mean.epoch', with the epoch set to the sampling period. In other words, it computes the instantaneous sum of vector magnitudes of the acceleration at each record point.
}
\value{
A vector or array giving the computed epochal summaries, with optionally a time column added.
}

\seealso{
\code{\link{plot.AccData}}, \code{\link{summary.AccData}}, \code{\link{aggregate}}, \code{\link{acf}}
}
\examples{

dat <- read.bin(system.file("binfile/TESTfile.bin", package = "ReadGenea")[1], calibrate = TRUE)

plot(dat[,1], svm(dat), log = "y")
lines(mean.epoch(dat, incl.date = T))

#these following should give all the same results, but by different ways
lines(mean.epoch(dat, epoch.size = 60, incl.date = T), col = 2)
lines(apply.epoch(dat, epoch.size = 60, FUN = function(A) mean(svm(A)), incl.date = TRUE), col = 3)
lines(apply.epoch(dat, epoch.size = 60, FUN = function(A) sum(A^2) / nrow(A), incl.date = TRUE), col = 4)

#plot some statistics
par(mfrow = c(5,1))
plot(sd.epoch(dat), type="l")
plot(median.epoch(dat), type= "l")
plot(mad.epoch(dat), type= "l")
plot(autocor.epoch(dat), type= "l")
tmp = quantile.epoch(dat, quantiles= c(0.1, 0.25, 0.5, 0.75, 0.9)); matplot(tmp, type = "l")


}
\keyword{manip}
