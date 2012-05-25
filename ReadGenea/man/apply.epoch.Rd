\name{apply.epoch}
\alias{apply.epoch, sd.epoch, mean.epoch}
\title{
Compute epochal summary statistics.
}
\description{
Computes epochal summary statistics for an "AccData" object, matrix, or vector, and collates into a matrix or vector.
}
\usage{
apply.epoch(obj, epoch=10, incl.date = FALSE, FUN){
}
\arguments{
  \item{obj}{The object to compute statistics for. Can be an "AccData" object, a matrix, or a vector.}
  \item{epoch}{Numeric giving intervals to consider and aggregate. For "AccData" \code{obj} taken as seconds. Otherwise, considered as rows, or as individual readings.}
}
\details{
	The function extracts useful information from a .bin file, such as information about the Genea device used to produce the output, and characteristics of the subject who wore the device. The function also accepts data that has been compressed in `gzip', `bzip2' or `xz' formats. See \code{file}.
	With \code{more} set to TRUE, additional data is extracted, mainly for internal use in \code{read.bin}.
}
\value{
A \code{data.frame} with extracted header information, each row a particular header field with its value. 
If \code{more} is TRUE, an attribute "calibration" is attached to the object, consisting of a list with measurement offsets, sampling frequency estimates, start times and time zones, data position offsets, and if mmap is detected, byte locations and increments for mmap reading.
}

\section{Warning}{
This function is specific to header structure in Geneactiv output files. By design, it should be compatible with all firmware and software versions to date (as of version of current release). If order or field names are changed in future .bin files, this function may have to be updated appropriately.
The function works by looking for appropriate section headings in the .bin files.
} 
\seealso{
\code{\link{read.bin}}
}
\examples{

fileheader <- header.info(system.file("binfile/TESTfile.bin", package = "ReadGenea")[1], more = TRUE)
print(fileheader)
attr(fileheader, "calibration")
}
\keyword{manip}
