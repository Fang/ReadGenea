\name{read.bin}
\alias{read.bin}
\title{
Proceessing function for binary files.
}
\description{
A function to process binary accelerometer files and convert the information into R objects.
}
\usage{
read.bin(binfile, outfile = NULL, start = NULL, end = NULL, verbose = FALSE, do.temp = TRUE, calibrate = FALSE, gain = NULL, offset = NULL, luxv = NULL, voltv = NULL, tformat="seconds", warn=FALSE)
}
\arguments{
  \item{binfile}{
A filename of a binary file to process.
}
  \item{outfile}{
An optional filename specifying where to save the processed data object.
}
  \item{start}{Either:
A time representation of when the processing is to occur, see \code{\link{reformat.time}} for possible representations, or 
a page number corresponding to the start of the time window, or
a proportional point (between 0 and 1) to begin reading at.
}
  \item{end}{Either:
A time representation of when to end processing, see \code{\link{reformat.time}} for possible representations, or 
a page number corresponding to the end of the time window, or
a proportional point (between 0 and 1) to begin reading at.
}
  \item{verbose}{
	A boolean variable indicating whether information should be printed.
}
  \item{do.temp}{
	A boolean variable indicating whether the temperature signal should be extracted.
}
  \item{calibrate}{
	A boolean variable indicating whether the raw accelerometer values and the light variable should be calibrated.
}
  \item{gain}{
	A vector of 3 values for manual gain calibration of the raw (x,y,z) axes.  If \code{gain=NULL}, the gain calibration values are taken from within the output file itself.
}
  \item{offset}{
	A vector of 3 value for manual offset calibration of the raw (x,y,z) axes.  If \code{offset=NULL}, the offset calibration values are taken from within the output file itself.
}
  \item{luxv}{
	A value for manual lux calibration of the light meter.  If \code{luxv=NULL}, the lux calibration value is taken from within the output file itself.
}
  \item{voltv}{
	A value for manual volts calibration of the light meter.  If \code{voltv=NULL}, the volts calibration value is taken from within the output file itself.
}
 \item{tformat}{
        A character string indicating which (numerical) format the timestamps should be saved in.  See \code{\link{reformat.time}} for more details.
}
\item{warn}{
        A boolean variable indicating whether the function should warn you before reading in a large number of pages (.=500).  Defaults to \code{FALSE}.
}
\item{downsample}{
	Either an integer rate to downsample by (processing every \code{downsample} observation in the data), or a 2 variable vector giving the downsample rate and offset (otherwise defaulting to 1).
}
\item{blocksize}{
	Integer value giving maximum number of data pages to read in each pass. Defaults to infinite - smaller sizes will split very large data files to read chunk by chunk, reducing memory requirements. \code{blocksize = 1e4} seems often reasonable.
}

}
\details{
The function reads in the desired analysis time window specified by start and end.  These can either be numeric pages, or character timestamps.  The \code{start} and \code{end} variables are checked for consistency.  They are then converted to POSIXct objects and compared to the timestamps within \code{binfile}.  Accordingly, the relevant pages between \code{start} and \code{end} are processed using \code{\link{convert.hexstream}}.  Note that the processing is conservative, meaning that if a specified timestamp falls within the span of a page, that (whole) page is processed.  Note also that the function assumes a header in the binary file of a certain format, 59 lines long.  It also assumes the format of the header is of a standard format so that the calibration data can be extracted correctly.  The processed data object list is optionally saved to a file specified by \code{outfile}.
}
\value{A list of three components:
\item{data.out}{A 6 or 7 column matrix of the processed pages, the rows of which are the processed observations in order of processed pages.  The matrix has columns (timestamp,x-axis,y-axis,z-axis,light,button) or (timestamp,x-axis,y-axis,z-axis,light,button,temperature) if \code{do.temp=TRUE}.}
\item{page.timestamps}{The timestamps as POSIXct representations (as opposed to those within the \code{proc.file} array.}
\item{freq}{The effective sampling frequency (in Hz).}
}
\section{Warning}{
	\strong{Reading in an entire .bin file will take a long time if the file contains a lot of datasets.}
}
\seealso{
\code{\link{convert.hexstream}}, \code{\link{reformat.time}} 
}
\examples{

#Not run: process a binary file

#processedfile<-read.bin("binfile.txt","myprocessedfile",start=1,end=10,do.temp=TRUE,calibrate=TRUE)

#processedfile2<-read.bin("binfile2.txt",start="2010-10-02 12:32:01",end="2010-10-04 12:05:11",do.temp=TRUE,calibrate=TRUE)
}
\keyword{manip}
